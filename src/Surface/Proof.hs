{-# LANGUAGE GADTs, ImplicitParams #-}
module Surface.Proof where

import Context
import Control.State
import Control.Monad hiding (fail)
import Control.Monad.Free.Freer
import Data.Foldable (for_)
import Data.Functor.Classes
import Data.Functor.Foldable hiding (Mu, Nil)
import qualified Data.HashMap.Lazy as H
import Data.List (intercalate, union, (\\))
import Data.Result
import Expr
import GHC.Stack
import Judgement
import Module
import Prelude hiding (fail)
import Surface.Binder
import Text.Pretty

data ProofF a where
  J :: HasCallStack => Judgement a -> ProofF a
  S :: State ProofState a -> ProofF a
  R :: Result a -> ProofF a

type Proof = Freer ProofF

data ProofState = ProofState
  { proofNextName :: Name
  , proofContext :: Context
  , proofEnvironment :: Environment }
  deriving (Eq, Show)

type Environment = H.HashMap Name Binding

data Binding = Binding { bindingType :: Type, bindingValue :: Expr }
  deriving (Eq, Show)


-- Judgement constructors

checkModule :: HasCallStack => Module -> Proof ()
checkModule module' = J (CheckModule module') `Then` return

checkDeclaration :: HasCallStack => Module -> Declaration -> Proof ()
checkDeclaration module' declaration = J (CheckDeclaration module' declaration) `Then` return

checkConstructor :: HasCallStack => Module -> Declaration -> Constructor -> Proof ()
checkConstructor module' declaration constructor = J (CheckConstructor module' declaration constructor) `Then` return


check :: HasCallStack => Term -> Type -> Proof ()
check term ty = J (Check term ty) `Then` return

infer :: HasCallStack => Term -> Proof Type
infer term = J (Infer term) `Then` return


isType :: HasCallStack => Term -> Proof ()
isType term = J (IsType term) `Then` return


alphaEquivalent :: HasCallStack => Expr -> Expr -> Proof Bool
alphaEquivalent e1 e2 = J (AlphaEquivalent e1 e2) `Then` return

equate :: HasCallStack => Expr -> Expr -> Proof ()
equate e1 e2 = J (Equate e1 e2) `Then` return


unify :: HasCallStack => Type -> Type -> Proof ()
unify t1 t2 = J (Unify t1 t2) `Then` return

solve :: HasCallStack => Name -> Suffix -> Type -> Proof ()
solve name suffix ty = J (Solve name suffix ty) `Then` return


fresh :: HasCallStack => Maybe Expr -> Proof Name
fresh declaration = J (Fresh declaration) `Then` return

restore :: HasCallStack => Proof Extension
restore = J Judgement.Restore `Then` return

replace :: HasCallStack => Suffix -> Proof Extension
replace suffix = J (Judgement.Replace suffix) `Then` return


normalize :: HasCallStack => Expr -> Proof Expr
normalize expr = J (Normalize expr) `Then` return

whnf :: HasCallStack => Expr -> Proof Expr
whnf expr = J (WHNF expr) `Then` return


-- State constructors

get :: Proof ProofState
get = S Get `Then` return

gets :: (ProofState -> result) -> Proof result
gets f = fmap f get

put :: ProofState -> Proof ()
put s = S (Put s) `Then` return

modify :: (ProofState -> ProofState) -> Proof ()
modify f = get >>= put . f


-- Result constructors

fail :: HasCallStack => String -> Proof a
fail message = let ?callStack = modifyCallStack (filter ((/= "J") . fst)) callStack in
  wrap (R (Error [ message, prettyCallStack callStack ]))
  where modifyCallStack f = fromCallSiteList . f . getCallStack


-- Proof evaluation

initialState :: ProofState
initialState = ProofState (I 0) Nil H.empty

run :: HasCallStack => Proof a -> Result a
run = runAll initialState

runAll :: HasCallStack => ProofState -> Proof a -> Result a
runAll context proof = case runStep context proof of
  Left result -> result
  Right next -> uncurry runAll next

runSteps :: HasCallStack => ProofState -> Proof a -> [(ProofState, Proof a)]
runSteps context proof = let ?callStack = popCallStack callStack in (context, proof) : case runStep context proof of
  Left result -> [ (context, R result `Then` return) ]
  Right next -> uncurry runSteps next

-- | Like runSteps, but filtering out gets and puts.
runSteps' :: HasCallStack => ProofState -> Proof a -> [(ProofState, Proof a)]
runSteps' context = filter isSignificant . runSteps context
  where isSignificant = iterFreer (\ p _ -> case p of { S _ -> False ; _ -> True }) . (True <$) . snd

runStep :: HasCallStack => ProofState -> Proof a -> Either (Result a) (ProofState, Proof a)
runStep context proof = let ?callStack = popCallStack callStack in case proof of
  Return a -> Left $ Result a
  Then proof cont -> case proof of
    J judgement -> Right (context, decompose judgement >>= cont)
    S state -> case state of
      Get -> Right (context, cont context)
      Put context' -> Right (context', cont ())
    R result -> case result of
      Error e -> Left (Error e)
      Result a -> Right (context, cont a)


decompose :: HasCallStack => Judgement a -> Proof a
decompose judgement = let ?callStack = popCallStack callStack in case judgement of
  CheckModule module' -> checkModule' module'
  CheckDeclaration m d -> checkDeclaration' m d
  CheckConstructor m d c -> checkConstructor' m d c

  Check term ty -> check' term ty
  Infer term -> infer' term
  IsType ty -> isType' ty

  AlphaEquivalent e1 e2 -> alphaEquivalent' e1 e2
  Equate e1 e2 -> equate' e1 e2

  Unify t1 t2 -> unify' t1 t2
  Solve name suffix ty -> solve' name suffix ty

  Fresh declaration -> fresh' declaration
  Judgement.Restore -> restore'
  Judgement.Replace suffix -> replace' suffix

  Normalize expr -> normalize' expr
  WHNF expr -> whnf' expr


-- Judgement interpreters

checkModule' :: HasCallStack => Module -> Proof ()
checkModule' module' = let ?callStack = popCallStack callStack in do
  for_ (moduleDeclarations module') addBindings
  for_ (moduleDeclarations module') (checkDeclaration module')

checkDeclaration' :: HasCallStack => Module -> Declaration -> Proof ()
checkDeclaration' mod@(Module modName _) decl = let ?callStack = popCallStack callStack in do
  isType (declarationType decl)
  env <- getEnvironment
  let ty = declarationType decl
  context [ pretty (declarationName decl) ] $ case decl { declarationType = generalize (freeVariables ty \\ H.keys env) ty } of
    Declaration _ ty term -> check term ty
    Data _ _ constructors -> for_ constructors (checkConstructor mod decl)
  where context cs = contextualizeErrors (fmap ((intercalate "." (modName : cs) ++ ": ") ++))

checkConstructor' :: HasCallStack => Module -> Declaration -> Constructor -> Proof ()
checkConstructor' _ decl (Constructor _ sig) = let ?callStack = popCallStack callStack in do
  modifyContext (:< Sep)
  env <- getEnvironment
  tyVariables <- traverse (fresh . Just) (domain (declarationType decl))
  flip (foldr (>-)) (fmap (T . (::: typeT)) (freeVariables sig \\ H.keys env)) $ do
    isType sig
    equate (codomain sig) (foldl (#) (var (declarationName decl)) (fmap var tyVariables))
  _ <- skimContext []
  return ()


check' :: HasCallStack => Term -> Type -> Proof ()
check' term ty = let ?callStack = popCallStack callStack in case (unfix term, unfix ty) of
  (Abs n body, Pi n1 t tbody) -> T (n1 ::: t) >- T (n ::: t) >- check body tbody

  (Var name@N{}, _) -> do
    ty' <- findTyping name
    unify ty' ty

  (Pair a b, Product t1 t2) -> check a t1 >> check b t2

  (InL l, Sum t1 _) -> check l t1
  (InR r, Sum _ t2) -> check r t2

  _ -> do
    ty' <- infer term
    unify ty ty'

infer' :: HasCallStack => Term -> Proof Type
infer' term = let ?callStack = popCallStack callStack in case unfix term of
  Pair x y -> (.*.) <$> infer x <*> infer y

  Fst p -> var . fst <$> inferPair p
  Snd p -> var . snd <$> inferPair p

  InL l -> do
    a <- infer l
    b <- fresh Nothing
    return (a .+. var b)

  InR r -> do
    a <- fresh Nothing
    b <- infer r
    return (var a .+. b)

  Case subject ifL ifR -> do
    ty <- infer subject
    l <- fresh Nothing
    r <- fresh Nothing
    unify ty (var l .+. var r)
    b <- fresh Nothing
    tl <- infer ifL
    tr <- infer ifR
    unify tl (var l .->. var b)
    unify tr (var r .->. var b)
    return (var b)

  Unit -> return unitT

  Var name -> findTyping name >>= specialize

  Abs name body -> do
    a <- fresh Nothing
    v <- T (name ::: var a) >- infer body
    return (var a .->. v)

  App f arg -> do
    a <- infer arg
    b <- fresh Nothing
    check f (a .->. var b)
    return (var b)

  -- Types
  UnitT -> return typeT
  Type -> return typeT -- Impredicativity.
  Product{} -> isType term >> return typeT
  Sum{} -> isType term >> return typeT

  Pi name ty body -> inferDType name ty body
  Mu name ty body -> do
    isType ty
    T (name ::: ty) >- check body ty
    return ty
  Sigma name ty body -> inferDType name ty body

  Let name value body -> do
    t <- generalizeOver (infer value)
    T (name ::: t) >- infer body

  As term ty -> do
    a <- fresh (Just ty)
    inferred <- infer term
    unify inferred (var a)
    return ty
  where inferPair term = do
          ty <- infer term
          a <- fresh Nothing
          b <- fresh Nothing
          unify ty (var a .*. var b)
          return (a, b)

        inferDType name ty body = do
          result <- T (name ::: ty) >- infer body
          isType result
          return typeT


isType' :: HasCallStack => Term -> Proof ()
isType' ty = let ?callStack = popCallStack callStack in case unfix ty of
  UnitT -> return ()
  Type -> return ()
  Sum a b -> do
    isType a
    isType b
  Product a b -> do
    isType a
    isType b

  Pi name ty body -> do
    isType ty
    T (name ::: ty) >- isType body

  Var name -> do
    def <- lookupDefinition name
    case def of
      Just ty' -> isType ty'
      _ -> findTyping name >>= isType

  App f arg -> do
    a <- infer arg
    b <- fresh Nothing
    check f (a .->. var b)
    isType (var b)

  _ -> fail ("Expected a Type but got " ++ pretty ty)


alphaEquivalent' :: HasCallStack => Expr -> Expr -> Proof Bool
alphaEquivalent' e1 e2
  | e1 == e2 = return True
  | otherwise = let ?callStack = popCallStack callStack in case (unfix e1, unfix e2) of
    (Abs n1 b1, Abs n2 b2)
      | n1 == n2 -> alphaEquivalent b1 b2
      | otherwise -> let new = var (freshNameIn (n1 : n2 : freeVariables b1 `union` freeVariables b2)) in
        alphaEquivalent (substitute new n1 b1) (substitute new n2 b2)
    (Pi n1 t1 b1, Pi n2 t2 b2) -> let new = var (freshNameIn (n1 : n2 : freeVariables b1 `union` freeVariables b2)) in
      alphaEquivalent t1 t2 >> alphaEquivalent (substitute new n1 b1) (substitute new n2 b2)
    (Let n1 v1 b1, Let n2 v2 b2) -> let new = var (freshNameIn (n1 : n2 : freeVariables b1 `union` freeVariables b2 `union` freeVariables v1 `union` freeVariables v2)) in
      alphaEquivalent (substitute new n1 v1) (substitute new n2 v2) >> alphaEquivalent (substitute new n1 b1) (substitute new n2 b2)

    (Var n1, Var n2) -> return (n1 == n2)

    (a1, a2) -> case zipExprFWith (==) alphaEquivalent a1 a2 of
      Just equivalences -> do
        eq <- sequenceA equivalences
        return (and eq)
      _ -> return False


equate' :: HasCallStack => Expr -> Expr -> Proof ()
equate' e1 e2 = let ?callStack = popCallStack callStack in do
  equivalent <- alphaEquivalent e1 e2
  unless equivalent $ do
    nf1 <- whnf e1
    nf2 <- whnf e2
    case zipExprFWith (,) equate (unfix nf1) (unfix nf2) of
      Just _ -> return ()
      _ -> fail ("Could not judge equality of " ++ pretty e1 ++ " to " ++ pretty e2)


unify' :: HasCallStack => Type -> Type -> Proof ()
unify' t1 t2 = let ?callStack = popCallStack callStack in unless (t1 == t2) $ case (unfix t1, unfix t2) of
  (Product a1 b1, Product a2 b2) -> unify a1 a2 >> unify b1 b2
  (Sum a1 b1, Sum a2 b2) -> unify a1 a2 >> unify b1 b2
  (UnitT, UnitT) -> return ()
  (Type, Type) -> return ()

  (Abs _ b1, Abs _ b2) -> unify b1 b2 -- this should probably be pushing unknown declarations onto the context
  (Pi _ t1 b1, Pi _ t2 b2) -> unify t1 t2 >> unify b1 b2 -- this should probably be pushing typing constraints onto the context

  (Var name@N{}, _) -> do
    def <- lookupDefinition name
    case def of
      Just d -> unify d t2
      Nothing -> cannotUnify
  (_, Var name@N{}) -> do
    def <- lookupDefinition name
    case def of
      Just d -> unify t1 d
      Nothing -> cannotUnify

  (Var v1, Var v2) -> onTop $ \ (n := d) ->
    case (n == v1, n == v2, d) of
      (True, True, _) -> restore
      (True, False, Nothing) -> replace [ v1 := Just (var v2) ]
      (False, True, Nothing) -> replace [ v2 := Just (var v1) ]
      (True, False, Just t) -> unify t2 t >> restore
      (False, True, Just t) -> unify t1 t >> restore
      (False, False, _) -> unify t1 t2 >> restore
  (Var v, _) -> solve v [] t2
  (_, Var v) -> solve v [] t1
  (App a1 b1, App a2 b2) -> unify a1 a2 >> unify b1 b2

  (InL l1, InL l2) -> unify l1 l2
  (InR r1, InR r2) -> unify r1 r2
  (Case c1 l1 r1, Case c2 l2 r2) -> unify c1 c2 >> unify l1 l2 >> unify r1 r2

  (Pair a1 b1, Pair a2 b2) -> unify a1 a2 >> unify b1 b2
  (Fst p1, Fst p2) -> unify p1 p2
  (Snd p1, Snd p2) -> unify p1 p2

  (Unit, Unit) -> return ()

  _ -> cannotUnify
  where cannotUnify = let ?callStack = popCallStack callStack in fail ("Cannot unify " ++ pretty t1 ++ " with " ++ pretty t2)

solve' :: HasCallStack => Name -> Suffix -> Type -> Proof ()
solve' name suffix ty = let ?callStack = popCallStack callStack in onTop $ \ (n := d) ->
  case (n == name, n <? ty || n <? suffix, d) of
    (True, True, _) -> fail "Occurs check failed."
    (True, False, Nothing) -> replace (suffix ++ [ name := Just ty ])
    (True, False, Just v) -> do
      modifyContext (<>< suffix)
      unify v ty
      restore
    (False, True, _) -> do
      solve name (n := d : suffix) ty
      replace []
    (False, False, _) -> do
      solve name suffix ty
      restore


fresh' :: HasCallStack => Maybe Expr -> Proof Name
fresh' d = let ?callStack = popCallStack callStack in do
  s <- get
  let m = proofNextName s
  put s { proofNextName = succName m
        , proofContext = proofContext s :< D (m := d) }
  return m

restore' :: HasCallStack => Proof Extension
restore' = let ?callStack = popCallStack callStack in return Context.Restore

replace' :: HasCallStack => Suffix -> Proof Extension
replace' = let ?callStack = popCallStack callStack in return . Context.Replace


normalize' :: HasCallStack => Expr -> Proof Expr
normalize' expr = let ?callStack = popCallStack callStack in case unfix expr of
  Var name -> do
    binding <- lookupDefinition name
    case binding of
      Just term -> return term
      Nothing -> return (var name)

  Abs name body -> do
    declare (name := Nothing)
    makeLambda name <$> normalize body

  App op arg -> do
    Fix o <- normalize op
    a <- normalize arg
    case o of
      Abs name body -> do
        declare (name := Just a)
        normalize body
      Var v -> return (var v # a)
      _ -> error ("Application of non-abstraction value: " ++ pretty o)

  InL l -> inL <$> normalize l
  InR r -> inR <$> normalize r
  Case subject ifL ifR -> do
    Fix s <- normalize subject
    case s of
      InL l -> do
        i <- normalize ifL
        normalize (i # l)
      InR r -> do
        i <- normalize ifR
        normalize (i # r)
      _ -> error ("Case expression on non-sum value: " ++ pretty s)

  Pair a b -> pair <$> normalize a <*> normalize b

  Fst p -> do
    Fix p' <- normalize p
    case p' of
      Pair a _ -> return a
      _ -> error ("fst applied to non-product value: " ++ pretty p')

  Snd p -> do
    Fix p' <- normalize p
    case p' of
      Pair _ b -> return b
      _ -> error ("snd applied to non-product value: " ++ pretty p')

  Product a b -> (.*.) <$> normalize a <*> normalize b
  Sum a b -> (.+.) <$> normalize a <*> normalize b

  Let name value body -> do
    v <- normalize value
    declare (name := Just v)
    normalize body

  _ -> return expr


whnf' :: HasCallStack => Expr -> Proof Expr
whnf' expr = let ?callStack = popCallStack callStack in case unfix expr of
  Var v -> do
    binding <- lookupDefinition v
    case binding of
      Just a -> whnf a
      _ -> return (var v)

  App a b -> do
    op <- whnf a
    case unfix op of
      Abs name body -> whnf (substitute b name body)
      _ -> return (op # b)

  Fst p -> do
    pair <- whnf p
    case unfix pair of
      Pair a _ -> whnf a
      _ -> return (fst' pair)

  Snd p -> do
    pair <- whnf p
    case unfix pair of
      Pair _ b -> whnf b
      _ -> return (snd' pair)

  Case subject ifL ifR -> do
    sum <- whnf subject
    case unfix sum of
      InL l -> whnf (ifL # l)
      InR r -> whnf (ifR # r)
      _ -> return (makeCase subject ifL ifR)

  _ -> return expr


-- Conveniences

getContext :: Proof Context
getContext = gets proofContext

putContext :: Context -> Proof ()
putContext context = do
  s <- get
  put s { proofContext = context }

modifyContext :: (Context -> Context) -> Proof ()
modifyContext f = getContext >>= putContext . f

declare :: DefinitionConstraint -> Proof ()
declare binding = modifyContext (<>< [ binding ])

addBindings :: Declaration -> Proof ()
addBindings decl = case decl of
  Declaration name ty value ->
    modifyEnvironment (H.insert name (Binding ty value))
  Data name ty constructors -> do
    modifyEnvironment (H.insert name (Binding ty (datatypeSum ty constructors)))
    for_ constructors (\ (Constructor name ty) ->
      modifyEnvironment (H.insert name (Binding ty unit)))


getEnvironment :: Proof Environment
getEnvironment = gets proofEnvironment

putEnvironment :: Environment -> Proof ()
putEnvironment environment = do
  s <- get
  put s { proofEnvironment = environment }

modifyEnvironment :: (Environment -> Environment) -> Proof ()
modifyEnvironment f = getEnvironment >>= putEnvironment . f


findTyping :: HasCallStack => Name -> Proof Type
findTyping name = lookupTyping name >>= maybe (fail ("Missing type constraint for " ++ pretty name ++ " in context.")) return

lookupDefinition :: Name -> Proof (Maybe Expr)
lookupDefinition name = getContext >>= help
  where help (_ :< D (found := decl))
          | name == found = return decl
        help (context :< _) = help context
        help _ = gets (fmap bindingValue . H.lookup name . proofEnvironment)

lookupTyping :: Name -> Proof (Maybe Expr)
lookupTyping name = getContext >>= help
  where help (_ :< T (found ::: ty))
          | name == found = return (Just ty)
        help (context :< _) = help context
        help _ = gets (fmap bindingType . H.lookup name . proofEnvironment)


specialize :: Type -> Proof Type
specialize ty = case unfix ty of
  Pi _ t b -> do
    _ <- fresh (if t == typeT then Nothing else Just t)
    specialize b
  _ -> return ty

onTop :: (DefinitionConstraint -> Proof Extension) -> Proof ()
onTop f = do
  current <- getContext
  case current of
    context :< vd -> do
      putContext context
      case vd of
        D d -> do
          m <- f d
          case m of
            Context.Replace with -> modifyContext (<>< with)
            Context.Restore -> modifyContext (:< vd)
        _ -> onTop f >> modifyContext (:< vd)
    Nil -> fail "onTop called with empty context."

infixr 3 >-
(>-) :: Constraint -> Proof a -> Proof a
constraint >- ma = do
  modifyContext (:< constraint)
  a <- ma
  modifyContext extract
  return a
  where extract context = case (constraint, context) of
          (T (x ::: _), context :< T (y ::: _)) | x == y -> context
          (T _, context :< D d) -> extract context :< D d
          (D (x := _), context :< D (y := _)) | x == y -> context
          (D _, context :< T t) -> extract context :< T t
          (_, _ :< _) -> error "Bad context entry!"
          _ -> error "Missing constraint!"


(==>) :: Suffix -> Type -> Proof Type
[]                      ==> ty = return ty
((a := Nothing) : rest) ==> ty = makePi a typeT <$> rest ==> ty
((a := Just v) : rest)  ==> ty = makePi a v <$> rest ==> ty

generalizeOver :: Proof Type -> Proof Type
generalizeOver mt = do
  modifyContext (:< Sep)
  t <- mt
  rest <- skimContext []
  rest ==> t

skimContext :: Suffix -> Proof Suffix
skimContext rest = do
  context :< d <- getContext
  putContext context
  case d of
    Sep -> return rest
    D a -> skimContext (a : rest)
    T _ -> error "Unexpected type constraint."

contextualizeErrors :: ([String] -> [String]) -> Proof a -> Proof a
contextualizeErrors addContext = iterFreer alg . fmap pure
  where alg :: ProofF x -> (x -> Proof a) -> Proof a
        alg proof = Then $ case proof of
          R (Error es) -> R (Error (addContext es))
          other -> other


-- Instances

instance Show1 ProofF where
  liftShowsPrec sp sl d proof = case proof of
    J judgement -> showsUnaryWith (liftShowsPrec sp sl) "J" d judgement
    S state -> showsUnaryWith (liftShowsPrec sp sl) "S" d state
    R result -> showsUnaryWith (liftShowsPrec sp sl) "R" d result

instance Show a => Show (ProofF a) where
  showsPrec = showsPrec1


instance Pretty1 ProofF where
  liftPrettyPrec pp pl d proof = case proof of
    J judgement -> liftPrettyPrec pp pl d judgement
    S state -> liftPrettyPrec pp pl d state
    R result -> liftPrettyPrec pp pl d result

instance Pretty ProofState where
  prettyPrec _ (ProofState n c _)
    = showString "{ " . prettyPrec 0 n
    . showString ", " . prettyPrec 0 c
    {-}. showString ", " . prettyPrec 0 (H.keys e)-} . showString " }"


instance Eq1 ProofF where
  liftEq eq a b = case (a, b) of
    (J a, J b) -> liftEq eq a b
    (S a, S b) -> liftEq eq a b
    (R a, R b) -> liftEq eq a b
    _ -> False
