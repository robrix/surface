{-# LANGUAGE GADTs, ScopedTypeVariables, StandaloneDeriving #-}
module Surface.Proof where

import Context
import Control.Monad hiding (fail)
import Control.Monad.Free.Freer
import Data.Foldable (for_, sequenceA_)
import Data.Functor.Classes
import Data.Functor.Foldable hiding (Mu, Nil)
import qualified Data.HashMap.Lazy as H
import Data.List (intercalate, union, (\\), (!!))
import Data.Traversable (for)
import Expr
import GHC.Stack
import Module
import Prelude hiding (fail)
import Surface.Binder
import Text.Pretty

data ProofF a where
  CheckModule :: HasCallStack => Module -> ProofF ()
  CheckDeclaration :: HasCallStack => Module -> Declaration -> ProofF ()
  CheckConstructor :: HasCallStack => Module -> Declaration -> Constructor -> ProofF ()

  Check :: HasCallStack => Term -> Type -> ProofF ()
  Infer :: HasCallStack => Term -> ProofF Type
  IsType :: HasCallStack => Term -> ProofF ()

  AlphaEquivalent :: HasCallStack => Expr -> Expr -> ProofF Bool
  Equate :: HasCallStack => Expr -> Expr -> ProofF ()

  Unify :: HasCallStack => Type -> Type -> ProofF ()
  Solve :: HasCallStack => Name -> Suffix -> Type -> ProofF ()

  Fresh :: HasCallStack => Maybe Expr -> ProofF Name
  Restore :: HasCallStack => ProofF Extension
  Replace :: HasCallStack => Suffix -> ProofF Extension

  Normalize :: HasCallStack => Expr -> ProofF Expr
  WHNF :: HasCallStack => Expr -> ProofF Expr

  Get :: ProofF ProofState
  Put :: ProofState -> ProofF ()

  Error :: [String] -> ProofF a

deriving instance Show a => Show (ProofF a)
deriving instance Eq a => Eq (ProofF a)

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
checkModule module' = withFrozenCallStack $ CheckModule module' `Then` return

checkDeclaration :: HasCallStack => Module -> Declaration -> Proof ()
checkDeclaration module' declaration = withFrozenCallStack $ CheckDeclaration module' declaration `Then` return

checkConstructor :: HasCallStack => Module -> Declaration -> Constructor -> Proof ()
checkConstructor module' declaration constructor = withFrozenCallStack $ CheckConstructor module' declaration constructor `Then` return


check :: HasCallStack => Term -> Type -> Proof ()
check term ty = withFrozenCallStack $ Check term ty `Then` return

infer :: HasCallStack => Term -> Proof Type
infer term = withFrozenCallStack $ Infer term `Then` return


isType :: HasCallStack => Term -> Proof ()
isType term = withFrozenCallStack $ IsType term `Then` return


alphaEquivalent :: HasCallStack => Expr -> Expr -> Proof Bool
alphaEquivalent e1 e2 = withFrozenCallStack $ AlphaEquivalent e1 e2 `Then` return

equate :: HasCallStack => Expr -> Expr -> Proof ()
equate e1 e2 = withFrozenCallStack $ Equate e1 e2 `Then` return


unify :: HasCallStack => Type -> Type -> Proof ()
unify t1 t2 = withFrozenCallStack $ Unify t1 t2 `Then` return

solve :: HasCallStack => Name -> Suffix -> Type -> Proof ()
solve name suffix ty = withFrozenCallStack $ Solve name suffix ty `Then` return


fresh :: HasCallStack => Maybe Expr -> Proof Name
fresh declaration = withFrozenCallStack $ Fresh declaration `Then` return

restore :: HasCallStack => Proof Extension
restore = withFrozenCallStack $ Surface.Proof.Restore `Then` return

replace :: HasCallStack => Suffix -> Proof Extension
replace suffix = withFrozenCallStack $ Surface.Proof.Replace suffix `Then` return


normalize :: HasCallStack => Expr -> Proof Expr
normalize expr = withFrozenCallStack $ Normalize expr `Then` return

whnf :: HasCallStack => Expr -> Proof Expr
whnf expr = withFrozenCallStack $ WHNF expr `Then` return


-- State constructors

get :: Proof ProofState
get = Get `Then` return

gets :: (ProofState -> result) -> Proof result
gets f = fmap f get

put :: ProofState -> Proof ()
put s = Put s `Then` return

modify :: (ProofState -> ProofState) -> Proof ()
modify f = get >>= put . f


-- Errors

fail :: HasCallStack => String -> Proof a
fail message = Error [ message, prettyCallStack callStack ] `Then` return


-- Proof evaluation

initialState :: ProofState
initialState = ProofState (I 0) Nil H.empty

run :: HasCallStack => Proof a -> Either [String] a
run = flip runAll initialState

runAll :: HasCallStack => Proof a -> ProofState -> Either [String] a
runAll context proof = case runStep context proof of
  Left errors -> Left errors
  Right (Return a, _) -> Right a
  Right next -> uncurry runAll next

runSteps :: HasCallStack => Proof a -> ProofState -> [(Proof a, ProofState)]
runSteps proof context = (proof, context) : case runStep proof context of
  Left errors -> [ (Error errors `Then` return, context) ]
  Right r@(Return _, _) -> [ r ]
  Right next -> uncurry runSteps next

-- | Like runSteps, but filtering out gets and puts.
runSteps' :: HasCallStack => Proof a -> ProofState -> [(Proof a, ProofState)]
runSteps' = (filter isSignificant .) . runSteps
  where isSignificant = iterFreer (\ p _ -> case p of { Get -> False ; Put _ -> False ; _ -> True }) . (True <$) . fst

runStep :: forall a. HasCallStack => Proof a -> ProofState -> Either [String] (Proof a, ProofState)
runStep proof context = case proof of
  Return a -> Right (return a, context)
  Then proof yield -> go proof yield
  where go :: forall x . ProofF x -> (x -> Proof a) -> Either [String] (Proof a, ProofState)
        go proof yield = case proof of
          CheckModule module' -> run $ checkModule' module'
          CheckDeclaration m d -> run $ checkDeclaration' m d
          CheckConstructor m d c -> run $ checkConstructor' m d c

          Check term ty -> run $ check' term ty
          Infer term -> run $ infer' term
          IsType ty -> run $ isType' ty

          AlphaEquivalent e1 e2 -> run $ alphaEquivalent' e1 e2
          Equate e1 e2 -> run $ equate' e1 e2

          Unify t1 t2 -> run $ unify' t1 t2
          Solve name suffix ty -> run $ solve' name suffix ty

          Fresh declaration -> run $ fresh' declaration
          Surface.Proof.Restore -> run $ restore'
          Surface.Proof.Replace suffix -> run $ replace' suffix

          Normalize expr -> run $ normalize' expr
          WHNF expr -> run $ whnf' expr

          Get -> Right (yield context, context)
          Put context' -> Right (yield (), context')

          Error errors -> Left errors
          where run :: Proof x -> Either [String] (Proof a, ProofState)
                run = Right . flip (,) context . (>>= yield)


-- Judgement interpreters

checkModule' :: HasCallStack => Module -> Proof ()
checkModule' module' = do
  for_ (moduleDeclarations module') addBindings
  for_ (moduleDeclarations module') (checkDeclaration module')

checkDeclaration' :: HasCallStack => Module -> Declaration -> Proof ()
checkDeclaration' mod@(Module modName _) decl = do
  isType (declarationType decl)
  env <- getEnvironment
  let ty = declarationType decl
  context [ pretty (declarationName decl) ] $ case decl { declarationType = generalize (freeVariables ty \\ H.keys env) ty } of
    Declaration _ ty term -> check term ty
    Data _ _ constructors -> for_ constructors (checkConstructor mod decl)
  where context cs = contextualizeErrors (fmap ((intercalate "." (modName : cs) ++ ": ") ++))

checkConstructor' :: HasCallStack => Module -> Declaration -> Constructor -> Proof ()
checkConstructor' _ decl (Constructor _ sig) = do
  modifyContext (:< Sep)
  env <- getEnvironment
  tyVariables <- traverse (fresh . Just) (domain (declarationType decl))
  flip (foldr (>-)) (fmap (T . (::: typeT)) (freeVariables sig \\ H.keys env)) $ do
    isType sig
    equate (codomain sig) (foldl (#) (var (declarationName decl)) (fmap var tyVariables))
  _ <- skimContext []
  return ()


check' :: HasCallStack => Term -> Type -> Proof ()
check' term ty = case (unfix term, unfix ty) of
  (Abs n body, Pi n1 t tbody) -> T (n1 ::: t) >- T (n ::: t) >- check body tbody

  (Var name@N{}, _) -> do
    ty' <- findTyping name
    unify ty' ty

  (Tuple [a], _) -> check a ty
  (_, Product [t]) -> check term t
  (Tuple vs, Product ts) | length vs == length ts -> sequenceA_ (zipWith check vs ts)

  (Inj a i, Sum ts) | length ts > i -> check a (ts !! i)

  _ -> do
    ty' <- infer term
    unify ty ty'

infer' :: HasCallStack => Term -> Proof Type
infer' term = case unfix term of
  Tuple as -> productT <$> traverse infer as

  At a i
    | i < 0 -> error ("Subscript at negative index: " ++ show i)
    | otherwise -> do
      ty <- infer a
      vs <- replicateM (succ i) (var <$> fresh Nothing)
      unify ty (productT vs)
      return (vs !! i)

  Inj a i -> do
    a' <- infer a
    vinit <- replicateM i (var <$> fresh Nothing)
    vtail <- var <$> fresh Nothing
    return (sumT (vinit ++ [ a', vtail ]))

  Case scrutinee cases -> do
    scrutinee' <- infer scrutinee
    result <- fresh Nothing
    functions <- traverse infer cases
    args <- for functions $ \ t -> do
      arg <- fresh Nothing
      unify t (var arg .->. var result)
      return (var arg)
    unify scrutinee' (sumT args)
    return (var result)

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
  Type -> return typeT -- Impredicativity.
  Product [] -> return unitT
  Product _ -> isType term >> return typeT
  Sum [] -> return voidT
  Sum _ -> isType term >> return typeT

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
  where inferDType name ty body = do
          result <- T (name ::: ty) >- infer body
          isType result
          return typeT


isType' :: HasCallStack => Term -> Proof ()
isType' ty = case unfix ty of
  Type -> return ()
  Sum ts -> for_ ts isType
  Product ts -> for_ ts isType

  Pi name ty body -> do
    isType ty
    T (name ::: ty) >- isType body

  Mu name ty body -> do
    isType ty
    D (name := Just ty) >- isType body

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

  _ -> fail ("Expected a Type but got " ++ prettyExpr 0 ty "")


alphaEquivalent' :: HasCallStack => Expr -> Expr -> Proof Bool
alphaEquivalent' e1 e2
  | e1 == e2 = return True
  | otherwise = case (unfix e1, unfix e2) of
    (Abs n1 b1, Abs n2 b2)
      | n1 == n2 -> alphaEquivalent b1 b2
      | otherwise -> let new = var (freshNameIn (n1 : n2 : freeVariables b1 `union` freeVariables b2)) in
        alphaEquivalent (substitute new n1 b1) (substitute new n2 b2)
    (Pi n1 t1 b1, Pi n2 t2 b2) -> let new = var (freshNameIn (n1 : n2 : freeVariables b1 `union` freeVariables b2)) in
      alphaEquivalent t1 t2 >> alphaEquivalent (substitute new n1 b1) (substitute new n2 b2)
    (Let n1 v1 b1, Let n2 v2 b2) -> let new = var (freshNameIn (n1 : n2 : freeVariables b1 `union` freeVariables b2 `union` freeVariables v1 `union` freeVariables v2)) in
      alphaEquivalent (substitute new n1 v1) (substitute new n2 v2) >> alphaEquivalent (substitute new n1 b1) (substitute new n2 b2)

    (Var n1, Var n2) -> return (n1 == n2)

    (a1, a2) -> case zipExprFWith (==) alphaEquivalent a1 a2 of -- FIXME: this should probably be testing under renaming.
      Just equivalences -> do
        eq <- sequenceA equivalences
        return (and eq)
      _ -> return False


equate' :: HasCallStack => Expr -> Expr -> Proof ()
equate' e1 e2 = do
  equivalent <- alphaEquivalent e1 e2
  unless equivalent $ do
    nf1 <- whnf e1
    nf2 <- whnf e2
    case zipExprFWith (,) equate (unfix nf1) (unfix nf2) of
      Just _ -> return ()
      _ -> fail ("Could not judge equality of " ++ prettyExpr 0 e1 (" to " ++ prettyExpr 0 e2 ""))


unify' :: HasCallStack => Type -> Type -> Proof ()
unify' t1 t2 = unless (t1 == t2) $ case (unfix t1, unfix t2) of
  (Product [], Product []) -> return ()
  (Product (t1 : ts1), Product (t2 : ts2)) -> unify t1 t2 >> unify (productT ts1) (productT ts2)
  (Sum [], Sum []) -> return ()
  (Sum (t1 : ts1), Sum (t2 : ts2)) -> unify t1 t2 >> unify (sumT ts1) (sumT ts2)
  (Pi _ t1 b1, Pi _ t2 b2) -> unify t1 t2 >> unify b1 b2 -- this should probably be pushing typing constraints onto the context
  (Mu _ t1 b1, Mu _ t2 b2) -> unify t1 t2 >> unify b1 b2 -- this should probably be pushing typing constraints onto the context
  (Sigma _ t1 b1, Sigma _ t2 b2) -> unify t1 t2 >> unify b1 b2 -- this should probably be pushing typing constraints onto the context

  (Type, Type) -> return ()

  (Abs _ b1, Abs _ b2) -> unify b1 b2 -- this should probably be pushing unknown declarations onto the context

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

  (Inj a1 i1, Inj a2 i2) | i1 == i2 -> unify a1 a2
  (Case s1 [], Case s2 []) -> unify s1 s2
  (Case s1 (c1 : cs1), Case s2 (c2 :cs2)) -> unify c1 c2 >> unify (makeCase s1 cs1) (makeCase s2 cs2)

  (Tuple [], Tuple []) -> return ()
  (Tuple (v1 : vs1), Tuple (v2 : vs2)) -> unify v1 v2 >> unify (tuple vs1) (tuple vs2)
  (At a1 i1, At a2 i2) | i1 == i2 -> unify a1 a2

  _ -> cannotUnify
  where cannotUnify = fail ("Cannot unify " ++ prettyExpr 0 t1 (" with " ++ prettyExpr 0 t2 ""))

solve' :: HasCallStack => Name -> Suffix -> Type -> Proof ()
solve' name suffix ty = onTop $ \ (n := d) ->
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
fresh' d = do
  s <- get
  let m = proofNextName s
  put s { proofNextName = succName m
        , proofContext = proofContext s :< D (m := d) }
  return m

restore' :: HasCallStack => Proof Extension
restore' = return Context.Restore

replace' :: HasCallStack => Suffix -> Proof Extension
replace' = return . Context.Replace


normalize' :: HasCallStack => Expr -> Proof Expr
normalize' expr = case unfix expr of
  Var name -> do
    binding <- lookupDefinition name
    case binding of
      Just term -> return term
      Nothing -> return (var name)

  Abs name body -> do
    declare (name := Nothing)
    makeLambda name <$> normalize body

  App op arg -> do
    o <- normalize op
    a <- normalize arg
    case unfix o of
      Abs name body -> do
        declare (name := Just a)
        normalize body
      Var v -> return (var v # a)
      _ -> error ("Application of non-abstraction value: " ++ prettyExpr 0 o "")

  Inj a i
    | i < 0 -> error ("Injection at negative index: " ++ show i)
    | otherwise -> flip inj i <$> normalize a
  Case scrutinee cases -> do
    scrutinee' <- normalize scrutinee
    case unfix scrutinee' of
      Inj a i
        | length cases > i -> do
          f <- normalize (cases !! i)
          normalize (f # a)
        | otherwise -> error ("Injection out of bounds: " ++ show i ++ " >= " ++ show (length cases))
      _ -> error ("Case expression on non-sum value: " ++ prettyExpr 0 scrutinee' "")

  Tuple [v] -> normalize v
  Tuple vs -> tuple <$> traverse normalize vs

  At a i
    | i < 0 -> error ("Subscript at negative index: " ++ show i)
    | otherwise -> do
      a' <- normalize a
      case unfix a' of
        Tuple vs
          | length vs > i -> return (vs !! i)
          | otherwise -> error ("Subscript out of bounds: " ++ show i ++ " >= " ++ show (length vs))
        _ -> error ("Subscript of non-tuple value: " ++ prettyExpr 0 a' "")

  Product ts -> Fix . Product <$> traverse normalize ts
  Sum ts -> Fix . Sum <$> traverse normalize ts

  Let name value body -> do
    v <- normalize value
    declare (name := Just v)
    normalize body

  _ -> return expr


whnf' :: HasCallStack => Expr -> Proof Expr
whnf' expr = case unfix expr of
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

  At a i -> do
    tuple <- whnf a
    case unfix tuple of
      Tuple vs | length vs > i -> return (vs !! i)
      _ -> return (at tuple i)

  Case scrutinee cases -> do
    scrutinee' <- whnf scrutinee
    case unfix scrutinee' of
      Inj a i | length cases > i -> whnf ((cases !! i) # a)
      _ -> return (makeCase scrutinee cases)

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
    modifyEnvironment (H.insert name (Binding ty (datatypeSum name ty constructors)))
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
          Error es -> Error (addContext es)
          other -> other


-- Instances

instance Show1 ProofF where
  liftShowsPrec _ _ d proof = case proof of
    CheckModule module' -> showsUnaryWith showsPrec "CheckModule" d module'
    CheckDeclaration module' declaration -> showsBinaryWith showsPrec showsPrec "CheckDeclaration" d module' declaration
    CheckConstructor module' declaration constructor -> showsTernaryWith showsPrec showsPrec showsPrec "CheckConstructor" d module' declaration constructor

    Check term ty -> showsBinaryWith showsPrec showsPrec "Check" d term ty
    Infer term -> showsUnaryWith showsPrec "Infer" d term

    IsType ty -> showsUnaryWith showsPrec "IsType" d ty

    AlphaEquivalent e1 e2 -> showsBinaryWith showsPrec showsPrec "AlphaEquivalent" d e1 e2
    Equate e1 e2 -> showsBinaryWith showsPrec showsPrec "Equate" d e1 e2

    Unify t1 t2 -> showsBinaryWith showsPrec showsPrec "Unify" d t1 t2
    Solve name suffix ty -> showsTernaryWith showsPrec showsPrec showsPrec "Solve" d name suffix ty

    Fresh declaration -> showsUnaryWith showsPrec "Fresh" d declaration
    Surface.Proof.Restore -> showString "Restore"
    Surface.Proof.Replace suffix -> showsUnaryWith showsPrec "Replace" d suffix

    Normalize expr -> showsUnaryWith showsPrec "Normalize" d expr
    WHNF expr -> showsUnaryWith showsPrec "WHNF" d expr

    Get -> showString "Get"
    Put state -> showsUnaryWith showsPrec "Put" d state

    Error errors -> showsUnaryWith showsPrec "Error" d errors


instance Pretty1 ProofF where
  liftPrettyPrec _ _ d proof = case proof of
    CheckModule (Module name _) -> showsUnaryWith (const showString) "checkModule" d name
    CheckDeclaration (Module modName _) decl -> showsUnaryWith (const showString) "checkDeclaration" d (modName ++ "." ++ pretty (declarationName decl))
    CheckConstructor (Module modName _) decl constructor -> showsUnaryWith (const showString) "checkConstructor" d (modName ++ "." ++ pretty (declarationName decl) ++ "." ++ pretty (constructorName constructor))

    Check term ty -> showsBinaryWith prettyExpr prettyExpr "check" d term ty
    Infer term -> showsUnaryWith prettyExpr "infer" d term
    IsType ty -> showsUnaryWith prettyExpr "isType" d ty

    AlphaEquivalent e1 e2 -> showsBinaryWith prettyExpr prettyExpr "alphaEquivalent" d e1 e2
    Equate e1 e2 -> showsBinaryWith prettyExpr prettyExpr "equate" d e1 e2

    Unify t1 t2 -> showsBinaryWith prettyExpr prettyExpr "unify" d t1 t2
    Solve n s ty -> showsTernaryWith prettyPrec prettyPrec prettyExpr "solve" d n s ty

    Fresh declaration -> showsUnaryWith (maybe (showString "_") . prettyExpr) "fresh" d declaration
    Surface.Proof.Restore -> showString "restore"
    Surface.Proof.Replace suffix -> showsUnaryWith prettyPrec "replace" d suffix

    Normalize expr -> showsUnaryWith prettyExpr "normalize" d expr
    WHNF expr -> showsUnaryWith prettyExpr "whnf" d expr

    Get -> showString "Get"
    Put state -> shows state

    Error errors -> liftPrettyPrec prettyPrec prettyList d errors

instance Pretty ProofState where
  prettyPrec _ (ProofState n c _)
    = showString "{ " . prettyPrec 0 n
    . showString ", " . prettyPrec 0 c
    {-}. showString ", " . prettyPrec 0 (H.keys e)-} . showString " }"


instance Eq1 ProofF where
  liftEq _ a b = case (a, b) of
    (CheckModule m1, CheckModule m2) -> m1 == m2
    (CheckDeclaration m1 d1, CheckDeclaration m2 d2) -> m1 == m2 && d1 == d2
    (CheckConstructor m1 d1 c1, CheckConstructor m2 d2 c2) -> m1 == m2 && d1 == d2 && c1 == c2

    (Check tm1 ty1, Check tm2 ty2) -> tm1 == tm2 && ty1 == ty2
    (Infer tm1, Infer tm2) -> tm1 == tm2
    (IsType tm1, IsType tm2) -> tm1 == tm2

    (AlphaEquivalent a1 b1, AlphaEquivalent a2 b2) -> a1 == a2 && b1 == b2
    (Equate a1 b1, Equate a2 b2) -> a1 == a2 && b1 == b2

    (Unify a1 b1, Unify a2 b2) -> a1 == a2 && b1 == b2
    (Solve n1 s1 t1, Solve n2 s2 t2) -> n1 == n2 && s1 == s2 && t1 == t2

    (Fresh a1, Fresh a2) -> a1 == a2
    (Surface.Proof.Restore, Surface.Proof.Restore) -> True
    (Surface.Proof.Replace s1, Surface.Proof.Replace s2) -> s1 == s2

    (Normalize tm1, Normalize tm2) -> tm1 == tm2
    (WHNF tm1, WHNF tm2) -> tm1 == tm2

    (Get, Get) -> True
    (Put s1, Put s2) -> s1 == s2
    (Error es1, Error es2) -> es1 == es2
    _ -> False
