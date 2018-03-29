{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, InstanceSigs, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TupleSections #-}
module Surface.Proof where

import Context
import Control.Monad hiding (fail)
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Internal hiding (inj)
import Control.Monad.Effect.State
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

data ProofF r where
  CheckModule :: HasCallStack => Module -> ProofF ()
  CheckDeclaration :: HasCallStack => Module -> Declaration -> ProofF ()
  CheckConstructor :: HasCallStack => Module -> Declaration -> Constructor -> ProofF ()

  Check :: HasCallStack => Term -> Type -> ProofF ()
  Infer :: HasCallStack => Term -> ProofF Type
  IsType :: HasCallStack => Term -> ProofF ()

  AlphaEquivalent :: HasCallStack => Expr -> Expr -> ProofF Bool
  Equate :: HasCallStack => Expr -> Expr -> ProofF ()

  Unify :: HasCallStack => Type -> Type -> ProofF ()
  Solve :: HasCallStack => Name -> Suffix Expr -> Type -> ProofF ()

  Fresh :: HasCallStack => Maybe Expr -> ProofF Name
  Restore :: HasCallStack => ProofF (Extension Expr)
  Replace :: HasCallStack => Suffix Expr -> ProofF (Extension Expr)

  Normalize :: HasCallStack => Expr -> ProofF Expr
  WHNF :: HasCallStack => Expr -> ProofF Expr

deriving instance Show r => Show (ProofF r)
deriving instance Eq r => Eq (ProofF r)

type Proof = Eff '[ Fail, State ProofState, ProofF ]

data ProofState = ProofState
  { proofNextName :: Name
  , proofContext :: Context Expr
  , proofEnvironment :: Environment }
  deriving (Eq, Show)

type Environment = H.HashMap Name Binding

data Binding = Binding { bindingType :: Type, bindingValue :: Expr }
  deriving (Eq, Show)


-- Judgement constructors

checkModule :: HasCallStack => Module -> Proof ()
checkModule module' = send $ withFrozenCallStack $ CheckModule module'

checkDeclaration :: HasCallStack => Module -> Declaration -> Proof ()
checkDeclaration module' declaration = send $ withFrozenCallStack $ CheckDeclaration module' declaration

checkConstructor :: HasCallStack => Module -> Declaration -> Constructor -> Proof ()
checkConstructor module' declaration constructor = send $ withFrozenCallStack $ CheckConstructor module' declaration constructor


check :: HasCallStack => Term -> Type -> Proof ()
check term ty = send $ withFrozenCallStack $ Check term ty

infer :: HasCallStack => Term -> Proof Type
infer term = send $ withFrozenCallStack $ Infer term


isType :: HasCallStack => Term -> Proof ()
isType term = send $ withFrozenCallStack $ IsType term


alphaEquivalent :: HasCallStack => Expr -> Expr -> Proof Bool
alphaEquivalent e1 e2 = send $ withFrozenCallStack $ AlphaEquivalent e1 e2

equate :: HasCallStack => Expr -> Expr -> Proof ()
equate e1 e2 = send $ withFrozenCallStack $ Equate e1 e2


unify :: HasCallStack => Type -> Type -> Proof ()
unify t1 t2 = send $ withFrozenCallStack $ Unify t1 t2

solve :: HasCallStack => Name -> Suffix Expr -> Type -> Proof ()
solve name suffix ty = send $ withFrozenCallStack $ Solve name suffix ty


fresh :: HasCallStack => Maybe Expr -> Proof Name
fresh declaration = send $ withFrozenCallStack $ Fresh declaration

restore :: HasCallStack => Proof (Extension Expr)
restore = send $ withFrozenCallStack $ Surface.Proof.Restore

replace :: HasCallStack => Suffix Expr -> Proof (Extension Expr)
replace suffix = send $ withFrozenCallStack $ Surface.Proof.Replace suffix


normalize :: HasCallStack => Expr -> Proof Expr
normalize expr = send $ withFrozenCallStack $ Normalize expr

whnf :: HasCallStack => Expr -> Proof Expr
whnf expr = send $ withFrozenCallStack $ WHNF expr


-- Proof evaluation

initialState :: ProofState
initialState = ProofState (I 0) Nil H.empty

runProof :: HasCallStack => Proof a -> Either [String] a
runProof = flip runAll initialState

runAll :: HasCallStack => Proof a -> ProofState -> Either [String] a
runAll context proof = case runStep context proof of
  Left errors -> Left errors
  Right (Val a, _) -> Right a
  Right next -> uncurry runAll next

runSteps :: HasCallStack => Proof a -> ProofState -> [(Proof a, ProofState)]
runSteps proof context = (proof, context) : case runStep proof context of
  Left _ -> []
  Right r@(Val _, _) -> [ r ]
  Right next -> uncurry runSteps next

runStep :: forall a. HasCallStack => Proof a -> ProofState -> Either [String] (Proof a, ProofState)
runStep proof context = case proof of
  Val a -> Right (pure a, context)
  E u cont -> case decompose u of
    Right (Fail s) -> Left [s]
    Left u -> case decompose u of
      Right Get -> runStep (apply cont context) context
      Right (Put context) -> runStep (apply cont ()) context
      Left u -> case decompose u of
        Right proof -> go proof (apply cont)
        Left _ -> Left ["the impossible happened"]
  where go :: forall x . ProofF x -> (x -> Proof a) -> Either [String] (Proof a, ProofState)
        go proof yield = run $ case proof of
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
          Surface.Proof.Restore -> restore'
          Surface.Proof.Replace suffix -> replace' suffix

          Normalize expr -> normalize' expr
          WHNF expr -> whnf' expr
          where run :: Proof x -> Either [String] (Proof a, ProofState)
                run = Right . (, context) . (>>= yield)


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
  where context cs = contextualizeErrors ((intercalate "." (modName : cs) ++ ": ") ++)

checkConstructor' :: HasCallStack => Module -> Declaration -> Constructor -> Proof ()
checkConstructor' _ decl (Constructor _ sig) = do
  modifyContext (:< Sep)
  env <- getEnvironment
  tyVariables <- traverse (fresh . Just) (domain (declarationType decl))
  let sigVars = freeVariables sig \\ H.keys env
  constructorT <- infer (foldr makeLambda sig sigVars)
  flip (foldr (>-)) (zipWith ((T .) . (:::)) sigVars (domain constructorT)) $ do
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

  Sigma name ty body -> do
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

  _ -> fail ("Expected a Type but got " ++ prettyExpr 0 ty "")


alphaEquivalent' :: HasCallStack => Expr -> Expr -> Proof Bool
alphaEquivalent' e1 e2
  | e1 == e2 = return True
  | otherwise = case (unfix e1, unfix e2) of
    (Abs n1 b1, Abs n2 b2)
      | n1 == n2 -> alphaEquivalent b1 b2
      | otherwise -> let new = var (freshNameIn (n1 : n2 : freeVariables b1 `union` freeVariables b2)) in
        alphaEquivalent (substitute new n1 b1) (substitute new n2 b2)
    (Pi n1 t1 b1, Pi n2 t2 b2)
      | n1 == n2 -> alphaEquivalent t1 t2 >> alphaEquivalent b1 b2
      | otherwise -> let new = var (freshNameIn (n1 : n2 : freeVariables b1 `union` freeVariables b2)) in
        alphaEquivalent t1 t2 >> alphaEquivalent (substitute new n1 b1) (substitute new n2 b2)
    (Mu n1 t1 b1, Mu n2 t2 b2)
      | n1 == n2 -> alphaEquivalent t1 t2 >> alphaEquivalent b1 b2
      | otherwise -> let new = var (freshNameIn (n1 : n2 : freeVariables b1 `union` freeVariables b2)) in
        alphaEquivalent t1 t2 >> alphaEquivalent (substitute new n1 b1) (substitute new n2 b2)
    (Sigma n1 t1 b1, Sigma n2 t2 b2)
      | n1 == n2 -> alphaEquivalent t1 t2 >> alphaEquivalent b1 b2
      | otherwise -> let new = var (freshNameIn (n1 : n2 : freeVariables b1 `union` freeVariables b2)) in
        alphaEquivalent t1 t2 >> alphaEquivalent (substitute new n1 b1) (substitute new n2 b2)
    (Let n1 v1 b1, Let n2 v2 b2)
      | n1 == n2 -> alphaEquivalent v1 v2 >> alphaEquivalent b1 b2
      | otherwise -> let new = var (freshNameIn (n1 : n2 : freeVariables b1 `union` freeVariables b2 `union` freeVariables v1 `union` freeVariables v2)) in
        alphaEquivalent (substitute new n1 v1) (substitute new n2 v2) >> alphaEquivalent (substitute new n1 b1) (substitute new n2 b2)

    (Var n1, Var n2) -> return (n1 == n2)

    (a1, a2) -> case zipExprFWith (==) alphaEquivalent a1 a2 of
      Just equivalences -> do
        eq <- sequenceA equivalences
        return (and eq)
      _ -> return False


equate' :: HasCallStack => Expr -> Expr -> Proof ()
equate' e1 e2 = do
  equivalent <- alphaEquivalent e1 e2
  unless equivalent $ do
    Fix nf1 <- whnf e1
    Fix nf2 <- whnf e2
    case zipExprFWith (,) equate nf1 nf2 of
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

solve' :: HasCallStack => Name -> Suffix Expr -> Type -> Proof ()
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

restore' :: HasCallStack => Proof (Extension Expr)
restore' = return Context.Restore

replace' :: HasCallStack => Suffix Expr -> Proof (Extension Expr)
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

getContext :: Proof (Context Expr)
getContext = gets proofContext

putContext :: Context Expr -> Proof ()
putContext context = do
  s <- get
  put s { proofContext = context }

modifyContext :: (Context Expr -> Context Expr) -> Proof ()
modifyContext f = getContext >>= putContext . f

declare :: DefinitionConstraint Expr -> Proof ()
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

onTop :: (DefinitionConstraint Expr -> Proof (Extension Expr)) -> Proof ()
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
(>-) :: Constraint Expr -> Proof a -> Proof a
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


(==>) :: Suffix Expr -> Type -> Proof Type
[]                      ==> ty =                      return ty
((a := Nothing) : rest) ==> ty = makePi a typeT <$> rest ==> ty
((a := Just v)  : rest) ==> ty = makePi a v     <$> rest ==> ty

generalizeOver :: Proof Type -> Proof Type
generalizeOver mt = do
  modifyContext (:< Sep)
  t <- mt
  rest <- skimContext []
  rest ==> t

skimContext :: Suffix Expr -> Proof (Suffix Expr)
skimContext rest = do
  context :< d <- getContext
  putContext context
  case d of
    Sep -> return rest
    D a -> skimContext (a : rest)
    T _ -> error "Unexpected type constraint."

contextualizeErrors :: (String -> String) -> Proof a -> Proof a
contextualizeErrors addContext = interpose pure (\ (Fail s) _ -> fail (addContext s))


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


instance Pretty1 ProofF where
  liftPrettyPrec :: forall r . (Int -> r -> ShowS) -> ([r] -> ShowS) -> Int -> ProofF r -> ShowS
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
    Solve n s ty -> showsTernaryWith prettyPrec prettySuffix prettyExpr "solve" d n s ty

    Fresh declaration -> showsUnaryWith (maybe (showString "_") . prettyExpr) "fresh" d declaration
    Surface.Proof.Restore -> showString "restore"
    Surface.Proof.Replace suffix -> showsUnaryWith prettySuffix "replace" d suffix

    Normalize expr -> showsUnaryWith prettyExpr "normalize" d expr
    WHNF expr -> showsUnaryWith prettyExpr "whnf" d expr
    where prettySuffix :: Int -> Suffix Expr -> ShowS
          prettySuffix = liftPrettyPrec (liftPrettyPrec prettyExpr (showListWith (prettyExpr 0))) (liftPrettyList prettyExpr (showListWith (prettyExpr 0)))

instance Pretty (ProofF a) where
  prettyPrec = liftPrettyPrec (const (const id)) (const id)

instance Pretty ProofState where
  prettyPrec _ (ProofState n c _)
    = showString "{ " . prettyPrec 0 n
    . showString ", " . liftPrettyPrec (liftPrettyPrec prettyExpr (showListWith (prettyExpr 0))) (liftPrettyList prettyExpr (showListWith (prettyExpr 0))) 0 c
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
    _ -> False
