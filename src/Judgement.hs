{-# LANGUAGE FlexibleInstances, GADTs, RankNTypes #-}
module Judgement where

import Context hiding (S)
import qualified Context
import Control.Monad hiding (fail)
import Control.Monad.Free.Freer
import Control.State
import Data.Foldable (for_)
import Data.Functor.Classes
import Data.Functor.Foldable hiding (Nil)
import qualified Data.HashMap.Lazy as H
import Data.List (union)
import Data.Result
import Expr
import Module
import Prelude hiding (fail)
import Text.Pretty

data Judgement a where
  CheckModule :: Module -> Judgement ()
  CheckDeclaration :: Module -> Declaration -> Judgement ()

  Check :: Term -> Type -> Judgement ()
  Infer :: Term -> Judgement Type

  IsType :: Term -> Judgement ()

  AlphaEquivalent :: Expr -> Expr -> Judgement Bool
  Equate :: Expr -> Expr -> Judgement ()

  Unify :: Type -> Type -> Judgement ()
  Solve :: Name -> Suffix -> Type -> Judgement ()

  Fresh :: Maybe Expr -> Judgement Name
  Restore :: Judgement Extension
  Replace :: Suffix -> Judgement Extension

  Normalize :: Expr -> Judgement Expr
  WHNF :: Expr -> Judgement Expr


class Binder a where
  (<?) :: Name -> a -> Bool

class Binder1 f where
  liftIn :: (Name -> a -> Bool) -> Name -> f a -> Bool

instance (Foldable t, Binder a) => Binder (t a) where
  (<?) name = any (name <?)

instance Binder Name where
  (<?) = (==)

instance Binder Binding where
  name <? (_ := m) = name <? m

instance Binder TermEntry where
  name <? (_ ::: s) = name <? s

instance Binder1 f => Binder (Fix f) where
   (<?) name = liftIn (<?) name . unfix

instance Binder1 (ExprF Name) where
  liftIn occurs name expr = case expr of
    Abs n _ | n == name -> False
    Var v | v == name -> True
    _ -> any (occurs name) expr

instance Binder Entry where
  n <? t = case t of
    Ty d -> n <? d
    Tm d -> n <? d
    Sep -> False


applyContext :: Expr -> Context -> Expr
applyContext expr context = case context of
  Nil -> expr
  (rest :< Ty (name := d)) | Just t <- d -> applyContext (substitute t name expr) rest
  (rest :< _) -> applyContext expr rest

unify :: Type -> Type -> Proof ()
unify t1 t2 = J (Unify t1 t2) `andThen` return

unify' :: Type -> Type -> Proof ()
unify' t1 t2 = case (unfix t1, unfix t2) of
  (Function a1 b1, Function a2 b2) -> unify a1 a2 >> unify b1 b2
  (Product a1 b1, Product a2 b2) -> unify a1 a2 >> unify b1 b2
  (Sum a1 b1, Sum a2 b2) -> unify a1 a2 >> unify b1 b2
  (UnitT, UnitT) -> return ()
  (TypeT, TypeT) -> return ()

  (Abs _ b1, Abs _ b2) -> unify b1 b2 -- this should probably be pushing unknown declarations onto the context
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

  _ -> fail ("Cannot unify " ++ pretty t1 ++ " with " ++ pretty t2)


solve :: Name -> Suffix -> Type -> Proof ()
solve name suffix ty = J (Solve name suffix ty) `andThen` return

solve' :: Name -> Suffix -> Type -> Proof ()
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

specialize :: Scheme -> Proof Type
specialize (Type t) = return t
specialize s = do
  let (d, s') = unpack s
  b <- fresh d
  specialize (fmap (fromS b) s')
  where unpack :: Scheme -> (Maybe Expr, Schm (Index Name))
        unpack (Context.All s') = (Nothing, s')
        unpack (LetS t s') = (Just t, s')
        unpack (Type _) = error "unpack cannot be called with a Type Schm."

        fromS :: Name -> Index Name -> Name
        fromS b Z = b
        fromS _ (Context.S a) = a


normalize :: Expr -> Proof Expr
normalize expr = J (Normalize expr) `andThen` return

normalize' :: Expr -> Proof Expr
normalize' expr = case unfix expr of
  Var name -> do
    binding <- findBinding name
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

  Function a b -> (.->.) <$> normalize a <*> normalize b
  Product a b -> (.*.) <$> normalize a <*> normalize b
  Sum a b -> (.+.) <$> normalize a <*> normalize b

  Let name value body -> do
    v <- normalize value
    define name v
    normalize body

  _ -> return expr


whnf :: Expr -> Proof Expr
whnf expr = J (WHNF expr) `andThen` return

whnf' :: Expr -> Proof Expr
whnf' expr = case unfix expr of
  Var v -> do
    binding <- findBinding v
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


data ProofF a = J (Judgement a) | S (State ProofState a) | R (Result a)

type Proof = Freer ProofF

data ProofState = ProofState
  { proofNextName :: Name
  , proofContext :: Context
  , proofEnvironment :: H.HashMap String Declaration }
  deriving (Eq, Show)


getContext :: Proof Context
getContext = gets proofContext

putContext :: Context -> Proof ()
putContext context = do
  s <- get
  put s { proofContext = context }

modifyContext :: (Context -> Context) -> Proof ()
modifyContext f = getContext >>= putContext . f

get :: Proof ProofState
get = S Get `andThen` return

gets :: (ProofState -> result) -> Proof result
gets f = fmap f get

put :: ProofState -> Proof ()
put s = S (Put s) `andThen` return


andThen :: f x -> (x -> Freer f a) -> Freer f a
andThen = (Freer .) . flip Free

fresh' :: Maybe Expr -> Proof Name
fresh' d = do
  s <- get
  let m = proofNextName s
  put s { proofNextName = increment m
        , proofContext = proofContext s :< Ty (m := d) }
  return m
  where increment (I n) = I (succ n)
        increment (N s) = N (s ++ "'")

fresh :: Maybe Expr -> Proof Name
fresh declaration = J (Fresh declaration) `andThen` return


onTop :: (Binding -> Proof Extension) -> Proof ()
onTop f = do
  current <- getContext
  case current of
    context :< vd -> do
      putContext context
      case vd of
        Ty d -> do
          m <- f d
          case m of
            Context.Replace with -> modifyContext (<>< with)
            Context.Restore -> modifyContext (:< vd)
        _ -> onTop f >> modifyContext (:< vd)
    Nil -> fail "onTop called with empty context."

restore :: Proof Extension
restore = J Judgement.Restore `andThen` return

restore' :: Proof Extension
restore' = return Context.Restore

replace :: Suffix -> Proof Extension
replace suffix = J (Judgement.Replace suffix) `andThen` return

replace' :: Suffix -> Proof Extension
replace' = return . Context.Replace


checkModule :: Module -> Proof ()
checkModule module' = J (CheckModule module') `andThen` return

checkDeclaration :: Module -> Declaration -> Proof ()
checkDeclaration module' declaration = J (CheckDeclaration module' declaration) `andThen` return


infer :: Term -> Proof Type
infer term = J (Infer term) `andThen` return

check :: Term -> Type -> Proof ()
check term ty = J (Check term ty) `andThen` return

isType :: Term -> Proof ()
isType term = J (IsType term) `andThen` return


alphaEquivalent :: Expr -> Expr -> Proof Bool
alphaEquivalent e1 e2 = J (AlphaEquivalent e1 e2) `andThen` return

alphaEquivalent' :: Expr -> Expr -> Proof Bool
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

    (a1, a2) -> case zipExprFWith (==) alphaEquivalent a1 a2 of
      Just equivalences -> do
        eq <- sequenceA equivalences
        return (and eq)
      _ -> return False


equate :: Expr -> Expr -> Proof ()
equate e1 e2 = J (Equate e1 e2) `andThen` return

equate' :: Expr -> Expr -> Proof ()
equate' e1 e2 = do
  equivalent <- alphaEquivalent e1 e2
  unless equivalent $ do
    nf1 <- whnf e1
    nf2 <- whnf e2
    case zipExprFWith (,) equate (unfix nf1) (unfix nf2) of
      Just _ -> return ()
      _ -> fail ("Could not judge equality of " ++ pretty e1 ++ " to " ++ pretty e2)


define :: Name -> Type -> Proof ()
define name ty = declare (name := Just ty)

declare :: Binding -> Proof ()
declare binding = modifyContext (<>< [ binding ])

findEntry :: Name -> Proof (Either Binding TermEntry)
findEntry name = getContext >>= go
  where go context = case context of
          (_ :< Tm entry@(found ::: _)) | name == found -> return (Right entry)
          (_ :< Ty entry@(found := _))  | name == found -> return (Left entry)
          (context :< _)                                -> go context
          _ -> fail ("Missing variable " ++ pretty name ++ " in context.")

find :: Name -> Proof Scheme
find name = getContext >>= help
  where help (_ :< Tm (found ::: decl))
          | name == found = return decl
        help (context :< _) = help context
        help _ = fail ("Missing variable " ++ pretty name ++ " in context.")

findBinding :: Name -> Proof (Maybe Expr)
findBinding name = do
  context <- getContext
  contextualizeErrors (++ [ pretty context ]) $ help context
  where help (_ :< Ty (found := decl))
          | name == found = return decl
        help (context :< _) = help context
        help _ = return Nothing


fail :: String -> Proof a
fail = wrap . R . Error . (:[])


(>-) :: TermEntry -> Proof a -> Proof a
x ::: s >- ma = do
  modifyContext (:< Tm (x ::: s))
  a <- ma
  modifyContext extract
  return a
  where extract (context :< Tm (y ::: _)) | x == y = context
        extract (context :< Ty d) = extract context :< Ty d
        extract (_ :< _) = error "Bad context entry!"
        extract _ = error "Missing term variable!"


bind :: Name -> Scheme -> Schm (Index Name)
bind a = fmap help
  where help :: Name -> Index Name
        help b | a == b = Z
               | otherwise = Context.S b

(==>) :: Suffix -> Type -> Scheme
[]                        ==> ty = Type ty
((a := Nothing) : rest)   ==> ty = All (bind a (rest ==> ty))
((a := Just v) : rest)    ==> ty = LetS v (bind a (rest ==> ty))

generalizeOver :: Proof Type -> Proof Scheme
generalizeOver mt = do
  modifyContext (:< Sep)
  t <- mt
  rest <- skimContext []
  return (rest ==> t)
  where skimContext :: Suffix -> Proof Suffix
        skimContext rest = do
          context :< d <- getContext
          putContext context
          case d of
            Sep -> return rest
            Ty a -> skimContext (a : rest)
            Tm _ -> error "Unexpected term variable."

contextualizeErrors :: ([String] -> [String]) -> Proof a -> Proof a
contextualizeErrors addContext = iterFreer alg . fmap pure
  where alg :: (x -> Proof a) -> ProofF x -> Proof a
        alg cont proof = Freer . Free cont $ case proof of
          R (Error es) -> R (Error (addContext es))
          other -> other


decompose :: Judgement a -> Proof a
decompose judgement = case judgement of
  CheckModule module' ->
    for_ (moduleDeclarations module') (checkDeclaration module')

  CheckDeclaration (Module modName _) (Declaration name ty term) -> do
    for_ (freeVariables ty) (\ name -> do
      context <- getContext
      unless (name <? context) $ declare (name := Just typeT))
    contextualizeErrors (fmap ((modName ++ "." ++ name ++ ": ") ++)) $ check term ty

  Infer term -> case unfix term of
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

    Var name -> find name >>= specialize

    Abs name body -> do
      a <- fresh Nothing
      v <- name ::: Type (var a) >- infer body
      return (var a .->. v)

    App f arg -> do
      a <- infer arg
      b <- fresh Nothing
      check f (a .->. var b)
      return (var b)

    -- Types
    UnitT -> return typeT
    TypeT -> return typeT -- Impredicativity.
    Function{} -> isType term >> return typeT
    Product{} -> isType term >> return typeT
    Sum{} -> isType term >> return typeT

    Pi name ty body -> do
      result <- name ::: Type ty >- infer body
      isType result
      return typeT

    Let name value body -> do
      t <- generalizeOver (infer value)
      name ::: t >- infer body

    As term ty -> do
      a <- fresh (Just ty)
      inferred <- infer term
      unify inferred (var a)
      return ty

  Check term ty -> case (unfix term, unfix ty) of
    (Abs n body, Function t1 t2) -> n ::: Type t1 >- check body t2
    (Abs n body, Pi n1 t tbody) -> do
      ty <- n1 ::: Type t >- infer tbody
      n ::: Type t >- check body ty

    (Pair a b, Product t1 t2) -> check a t1 >> check b t2

    (InL l, Sum t1 _) -> check l t1
    (InR r, Sum _ t2) -> check r t2

    _ -> do
      ty' <- infer term
      unify ty ty'

  IsType ty -> case unfix ty of
    UnitT -> return ()
    TypeT -> return ()
    Sum a b -> do
      isType a
      isType b
    Product a b -> do
      isType a
      isType b
    Function a b -> do
      isType a
      isType b

    Pi name ty body -> do
      isType ty
      name ::: Type ty >- isType body

    Var name -> do
      entry <- findEntry name
      case entry of
        Left (_ := Just ty') -> isType ty'
        Left (_ := Nothing)  -> fail ("Expected a type for variable but got a hole: " ++ pretty ty)
        Right (_ ::: scheme) -> specialize scheme >>= isType

    App f arg -> unify f (arg .->. typeT)

    _ -> fail ("Expected a Type but got " ++ pretty ty)

  AlphaEquivalent e1 e2 -> alphaEquivalent' e1 e2
  Equate e1 e2 -> equate' e1 e2

  Unify t1 t2 -> unify' t1 t2
  Solve name suffix ty -> solve' name suffix ty

  Fresh declaration -> fresh' declaration
  Judgement.Restore -> restore'
  Judgement.Replace suffix -> replace' suffix

  Normalize expr -> normalize' expr

  WHNF expr -> whnf' expr
  where inferPair term = do
          ty <- infer term
          a <- fresh Nothing
          b <- fresh Nothing
          unify ty (var a .*. var b)
          return (a, b)


initialState :: ProofState
initialState = ProofState (I 0) Nil H.empty

run :: Proof a -> Result a
run = runAll initialState

runAll :: ProofState -> Proof a -> Result a
runAll context proof = case runStep context proof of
  Left result -> result
  Right next -> uncurry runAll next

runSteps :: ProofState -> Proof a -> [Either (Result a) (ProofState, Proof a)]
runSteps context proof = Right (context, proof) : case runStep context proof of
  Left result -> [ Left result ]
  Right next -> uncurry runSteps next

runStep :: ProofState -> Proof a -> Either (Result a) (ProofState, Proof a)
runStep context proof = case runFreer proof of
  Pure a -> Left $ Result a
  Free cont proof -> case proof of
    J judgement -> Right (context, decompose judgement >>= cont)
    S state -> case state of
      Get -> Right (context, cont context)
      Put context' -> Right (context', cont ())
    R result -> case result of
      Error e -> Left (Error e)
      Result a -> Right (context, cont a)


-- Instances

instance Show1 Judgement where
  liftShowsPrec _ _ d judgement = case judgement of
    CheckModule module' -> showsUnaryWith showsPrec "CheckModule" d module'
    CheckDeclaration module' declaration -> showsBinaryWith showsPrec showsPrec "CheckDeclaration" d module' declaration

    Check term ty -> showsBinaryWith showsPrec showsPrec "Check" d term ty
    Infer term -> showsUnaryWith showsPrec "Infer" d term

    IsType ty -> showsUnaryWith showsPrec "IsType" d ty

    AlphaEquivalent e1 e2 -> showsBinaryWith showsPrec showsPrec "AlphaEquivalent" d e1 e2
    Equate e1 e2 -> showsBinaryWith showsPrec showsPrec "Equate" d e1 e2

    Unify t1 t2 -> showsBinaryWith showsPrec showsPrec "Unify" d t1 t2
    Solve name suffix ty -> showsTernaryWith showsPrec showsPrec showsPrec "Solve" d name suffix ty

    Fresh declaration -> showsUnaryWith showsPrec "Fresh" d declaration
    Judgement.Restore -> showString "Restore"
    Judgement.Replace suffix -> showsUnaryWith showsPrec "Replace" d suffix

    Normalize expr -> showsUnaryWith showsPrec "Normalize" d expr
    WHNF expr -> showsUnaryWith showsPrec "WHNF" d expr

instance Show a => Show (Judgement a) where
  showsPrec = showsPrec1

instance Show1 ProofF where
  liftShowsPrec sp sl d proof = case proof of
    J judgement -> showsUnaryWith (liftShowsPrec sp sl) "J" d judgement
    S state -> showsUnaryWith (liftShowsPrec sp sl) "S" d state
    R result -> showsUnaryWith (liftShowsPrec sp sl) "R" d result

instance Show a => Show (ProofF a) where
  showsPrec = showsPrec1

instance Pretty1 Judgement where
  liftPrettyPrec _ _ d judgement = case judgement of
    CheckModule (Module name _) -> showsUnaryWith (const showString) "checkModule" d name
    CheckDeclaration (Module modName _) (Declaration name _ _) -> showsUnaryWith (const showString) "checkDeclaration" d (modName ++ "." ++ name)

    Check term ty -> showsBinaryWith prettyPrec prettyPrec "check" d term ty
    Infer term -> showsUnaryWith prettyPrec "infer" d term
    IsType ty -> showsUnaryWith prettyPrec "isType" d ty

    AlphaEquivalent e1 e2 -> showsBinaryWith prettyPrec prettyPrec "alphaEquivalent" d e1 e2
    Equate e1 e2 -> showsBinaryWith prettyPrec prettyPrec "equate" d e1 e2

    Unify t1 t2 -> showsBinaryWith prettyPrec prettyPrec "unify" d t1 t2
    Solve n s ty -> showsTernaryWith prettyPrec prettyPrec prettyPrec "solve" d n s ty

    Fresh declaration -> showsUnaryWith (maybe (showString "_") . prettyPrec) "fresh" d declaration
    Judgement.Restore -> showString "restore"
    Judgement.Replace suffix -> showsUnaryWith prettyPrec "replace" d suffix

    Normalize expr -> showsUnaryWith prettyPrec "normalize" d expr
    WHNF expr -> showsUnaryWith prettyPrec "whnf" d expr

instance Pretty1 ProofF where
  liftPrettyPrec pp pl d proof = case proof of
    J judgement -> liftPrettyPrec pp pl d judgement
    S state -> liftPrettyPrec pp pl d state
    R result -> liftPrettyPrec pp pl d result

instance Pretty ProofState where
  prettyPrec _ (ProofState n c e)
    = showString "{ " . prettyPrec 0 n
    . showString ", " . prettyPrec 0 c
    . showString ", " . prettyPrec 0 e . showString " }"

instance Eq1 ProofF where
  liftEq eq a b = case (a, b) of
    (J a, J b) -> liftEq eq a b
    (S a, S b) -> liftEq eq a b
    (R a, R b) -> liftEq eq a b
    _ -> False

instance Eq1 Judgement where
  liftEq _ a b = case (a, b) of
    (CheckModule m1, CheckModule m2) -> m1 == m2
    (CheckDeclaration m1 d1, CheckDeclaration m2 d2) -> m1 == m2 && d1 == d2

    (Check tm1 ty1, Check tm2 ty2) -> tm1 == tm2 && ty1 == ty2
    (Infer tm1, Infer tm2) -> tm1 == tm2
    (IsType tm1, IsType tm2) -> tm1 == tm2

    (AlphaEquivalent a1 b1, AlphaEquivalent a2 b2) -> a1 == a2 && b1 == b2
    (Equate a1 b1, Equate a2 b2) -> a1 == a2 && b1 == b2

    (Unify a1 b1, Unify a2 b2) -> a1 == a2 && b1 == b2
    (Solve n1 s1 t1, Solve n2 s2 t2) -> n1 == n2 && s1 == s2 && t1 == t2

    (Fresh a1, Fresh a2) -> a1 == a2
    (Judgement.Restore, Judgement.Restore) -> True
    (Judgement.Replace s1, Judgement.Replace s2) -> s1 == s2

    (Normalize tm1, Normalize tm2) -> tm1 == tm2
    (WHNF tm1, WHNF tm2) -> tm1 == tm2

    _ -> False

instance Eq (Judgement a) where
  (==) = liftEq (const (const False))
