{-# LANGUAGE GADTs #-}
module REPL where

import Context
import Control.Applicative
import Control.Monad.Free.Freer
import Data.Foldable (for_)
import Data.Functor.Classes (showsUnaryWith)
import Expr
import Parser
import Surface.Proof
import System.Console.Haskeline
import Text.Pretty
import Text.Trifecta

data REPLF a where
  Prompt :: String -> REPLF (Maybe String)
  Output :: (a -> String) -> Either [String] a -> REPLF ()

type REPL = Freer REPLF

data Command
  = Run Expr
  | TypeOf Expr
  | WHNF Expr
  | Help
  | Quit

prompt :: String -> REPL (Maybe String)
prompt s = Prompt s `Then` return

output :: Pretty a => Either [String] a -> REPL ()
output a = Output pretty a `Then` return

output' :: (a -> String) -> Either [String] a -> REPL ()
output' s a = Output s a `Then` return

repl :: REPL ()
repl = do
  input <- prompt "λ: "
  maybe (pure ()) handleInput input

handleInput :: String -> REPL ()
handleInput input =
  case Parser.parseString command input of
    Right Help -> output (Left
      [ ":help, :h, :?     - print this help text"
      , ":quit, :q         - exit the REPL"
      , ":type, :t <expr>  - print the type of <expr>"
      ] :: Either [String] ()) >> repl
    Right Quit -> pure ()
    Right (Run expr) -> output' (prettyExpr 0) (run (infer expr >> normalize expr)) >> repl
    Right (TypeOf expr) -> do
      output' (prettyExpr 0) (run (do
        ty <- infer expr
        context <- getContext
        return (expr `as` applyContext ty context)))
      repl
    Right (REPL.WHNF expr) -> output (run (infer expr >> whnf expr)) >> repl
    error -> output error >> repl


command :: (Monad m, TokenParsing m) => m Command
command = whiteSpace *> (colon *> meta <|> eval) <* eof <?> "command"
  where meta = (Help <$ (long "help" <|> short 'h' <|> short '?') <?> "help")
           <|> (Quit <$ (long "quit" <|> short 'q') <?> "quit")
           <|> (TypeOf <$> ((long "type" <|> short 't') *> expr) <?> "type of")
           <|> (REPL.WHNF <$> ((long "whnf" <|> short 'w') *> expr) <?> "whnf")
           <?> "command; use :? for help"

        eval = Run <$> expr <?> "expression"

        short = symbol . (:[])
        long = symbol

green :: String
green = "\ESC[1;32m\STX"

plain :: String
plain = "\ESC[0m\STX"

runREPL :: REPL a -> IO a
runREPL repl = do
  prefs <- readPrefs "~/.local/surface/repl_prefs"
  runInputTWithPrefs prefs settings (iterFreer alg (fmap pure repl))
  where alg :: REPLF x -> (x -> InputT IO a) -> InputT IO a
        alg repl cont = case repl of
          Prompt s -> getInputLine (green ++ s ++ plain) >>= cont
          Output pretty r -> case r of
            Right a -> outputStrLn (pretty a) >>= cont
            Left es -> for_ es outputStrLn >>= cont
        settings = Settings
          { complete = noCompletion
          , historyFile = Just "~/.local/surface/repl_history"
          , autoAddHistory = True }


-- Instances

instance Pretty Command where
  prettyPrec d command = case command of
    Run expr -> prettyPrec d expr
    TypeOf expr -> showsUnaryWith prettyPrec ":type" d expr
    REPL.WHNF expr -> showsUnaryWith prettyPrec ":whnf" d expr
    Help -> showString ":help"
    Quit -> showString ":quit"
