{-# LANGUAGE GADTs #-}
module REPL where

import Context
import Control.Applicative
import Control.Monad.Free.Freer
import Data.Foldable (for_)
import Data.Result
import Data.Version (showVersion)
import Expr
import Judgement
import Parser
import qualified Paths_refinement as Library (version)
import System.Console.Haskeline
import Text.Pretty
import Text.Trifecta hiding (Result)

data REPLF a where
  Prompt :: String -> REPLF (Maybe String)
  Output :: Pretty a => Result a -> REPLF ()

type REPL = Freer REPLF

data Command
  = Run Expr
  | Help
  | Quit
  | Version

prompt :: String -> REPL (Maybe String)
prompt s = Prompt s `andThen` return

output :: Pretty a => Result a -> REPL ()
output a = Output a `andThen` return


repl :: REPL ()
repl = do
  input <- prompt "λ . "
  maybe (pure ()) handleInput input

handleInput :: String -> REPL ()
handleInput input =
  case Parser.parseString command input of
    Result Help -> output (Error
      [ ":help, :h, :? - print this help text"
      , ":quit, :q     - exit the REPL"
      , ":version      - print version information"
      ] :: Result ()) >> repl
    Result Version -> output (Error [ showVersion Library.version ] :: Result ()) >> repl
    Result Quit -> pure ()
    Result (Run expr) -> output (run (I 0, Nil) (infer expr)) >> repl
    error -> output error >> repl


command :: Parser Command
command = whiteSpace *> (char ':' *> meta <|> eval) <* eof <?> "command"
  where meta = (Help <$ (long "help" <|> short 'h' <|> short '?') <?> "help")
           <|> (Quit <$ (long "quit" <|> short 'q') <?> "quit")
           <|> (Version <$ (long "version" <|> short 'v') <?> "version")
           <?> "command; use :? for help"

        eval = Run <$> expr <?> "expression"

        short = symbol . (:[])
        long = symbol


runREPL :: REPL a -> IO a
runREPL = runInputT defaultSettings . iterFreer alg . fmap pure
  where alg :: (x -> InputT IO a) -> REPLF x -> InputT IO a
        alg cont repl = case repl of
          Prompt s -> getInputLine s >>= cont
          Output r -> case r of
            Result a -> outputStrLn (pretty a) >>= cont
            Error es -> for_ es outputStrLn >>= cont


-- Instances

instance Pretty Command where
  prettyPrec d command = case command of
    Run expr -> prettyPrec d expr
    Help -> showString ":help"
    Quit -> showString ":quit"
    Version -> showString ":version"
