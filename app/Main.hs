module Main where

import Data.Foldable (for_)
import Data.Result
import Data.Version (showVersion)
import Options.Applicative
import Parser
import qualified Paths_surface as Library (version)
import qualified REPL
import Text.Pretty

data Command
  = Run FilePath
  | Interactive

command :: Parser Command
command
  =  flag' Interactive (long "--interactive" <> short 'i')
 <|> Run <$> strArgument (metavar "FILE")

arguments :: ParserInfo Command
arguments = info
  (version <*> helper <*> Main.command)
    (fullDesc
  <> progDesc "Surface is a small experiment in proof refinement–style typechecking and evaluation of dependently-typed languages."
  <> header "surface - a dependently typed language with nothing much to say for itself")

main :: IO ()
main = do
  command <- execParser arguments
  case command of
    Interactive -> REPL.runREPL REPL.repl
    Run path -> do
      result <- parseFromFile source path
      printResult result

printResult :: (Traversable f, Pretty a) => Result (f a) -> IO ()
printResult result = case result of
  Result a -> for_ a prettyPrint
  Error es -> for_ es putStr

versionString :: String
versionString = "Surface version " <> showVersion Library.version

version :: Parser (a -> a)
version = infoOption versionString (long "version" <> short 'V' <> help "output the version of the program")
