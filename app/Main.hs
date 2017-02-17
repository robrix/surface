module Main where

import Data.Foldable (for_, toList, traverse_)
import Data.Result
import Data.Semigroup
import Data.Version (showVersion)
import Judgement
import Options.Applicative
import Parser
import qualified Paths_surface as Library (version)
import qualified REPL
import Text.Pretty

data Command
  = Run FilePath
  | Debug FilePath
  | Interactive

command :: Parser Command
command
  =  flag' Interactive (long "interactive" <> short 'i' <> help "Launch the interactive REPL.")
 <|> Run <$> strArgument (metavar "FILE" <> help "The program to run.")

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
      printResult $ do
        modules <- result
        for_ modules (run . checkModule)
    Debug path -> do
      result <- parseFromFile source path
      traverse_ (traverse_ prettyPrint) $ do
        modules <- result
        return $ do
          m <- toList modules
          runSteps initialState (checkModule m)

printResult :: Pretty a => Result a -> IO ()
printResult result = case result of
  Result a -> prettyPrint a
  Error es -> for_ es putStr

versionString :: String
versionString = "Surface version " <> showVersion Library.version

version :: Parser (a -> a)
version = infoOption versionString (long "version" <> short 'V' <> help "Output version info.")
