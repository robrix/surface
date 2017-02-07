module Main where

import Data.Foldable (for_)
import Data.Result
import Data.Version (showVersion)
import Parser
import qualified Paths_refinement as Library (version)
import Options.Applicative
import Text.Pretty

newtype Command = Run FilePath

command :: Parser Command
command = Run <$> strArgument (metavar "FILE")

arguments :: ParserInfo Command
arguments = info
  (version <*> helper <*> Main.command)
    (fullDesc
  <> progDesc "refinement is a small experiment in proof refinement–style typechecking and evaluation of dependently-typed languages."
  <> header "refinement - a dependently typed language with nothing much to say for itself")

main :: IO ()
main = do
  command <- execParser arguments
  case command of
    Run path -> do
      result <- parseFromFile module' path
      case result of
        Result a -> prettyPrint a
        Error es -> for_ es putStr

versionString :: String
versionString = "refinement version " <> showVersion Library.version

version :: Parser (a -> a)
version = infoOption versionString (long "version" <> short 'V' <> help "output the version of the program")
