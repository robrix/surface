module Main where

import Data.Semigroup
import Data.Version (showVersion)
import Options.Applicative
import qualified Paths_surface as Library (version)
import Surface.Command

command :: Parser Command
command
  =  flag' Interactive (long "interactive" <> short 'i' <> help "Launch the interactive REPL.")
 <|> flag Run Debug (long "debug" <> short 'd' <> help "Print debugging information.")
    <*> strArgument (metavar "FILE" <> help "The program to run.")

arguments :: ParserInfo Command
arguments = info
  (version <*> helper <*> Main.command)
    (fullDesc
  <> progDesc "Surface is a small experiment in proof refinement–style typechecking and evaluation of dependently-typed languages."
  <> header "surface - a dependently typed language with nothing much to say for itself")

main :: IO ()
main = execParser arguments >>= runCommand

versionString :: String
versionString = "Surface version " <> showVersion Library.version

version :: Parser (a -> a)
version = infoOption versionString (long "version" <> short 'V' <> help "Output version info.")
