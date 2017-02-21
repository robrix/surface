module Surface.Command where

import Data.Foldable (for_, toList, traverse_)
import Data.Result
import Parser
import qualified REPL
import Surface.Proof
import Text.Pretty

data Command
  = Run FilePath
  | Debug FilePath
  | Interactive

runCommand :: Command -> IO ()
runCommand command = case command of
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
          runSteps' initialState (checkModule m)

printResult :: Pretty a => Result a -> IO ()
printResult result = case result of
  Result a -> prettyPrint a
  Error es -> for_ es putStrLn
