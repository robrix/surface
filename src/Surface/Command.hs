module Surface.Command where

import Data.Foldable (for_, toList, traverse_)
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
          runSteps' (checkModule m) initialState

printResult :: Pretty a => Either [String] a -> IO ()
printResult result = case result of
  Right a -> prettyPrint a
  Left es -> for_ es putStrLn
