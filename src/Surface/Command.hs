module Surface.Command where

data Command
  = Run FilePath
  | Debug FilePath
  | Interactive
