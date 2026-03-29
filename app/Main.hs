module Main (main) where

import qualified Brainfuck as BF
import Control.Exception (throw)
import Options.Applicative

data Options = Options
  { source :: FilePath,
    optimize :: Bool
  }

options :: Parser Options
options = do
  source <- argument str (metavar "FILE")
  optimize <- switch (short 'O' <> long "optimize" <> help "Enable optimizations")
  return Options {..}

main :: IO ()
main = do
  Options {..} <-
    execParser $
      info (options <**> helper) $
        fullDesc <> progDesc "Brainfuck interpreter"
  code <- readFile source
  let program = either throw id $ BF.parse source code
  BF.runIORuntime $ (if optimize then BF.execute . BF.compile else BF.interpret) program
