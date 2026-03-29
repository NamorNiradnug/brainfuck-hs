module Main (main) where

import qualified Brainfuck as BF
import Control.Exception (throw)
import Data.Function
import Options.Applicative

data Command = AST | Interpret | Compile | Execute

data Options = Options
  { source :: FilePath,
    optCommand :: Command
  }

options :: Parser Options
options = do
  source <- argument str (metavar "FILE")
  optCommand <-
    hsubparser
      ( command "ast" (info (pure AST) (progDesc "Parse and dump the AST"))
          <> command "run" (info (pure Interpret) (progDesc "Run directly without optimizations"))
          <> command "compile" (info (pure Compile) (progDesc "Compile and output the optimized representation"))
          <> command "exec" (info (pure Execute) (progDesc "Compile and run the optimized script"))
      )
  return Options {..}

main :: IO ()
main = do
  Options {..} <-
    execParser $
      info (options <**> helper) $
        fullDesc <> progDesc "Brainfuck interpreter"
  code <- readFile source
  let program = either throw id $ BF.parse source code
  program & case optCommand of
    AST -> print
    Interpret -> BF.runIORuntime . BF.interpret
    Compile -> print . BF.compile
    Execute -> BF.runIORuntime . BF.execute . BF.compile
