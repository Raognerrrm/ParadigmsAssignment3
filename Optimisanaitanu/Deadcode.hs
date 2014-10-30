module Deadcode where
import IParser
import FlowGraph


main = do
  let x = (IProgram [IFunction "main" (IArguments []) []])
  let y = (IProgram [IFunction "main" (IArguments []) []])
  if x == y then putStrLn "true"
  else putStrLn "false"