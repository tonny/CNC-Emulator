module Main (main) where
--import UU.Parsing
import Char
import Scanner
--import Parser
--import GramaticaAbstracta

{-- fglasgow-exts -fallow-undecidable-instances --}

main :: IO ()
main = do s <- readFile "test"
          let token = scanner s
--          arbol <- parseIO pCodigoCnc (token)
          putStrLn (show token)
--          putStrLn (show arbol)
