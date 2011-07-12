module Scanner where
--import UU.Parsing
import Char
{-# LANGUAGE UndecidableInstances #-}

type Fila    = Int
type Columna = Int
type Cadena  = String

data Simbolo = Simbolo Tipo Cadena Fila Columna

data Tipo = Keyword 
          | Error
          | EndBlock
          | Numero
    deriving (Eq, Ord)

instance Show Simbolo where
   show (Simbolo t s f c) = show t ++ show s ++ " " ++ show f ++ " " ++ show c ++ "\n"

instance Show Tipo where
   show Keyword  = "Palabra Reservada: "
   show Error    = "Error            : "
   show Numero   = "Numero           : "
   show EndBlock = "Fin de Bloque    : "

scanner :: Cadena -> [Simbolo]
scanner x = scan x 1 1

scan :: Cadena -> Fila -> Columna -> [Simbolo]
scan [] _ _     = []
scan (x:xs) f c 
 | x == '\n' = scan xs (f+1) 1
 | isSpace x = scan xs f (c+1)
 | x == ';'  = scan (eliminar xs) f (c+1)
 | elem x ['G','M'] = let res = funcionG xs in 
                       case res of
                        "error" -> (Simbolo Error "Error2 " f c):scan [] f c
                        _       -> (Simbolo Keyword res f c):scan (rm ((length res)-1) ) f (c+3)
 | elem x ['A'..'Z'] = (Simbolo Keyword  (x:[]) f c ):scan xs f (c+1) 
 | x == '#' = (Simbolo EndBlock (x:[]) f c): scan xs f (c+1) 
 | isDigit x || x == '-' = let num = entero xs
                               tam = length num
                           in 
                            case num of 
                            "error" -> (Simbolo Error "Numero Invalido" f c):scan [] f c
                            _       -> (Simbolo Numero (x:num) f c):scan (rm tam) f (c+(tam+1))
 | otherwise = return (Simbolo Error "Simbolo no valido" f c)
 where
 eliminar :: Cadena -> Cadena               
 eliminar f = dropWhile ( /= '\n') f
 
 rm :: Int -> Cadena               
 rm d = drop d xs
 
 funcionG :: Cadena -> Cadena
 funcionG []     = []
 funcionG (y:ys) | isDigit y && isDigit (head ys) = x:y:(head ys):funcionG []
                 | isDigit y                      = x:y:funcionG []
                 | otherwise                      = "error"

 entero :: Cadena -> Cadena
 entero []     = []
 entero (y:ys) | isDigit y = y:entero ys
               | y == '.'  = y:[] ++ takeWhile isDigit ys
               | otherwise = entero []

