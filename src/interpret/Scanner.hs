module Scanner where
--import UU.Parsing
import Char
{-# LANGUAGE UndecidableInstances #-}

data Simbolo = Simbolo Tipo String Fila Columna

type Fila = Int
type Columna = Int

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
--   show Entero   = "Entero           : "
--   show Decimal  = "Decimal          : "
   show Numero   = "Numero           : "
   show EndBlock = "Fin de Bloque    : "

--scan :: String -> Integer -> Integer -> [Token]
scanner x = (scan x 1 1)

scan [] _ _     = []
scan (x:xs) f c | x == '\n' = scan xs (f+1) 1
                | isSpace x = scan xs f (c+1)
                | x == ';'  = scan (eliminar xs) f (c+1)
                | x == 'G'  = let res = funcionG xs
                                in case res of
                                   "error" -> (Simbolo Error "Error2 " f c):scan [] f c
                                   _       -> (Simbolo Keyword res f c):scan (rm ((length res)-1) ) f (c+3)
                | elem x ['A'..'Z'] = (Simbolo Keyword  (x:[]) f c ):scan xs f (c+1) 
                | x == '#' = (Simbolo EndBlock (x:[]) f c): scan xs f (c+1) 
                | isDigit x || x == '-'  = let num = entero xs
                                               tam = length num
                                           in 
                                           case num of 
                                           "error" -> (Simbolo Error "Numero Invalido" f c):scan [] f c
                                           _       -> (Simbolo Numero (x:num) f c):scan (rm tam) f (c+(tam+1))

                                              {--   if elem '.' num 
                                                      then (Token Decimal (x:num) f c):scan (rm tam) f (c+(tam+1))
                                                      else (Token Entero (x:num) f c):scan (rm tam) f (c+(tam+1))--}
                | otherwise = return (Simbolo Error "Simbolo no valido" f c)

                where               
                  eliminar f = dropWhile ( /= '\n') f
                  
                  rm d = drop d xs

                  funcionG []     = []
                  funcionG (y:ys) | isDigit y && isDigit (head ys) = x:y:(head ys):funcionG [] 
                                  | isDigit y                      = x:y:funcionG []
                                  | otherwise                      = "error"

                  entero []     = []
                  entero (y:ys) | isDigit y = y:entero ys
                                | y == '.' = y:[]++(takeWhile isDigit ys)
                                | otherwise = entero []


{-
instance (Eq Tipo) => (Eq Token) where
   (Token Error _ _ _ ) == (Token Error _ _ _ ) = True
   (Token Entero _ _ _) == (Token Entero _ _ _) = True
   (Token Decimal _ _ _) == (Token Decimal _ _ _) = True
   (Token t1    s _ _ ) == (Token t2   s1 _ _ ) = t1 == t2 && s == s1

instance Ord Token where
  compare x y | x == y = EQ
              | x <= y = LT
              | otherwise = GT
  (Token tok1 str1 _ _ ) <= (Token tok2 str2 _ _ ) 
         = tok1 < tok2 || (tok1 == tok2 && str1 <= str2)

instance Symbol Token where

--obtenerValor :: Token -> String
obtenerValor (Token _ v _ _ ) = v

tSym :: Tipo -> String -> Parser Token String
tSym tok str = obtenerValor <$> pSym (Token tok str 0 0)


pKeyword k = tSym Keyword k
pEntero    = (\ x ->(read x)::Int) <$> tSym Entero ""
pDecimal   = (\ x ->(read x)::Float) <$> tSym Decimal ""
-}
