module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore
import Mensajes

main :: IO ()
main = start gui

gui :: IO()
gui =
 do 
   fram <- frame [text := "CNC-Emulator"]
   pMain <- panel fram [ bgcolor := black, clientSize := sz 900 800]
-- pTeclado <- panel f []
-- t <- textCtrlRes f [text := "Hello StaticText!", clientSize := sz 400 400]
 
   video <- textCtrlRich pMain [ bgcolor := black , textColor := green
                               , font := fontFixed { _fontSize = 10 } ]

 ----------------------- Botones de la Pantalla -------------------------------

-- salir <- button pMain [text := "Salir"]
   a <- button pMain [text := "A", clientSize := sz 45 40, bgcolor := white]
   b <- button pMain [text := "B", clientSize := sz 45 40, bgcolor := white]
   c <- button pMain [text := "C", clientSize := sz 45 40, bgcolor := white]
   d <- button pMain [text := "D", clientSize := sz 45 40, bgcolor := white]
   e <- button pMain [text := "E", clientSize := sz 45 40, bgcolor := white]
   f <- button pMain [text := "F", clientSize := sz 45 40, bgcolor := white]
   g <- button pMain [text := "G", clientSize := sz 45 40, bgcolor := white]
   let ag = [a,b,c,d,e,f,g]

   h <- button pMain [text := "H", clientSize := sz 45 40, bgcolor := white]
   i <- button pMain [text := "I", clientSize := sz 45 40, bgcolor := white]
   j <- button pMain [text := "J", clientSize := sz 45 40, bgcolor := white]
   k <- button pMain [text := "K", clientSize := sz 45 40, bgcolor := white]
   l <- button pMain [text := "L", clientSize := sz 45 40, bgcolor := white]
   m <- button pMain [text := "M", clientSize := sz 45 40, bgcolor := white]
   n <- button pMain [text := "N", clientSize := sz 45 40, bgcolor := white]
   let hn = [h, i, j, k, l, m, n]

   o <- button pMain [text := "O", clientSize := sz 45 40, bgcolor := white]
   p <- button pMain [text := "P", clientSize := sz 45 40, bgcolor := white]
   q <- button pMain [text := "Q", clientSize := sz 45 40, bgcolor := white]
   r <- button pMain [text := "R", clientSize := sz 45 40, bgcolor := white]
   s <- button pMain [text := "S", clientSize := sz 45 40, bgcolor := white]
   t <- button pMain [text := "T", clientSize := sz 45 40, bgcolor := white]
   u <- button pMain [text := "U", clientSize := sz 45 40, bgcolor := white]
   let ou = [o,p,q,r,s,t,u]

   v <- button pMain [text := "V", clientSize := sz 45 40, bgcolor := white]
   w <- button pMain [text := "W", clientSize := sz 45 40, bgcolor := white]
   x <- button pMain [text := "X", clientSize := sz 45 40, bgcolor := white]
   y <- button pMain [text := "Y", clientSize := sz 45 40, bgcolor := white]
   z <- button pMain [text := "Z", clientSize := sz 45 40, bgcolor := white]
   eob <- button pMain [ text := "EOB", clientSize := sz 90 40
                       , bgcolor := yellow ]
   let veob = [v,w,x,y,z,eob]

   ins <- button pMain [ text := "INS", clientSize := sz 45 40, bgcolor := blue]
   del <- button pMain [ text := "DEL", clientSize := sz 45 40, bgcolor := blue]
   err <- button pMain [ text := "ERROR\nMSOS", clientSize := sz 45 40
                       , font := fontFixed { _fontSize = 8 }
                       , bgcolor := blue]
   hel <- button pMain [ text := "HELP", clientSize := sz 45 40
                       , bgcolor := blue]
--                       , font := fontFixed { _fontSize = 8 }]
   zoin <- button pMain [ text := "ZOOM\nIN", clientSize := sz 45 40
                        , font := fontFixed { _fontSize = 8 }
                        , bgcolor := blue]
   zoout <- button pMain [ text := "ZOOM\nOUT", clientSize := sz 45 40
                         , font := fontFixed { _fontSize = 8 }
                         , bgcolor := blue]
   espac <- button pMain [text := "SPACE", clientSize := sz 45 40
                         , font := fontFixed { _fontSize = 8}
                         , bgcolor := blue]
   let inspac = [ins,del,err,hel,zoin,zoout,espac]

   siete <- button pMain [ text := "@\n7", clientSize := sz 45 40
                         , font := fontFixed { _fontSize = 8 }]
   ocho <- button pMain [ text := "(\n8", clientSize := sz 45 40
                        , font := fontFixed { _fontSize = 8 }]
   nueve <- button pMain [ text := ")\n9", clientSize := sz 45 40
                         , font := fontFixed { _fontSize = 8 }]
   eslash <- button pMain [ text := "/", clientSize := sz 45 40, bgcolor := blue
                          , font := fontFixed { _fontSize = 16 }]
   arriba <- button pMain [ text := "^", clientSize := sz 45 40, bgcolor := blue 
                          , font := fontFixed { _fontSize = 18 }] 
   let num1 = [siete,ocho,nueve,eslash]

   cuatro <- button pMain [ text := "$\n4", clientSize := sz 45 40
                          , font := fontFixed { _fontSize = 8 }]
   cinco <- button pMain [ text := "%\n5", clientSize := sz 45 40
                         , font := fontFixed { _fontSize = 8 }]
   seis <- button pMain [ text := "&\n6", clientSize := sz 45 40
                        , font := fontFixed { _fontSize = 8 }]
   mult <- button pMain [ text := "X", clientSize := sz 45 40, bgcolor := blue 
                        , font := fontFixed { _fontSize = 16 }]
   izq <- button pMain [ text := "<", clientSize := sz 45 40, bgcolor := blue
                       , font := fontFixed { _fontSize = 14 }]
   der <- button pMain [ text := ">", clientSize := sz 45 40, bgcolor := blue
                       , font := fontFixed { _fontSize = 14 }]
  
   uno <- button pMain [ text := "*\n1", clientSize := sz 45 40
                       , font := fontFixed { _fontSize = 8 }]
   dos <- button pMain [ text := "\"\n2", clientSize := sz 45 40
                       , font := fontFixed { _fontSize = 8 }]
   tres <- button pMain [ text := "!\n3", clientSize := sz 45 40
                        , font := fontFixed { _fontSize = 8 }]
   res <- button pMain [ text := "-", clientSize := sz 45 40, bgcolor := blue
                       , font := fontFixed { _fontSize = 14 }]
   aba <- button pMain [ text := "V", clientSize := sz 45 40, bgcolor := blue
                       , font := fontFixed { _fontSize = 14 }]

   coma <- button pMain [ text := "<\n,", clientSize := sz 45 40
                        , font := fontFixed { _fontSize = 8 }]
   cero <- button pMain [ text := ";\n0", clientSize := sz 45 40
                        , font := fontFixed { _fontSize = 8 }]
   punto <- button pMain [ text := ">\n.", clientSize := sz 45 40
                         , font := fontFixed { _fontSize = 8 }]
   mas <- button pMain [ text := "+", clientSize := sz 45 40, bgcolor := blue
                       , font := fontFixed { _fontSize = 14 }]
   igual <- button pMain [ text := "=", clientSize := sz 45 40, bgcolor := blue
                         , font := fontFixed { _fontSize = 14 }]
   shift <- button pMain [ text := "SHIFT", clientSize := sz 45 40
                         , bgcolor := yellow
                         , font := fontFixed { _fontSize = 8 }]
   enter <- button pMain [ text := "ENTER", clientSize := sz 45 40
                         , bgcolor := blue
                         , font := fontFixed { _fontSize = 8 }]
   let coment = [coma,cero,punto,mas,igual,shift,enter]

   f1 <- button pMain [text := "F1", clientSize := sz 50 40, bgcolor := white]
   f2 <- button pMain [text := "F2", clientSize := sz 50 40, bgcolor := white]
   f3 <- button pMain [text := "F3", clientSize := sz 50 40, bgcolor := white]
   f4 <- button pMain [text := "F4", clientSize := sz 50 40, bgcolor := white]
   f5 <- button pMain [text := "F5", clientSize := sz 50 40, bgcolor := white]
   f6 <- button pMain [text := "F6", clientSize := sz 50 40, bgcolor := white]
   f7 <- button pMain [text := "F7", clientSize := sz 50 40, bgcolor := white]
   f8 <- button pMain [text := "F8", clientSize := sz 50 40, bgcolor := white]
   f9 <- button pMain [text := "F9", clientSize := sz 50 40, bgcolor := white]
   let f1f9 = [f1,f2,f3,f4,f5,f6,f7,f8,f9]

   up  <- button pMain [text := "^", clientSize := sz 50 40
                       , bgcolor := blue]
   f10 <- button pMain [ text := "F10", clientSize := sz 50 40
                       , bgcolor := white]
   f11 <- button pMain [ text := "F11", clientSize := sz 50 40
                       , bgcolor := white]
   f12 <- button pMain [ text := "F12", clientSize := sz 50 40
                       , bgcolor := white]
   f13 <- button pMain [ text := "F13", clientSize := sz 50 40
                       , bgcolor := white]
   f14 <- button pMain [ text := "F14", clientSize := sz 50 40
                       , bgcolor := white]
   dow <- button pMain [ text := "V"
                       , clientSize := sz 50 40
                       , bgcolor := blue]
   exit <- button pMain [ text := "EXIT"
                        , clientSize := sz 50 40
                        , bgcolor := yellow]
   
   etiqueta <- staticText pMain [ text    := "\\\n/\\ ROMI  MACH 9"
                                , bgcolor := white
                                , font    := fontFixed { _fontSize = 16 }]

   stop <- button pMain [ text       := "CYCLE\nSTOP"
                        , clientSize := sz 60 60
                        , bgcolor    := red
                        , font       := fontFixed { _fontSize = 8 }]

   blk <- button pMain [ text       := "BLK\nBLK"
                       , clientSize := sz 60 60
                       , bgcolor    := yellow
                       , font       := fontFixed { _fontSize = 8 }]

   start <- button pMain [ text       := "CYCLE\nSTART"
                         , clientSize := sz 60 60
                         , bgcolor    := green
                         , font       := fontFixed { _fontSize = 8 }]

   pEmergencia1 <- panel pMain [ bgcolor := red, clientSize := sz 100 100]
   pEmergencia2 <- panel pMain [ bgcolor := darkgrey, clientSize := sz 100 100]
   pSelector1 <- panel pMain [ bgcolor := white, clientSize := sz 100 100]
   pSelector2 <- panel pMain [ bgcolor := blue, clientSize := sz 100 100]

   ------ Definiendo la barra de menus y estado del simulador ------------------
   -----------------------------------------------------------------------------
   mArchivo <- menuPane          [text := menuArchivo]
   mNuevo   <- menuItem mArchivo [text := abrirArchivo]
   mSalir   <- menuItem mArchivo [text := salirArchivo]

   mOpciones <- menuPane [text := menuOpciones]
 
   mAyuda  <- menuPane        [text := menuAyuda]
   mAcerca <- menuItem mAyuda [text := acercaAyuda]

   --------------------- Barra de estador ---------------------------------------
   estado   <- statusField       [text := ""]

   ---------------------- Definiendo propiedades --------------------------------
   ------------------------------------------------------------------------------
   -- para que no se edite el texto del video
   textCtrlSetEditable video False

   appendText video "Hola mundo terminal"

   set mSalir [on command := close fram]

   set fram [ statusBar := [estado]
            , menuBar := [mArchivo, mOpciones, mAyuda]
            , layout := container pMain $ boxed "CNC Mach-9MP" $ column 20
                        [ row 1 [ boxed "" $ column 20
                                             [ row 10 [ fill $ widget video
                                                      , column 10 $ map (vfill . widget) f1f9]
                                             , row 0 [ hfill $ widget up
                                                      , hfill $ widget f10
                                                      , hfill $ widget f11
                                                      , hfill $ widget f12
                                                      , hfill $ widget f13
                                                      , hfill $ widget f14
                                                      , hfill $ widget dow
                                                      , hglue
                                                      , hfill $ widget exit]
                                             ]
                                , boxed "" $ column 0
                                             [ row 1 $ map widget ag
                                             , row 1 $ map (hfill . widget) hn
                                             , row 1 $ map (hfill . widget) ou
                                             , row 1 $ map (hfill . widget) veob
                                             , row 1 $ map (hfill . widget) inspac
--                                           , vglue
                                             , row 1 $ (map widget num1) ++ 
                                                       [hglue] ++
                                                       [hfill $ widget arriba]++
                                                       [hglue]
                                             , row 1 [ widget $ cuatro
                                                     , widget $ cinco
                                                     , widget $ seis
                                                     , widget $ mult
                                                     , hfill $ widget izq
                                                     , hfill $ widget der]
                                             , row 1 [ widget uno
                                                     , widget dos
                                                     , widget tres
                                                     , widget res
                                                     , hglue
                                                     , hfill $ widget aba
                                                     , hglue ]
                                            , row 1 $ map (hfill . widget) coment
                                            ]
                                ]
                        , row 20 [ boxed "" $ row 1 [ fill $ widget $ pEmergencia1
                                                   , fill $ widget $ pEmergencia2
                                                   , fill $ widget $ pSelector1
                                                   , fill $ widget $ pSelector2]
                                , column 20 [ boxed "" $ row 0 [ hfill $ widget etiqueta]
                                            , row 20 [ widget $ stop
                                                     , widget $ blk
                                                     , widget $ start]
                                            ]
                                ]
                        ]
            ]

  -- set salir [on command := set t [text := "Cambio"]]
{-
   set fram [ statusBar := [estado]
            , menuBar   := [mArchivo,mOpciones,mAyuda]
            , layout := container pMain $ boxed "CNC Mach-9MP" $ column 20 
                        [ row 1 [ column 1 [ fill $ widget video] 
                                , column 10 $ map (vfill . widget) f1f9 
                                , boxed "" $ column 0 [ row 1 $ map widget ag
                                                      , row 1 $ map (hfill . widget) hn
                                                      , row 1 $ map (hfill . widget) ou
                                                      , row 1 $ map (hfill . widget) veob
                                                      , row 1 $ map (hfill . widget) inspac
--                                                      , vglue
                                                      , row 1 $ (map widget num1) ++ 
                                                                [hglue] ++
                                                                [hfill $ widget arriba]++
                                                                [hglue]
                                                      , row 1 [ widget $ cuatro
                                                              , widget $ cinco
                                                              , widget $ seis
                                                              , widget $ mult
                                                              , hfill $ widget izq
                                                              , hfill $ widget der]
                                                      , row 1 [ widget uno
                                                              , widget dos
                                                              , widget tres
                                                              , widget res
                                                              , hglue
                                                              , hfill $ widget aba
                                                              , hglue
                                                              ]
                                                      , row 1 $ map (hfill . widget) coment
                                                      ]
                                ]
                        , row 2 [row 0 [ hfill $ widget up
                                       , hfill $ widget f10
                                       , hfill $ widget f11
                                       , hfill $ widget f12
                                       , hfill $ widget f13
                                       , hfill $ widget f14
                                       , hfill $ widget dow
                                       , hglue
                                       , hfill $ widget exit]]
                       ]               
            , clientSize := sz 700 800
            ]
-}

   return ()
