module Ventana where

import Graphics.UI.WX
import Graphics.UI.WXCore
import Mensajes
import Datas 

type Texto  = String
type Tamano = Int
type Alerta = TextCtrl ()

-- Funcion que nos devuleve los menus predeterminados, sin que estos esten 
-- activos, eso quiere decir que no estan pintados
menuPrin :: Panel () -> MenuPrincipal
menuPrin p = MenuPrin { reposo          = crearBoton p "REPOSO"               8 black 
                      , opeManual       = crearBoton p "OPERACION\nMANUAL"    8 black
                      , ediPrograma     = crearBoton p "EDICION\nPROGRAMA"    8 black
                      , cargarSalvar    = crearBoton p "GUARDAR /\nSALIR"     8 black
                      , refTrabajo      = crearBoton p "REFER.\nTRABAJO"      8 black
                      , pruebaPrograma  = crearBoton p "HACER PRUE\nBA PROGR" 8 black
                      , opeAutomatico   = crearBoton p "OPERACION\nAUTOMANT." 8 black
                      , monitor         = crearBoton p "MONITOR"              8 black
                      , soporte         = crearBoton p "SOPORTE"              8 black
                      , onMando         = crearBoton p "ON\nMANDO"            8 black
                      , seguridadPuerta = crearBoton p "SEGURIDAD\nPUERTA"    8 black
                      , paraHusPrinc    = crearBoton p "PARA HUS.\nPRINC."    8 black
                      , operadorLibera  = crearBoton p "OPERADOR\nLIBERA"     8 black
                      , retrocedeCah    = crearBoton p "RETROCEDE\nCAH"       8 black
                      , jugHusHorario   = crearBoton p "JOG HUS.\nHORARIO"    8 black
                      , jugHusAntiHora  = crearBoton p "JOG HUS.\nANTI-HOR"   8 black
                      , manualRefriger  = crearBoton p "MANUAL\nREFRIGER"     8 black
                      , offRefriger     = crearBoton p "OFF\nREFRIGER"         8 black
                      , automatRefriger = crearBoton p "AUTOMAT.\nREGRIGER"   8 black
                      , onTVirutas      = crearBoton p "ON\nT.VIRUTAS"        8 black
                      , offTVirutas     = crearBoton p "OFF/RETR.\nT.VIRUTAS" 8 black
                      , limpiezaProtec  = crearBoton p "LIMPIEZA\nPROTEC. "   8 black
                      , vacioMenu       = crearBoton p ""                     1 black 
                      }

-- Funcion para crear Textos, renderizando algunas caracterizticas basicas que
-- debe tener un texto por defecto.
crearText :: Panel() -> Texto -> Color -> Tamano -> Int -> Int -> IO (TextCtrl ())
crearText p t cf ta l a = textCtrlRich p [ bgcolor    := cf
                                         , textColor  := green
                                         , font       := fontFixed { _fontSize = ta }
                                         , text       := t
                                         , clientSize := sz l a
                                         ]

-- Funcion que crear botones, renderizando algunas caracteristicas basicas que
-- debe tener un bonton por defecto.
crearBoton :: Panel () -> Texto -> Tamano -> Color -> IO(Button ())
crearBoton p t f c = button p [ clientSize := sz 45 40
                              , bgcolor    := c
                              , font       := fontFixed { _fontSize = f }
                              , text       := t
                              , enabled    :~ not] 

-- La funcion crearInterfaz agrega todos lo elementos que se encesita para
-- el funcionamiento de la fresadora, tambien maneja todos los estados y 
-- errores que se produce al momento de manipular la pantalla.
crearInterfaz :: Frame () -> Panel () -> IO () 
crearInterfaz fram pMain =
 do
   video <- panel pMain [ bgcolor := black
                        , clientSize := sz 600 300 ] 

   alert <- crearText video alerta black 20 400 200
   textVideo <- textCtrlRich video [ clientSize := sz 400 300
                                   , bgcolor    := grey 
                                   , textColor  := green
                                   , font       := fontFixed { _fontSize = 30 }
                                   , text       := "Fresa Apagada"
                                   ]

   menuPrincipal <-variable [ value := menuPrin video]

   --------------------- Barra de estador ---------------------------------------
   estado   <- statusField   [text := "Maquina Pagada"]

   estados <- toIO (fram,video,(VacioMP,VacioMI,VacioOM),(menuPrincipal),VacioM,[VacioM])

  -------------------- Botones de la Pantalla ---------------------------------
   a <- crearBoton pMain "A" 14 white
   b <- crearBoton pMain "B" 14 white
   c <- crearBoton pMain "C" 14 white
   d <- crearBoton pMain "D" 14 white
   e <- crearBoton pMain "E" 14 white
   f <- crearBoton pMain "F" 14 white
   g <- crearBoton pMain "G" 14 white
   let ag = [a,b,c,d,e,f,g]
   ----------------------------------------------------------------------------
   h <- crearBoton pMain "H" 14 white
   i <- crearBoton pMain "I" 14 white
   j <- crearBoton pMain "J" 14 white
   k <- crearBoton pMain "K" 14 white
   l <- crearBoton pMain "L" 14 white
   m <- crearBoton pMain "M" 14 white
   n <- crearBoton pMain "N" 14 white
   let hn = [h, i, j, k, l, m, n]
   ----------------------------------------------------------------------------
   o <- crearBoton pMain "O" 14 white
   p <- crearBoton pMain "P" 14 white
   q <- crearBoton pMain "Q" 14 white
   r <- crearBoton pMain "R" 14 white
   s <- crearBoton pMain "S" 14 white
   t <- crearBoton pMain "T" 14 white
   u <- crearBoton pMain "U" 14 white
   let ou = [o,p,q,r,s,t,u]
   ----------------------------------------------------------------------------
   v   <- crearBoton pMain "V"   14 white
   w   <- crearBoton pMain "W"   14 white
   x   <- crearBoton pMain "X"   14 white
   y   <- crearBoton pMain "Y"   14 white
   z   <- crearBoton pMain "Z"   14 white
   eob <- crearBoton pMain "EOB" 14 yellow
   set eob [clientSize := sz 90 40]
   let veob = [v,w,x,y,z,eob]
   ----------------------------------------------------------------------------
   ins   <- crearBoton pMain "INS"         9 blue
   del   <- crearBoton pMain "DEL"         9 blue
   err   <- crearBoton pMain "ERROR\nMSOS" 8 blue
   hel   <- crearBoton pMain "HELP"        9 blue
   zoin  <- crearBoton pMain "ZOOM\nIN"    8 blue
   zoout <- crearBoton pMain "ZOOM\nOUT"   8 blue
   espac <- crearBoton pMain "SPACE"       8 blue
   let inspac = [ins,del,err,hel,zoin,zoout,espac]
   ----------------------------------------------------------------------------
   siete  <- crearBoton pMain "@\n7"  8 white
   ocho   <- crearBoton pMain "(\n8"  8 white
   nueve  <- crearBoton pMain ")\n9"  8 white
   eslash <- crearBoton pMain "/"    16 blue
   arriba <- crearBoton pMain "^"    18 blue 
   let num1 = [siete,ocho,nueve,eslash]
   ----------------------------------------------------------------------------
   cuatro <- crearBoton pMain "$\n4" 8 white
   cinco  <- crearBoton pMain "%\n5" 8 white
   seis   <- crearBoton pMain "&\n6" 8 white
   mult   <- crearBoton pMain "X"   16 blue
   izq    <- crearBoton pMain "<"   14 blue
   der    <- crearBoton pMain ">"   14 blue
   let cude = [cuatro,cinco,seis,mult,izq,der]
   ----------------------------------------------------------------------------
   uno  <- crearBoton pMain "*\n1"  8 white
   dos  <- crearBoton pMain "\"\n2" 8 white
   tres <- crearBoton pMain "!\n3"  8 white
   res  <- crearBoton pMain "-"    14 blue
   aba  <- crearBoton pMain "V"    14 blue
   let unab = [uno,dos,tres,res,aba]
   ----------------------------------------------------------------------------
   coma  <- crearBoton pMain "<\n,"  8 white
   cero  <- crearBoton pMain ";\n0"  8 white
   punto <- crearBoton pMain ">\n."  8 white
   mas   <- crearBoton pMain "+"    14 blue
   igual <- crearBoton pMain "="    14 blue
   shift <- crearBoton pMain "SHIFT" 8 yellow 
   enter <- crearBoton pMain "ENTER" 8 blue
   let coment = [coma,cero,punto,mas,igual,shift,enter]
   set shift [on command := mostrarMenu estado video textVideo alert menuPrincipal] -- Avilita el menu principal
   ----------------------------------------------------------------------------
   f1 <- crearBoton pMain "F1" 14 white
   f2 <- crearBoton pMain "F2" 14 white
   f3 <- crearBoton pMain "F3" 14 white
   f4 <- crearBoton pMain "F4" 14 white
   f5 <- crearBoton pMain "F5" 14 white
   f6 <- crearBoton pMain "F6" 14 white
   f7 <- crearBoton pMain "F7" 14 white
   f8 <- crearBoton pMain "F8" 14 white
   f9 <- crearBoton pMain "F9" 14 white
   let f1f9 = [f1,f2,f3,f4,f5,f6,f7,f8,f9]
   ----------------------------------------------------------------------------
   up   <- crearBoton pMain "^"    18 blue
   f10  <- crearBoton pMain "F10"  14 white
   set f10 [ on command := do activar estado estados
                              repaint video]
   f11  <- crearBoton pMain "F11"  14 white
   f12  <- crearBoton pMain "F12"  14 white
   f13  <- crearBoton pMain "F13"  14 white
   f14  <- crearBoton pMain "F14"  14 white
   dow  <- crearBoton pMain "V"    14 blue
   exit <- crearBoton pMain "EXIT" 14 yellow
   let upex = [up,f10,f11,f12,f13,f14,dow,exit]
   ----------------------------------------------------------------------------
   etiqueta <- staticText pMain [ text    := "\\\n/\\ ROMI  MACH 9"
                                , bgcolor := white
                                , font    := fontFixed { _fontSize = 16 }]

   stop <- crearBoton pMain "CYCLE\nSTOP" 8 red
   set stop [ clientSize := sz 60 60 ]

   blk <- crearBoton pMain "BLK\nBLK" 8 yellow
   set blk [ clientSize := sz 60 60 ]

   start <- crearBoton pMain "CYCLE\nSTART" 8 green
   set start [ clientSize := sz 60 60 ] 
   ----------------------------------------------------------------------------
   let matriz = ag ++ hn ++ ou ++ veob ++ inspac ++ num1 ++ cude ++ unab ++
                coment ++ f1f9 ++ upex ++ [arriba,stop,blk,start]
   ----------------------------------------------------------------------------
--   pEmergencia1 <- panel pMain [ bgcolor := red, clientSize := sz 100 100]
   pEmergencia1 <- button pMain [ bgcolor := red, clientSize := sz 100 100
                                , on command := cambiar estado video textVideo inicio alert matriz]

   pEmergencia2 <- panel pMain [ bgcolor := darkgrey, clientSize := sz 100 100]
   pSelector1 <- panel pMain [ bgcolor := white, clientSize := sz 100 100]
   pSelector2 <- panel pMain [ bgcolor := blue, clientSize := sz 100 100]

   ---------------------- Definiendo propiedades --------------------------------
   ------------------------------------------------------------------------------
   -- para que no se edite el texto del video
   textCtrlSetEditable textVideo False

   set video [layout := floatCentre $ column 1 [fill $ widget textVideo] ] 

--   appendText video inicio

   ----------------------------------------------------------------------------
   set fram [ statusBar := [estado]
            , layout := minsize (sz 800 700) $ container pMain $ boxed "CNC Mach-9MP" $ column 20
                        [ row 1 [ boxed "" $ column 20
                                             [ row 10 [ boxed "" $ fill $ widget video
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
            , clientSize := sz 1100 760
            ]
   repaint f
----------------------------------------------------------------------------

-- funciona que activa y desactiva la funcionalidad del teclado de la pantalla
-- y a la vez mustra las alertas por pantalla.
cambiar :: StatusField -> Panel() -> Alerta -> Texto -> Alerta -> [Button()] -> IO()
cambiar s p te ca al bo =
  do set s [text := "Maquina Prendida"] 
--                   appendText t c
     mapM_ (\ tc -> set tc [ enabled :~ not ]) bo
     set te [ font      := fontFixed { _fontSize = 30 }
            , textColor := green
            , text      := ca]
     set p [layout :=minsize ( sz 400 250) $ column 1 [ row 1 [hglue, hfill $ widget al, hglue
                                       ]
                              , row 1 [ fill $ widget te] ] 
          ]
--                 textCtrlSetValue t c

-- Funciona que se encarga de dibujar y dar funcionalidad a los menus del video
-- de la fresadora.
mostrarMenu :: StatusField -> Panel() -> Alerta -> Alerta ->Var MenuPrincipal -> IO()
mostrarMenu s p t al mp =
  do set s [text := "La fresa ya no esta en parada de emergencia"]
     set t [ bgcolor := grey
           , textColor := green
           , text := "IND.ROMI S/A\n REV 80-001 \n CNC MACH9" 
           ]
     set al [text := "Servos Desconec."]
     rep <- reposo =<< (get mp value)
     set rep [clientSize := sz 70 33]

--     ven <- panel p [bgcolor := yellow]
     opm <- opeManual =<< (get mp value)
     set opm [clientSize := sz 75 33]
     edp <- ediPrograma =<< (get mp value)
     set edp [clientSize := sz 75 33]
     cas <- cargarSalvar =<<(get mp value)
     set cas [clientSize := sz 75 35]
     ref <- refTrabajo =<< (get mp value)
     set ref [clientSize := sz 75 35]
     prp <- pruebaPrograma =<< (get mp value)
     set prp [clientSize := sz 75 40]
     oau <- opeAutomatico =<< (get mp value)
     set oau [clientSize := sz 75 40]
     mon <- monitor =<< (get mp value)
     set mon [clientSize := sz 75 40]
     sop <- soporte =<< (get mp value)
     set sop [clientSize := sz 75 40]

     onm <- onMando =<< (get mp value)
     set onm [clientSize := sz 75 30]
     sep <- seguridadPuerta =<< (get mp value)
     set sep [clientSize := sz 75 30]
     php <- paraHusPrinc =<< (get mp value)
     set php [clientSize := sz 75 30]
     opl <- operadorLibera =<< (get mp value)
     set opl [clientSize := sz 75 30]
     rec <- retrocedeCah =<< (get mp value)
     set rec [clientSize := sz 75 30]
--     tes <- crearBoton p "test" 10 black
     set p [ layout := column 1 
                       [ row 1 [ column 1
                                 [ row 1 [ column 1 [ row 1 [fill $ widget al]
--                                                     , vglue
                                                     , row 1 [ fill $ widget t] 
                                                     ]
                                          , column 11 [  widget rep
                                                     ,  widget opm
                                                     ,  widget edp
                                                     ,  widget cas
                                                     ,  widget ref
                                                     ,  widget prp
                                                     ,  widget oau
                                                     ,  widget mon
                                                     ]]
                                 , row 10 [ hglue, hglue
                                         , (hfill . widget) onm
                                         , (hfill . widget) sep
                                         , (hfill . widget) php
                                         , (hfill . widget) opl
                                         , (hfill . widget) rec
                                         , hglue
                                         , hglue
                                         , widget sop]
                                 ]
                               ]
                       ]
           , clientSize := sz 687 420
           ]
    -- set p [ layout :=  column 0 [ row 0 [ hfill $ widget a] ]]
     repaint p


activar :: StatusField -> Ambiente -> IO ()
activar f am = 
  do case (getMenu am) of
      VacioM ->  do o <- (onMando =<< (get (getCuerpoP  (getCuerpoMenu am)) value ))
                    set o [ bgcolor := red ] 
                    set f [text := "Activo ON MANDO"]
      _      -> set f [text := "preciono F10"]

--     set f [statusBar := [est]]
--     set (getFrame am) [ statusBar := [estado]]
     
