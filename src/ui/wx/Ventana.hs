module Ventana where

-- Autor    : Antonio Mamani Q.
-- Proyecto : CNC-Emulador
-- Version  : 0.1

import Graphics.UI.WX
import Graphics.UI.WXCore
import Mensajes
import Datas 
import Arbol

type Texto  = String
type Tamano = Int
type Alerta = TextCtrl ()
type PanelDer = Arbol (Paneles,String)

-- La funcion crearInterfaz agrega todos lo elementos que se encesita para
-- el funcionamiento de la fresadora, tambien maneja todos los estados y 
-- errores que se produce al momento de manipular la pantalla.
crearInterfaz :: Frame () -> Panel () -> IO () 
crearInterfaz fram pMain =
 do
   video     <- panel pMain [ bgcolor := black
                            , clientSize := sz 600 300 ] 

   alert     <- crearText video alerta black 20 400 200
   textVideo <- textCtrlRich video [ clientSize := sz 400 300
                                   , bgcolor    := grey 
                                   , textColor  := green
                                   , font       := fontFixed { _fontSize = 30 }
                                   , text       := "Fresa Apagada"
                                   ]

   --------------------- Barra de estador ---------------------------------------
   estado           <- statusField [text  := "Maquina Pagada"]
   menuActual       <- variable    [value := (Raiz,False)]
   menus            <- variable    [value := [Raiz]]
   menuInf          <- variable    [value := (VacioMI,0,VacioMI,0)]
   paradaEmergencia <- variable    [value := False]
   servos           <- variable    [value := False]
   ---------------- Ambiente que se maneja los estados del simulador -------------
   ambiente         <- return $ (menuActual,menus,menuInf,paradaEmergencia,servos)
   ------------------------ crear el arbol de paneles ----------------------------
   panelDer         <- return $ crearP video 
   ---------------creación de botones para los paneles video ---------------------
   let pan = [ "ON\nMANDO"        ,"SEGURIDAD\nPUERTA"   ,"PARA HUS.\nPRINC.","OPERADOR\nLIBERA","RETROCEDE\nCAH"
             , "JOG HUS.\nHORARIO","JOG HUS.\nANTI-HOR"  ,"MANUAL\nREFRIGER" ,"OFF\nREFRIGER"   ,"AUTOMAT.\nREGRIGER" 
             , "ON\nT.VIRUTAS"    ,"OFF/RETR.\nT.VIRUTAS","LIMPIEZA\nPROTEC.","      "          ,"   "
             ] 
   paneles@[ onMando       ,seguridadPuerta,paraHusPrinc,operadorLibera ,retrocedeCah,jugHusHorario
           , jugHusAntiHora,manualRefriger ,offRefriger ,automatRefriger,onTVirutas  ,offTVirutas 
           , limpiezaProtec,vacioMenu1     , vacioMenu2
           ] :: [Button ()] <- mapM (\l -> crearB video l) pan
 
   mapM_ (\ bo -> set bo [visible :~ not]) paneles

   let menuInferior = [ [onMando, seguridadPuerta, paraHusPrinc, operadorLibera, retrocedeCah]
                      , [jugHusHorario, jugHusAntiHora, manualRefriger, offRefriger, automatRefriger]
                      , [onTVirutas, offTVirutas, limpiezaProtec, vacioMenu1, vacioMenu2]]

  ----------------------------------------------------------------------------- 
  -------------------- Botones de la Pantalla ---------------------------------

   ag@[a,b,c,d,e,f,g] :: [Button ()] <- mapM (\l -> crearBoton pMain l 14 white) (map (:"") ['A'..'G'])
   ----------------------------------------------------------------------------
   hn@[h,i,j,k,l,m,n] :: [Button ()] <- mapM (\l -> crearBoton pMain l 14 white) (map (:"") ['H'..'N'])
   ----------------------------------------------------------------------------
   ou@[o,p,q,r,s,t,u] :: [Button ()] <- mapM (\l -> crearBoton pMain l 14 white) (map (:"") ['O'..'U'])
   ----------------------------------------------------------------------------
   [v,w,x,y,z]        :: [Button ()] <- mapM (\l -> crearBoton pMain l 14 white) (map (:"") ['V'..'Z'])
   eob <- crearBoton pMain "EOB" 14 yellow
   set eob [clientSize := sz 90 40]
   let veob = [v,w,x,y,z,eob]
   ----------------------------------------------------------------------------
   let tip = ["INS","DEL","ERROR\nMSOS","HELP","ZOOM\nIN","ZOOM\nOUT","SPACE"]
   inspac@[ins,del,err,hel,zoin,zoout,espac ] :: [Button ()] <- mapM (\l -> crearBoton pMain l 8 blue) tip
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
   set shift [on command := mostrarMenu estado ambiente video textVideo alert panelDer menuInferior ] -- Avilita el menu principal
   enter <- crearBoton pMain "ENTER" 8 blue
--   set enter [on command := esRojo estado menuPrincipal]
   let coment = [coma,cero,punto,mas,igual,shift,enter] 
   ----------------------------------------------------------------------------
   let f19 = zipWith (\a b -> a:b:[]) (repeat 'F') ['1'..'9']
   f1f9@[f1,f2,f3,f4,f5,f6,f7,f8,f9] :: [Button()] <- mapM (\l -> crearBoton pMain l 14 white) f19

   set f2 [ on command := do moverPanel estado "f2" ambiente  panelDer
                             mostrarMenu estado ambiente video textVideo alert panelDer menuInferior]

   set f3 [ on command := do moverPanel estado "f3" ambiente  panelDer
                             mostrarMenu estado ambiente video textVideo alert panelDer menuInferior]

   set f4 [ on command := do moverPanel estado "f4" ambiente  panelDer
                             mostrarMenu estado ambiente video textVideo alert panelDer menuInferior]

   set f5 [ on command := do moverPanel estado "f5" ambiente  panelDer
                             mostrarMenu estado ambiente video textVideo alert panelDer menuInferior]

   ----------------------------------------------------------------------------
   [f10,f11,f12,f13,f14] <- mapM (\l -> crearBoton pMain l 14 white) ["F10","F11","F12","F13","F14"]
   up   <- crearBoton pMain "^"    18 blue
   dow  <- crearBoton pMain "V"    14 blue
   exit <- crearBoton pMain "EXIT" 14 yellow
   let upex = [up,f10,f11,f12,f13,f14,dow,exit]

   set up [ on command := do mover estado ambiente "^" menuInferior
                             mostrarMenu estado ambiente video textVideo alert panelDer menuInferior]

   set f10 [ on command := activar estado ambiente "f10" menuInferior ]

   set f11 [ on command := activar estado ambiente "f11" menuInferior ]

   set f12 [ on command := activar estado ambiente "f12" menuInferior ] 

   set f13 [ on command := activar estado ambiente "f13" menuInferior ]

   set f14 [ on command := activar estado ambiente "f14" menuInferior ]

   set dow [ on command := do mover estado ambiente "V" menuInferior
                              mostrarMenu estado ambiente video textVideo alert panelDer menuInferior]

   set exit [ on command := do moverPanel estado "exit" ambiente  panelDer
                               mostrarMenu estado ambiente video textVideo alert panelDer menuInferior]

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
     set p [layout := minsize ( sz 400 250) $ column 1 [ row 1 [hglue, hfill $ widget al, hglue
                                       ]
                              , row 1 [ fill $ widget te] ] 
          ]
--                 textCtrlSetValue t c

-- Funciona que se encarga de dibujar y dar funcionalidad a los menus del video
-- de la fresadora.
-- AGARRAR LOS CASOS EN LOS CUALES SE ENCUATRA LA FRESA PARA CAMBIAR LOS PANELES ojo
mostrarMenu :: StatusField -> Ambiente -> Panel() -> Alerta -> Alerta -> PanelDer -> [[Button ()]] -> IO()
mostrarMenu s am p t al pd m =
  do valor <- get (getMenuInf am) value
     -- obtiene los botones que se pintaran en el menu inferior
     [onm,sep,php,opl,rec] <- return $ [ xs | xs <- m !! (segundo valor)] -- hablar en la documentación 
     let mi = m !! (segundo valor)

     ----------- para activar los paneles del video
     ma <- get (getMenu am) value
     let mp = map snd ( getHijos pd [] (fst ma))
--     [rep,opm,edp,cas,ref,prp,oau,mon,sop] <- return $ [ crearB p xs | xs <- mp]
     pe <- get (getParadaEmer am) value
     case fst ma of
      Raiz         -> do if pe
                          then set al [text := "Servos Desconec."]
                          else set al [text := "Parada de Emergencia"]
                         set t [ bgcolor := grey
                               , textColor := green
                               , text := "IND.ROMI S/A\n REV 80-001 \n CNC MACH9" 
                               ]
                         if snd ma 
                          then return() 
                          else do mapM_ (\ bo -> set bo [visible :~ not]) mi  
                                  varSet (getMenu am) (Raiz,True)

      OpeManual    -> do set al [text := "OPERACION MANUAL"]
                         varSet (getMenu am) (OpeManual,True)

      RefTrabajo   -> do set al [text := "REFERENCIA TRABAJO"]
                         varSet (getMenu am) (RefTrabajo,True)

      Mdi          -> do set al [text := "MDI"]
                         varSet (getMenu am) (Mdi,True)
 
      EdiPrograma  -> do set al [text := "EDICION PROGRAMA"]
                         varSet (getMenu am) (EdiPrograma,True)

      CargarSalvar -> do set al [text := "CARGAR SALVAR"]
                         varSet (getMenu am) (CargarSalvar,True)

      _         -> set s [text := "NOo funciona"]
 
     rep <- crearB p (mp !! 0)
     opm <- crearB p (mp !! 1)
     edp <- crearB p (mp !! 2)
     cas <- crearB p (mp !! 3)
     ref <- crearB p (mp !! 4)
     prp <- crearB p (mp !! 5)
     oau <- crearB p (mp !! 6)
     mon <- crearB p (mp !! 7)
     sop <- crearB p (mp !! 8)

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
                                         , hfill $ widget onm
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

-- funcion para activar los botones del menu inferior
activar :: StatusField -> Ambiente -> String -> [[Button ()]] -> IO ()
activar st am co mi = 
  do valor <- get (getMenuInf am) value
     case segundo valor of
      0 -> case co of
            "f10" -> do fila <- return $ ( mi !! 0) !! 0
                        set fila [ bgcolor := darkgrey ]
                        set st [text := "Activo On Mando"]
                        varSet (getMenuInf am) (OnMando,0,OnMando,cuarto valor)
                        desactivar (tercero valor) mi
            "f11" -> do fila <- return $ ( mi !! 0) !! 1
                        set fila [ bgcolor := darkgrey ]
                        set st [text := "Seguridad Puerta"]
                        varSet (getMenuInf am) (SeguridadPuerta,0,SeguridadPuerta,cuarto valor)
                        desactivar (tercero valor) mi
            "f12" -> do fila <- return $ ( mi !! 0) !! 2
                        set fila [ bgcolor := darkgrey ]
                        set st [text := "Parada Hus Princ"]
                        varSet (getMenuInf am) (ParaHusPrinc,0,ParaHusPrinc,cuarto valor)
                        desactivar (tercero valor) mi
            "f13" -> do fila <- return $ ( mi !! 0) !! 3
                        set fila [ bgcolor := darkgrey ]
                        set st [text := "Operador Libera"]
                        varSet (getMenuInf am) (OperadorLibera,0,OperadorLibera,cuarto valor)
                        desactivar (tercero valor) mi
            "f14" -> do fila <- return $ ( mi !! 0) !! 4
                        set fila [ bgcolor := darkgrey ]
                        set st [text := "Retrocede Cah"]
                        varSet (getMenuInf am) (RetrocedeCah,0,RetrocedeCah,cuarto valor)
                        desactivar (tercero valor) mi
            _     -> do set st [text := "NOOO"]

      1 -> case co of
            "f10" -> do fila <- return $ ( mi !! 1) !! 0
                        set fila [ bgcolor := darkgrey ]
                        set st [text := "Jug Hus Horario"]
                        varSet (getMenuInf am) (JugHusHorario,1,JugHusHorario,cuarto valor)
                        desactivar (tercero valor) mi
            "f11" -> do fila <- return $ ( mi !! 1) !! 1
                        set fila [ bgcolor := darkgrey ]
                        set st [text := "Jug Hus Anti Horario"]
                        varSet (getMenuInf am) (JugHusAntiHora,1,JugHusAntiHora,cuarto valor)
                        desactivar (tercero valor) mi
            "f12" -> do fila <- return $ ( mi !! 1) !! 2
                        set fila [ bgcolor := darkgrey ]
                        set st [text := "Manual Refriger"]
                        varSet (getMenuInf am) (ManualRefriger,1,ManualRefriger,cuarto valor)
                        desactivar (tercero valor) mi
            "f13" -> do fila <- return $ ( mi !! 1) !! 3
                        set fila [ bgcolor := darkgrey ]
                        set st [text := "Off Refriger"]
                        varSet (getMenuInf am) (OffRefriger,1,OffRefriger,cuarto valor)
                        desactivar (tercero valor) mi
            "f14" -> do fila <- return $ ( mi !! 1) !! 4
                        set fila [ bgcolor := darkgrey ]
                        set st [text := "Automatico Refriger"]
                        varSet (getMenuInf am) (AutomatRefriger,1,AutomatRefriger,cuarto valor)
                        desactivar (tercero valor) mi
            _     -> do set st [text := "NOOO"]

      2 -> case co of
            "f10" -> do fila <- return $ ( mi !! 2) !! 0
                        set fila [ bgcolor := darkgrey ]
                        set st [text := "On T. Virutas"]
                        varSet (getMenuInf am) (OnTVirutas,2,OnTVirutas,cuarto valor)
                        desactivar (tercero valor) mi
            "f11" -> do fila <- return $ ( mi !! 2) !! 1
                        set fila [ bgcolor := darkgrey ]
                        set st [text := "Off T. Virutas"]
                        varSet (getMenuInf am) (OffTVirutas,2,OffTVirutas,cuarto valor)
                        desactivar (tercero valor) mi
            "f12" -> do fila <- return $ ( mi !! 2) !! 2
                        set fila [ bgcolor := darkgrey ]
                        set st [text := "Limpieza Protec"]
                        varSet (getMenuInf am) (LimpiezaProtec,2,LimpiezaProtec,cuarto valor)
                        desactivar (tercero valor) mi
            "f13" -> do fila <- return $ ( mi !! 2) !! 3
                        set st [text := "No tiene funcionalidad 1"]
            "f14" -> do fila <- return $ ( mi !! 2) !! 4
                        set st [text := "No tiene funcionalidad 2"]
            _     -> do set st [text := "NOOO"]

      _ -> do set st [text := "No esta activo"]

-- funcion que desactiva los botones del menu inferior
desactivar :: MenuInferior -> [[Button ()]] -> IO ()
desactivar tmi mi = 
  do case tmi of
      OnMando         -> do bu <- return $ (mi !! 0) !! 0
                            set bu [ bgcolor := black ]
      SeguridadPuerta -> do bu <- return $ (mi !! 0) !! 1
                            set bu [ bgcolor := black ]
      ParaHusPrinc    -> do bu <- return $ (mi !! 0) !! 2
                            set bu [ bgcolor := black ]
      OperadorLibera  -> do bu <- return $ (mi !! 0) !! 3
                            set bu [ bgcolor := black ]
      RetrocedeCah    -> do bu <- return $ (mi !! 0) !! 4
                            set bu [ bgcolor := black ]
      JugHusHorario   -> do bu <- return $ (mi !! 1) !! 0
                            set bu [ bgcolor := black ]
      JugHusAntiHora  -> do bu <- return $ (mi !! 1) !! 1
                            set bu [ bgcolor := black ]
      ManualRefriger  -> do bu <- return $ (mi !! 1) !! 2
                            set bu [ bgcolor := black ]
      OffRefriger     -> do bu <- return $ (mi !! 1) !! 3
                            set bu [ bgcolor := black ]
      AutomatRefriger -> do bu <- return $ (mi !! 1) !! 4
                            set bu [ bgcolor := black ]
      OnTVirutas      -> do bu <- return $ (mi !! 2) !! 0
                            set bu [ bgcolor := black ]
      OffTVirutas     -> do bu <- return $ (mi !! 2) !! 1
                            set bu [ bgcolor := black ]
      LimpiezaProtec  -> do bu <- return $ (mi !! 2) !! 2
                            set bu [ bgcolor := black ]
      _               -> do bu <- return $ (mi !! 2) !! 4
                            set bu [ bgcolor := black ]

-- Funcion que nos permite mover los tres paneles que tiene el menu inferior
mover :: StatusField -> Ambiente -> String -> [[Button ()]] -> IO ()
mover st am co mi =
  do valor <- get (getMenuInf am) value
     mapM_ (\ bo -> set bo [visible :~ not]) (mi !! (segundo valor))
     case segundo valor of
      0 -> case co of
            "^" -> do varSet (getMenuInf am) (primero valor,1,tercero valor,0)
                      set st [text := "Se movio al panel inferior 1"]
                      mapM_ (\ bo -> set bo [visible :~ not]) (mi !! 1)
            "V" -> do varSet (getMenuInf am) (primero valor,2,tercero valor,0)
                      set st [text := "se movio al panel inferior 2"]
                      mapM_ (\ bo -> set bo [visible :~ not]) (mi !! 2)
      1 -> case co of
            "^" -> do varSet (getMenuInf am) (primero valor,2,tercero valor,1)
                      set st [text := "Se movio al panel inferior 2"]
                      mapM_ (\ bo -> set bo [visible :~ not]) (mi !! 2)
            "V" -> do varSet (getMenuInf am) (primero valor,0,tercero valor,1)
                      set st [text := "Se movio al panel inferior 0"]
                      mapM_ (\ bo -> set bo [visible :~ not]) (mi !! 0)
      2 -> case co of
            "^" -> do varSet (getMenuInf am) (primero valor,0,tercero valor,2)
                      set st [text := "Se movio al panel inferior 0"]
                      mapM_ (\ bo -> set bo [visible :~ not]) (mi !! 0)
            "V" -> do varSet (getMenuInf am) (primero valor,1,tercero valor,2)
                      set st [text := "Se movio al panel inferior 1"]
                      mapM_ (\ bo -> set bo [visible :~ not]) (mi !! 1)

          
      -- mapM_ (\ bo -> set bo [visible :~ not]) (mi !! (cuarto valor))
      -- else do mapM_ (\ bo -> set bo [visible :~ not]) (mi !! (segundo valor))
  
   --------------------------------------------------------------------------------

moverPanel :: statusField -> String -> Ambiente -> PanelDer -> IO ()
moverPanel sf st am md =
 do menup <- get (getMenu am) value
    memor <- get (getMemoria am) value
    let hijos = getHijos md [] (fst menup)
    case st of
     "f2"   -> do varSet (getMenu am) (fst (hijos!!1), False)
                  varSet (getMemoria am) (memor ++ [ fst menup ])
     "f3"   -> do varSet (getMenu am) (fst (hijos!!2), False)
                  varSet (getMemoria am) (memor ++ [ fst menup ])
     "f4"   -> do varSet (getMenu am) (fst (hijos!!3), False)
                  varSet (getMemoria am) (memor ++ [ fst menup ])
     "f5"   -> do varSet (getMenu am) (fst (hijos!!4), False)
                  varSet (getMemoria am) (memor ++ [ fst menup ]) 
     "exit" -> if length memor == 1 
                then do varSet (getMenu am) (Raiz, True)
                        varSet (getMemoria am) memor
                else do varSet (getMenu am) ( last $ init memor, True)
                        varSet (getMemoria am) (init memor)
