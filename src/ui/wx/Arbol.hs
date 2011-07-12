module Arbol  where

-- Autor    : Antonio Mamani Q.
-- Proyecto : CNC-Emulador
-- Version  : 0.1

import Data.List
import Datas
import Graphics.UI.WX

data Arbol a = Bt a [Arbol a]
       deriving Show

--data Rose a = Node a [Rose a] | Nil

ramas :: Arbol (Paneles,String) -> [Arbol (Paneles,String)]
ramas (Bt _ res ) = res

ventanas :: [Arbol (Paneles, String)] -> [(Paneles, String)]
ventanas xs = map (\ x -> get x) xs
      where
      get (Bt a res) = a

-- Funcion que recibe un arbol, una lista vacia y el panel que estamos buscando
-- y nos devuelve sus hijos 
getHijos :: Arbol (Paneles, String) -> [Arbol (Paneles,String)] -> Paneles -> [(Paneles,String)]
getHijos (Bt _ [])        []     a = []
getHijos (Bt x h@(y:ys))  []     a | a == fst x = ventanas h
                                   | otherwise  = getHijos y ys a
getHijos (Bt _ [])        (x:xs) a = getHijos x xs a
getHijos (Bt b h@(x:res)) (y:ys) a | a == fst b = ventanas h
                                   | otherwise  = (getHijos x res a)++(getHijos y ys a)

--getPadre :: Arbol (Paneles, String) -> [Arbol (Paneles, String)] -> Paneles -> [(Paneles,String)]
--getPadre (Bt _ []) [] a = []
--getPadre (Bt x)

-- Funcion que crear botones, renderizando algunas caracteristicas basicas que
-- debe tener un bonton por defecto.
crearB :: Panel () -> String -> IO (Button ())
crearB p t = button p [ clientSize := sz 75 38
                          , bgcolor    := black
                          , font       := fontFixed { _fontSize = 8 }
                          , text       := t
                          , enabled    :~ not]
--                          , visible    :~ not] 


-- Funcion que maneja el arbol de paneles con sus respectivas ramas
crearP :: Panel () -> Arbol (Paneles,String)
crearP p =
  Bt (Raiz, "  ") [ Bt (Reposo, "REPOSO") []
                , opeManual p
                , ediPrograma p
                , cargarSalvar p 
                , refTrabajo p 
                , pruebaPrograma p
                , opeAutomatico p
                , monitor p
                , soporte p
                ]

opeManual :: Panel() -> Arbol (Paneles, String)
opeManual p = 
  Bt (OpeManual, "OPEPACION\nMANUAL")
      [ Bt (Volante    , "VOLANTE")        []
      , Bt (Continuo   , "CONTINUO")       []
      , Bt (Incremental, "INCRE-\nMENTAL") []
      , Bt (MedirManual, "MEDIR-MANUAL")   []
      , Bt (Mdi        , "MDI")            [ Bt (GraficosMdi   , "GRAFICOS")       []
                                                    , Bt (VacioMdi1     , "     ")          []
                                                    , Bt (StatusMdi     , "STATUS")         []
                                                    , Bt (VacioMdi2     , "     ")          []
                                                    , Bt (DiagnosticoMdi, "DIAGNOS-\nTICO") []
                                                    , Bt (VacioMdi3     , "     ")          []
                                                    , Bt (CodigoG       , "CODIGOS G")      []
                                                    , Bt (CodigoM       , "CODIGOS M")      []
                                                    , Bt (DirectMdi     , "DIRECT")         []
                                           ]
      , Bt (Vacio     , "   ")             []
      , Bt (RefAlmacen, "REFER.\nALMACEN") []
      , Bt (Referencia, "REFE-\nRENCIA")   []
      , Bt (RefMaquina, "REFE-\nMAQUINA")  []
      ]

ediPrograma :: Panel() -> Arbol (Paneles, String)
ediPrograma p =
  Bt (EdiPrograma, "EDICION\nPROGRAMA")
      [ Bt (Display, "DISPLAY") [ Bt (GraficosD, "GRAFICOS" ) []
                                         , Bt (ListaD   , "LISTA"    ) []
                                         , Bt (VacioD1  , "     "    ) []
                                         , Bt (Pesquisa , "PESQUISA" ) []
                                         , Bt (VacioD2  , "     "    ) []
                                         , Bt (VacioD3  , "     "    ) []
                                         , Bt (CodigoGD , "CODIGOS G") []
                                         , Bt (CodigoMD , "CODIGOS M") []
                                         , Bt (DirectD  , "DIRECT"   ) []
                                         ]
      , Bt (Editar, "EDITAR")   [ Bt (GraficosE, "GRAFICOS" ) []
                                         , Bt (ListaE   , "LISTA"    ) []
                                         , Bt (InserPrograma, "INSERTAR\nPROGRAMA") []
                                         , Bt (PesquisaE, "PESQUISA" ) []
                                         , Bt (VacioE1  ,  "   "      ) []
                                         , Bt (VacioE2  ,  "   "      ) []
                                         , Bt (CodigoGE ,  "CODIGOS G") []
                                         , Bt (CodigoME ,  "CODIGOS M") []
                                         , Bt (DirectE  ,  "DIRECT"   ) []
                                         ]
      , Bt (Instruir, "INSTRUIR") [ Bt (VolanteI,  "INSTRUIR") []
                                           , Bt (ContinuoI, "CONTINUO") []
                                           , Bt (IncrementalI, "INCRE-\nMENTAL" ) []
                                           , Bt (VacioI1, "   "      ) []
                                           , Bt (InstruirMan, "INSTRUIR\nMANUAL") []
                                           , Bt (InstruirMdi, "INSTRUIR\nMDI"   ) []
                                           , Bt (BorrarProg, "BORRAR\nPROGRAMA" ) []
                                           , Bt (VacioI2,  "    "     ) []
                                           , Bt (DirectI,  "DIRECT"   ) []
                                           ]
      , Bt (ProgNuevo    , "PROG NUEV")          []
      , Bt (ProxPrograma , "PROX PROG")          []
      , Bt (RenumPrograma, "RENUMERAR\nPROGRAMA")[]
      , Bt (BorrarProgra , "BORRAR\nPROGRAMA")   []
      , Bt (BorrarTodos  , "BORRAR\nTODOS")      []
      , Bt (Direct       , "DIRECT")             []
      ]

cargarSalvar :: Panel () -> Arbol (Paneles, String)
cargarSalvar p = 
  Bt (CargarSalvar      , "CARGAR /\nSALVAR")
      [ Bt (Salvar      , "SALVAR")    []
      , Bt (Verificar   , "VERIFICAR") []
      , Bt (Cargar      , "CARGAR" )   [] --aqui existe mas menus
      , Bt (VacioC1     , "   ")       []
      , Bt (VacioC2     , "   ")       []
      , Bt (VacioC3     , "   ")       []
      , Bt (VacioC4     , "   ")       []
      , Bt (SelDisSalvar, "SEL DIS\nSALVAR") []
      , Bt (SelDisCargar, "SEL DIS\nCARGAR") []
      ]

refTrabajo :: Panel () -> Arbol (Paneles,String)
refTrabajo p = 
  Bt (RefTrabajo,"REFER.\nTRABAJO")
      [ Bt (Metrico     , "METRICO"              ) []
      , Bt (Pulgada     , "PULGADA"              ) []
      , Bt (IgnoraBloque, "IGNORA\nBLOQUE"       ) []
      , Bt (ParadaOpcio , "PARADA\nOPCIONAL"     ) []
      , Bt (IniMedioProg, "INICIO ME-\nDIO PROGR") []
      , Bt (RefHerramien, "REFER. DE\nHERRAM."   ) []
      , Bt (CorrecFija  , "CORREC.\nFIJAC"       ) []
      , Bt (Status      , "STATUS"               ) []
      , Bt (DirectRT    , "DIRECT"               ) []
      ]
pruebaPrograma :: Panel () -> Arbol (Paneles, String)
pruebaPrograma p = 
  Bt (PruebaPrograma, "HACER PRU\nBA PROG") 
      [ Bt (VarifRapido , "VERIF.\nRAPIDO"   ) []
      , Bt (VerConAvance, "VERIF.\nCON VANCE") []
      , Bt (EjecutarSeco, "EJECUTA\nSECO"    ) []
      , Bt (EjecutCeroZ , "EJECUTA\nSECO Z"  ) []
      , Bt (VacioPru1   , "  ") []
      , Bt (VacioPru2   , "  ") []
      , Bt (VacioPru3   , "  ") []
      , Bt (VacioPru4   , "  ") []
      , Bt (VacioPru5   , "  ") []
      ]

opeAutomatico :: Panel () -> Arbol (Paneles, String)
opeAutomatico p =
  Bt (OpeAutomatico, "OPERACION\nAUTOMAT.")
      [ Bt (ReferTrabajo, "REFER.\nTRABAJO") []
      , Bt (Movimiento  , "MOVIMIENTO"     ) []
      , Bt (MdiOA       , "MDI"            ) []
      , Bt (MonitorOA   , "MONITOR"        ) []
      , Bt (Parametros  , "PARA-\nMETROS"  ) []
      , Bt (Edicion     , "EDICION"        ) []
      , Bt (StatusOA    , "STATUS"         ) []
      , Bt (Graficos    , "GRAFICOS"       ) []
      , Bt (DirectOA    , "DIRECT"         ) []
      ]

monitor :: Panel () -> Arbol (Paneles, String)
monitor p = 
  Bt (Monitor, "MONITOR")
      [ Bt (ProximoGrupo, "PROXIMO\nGRUPO") []
      , Bt (RecargaHerr , "RECARGA\nHERR.") []
      , Bt (RecargaTotal, "RECARGA\nTOTAL") []
      , Bt (Normal      , "NORMAL"        ) []
      , Bt (CargarM     , "CARGAR"        ) []
      , Bt (SalvarM     , "SALVAR"        ) []
      , Bt (Cerrar      , "CERRAR"        ) []
      , Bt (Diagnosticar, "DIAGNOST."     ) []
      , Bt (VacioM      , "   "           ) []
      ]

soporte :: Panel () -> Arbol (Paneles, String)
soporte p =
  Bt (Soporte, "SOPORTE")
      [ Bt (ProtegerProg, "PROTEGER\nPROGRAMA") []
      , Bt (VacioP1     ,  "   "              ) []
      , Bt (Diagnostico ,  "DIAGNOS-\nTICO"   ) []
      , Bt (ControlAcce ,  "CONTROL\nACCESO"  ) []
      , Bt (ParameAltMaq, "PARAMET.\nALT.MAQ.") []
      , Bt (Pal         , "PAL"               ) []
      , Bt (VacioP2     , "   "               ) []
      , Bt (VacioP3     , "   "               ) []
      , Bt (Logon       , "LOGON"             ) []
      ]


------------------------------------------------------------------------------------
{-
ramas :: Arbol (Paneles,Button()) -> [Arbol (Paneles,Button())]
ramas (Bt _ res ) = res

ventanas :: [Arbol (Paneles, IO(Button()))] -> [(Paneles, IO(Button()))]
ventanas xs = map (\ x -> get x) xs
      where
      get (Bt a res) = a

-- Funcion que recibe un arbol, una lista vacia y el panel que estamos buscando
-- y nos devuelve sus hijos 
getHijos :: Arbol (Paneles, IO(Button ())) -> [Arbol (Paneles,IO (Button ()))] -> Paneles -> [(Paneles,IO(Button()))]
getHijos (Bt _ [])        []     a = []
getHijos (Bt x h@(y:ys))  []     a | a == fst x = ventanas h
                                   | otherwise  = getHijos y ys a
getHijos (Bt _ [])        (x:xs) a = getHijos x xs a
getHijos (Bt b h@(x:res)) (y:ys) a | a == fst b = ventanas h
                                   | otherwise  = (getHijos x res a)++(getHijos y ys a) 

-- Funcion que maneja el arbol de paneles con sus respectivas ramas
crearP :: Panel () -> Arbol (Paneles,IO (Button ()))
crearP p =
  Bt (Raiz, crearB p "") [ Bt (Reposo, crearB p "REPOSO") []
                         , opeManual p
                         , ediPrograma p
                         , cargarSalvar p 
                         , refTrabajo p 
                         , pruebaPrograma p
                         , opeAutomatico p
                         , monitor p
                         , soporte p
                         ]

opeManual :: Panel() -> Arbol (Paneles, IO( Button()))
opeManual p = 
  Bt (OpeManual, crearB p "OPEPACION\nMANUAL")
      [ Bt (Volante    , crearB p "VOLANTE")        []
      , Bt (Continuo   , crearB p "CONTINUO")       []
      , Bt (Incremental, crearB p "INCRE-\nMENTAL") []
      , Bt (MedirManual, crearB p "MEDIR-MANUAL")   []
      , Bt (Mdi        , crearB p "MDI")            [ Bt (GraficosMdi   , crearB p "GRAFICOS")       []
                                                    , Bt (VacioMdi1     , crearB p "     ")          []
                                                    , Bt (StatusMdi     , crearB p "STATUS")         []
                                                    , Bt (VacioMdi2     , crearB p "     ")          []
                                                    , Bt (DiagnosticoMdi, crearB p "DIAGNOS-\nTICO") []
                                                    , Bt (VacioMdi3     , crearB p "     ")          []
                                                    , Bt (CodigoG       , crearB p "CODIGOS G")      []
                                                    , Bt (CodigoM       , crearB p "CODIGOS M")      []
                                                    , Bt (DirectMdi     , crearB p "DIRECT")         []
                                                    ]
      , Bt (Vacio     , crearB p "   ")             []
      , Bt (RefAlmacen, crearB p "REFER.\nALMACEN") []
      , Bt (Referencia, crearB p "REFE-\nRENCIA")   []
      , Bt (RefMaquina, crearB p "REFE-\nMAQUINA")  []
      ]

ediPrograma :: Panel() -> Arbol (Paneles, IO(Button()))
ediPrograma p =
  Bt (EdiPrograma,crearB p "EDICION\nPROGRAMA")
      [ Bt (Display, crearB p "DISPLAY") [ Bt (GraficosD, crearB p "GRAFICOS" ) []
                                         , Bt (ListaD   , crearB p "LISTA"    ) []
                                         , Bt (VacioD1  , crearB p "     "    ) []
                                         , Bt (Pesquisa , crearB p "PESQUISA" ) []
                                         , Bt (VacioD2  , crearB p "     "    ) []
                                         , Bt (VacioD3  , crearB p "     "    ) []
                                         , Bt (CodigoGD , crearB p "CODIGOS G") []
                                         , Bt (CodigoMD , crearB p "CODIGOS M") []
                                         , Bt (DirectD  , crearB p "DIRECT"   ) []
                                         ]
      , Bt (Editar, crearB p "EDITAR")   [ Bt (GraficosE, crearB p "GRAFICOS" ) []
                                         , Bt (ListaE   , crearB p "LISTA"    ) []
                                         , Bt (InserPrograma, crearB p "INSERTAR\nPROGRAMA") []
                                         , Bt (PesquisaE, crearB p "PESQUISA" ) []
                                         , Bt (VacioE1  , crearB p "   "      ) []
                                         , Bt (VacioE2  , crearB p "   "      ) []
                                         , Bt (CodigoGE , crearB p "CODIGOS G") []
                                         , Bt (CodigoME , crearB p "CODIGOS M") []
                                         , Bt (DirectE  , crearB p "DIRECT"   ) []
                                         ]
      , Bt (Instruir, crearB p "INSTRUIR") [ Bt (VolanteI, crearB p "INSTRUIR") []
                                           , Bt (ContinuoI, crearB p "CONTINUO") []
                                           , Bt (IncrementalI, crearB p "INCRE-\nMENTAL" ) []
                                           , Bt (VacioI1, crearB p "   "      ) []
                                           , Bt (InstruirMan, crearB p "INSTRUIR\nMANUAL") []
                                           , Bt (InstruirMdi, crearB p "INSTRUIR\nMDI"   ) []
                                           , Bt (BorrarProg, crearB p "BORRAR\nPROGRAMA" ) []
                                           , Bt (VacioI2, crearB p "    "     ) []
                                           , Bt (DirectI, crearB p "DIRECT"   ) []
                                           ]
      , Bt (ProgNuevo    , crearB p "PROG NUEV")          []
      , Bt (ProxPrograma , crearB p "PROX PROG")          []
      , Bt (RenumPrograma, crearB p "RENUMERAR\nPROGRAMA")[]
      , Bt (BorrarProgra , crearB p "BORRAR\nPROGRAMA")   []
      , Bt (BorrarTodos  , crearB p "BORRAR\nTODOS")      []
      , Bt (Direct       , crearB p "DIRECT")             []
      ]

cargarSalvar :: Panel () -> Arbol (Paneles, IO (Button ()))
cargarSalvar p = 
  Bt (CargarSalvar      , crearB p "CARGAR /\nSALVAR")
      [ Bt (Salvar      , crearB p "SALVAR")    []
      , Bt (Verificar   , crearB p "VERIFICAR") []
      , Bt (Cargar      , crearB p "CARGAR" )   []
      , Bt (VacioC1     , crearB p "   ")       []
      , Bt (VacioC2     , crearB p "   ")       []
      , Bt (VacioC3     , crearB p "   ")       []
      , Bt (VacioC4     , crearB p "   ")       []
      , Bt (SelDisSalvar, crearB p "SEL DIS\nSALVAR") []
      , Bt (SelDisCargar, crearB p "SEL DIS\nCARGAR") []
      ]

refTrabajo :: Panel () -> Arbol (Paneles,IO (Button ()))
refTrabajo p = 
  Bt (RefTrabajo, crearB p "REFER.\nTRABAJO")
      [ Bt (Metrico     , crearB p "METRICO")               []
      , Bt (Pulgada     , crearB p "PULGADA")               []
      , Bt (IgnoraBloque, crearB p "IGNORA\nBLOQUE")        []
      , Bt (ParadaOpcio , crearB p "PARADA\nOPCIONAL")      []
      , Bt (IniMedioProg, crearB p "INICIO ME-\nDIO PROGR") []
      , Bt (RefHerramien, crearB p "REFER. DE\nHERRAM.")    []
      , Bt (CorrecFija  , crearB p "CORREC.\nFIJAC")        []
      , Bt (Status      , crearB p "STATUS")                []
      , Bt (DirectRT    , crearB p "DIRECT")                []
      ]
pruebaPrograma :: Panel () -> Arbol (Paneles, IO(Button ()))
pruebaPrograma p = 
  Bt (PruebaPrograma, crearB p "HACER PRU\nBA PROG") 
      [ Bt (VarifRapido , crearB p "VERIF.\nRAPIDO")    []
      , Bt (VerConAvance, crearB p "VERIF.\nCON VANCE") []
      , Bt (EjecutarSeco, crearB p "EJECUTA\nSECO")     []
      , Bt (EjecutCeroZ , crearB p "EJECUTA\nSECO Z")   []
      , Bt (VacioPru1   , crearB p "  ") []
      , Bt (VacioPru2   , crearB p "  ") []
      , Bt (VacioPru3   , crearB p "  ") []
      , Bt (VacioPru4   , crearB p "  ") []
      , Bt (VacioPru5   , crearB p "  ") []
      ]

opeAutomatico :: Panel () -> Arbol (Paneles, IO(Button ()))
opeAutomatico p =
  Bt (OpeAutomatico, crearB p "OPERACION\nAUTOMAT.")
      [ Bt (ReferTrabajo, crearB p "REFER.\nTRABAJO") []
      , Bt (Movimiento  , crearB p "MOVIMIENTO")      []
      , Bt (MdiOA       , crearB p "MDI")             []
      , Bt (MonitorOA   , crearB p "MONITOR")         []
      , Bt (Parametros  , crearB p "PARA-\nMETROS")   []
      , Bt (Edicion     , crearB p "EDICION")         []
      , Bt (StatusOA    , crearB p "STATUS")          []
      , Bt (Graficos    , crearB p "GRAFICOS")        []
      , Bt (DirectOA    , crearB p "DIRECT")          []
      ]

monitor :: Panel () -> Arbol (Paneles, IO( Button ()))
monitor p = 
  Bt (Monitor,crearB p "MONITOR")
      [ Bt (ProximoGrupo, crearB p "PROXIMO\nGRUPO") []
      , Bt (RecargaHerr, crearB p "RECARGA\nHERR." ) []
      , Bt (RecargaTotal, crearB p "RECARGA\nTOTAL") []
      , Bt (Normal, crearB p "NORMAL")       []
      , Bt (CargarM, crearB p "CARGAR")      []
      , Bt (SalvarM, crearB p "SALVAR")      []
      , Bt (Cerrar, crearB p "CERRAR")       []
      , Bt (Diagnosticar, crearB p "DIAGNOST.") []
      , Bt (VacioM, crearB p "   ")      []
      ]

soporte :: Panel () -> Arbol (Paneles, IO( Button ()))
soporte p =
  Bt (Soporte, crearB p "SOPORTE")
      [ Bt (ProtegerProg, crearB p "PROTEGER\nPROGRAMA") []
      , Bt (VacioP1, crearB p "   ")                     []
      , Bt (Diagnostico, crearB p "DIAGNOS-\nTICO")      []
      , Bt (ControlAcce, crearB p "CONTROL\nACCESO")     []
      , Bt (ParameAltMaq, crearB p "PARAMET.\nALT.MAQ.") []
      , Bt (Pal, crearB p "PAL")          []
      , Bt (VacioP2, crearB p "   ")      []
      , Bt (VacioP3, crearB p "   ")      []
      , Bt (Logon, crearB p "LOGON")      []
      ]
-}

