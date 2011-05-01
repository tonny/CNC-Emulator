module Datas where
import Graphics.UI.WX

type Exit       = [Menus]
type TipoMenu   = (PrincipalMenu,MenuInferior,OperacionMan) 
type CuerpoMenu = (Var MenuPrincipal)-- ,OperacionManual,Mdi,EdicionPrograma)
type Ambiente   = (Frame (),Panel (),TipoMenu, CuerpoMenu,Menus,Exit) 
--type Estados  = (Var Menus)

--getMenu :: Estados ->Var Menus
--getMenu (a) = a

data Menus = Principal -- esta caso llama a al data Principal Menu
           | Operacion
           | MDI
           | Edicion
           | VacioM
        deriving Eq

data PrincipalMenu = Reposo
                   | OpeManual
                   | DirPrograma
                   | Guradar
                   | RefTrabajo
                   | PruebaPrograma
                   | OpeAutimatico
                   | Monitor
                   | Soporte
                   | VacioMP
{-                   
                   | SeguridadPuerta
                   | ParaHusPrinc
                   | OperadorLibera
                   | RetrocedeCah
                   | JugHusHorario
                   | JugHusAntiHora
                   | ManualRefriger
                   | OffRefriger
                   | AutomatRefriger
                   | OnTVirutas
                   | OffTVirutas
                   | LimpiezaProtec
                   | Vacio
-}
             deriving Eq

data MenuInferior =  OnMando
                   | SeguridadPuerta
                   | ParaHusPrinc
                   | OperadorLibera
                   | RetrocedeCah
                   | JugHusHorario
                   | JugHusAntiHora
                   | ManualRefriger
                   | OffRefriger
                   | AutomatRefriger
                   | OnTVirutas
                   | OffTVirutas
                   | LimpiezaProtec
                   | VacioMI

-- El data MenuPrincipal nos sirve para poder manejar todos los menus 
-- que se puede utilizar en la pantalla principal del fresador
-- Window a
data MenuPrincipal = MenuPrin { reposo          :: IO (Button ())
                              , opeManual       :: IO (Button ())
                              , ediPrograma     :: IO (Button ())
                              , cargarSalvar    :: IO (Button ())
                              , refTrabajo      :: IO (Button ())
                              , pruebaPrograma  :: IO (Button ())
                              , opeAutomatico   :: IO (Button ())
                              , monitor         :: IO (Button ())
                              , soporte         :: IO (Button ())
                              , onMando         :: IO (Button ())
                              , seguridadPuerta :: IO (Button ())
                              , paraHusPrinc    :: IO (Button ())
                              , operadorLibera  :: IO (Button ())
                              , retrocedeCah    :: IO (Button ())
                              , jugHusHorario   :: IO (Button ())
                              , jugHusAntiHora  :: IO (Button ())
                              , manualRefriger  :: IO (Button ())
                              , offRefriger     :: IO (Button ())
                              , automatRefriger :: IO (Button ())
                              , onTVirutas      :: IO (Button ())
                              , offTVirutas     :: IO (Button ())
                              , limpiezaProtec  :: IO (Button ())
                              , vacioMenu       :: IO (Button ())
                              }

--data MenuInfierior = MenuInferior 
-- Datas de operaciones manuales que esta en el menu principal
data OperacionMan = Volante
                  | Continuo
                  | Incremental
                  | MedirManual
                  | MDi
                  | VacioOper
                  | RefAlmacen
                  | Referencia
                  | RefMaquina
                  | VacioOM
                deriving Eq

data OperacionManual = OperacionManual { volante     :: IO (Button ())
                                       , continuo    :: IO (Button ())
                                       , incremental :: IO (Button ())
                                       , medirManual :: IO (Button ())
                                       , mdi         :: IO (Button ())
                                       , vacioOper   :: IO (Button ())
                                       , refAlmacen  :: IO (Button ())
                                       , referencia  :: IO (Button ())
                                       , refMaquina  :: IO (Button ())
                                       }

-- Datas para el MDI que esta en el menu de Operacion Manual
data Mdi = Mdi { graficos    :: IO (Button ())
               , vacioMdi    :: IO (Button ())
               , status      :: IO (Button ())
               , diagnostico :: IO (Button ())
               , codigosG    :: IO (Button ())
               , codigodM    :: IO (Button ())
               , directMdi   :: IO (Button ())
               }

-- Edicion Programa esta definido en el Menu Principal
data EdicionPrograma = EdiPrograma { display     :: IO (Button ())
                                   , editar      :: IO (Button ())
                                   , instruir    :: IO (Button ())
                                   , progrNuevo  :: IO (Button ())
                                   , proxProg    :: IO (Button ())
                                   , renumProg   :: IO (Button ())
                                   , borrarProg  :: IO (Button ())
                                   , borrarTodos :: IO (Button ())
                                   , direct      :: IO (Button ())
                                   }


getFrame :: Ambiente -> Frame ()
getFrame (f,_,_,_,_,_) = f

getPanel :: Ambiente -> Panel ()
getPanel (_,p,_,_,_,_) = p

getTipoMenu :: Ambiente -> TipoMenu
getTipoMenu (_,_,m,_,_,_) = m

getCuerpoMenu :: Ambiente -> CuerpoMenu
getCuerpoMenu (_,_,_,m,_,_) = m

getMenu :: Ambiente -> Menus
getMenu (_,_,_,_,m,_) = m

getExit :: Ambiente -> Exit
getExit (_,_,_,_,_,m) = m

getMenuPrincipal :: TipoMenu -> PrincipalMenu
getMenuPrincipal (p,_,_) = p

getMenuInferior :: TipoMenu -> MenuInferior
getMenuInferior (_,i,_) = i

getMenuOperacionMan :: TipoMenu -> OperacionMan
getMenuOperacionMan (_,_,o) = o

getCuerpoP :: CuerpoMenu -> Var MenuPrincipal
getCuerpoP (a) = a

toIO :: t -> IO(t)
toIO t = do {return(t)}
