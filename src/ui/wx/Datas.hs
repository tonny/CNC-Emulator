module Datas where
import Graphics.UI.WX

type Posicion = Int
type MemoriaInf = MenuInferior
type MemoriaPos = Int

type Ambiente = ( Var (Paneles,Bool) --menu que esta pintado en el panel video
                , Var [Paneles] --memoria que guarda los paneles aneteriores
                , Var (MenuInferior,Posicion,MemoriaInf,MemoriaPos)
                , Var Bool -- parada de emergencia
                , Var Bool  -- servos Activo
                )

data Menus = Principal -- esta caso llama a al data Principal Menu
           | Operacion
           | MDI
--           | VacioM
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
            deriving Eq

data Paneles =
    Reposo         | OpeManual     | EdiPrograma   | CargarSalvar | RefTrabajo 
  | PruebaPrograma | OpeAutomatico | Monitor       | Soporte
   -- opManual
  | Volante        | Continuo      | Incremental   | MedirManual  | Mdi          
  | Vacio          | RefAlmacen    | Referencia    | RefMaquina
  -- EdiPrograma
  | Display        | Editar        | Instruir      | ProgNuevo    | ProxPrograma 
  | RenumPrograma  | BorrarProgra  | BorrarTodos   | Direct
  -- Salvar gurdar
  | Salvar         | Verificar     | Cargar        | VacioC1      | VacioC2      
  | VacioC3        | VacioC4       | SelDisSalvar  | SelDisCargar
  --- RefTrabajo 
  | Metrico        | Pulgada       | IgnoraBloque  | ParadaOpcio  | IniMedioProg 
  | RefHerramien   | CorrecFija    | Status        | DirectRT
  -- PruebaProgra 
  | VarifRapido    | VerConAvance  | EjecutarSeco  | EjecutCeroZ  | VacioPru1      
  | VacioPru2      | VacioPru3     | VacioPru4     | VacioPru5
  -- OpeAutomatico
  | ReferTrabajo   | Movimiento    | MdiOA         | MonitorOA    | Parametros 
  | Edicion        | StatusOA      | Graficos      | DirectOA
  --  Monitor
  | ProximoGrupo   | RecargaHerr   | RecargaTotal  | Normal       | CargarM       
  | SalvarM        | Cerrar        | Diagnosticar  | VacioM
  -- Soporte  
  | ProtegerProg   | VacioP1       | Diagnostico   | ControlAcce  | ParameAltMaq
  | Pal            | VacioP2       | VacioP3       | Logon
  -- Mdi   
  | GraficosMdi    | VacioMdi1     | StatusMdi     | VacioMdi2    | DiagnosticoMdi  
  | VacioMdi3      | CodigoG       | CodigoM       | DirectMdi
  -- Display  
  | GraficosD      | ListaD        | VacioD1       | Pesquisa     | VacioD2     
  | VacioD3        | CodigoGD      | CodigoMD      | DirectD
   -- Editar 
  | GraficosE      | ListaE        | InserPrograma | PesquisaE    | VacioE1     
  | VacioE2        | CodigoGE      | CodigoME      | DirectE
  -- Instruir
  | VolanteI       | ContinuoI     | IncrementalI  | VacioI1      | InstruirMan  
  | InstruirMdi    | BorrarProg    | VacioI2       | DirectI
  | Raiz -- para poner la cabezera
  | VacioMenu
   deriving (Show,Eq)


getMenu :: Ambiente -> Var (Paneles,Bool)
getMenu (a,_,_,_,_) = a

getMemoria :: Ambiente -> Var [Paneles]
getMemoria (_,a,_,_,_) = a

getMenuInf :: Ambiente -> Var (MenuInferior,Posicion,MemoriaInf,MemoriaPos)
getMenuInf (_,_,a,_,_) = a

getParadaEmer :: Ambiente -> Var Bool
getParadaEmer (_,_,_,a,_) = a

getServos :: Ambiente -> Var Bool
getServos (_,_,_,_,a) = a

primero :: (MenuInferior,Posicion,MemoriaInf,MemoriaPos) -> MenuInferior 
primero (a,_,_,_) = a

segundo :: (MenuInferior,Posicion,MemoriaInf,MemoriaPos) -> Posicion
segundo (_,a,_,_) = a

tercero :: (MenuInferior,Posicion,MemoriaInf,MemoriaPos) -> MemoriaInf
tercero (_, _,a,_) = a

cuarto :: (MenuInferior,Posicion,MemoriaInf,MemoriaPos) -> MemoriaPos
cuarto (_, _,_,a) = a

toIO :: t -> IO t
toIO t = do {return(t)}
