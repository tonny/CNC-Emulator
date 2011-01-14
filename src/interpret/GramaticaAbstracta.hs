module GramaticaAbstracta where
--pRaiz = pCodigoCnc

type Nume = Float

data CodigoCnc = Cnc [Cnc]
      deriving Show 

{-
===============================================================================
                            CONTROL DE CORDENADAS
===============================================================================
Funcion G70 Sistema de Unidad Pulgada
Funcion G71 Sistema de Unidad Milimetrico
-}

data Cnc = Cordenadas [ControlCordenadas]
   deriving Show

data ControlCordenadas = Pulgada                PulgadaG70
                       | Melimetrico            MelimetricoG71
                       | Absoluto               AbsolutoG90
                       | Incremental            IncrementalG91
                       | OrigenTemporal         OrigenTemporalG92
                       | AvanceEjeRotacional    AvanceEjeRotacionalG94
                       | CancelarReferenciaG92  CancelarReferenciaG99 
                       | Rotacion               RotacionG74

                deriving Show

data PulgadaG70 = PulgadaG70 NSecuencia
           deriving Show

data MelimetricoG71 = MelimetricoG71 NSecuencia
          deriving Show

data AbsolutoG90 = AbsolutoG90 Ejes
          deriving Show

data IncrementalG91 = IncrementalG91 Ejes
          deriving Show

data OrigenTemporalG92 = OrigenTemporalG92 NSecuencia
            deriving Show

data AvanceEjeRotacionalG94 = AvanceEjeRotacionalG94 NSecuencia
            deriving Show

data CancelarReferenciaG99 = CancelarReferenciaG99 
         deriving Show

data RotacionG74 = RotacionG74
         deriving Show

-- ===============================================================================
--                  2. POCICIONAMIENTO Y TIEMPO DE ESPERA
-- ===============================================================================

-- ==================== Funciones G de Interpolacion Lineal ======================

data PosTiempoEspera = InterpolacionLineal    InterpolacionLin 
                     | InterpolacionCircular  InterpolacionCir
            deriving Show

-- combina con A..Z
data InterpolacionLin = MovRapidoG00
                      | LinealG01    
                      | PuntoLineaG73
                deriving Show

-- combina con A..Z
data InterpolacionCir = ArcoHorarioG02  
                      | ArcoAntiHorarioG03
                deriving Show
-- =================== Tiempor de Permanencia (DWELL) G04 ========================

-- combina con todos A..Z
data Permanencia = PermanenciaG04
            deriving Show

-- ============================= Arco Tangente G05 ===============================

-- combina con todos A..Z
data ArcoTangente = ArcoTangenteG04
          deriving Show

-- ===============================================================================
--                           3. Modo de lo Ejes
-- ===============================================================================

-- ============================= Seleccion de Plano ==============================

data SeleccionPlano = Plano SeleccionPlan Nume
           deriving Show

data SeleccionPlan = PlanoxyG17
                   | PlanoxzG18
                   | PlanoyzG19
        deriving Show

-- ================= Compensacion de Rayo de la Herramienta ======================

data CompensacionRayo = CompensacionRayo Compensacion Nume [EjesLineales]
          deriving Show

data Compensacion = CancelCompocicionG40
                  | HerramientaIzqG41
                  | HerramientaDerG42
            deriving Show

-- ======================= Corrector de Fijacion - G45 ===========================

--Falta par EHNOP
data Corrector = CorrectorFijacionG45
    deriving Show

-- ===============================================================================
--                           4. Ciclos Fijos
-- ===============================================================================

-- ========================= Cancelacion de ciclo fijo ===========================

data CancelCicloFijo = CancelCicloFijoG80 Nume
            deriving Show

--pFlujoPerforacion = 

-- ===============================================================================
--                       Chanflo o Arredondamiento
-- ===============================================================================

data Q = Q Nume
 deriving Show

-- ===============================================================================
--                     Funciones de Posicionamiento 
--              Lineales - Rotacionales - Pocicion Centro
-- ===============================================================================

data EjesLineales = X Nume 
                  | Y Nume
                  | Z Nume
                  | EmptyEje
           deriving Show

data EjesRotacional = U Nume
                    | V Nume
                    | W Nume
                    | EmptyRotacion
          deriving Show

data PosicionCentro = I Nume
                    | J Nume
                    | K Nume
            deriving Show

data Ejes = Ejes [EjesRotacional] [EjesLineales] 
       deriving Show

-- ===============================================================================
--             Funcion F - Velocidad de Avance
-- ===============================================================================

data F = F Nume
   deriving Show

-- ===============================================================================
--             Funcion D - Divide en Bloques
-- ===============================================================================

data D = D Nume
   deriving Show

-- ===============================================================================
--              Funcion M - Miselaneas 
-- ===============================================================================

data M = M Cord
   deriving Show

 --Controlar en las Condiciones de contexto
data Cord = ControlEjeArbol    ControlArbol 
          | ControlDelPrograma ControlPrograma
--        | CambioGrupo        CambioGrupo
   deriving Show

data ControlArbol = RotacionDer String
                  | RotacionIzq String
                  | ParadaArbol String
          deriving Show

data ControlPrograma = ParadaPrograma String
                     | ParadaOpcional String
                     | FinPrograma    String
                     | FinProgramaReb String
                     | CambioHerram   String
            deriving Show


-- ===============================================================================
--                     Remificaciones en programa 
--                          N - H - E - L - P 
-- ===============================================================================

data NSecuencia = N Nume
                | EmptyNSecuencia
        deriving Show

-- Desvio  Sub-Programa
data P = P Nume
   deriving Show

-- Llama a Sub-Rutina
data H = H Nume
  deriving Show

-- Fin secuencia 
data E = E Nume
  deriving Show

-- Repeticion de Block
data L = L Nume
  deriving Show

-- ===============================================================================
--               Velocidad de Rotacion del Eje Arbol - funcion S
-- ===============================================================================

data S = S Nume
  deriving Show

-- ===============================================================================
--     Seleccion del corrector de Herramienta y de Distancia - Funcion O
-- ===============================================================================

data O = O Nume
  deriving Show

-- ===============================================================================
--                Seleccion de Herramienta - Funcion T
-- ===============================================================================

data T = T Nume
  deriving Show

-- ===============================================================================
