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

data Cnc = Cordenadas  N ControlCordenadas -- EndBlock
         | Posiciones  N Posicionamiento   -- EndBlock
         | ModoEjes    N ModoEjes          -- EndBlock
         | CiclosFijos N CiclosFijos       -- EndBlock
         | AutoRutinas N AutoRutina        -- EndBlock
         | Funciones   N Funciones         -- EndBlock
   deriving Show

data ControlCordenadas = Formato                Formatos       NSecuencia
                       | TipoCordenadas         TipoCordenada  [BodyTipoCordenada]
                       | OrigenTemporal         OrigenTemporalG92
                       | AvanceEjeRotacional    AvanceEjeRota
                       | CancelarReferencia     CancelarReferenciaG99 
                       | Rotacion               RotacionG74

                deriving Show

-- =============================== Formato ====================================

data Formatos = PulgadaG70  
              | MelimetricoG71
        deriving Show

-- ============================= Tipo Cordenada ===============================

data TipoCordenada = AbsolutoG90    
                   | IncrementalG91
        deriving Show

data BodyTipoCordenada = Angulos        Angulo
                       | PosicionCentro PosicionCentro
                       | EjesLineales   EjesLineales
                       | EjesRotacional EjesRotacional
                       | Numeros        Nume
                       | CordR          R
            deriving Show

-- ================= Referencia para el eje rotacional ===========================

data AvanceEjeRota = AvanceEjeRotacionalG94 [BodyAvanceEje]
           deriving Show

data BodyAvanceEje = AvanceEjeNs N
                   | AvanceEjeC  Nume
                   | AvanceEjeF  F
            deriving Show

-- ===============================================================================


data OrigenTemporalG92 = OrigenTemporalG92 NSecuencia
            deriving Show

data CancelarReferenciaG99 = CancelarReferenciaG99 [BodyPosicion]
         deriving Show

-- ========================= Rotacion ============================================

data RotacionG74 = RotacionG74 [Rotacion]
         deriving Show

data Rotacion = RotaAngulo Angulo
--                RotacionA   A
--              | RotacionC   C
              | RotacionE   E
              | RotacionH   H
              | RotaPosCent PosicionCentro 
--              | RotacionI   I
--              | RotacionJ   J
              | RotacionM   M
              | RotacionN   N
              | RotacionP   P
              | RotacionR   R
              | RotacionEje EjesLineales
      deriving Show

-- ===============================================================================
--                  2. POCICIONAMIENTO Y TIEMPO DE ESPERA
-- ===============================================================================

data Posicionamiento = Movimiento Interpolacion [BodyPosicion]
         deriving Show

data Interpolacion = MovRapidoG00  
                   | LinealG01   
                   | PuntoLineaG73
                   | ArcoHorarioG02
                   | ArcoAntiHorarioG03
                   | PermanenciaG04
                   | ArcoTangenteG05
            deriving Show

-- ================================= Body Posicion ===============================

data BodyPosicion = PosAngulo   Angulo
                  | PosD        D
                  | PosE        E
                  | PosF        F
                  | PosH        H
                  | PosCentro   PosicionCentro
                  | PosL        L
                  | PosM        M
                  | PosN        N
                  | PosO        O
                  | PosP        P
                  | PosQ        Q
                  | PosR        R
                  | PosS        S
                  | PosT        T
                  | PosEjesLin  EjesLineales
                  | PosEjesRot  EjesRotacional
         deriving Show

-- ===============================================================================
--                           3. Modo de lo Ejes
-- ===============================================================================

data ModoEjes = SeleccionPlano       SeleccionPlano
              | CompensacionRayoHerr CompensacionRayo
              | CorrectorFijacion    Corrector
     deriving Show

-- ============================= Seleccion de Plano ==============================

data SeleccionPlano = Plano SeleccionPlan NSecuencia
           deriving Show

data SeleccionPlan = PlanoxyG17
                   | PlanoxzG18
                   | PlanoyzG19
        deriving Show

-- ================= Compensacion de Rayo de la Herramienta ======================

data CompensacionRayo = CompensacionRayo Compensacion [BodyCompensacion]
          deriving Show

data Compensacion = CancelCompocicionG40
                  | HerramientaIzqG41
                  | HerramientaDerG42
            deriving Show

data BodyCompensacion = CompensacionN    N
                      | CompensacionEjes EjesLineales
            deriving Show


-- ======================= Corrector de Fijacion - G45 ===========================

data Corrector = CorrectorFijacionG45 [BodyCorrector]
    deriving Show

data BodyCorrector = CorrectorE E
                   | CorrectorH H
                   | CorrectorN N
                   | CorrectorO O
                   | CorrectorP P
             deriving Show

-- ===============================================================================
--                           4. Ciclos Fijos
-- ===============================================================================

-- OJO falta G75 y G79
data CiclosFijos = CancelacionCiclo   CancelCicloFijo 
                 | CicloPersacion     FlujoPerforacion      [BodyPerforacion]
                 | PerforacionDescaga PerforacionDescargas
                 | RoscamientoMandri  RoscaMandri           [BodyCiclos]
        deriving Show

-- ========================= Cancelacion de ciclo fijo ===========================

data CancelCicloFijo = CancelCicloFijoG80 NSecuencia
        deriving Show

-- ================== Ciclo fijo de Perforacion | Permanente =====================

data FlujoPerforacion = CicloFijoPerforacion
                      | CicloFijoPerfPerma
                deriving Show

data BodyPerforacion = PerforacionV Nume
                     | PerforaCiclo BodyCiclos
                deriving Show

-- =========== Ciclo Fijo de perforacion con Descargas ===========================

-- OJO -> Controlar que no entre la fucncion V de los EjesRotacionales
data PerforacionDescargas = PerforConDescaga  [BodyPerfDescarga]
            deriving Show

data BodyPerfDescarga = DescargaCentro   PosicionCentro
                      | DescargaRotacion EjesRotacional
                      | DescargaCiclo    BodyCiclos
             deriving Show

-- ========== Ciclos Fijo de Roscamiento | Mandrilamiento ========================

data RoscaMandri = CicloRoscamiento
                 | CicloMandrilamien
                 | CicloMandrilParado
            deriving Show

data BodyCiclos = CicloD    D
                | CicloF    F
                | CicloN    N
                | CicloP    P
                | CicloR    R
                | CicloEjes EjesLineales
         deriving Show

-- ===============================================================================
--                         5. Auto Rutinas
-- ===============================================================================

--data AutoRutina = Rutina Helice [BodyRutina]
data AutoRutina =  RutinaA [BodyRutina]
                | RutinaB [BodyRutina]
           deriving Show

data Helice = HeliceHorario  
            | HeliceAntiHora
        deriving Show

data BodyRutina = RutinaPosCentro PosicionCentro
                | RutinaN         N
                | RutinaEjeLineal EjesLineales
         deriving Show

-- ===============================================================================
--                        6. Funciones
-- ===============================================================================

data Funciones = Espejo         Espejos         [BodyEspejo]
               | Reactiva       NSecuencia
               | Ejecutar       [BodyEjecutar]
               | CirculoAgujero [BodyAgujero]
               | PosRepite      [BodyPosRepite]
               | Alojamiento    Alojamiento
               | Escala         [BodyEscala]
               | Programable    [BodyPosicion]
               | Cavidad        [BodyCavidad]
        deriving Show

-- ======================== Imagen de Espejo =====================================

data Espejos = CancelaEspejo
             | EspejoDelEje 
        deriving Show

data BodyEspejo = EspejoN         N
                | EspejoPosCentro PosicionCentro
                | EspejoEjeLineal EjesLineales
        deriving Show

-- ========================= Body Ejecutar =======================================

data BodyEjecutar = EjecutarN N
                  | EjecutarL L
            deriving Show

-- =========================== Body Circulo Agujero ==============================

data BodyAgujero = AgujeroAngulos Angulo
                 | AgujeroE       E
                 | AgujeroH       H
                 | AgujeroI       Nume
                 | AgujeroJ       Nume
                 | AgujeroL       L
                 | AgujeroP       P
                 | AgujeroR       R
                 | AgujeroW       Nume
                 | AgujeroX       Nume
                 | AgujeroY       Nume
         deriving Show

-- ========================= Body G25 ============================================

data BodyPosRepite = PosRepiteE E
                   | PosRepiteF F
                   | PosRepiteH H
                   | PosRepiteI Nume
                   | PosRepiteJ Nume
                   | PosRepiteN N
                   | PosRepiteP P
                   | PosRepiteX Nume
                   | PosRepiteY Nume
            deriving Show

-- ==================== Alojamiento G26 | Saliencia G27 ==========================

data Alojamiento = Alojamientos Aloja [BodyAlojamiento]
         deriving Show

data Aloja = AlojamientoG26
           | AlojamientoG27
        deriving Show

data BodyAlojamiento = AlojaD         D
                     | AlojaF         F
                     | AlojaH         H
                     | AlojaPosCentro PosicionCentro
                     | AlojaL         L
                     | AlojaN         N
                     | AlojaO         O
                     | AlojaP         P
                     | AlojaQ         Q
                     | AlojaR         R
                     | AlojaEjeRota   EjesRotacional
                     | AlojaEjeLineal EjesLineales
            deriving Show

-- =========================== Body Escala =======================================

data BodyEscala = EscalaE          E
                | EscalaH          H
                | EscalaL          L
                | EscalaN          N
                | EscalaP          P
                | EscalaRotacional EjesRotacional
                | EscalaLineal     EjesLineales
          deriving Show

-- ============================== Body Cavidad ===================================

data BodyCavidad = CavidadD D
                 | CavidadE E
                 | CavidadH H
                 | CavidadN N
                 | CavidadP P
                 | CavidadX Nume
                 | CavidadZ Nume
        deriving Show

-- ===============================================================================
--                    Asignacion de Angulos
-- ===============================================================================

data Angulo = Angulo Angulos Nume
      deriving Show

data Angulos = PosicionActual
             | PuntoCentro
             | EspacionAngular
      deriving Show

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

data ControlArbol = RotacionDer --String
                  | RotacionIzq --String
                  | ParadaArbol --String
          deriving Show

data ControlPrograma = ParadaPrograma --String
                     | ParadaOpcional --String
                     | FinPrograma    --String
                     | FinProgramaReb --String
                     | CambioHerram   --String
            deriving Show


-- ===============================================================================
--                     Remificaciones en programa 
--                          N - H - E - L - P 
-- ===============================================================================

data NSecuencia = N Nume
                | EmptyNSecuencia
        deriving Show

data N = Num Nume
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

data R = R Nume
  deriving Show

-- ===============================================================================

