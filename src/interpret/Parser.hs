module Parser where

import UU.Parsing
import Scanner
import GramaticaAbstracta
import UU.Util.PermTree

pRaiz = pCnc

pCnc =  Cordenadas <$> pList pControlCordenadas
       
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--                          Funciones G de Movimientos 
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-- ===============================================================================
--                           1. CONTROL DE CORDENADAS
-- ===============================================================================
{-
Funcion G70 Sistema de Unidad Pulgada
Funcion G71 Sistema de Unidad Milimetrico

OJO falta las funciones 72 y 97 
-}

pControlCordenadas =  Pulgada               <$> pPulgadaG70
                  <|> Melimetrico           <$> pMelimetricoG71
                  <|> Absoluto              <$> pAbsolutoG90
                  <|> Incremental           <$> pIncrementalG91
                  <|> OrigenTemporal        <$> pOrigenTemporalG92
                  <|> AvanceEjeRotacional   <$> pAvanceEjeRotacionalG94
                  <|> CancelarReferenciaG92 <$> pCancelarReferenciaG99 
                  <|> Rotacion              <$> pRotacionG74

{-
Permite definir o redefinir en medio del programa la posicion del origen de
las coredenadas absolutas(cero programa) | G92 tambien puede recibir X Y Z ? <<<<==
-}

pOrigenTemporalG92 = OrigenTemporalG92 <$ pKeyword "G92" <*> pNSecuencia

pPulgadaG70 = PulgadaG70 <$ pKeyword "G70" <*> pNSecuencia

pMelimetricoG71 = MelimetricoG71 <$ pKeyword "G71" <*> pNSecuencia

pAbsolutoG90 = AbsolutoG90 <$ pKeyword "G90" <*> pEjes --Averiguar para meny (ABCIJKNRUVWXYZ)

pIncrementalG91 = IncrementalG91 <$ pKeyword "G91" <*> pEjes --Averiguar para meny (ABCIJKNRUVWXYZ)

-- Falta para C y F
pAvanceEjeRotacionalG94 = AvanceEjeRotacionalG94 <$ pKeyword "G94" <*> pNSecuencia

-- falta de A..Z
pCancelarReferenciaG99 = CancelarReferenciaG99 <$ pKeyword "G99"

--Falta para ACEHIJMNPRXYZ
pRotacionG74 = RotacionG74 <$ pKeyword "G74"

-- ===============================================================================
--                  2. POCICIONAMIENTO Y TIEMPO DE ESPERA
-- ===============================================================================

-- ==================== Funciones G de Interpolacion Lineal ======================

pPosTiempoEspera =  InterpolacionLineal   <$> pInterpolacionLin 
                <|> InterpolacionCircular <$> pInterpolacionCir

-- combina con A..Z
pInterpolacionLin =  MovRapidoG00  <$ pKeyword "G00"  
                 <|> LinealG01     <$ pKeyword "G01"
                 <|> PuntoLineaG73 <$ pKeyword "G73"

-- combina con A..Z
pInterpolacionCir =  ArcoHorarioG02     <$ pKeyword "G02"
                 <|> ArcoAntiHorarioG03 <$ pKeyword "G03"

-- =================== Tiempor de Permanencia (DWELL) G04 ========================

-- combina con todos A..Z
pPermanencia = PermanenciaG04 <$ pKeyword "G04"

-- ============================= Arco Tangente G05 ===============================

-- combina con todos A..Z
pArcoTangente = ArcoTangenteG04 <$ pKeyword "G05"

-- ===============================================================================
--                           3. Modo de lo Ejes
-- ===============================================================================

-- ============================= Seleccion de Plano ==============================

pSeleccionPlano = Plano <$> pSeleccionPlan <*> pNumero

pSeleccionPlan =  PlanoxyG17 <$ pKeyword "G17" 
              <|> PlanoxzG18 <$ pKeyword "G18"
              <|> PlanoyzG19 <$ pKeyword "G19"

-- ================= Compensacion de Rayo de la Herramienta ======================

pCompensacionRayo = CompensacionRayo <$> pCompensacion <*> pNumero <*> pList pEjesLineales 

pCompensacion =  CancelCompocicionG40 <$ pKeyword "G40"
             <|> HerramientaIzqG41    <$ pKeyword "G41"
             <|> HerramientaDerG42    <$ pKeyword "G42"

-- ======================= Corrector de Fijacion - G45 ===========================

--Falta par EHNOP
pCorrector = CorrectorFijacionG45 <$ pKeyword "G45" 

-- ===============================================================================
--                           4. Ciclos Fijos
-- ===============================================================================

-- ========================= Cancelacion de ciclo fijo ===========================

pCancelCicloFijo = CancelCicloFijoG80 <$ pKeyword "G80" <*> pNumero

--pFlujoPerforacion = 

-- ===============================================================================
--                       Chanflo o Arredondamiento
-- ===============================================================================

pQ = Q <$ pKeyword "Q" <*> pNumero

-- ===============================================================================
--                     Funciones de Posicionamiento 
--              Lineales - Rotacionales - Pocicion Centro
-- ===============================================================================

pEjesLineales =  X <$ pKeyword "X" <*> pNumero 
             <|> Y <$ pKeyword "Y" <*> pNumero
             <|> Z <$ pKeyword "Z" <*> pNumero
--             <|> pSucceed EmptyEje

pEjesRotacional =  U <$ pKeyword "U" <*> pNumero
               <|> V <$ pKeyword "V" <*> pNumero
               <|> W <$ pKeyword "W" <*> pNumero
--               <|> pSucceed EmptyRotacion

pPosicionCentro =  I <$ pKeyword "I" <*> pNumero
               <|> J <$ pKeyword "J" <*> pNumero
               <|> K <$ pKeyword "k" <*> pNumero

pEjes = Ejes <$> pList pEjesRotacional  <*> pList pEjesLineales

-- ===============================================================================
--                   Funcion F - Velocidad de Avance
-- ===============================================================================

pF = F <$ pKeyword "F" <*> pNumero

-- ===============================================================================
--                      Funcion D - Divide en Bloques
-- ===============================================================================

pD = D <$ pKeyword "D" <*> pNumero

-- ===============================================================================
--                            Funcion M - Miselaneas 
-- ===============================================================================

pM = M <$ pKeyword "M" <*> pCord
 --Controlar en las Condiciones de contexto
pCord =  ControlEjeArbol    <$> pControlArbol 
     <|> ControlDelPrograma <$> pControlPrograma
--     <|> CambioGrupo        <$> CambioGrupo

pControlArbol =  RotacionDer <$> pKeyword "03"
             <|> RotacionIzq <$> pKeyword "04"
             <|> ParadaArbol <$> pKeyword "05"

pControlPrograma =  ParadaPrograma <$> pKeyword "00"
                <|> ParadaOpcional <$> pKeyword "01"
                <|> FinPrograma    <$> pKeyword "02"
                <|> FinProgramaReb <$> pKeyword "30"
                <|> CambioHerram   <$> pKeyword "06"

-- ===============================================================================
--                     Remificaciones en programa 
--                          N - H - E - L - P 
-- ===============================================================================

pNSecuencia =  N <$ pKeyword "N" <*>  pNumero
           <|> pSucceed EmptyNSecuencia

-- Desvio  Sub-Programa
pP = P <$ pKeyword "P" <*> pNumero

-- Llama a Sub-Rutina
pH = H <$ pKeyword "H" <*> pNumero

-- Fin secuencia 
pE = E <$ pKeyword "E" <*> pNumero

-- Repeticion de Block
pL = L <$ pKeyword "L" <*> pNumero

-- ===============================================================================
--               Velocidad de Rotacion del Eje Arbol - funcion S
-- ===============================================================================

pS = S <$ pKeyword "S" <*> pNumero 

-- ===============================================================================
--     Seleccion del corrector de Herramienta y de Distancia - Funcion O
-- ===============================================================================

pO = O <$ pKeyword "O" <*> pNumero

-- ===============================================================================
--                     Seleccion de Herramienta - Funcion T
-- ===============================================================================

pT = T <$ pKeyword "T" <*> pNumero

-- ===============================================================================
--                       Union Scanner con el Parser
-- ===============================================================================

instance (Eq Tipo) => (Eq Simbolo) where
   (Simbolo Error _ _ _ ) == (Simbolo Error _ _ _ ) = True
   (Simbolo Numero _ _ _) == (Simbolo Numero _ _ _) = True
   (Simbolo t1    s _ _ ) == (Simbolo t2   s1 _ _ ) = t1 == t2 && s == s1

instance Ord Simbolo where
  compare x y | x == y = EQ
              | x <= y = LT
              | otherwise = GT
  (Simbolo tok1 str1 _ _ ) <= (Simbolo tok2 str2 _ _ ) 
         = tok1 < tok2 || (tok1 == tok2 && str1 <= str2)

instance Symbol Simbolo

--obtenerValor :: Token -> String
obtenerValor (Simbolo _ v _ _ ) = v

tSym :: Tipo -> String -> Parser Simbolo String
tSym tok str = obtenerValor <$> pSym (Simbolo tok str 0 0)

pKeyword k = tSym Keyword k
pNumero   = (\ x ->(read x)::Float) <$> tSym Numero ""

