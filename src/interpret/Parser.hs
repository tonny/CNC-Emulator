module Parser where

import UU.Parsing
import Scanner
import GramaticaAbstracta

pCodigoCnc = Cnc <$> pList pCnc
       
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--                          Funciones G de Movimientos 
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

pCnc =  Cordenadas  <$> pN <*> pControlCordenadas <* pEndBlock "#"
    <|> Posiciones  <$> pN <*> pPosicionamiento   <* pEndBlock "#"
    <|> ModoEjes    <$> pN <*> pModoEjes          <* pEndBlock "#"
    <|> CiclosFijos <$> pN <*> pCiclosFijos       <* pEndBlock "#"
    <|> AutoRutinas <$> pN <*> pAutoRutina        <* pEndBlock "#"
    <|> Funciones   <$> pN <*> pFunciones         <* pEndBlock "#"

-- ===============================================================================
--                           1. CONTROL DE CORDENADAS
-- ===============================================================================
{-
Funcion G70 Sistema de Unidad Pulgada
Funcion G71 Sistema de Unidad Milimetrico

OJO falta las funciones 72 y 97 
-}

pControlCordenadas =  Formato             <$> pFormatos <*> pNSecuencia
                  <|> OrigenTemporal      <$> pOrigenTemporalG92
                  <|> TipoCordenadas      <$> pTipoCordenada <*> pList pBodyTipoCordenada 
                  <|> AvanceEjeRotacional <$> pAvanceEjeRota
                  <|> CancelarReferencia  <$> pCancelarReferenciaG99 
                  <|> Rotacion            <$> pRotacionG74

-- =============================== Formato ====================================

pFormatos =  PulgadaG70     <$ pKeyword "G70"
         <|> MelimetricoG71 <$ pKeyword "G71"

-- ============================= Tipo Cordenada ===============================

pTipoCordenada =  AbsolutoG90    <$ pKeyword "G90"  --Averiguar para meny (ABCIJKNRUVWXYZ)
              <|> IncrementalG91 <$ pKeyword "G91"  --Averiguar para meny (ABCIJKNRUVWXYZ)

pBodyTipoCordenada =  Angulos        <$> pAngulo
                  <|> PosicionCentro <$> pPosicionCentro
                  <|> EjesLineales   <$> pEjesLineales
                  <|> EjesRotacional <$> pEjesRotacional
                  <|> Numeros        <$> pNumero
                  <|> CordR          <$> pR

-- ================= Referencia para el eje rotacional ===========================
-- OJO puede haver mas de C, N F
pAvanceEjeRota = AvanceEjeRotacionalG94 <$ pKeyword "G94" <*> pList pBodyAvanceEje

pBodyAvanceEje =  AvanceEjeNs <$> pN -- pNSecuencia OJO-->> Corregir 
              <|> AvanceEjeC  <$  pKeyword "C" <*> pNumero
              <|> AvanceEjeF  <$> pF

-- ===============================================================================
pCancelarReferenciaG99 = CancelarReferenciaG99 <$ pKeyword "G99" <*> pList pBodyPosicion

-- ============================== Rotacion G74 ===================================

pRotacionG74 = RotacionG74 <$ pKeyword "G74" <*> pList pRotacion

pRotacion = RotaAngulo   <$> pAngulo
--             RotacionA   <$> pA
--         <|> RotacionC   <$> pC
         <|> RotacionE   <$> pE
         <|> RotacionH   <$> pH
         <|> RotaPosCent <$> pPosicionCentro
--         <|> RotacionI   <$> pI
--         <|> RotacionJ   <$> pJ
         <|> RotacionM   <$> pM
         <|> RotacionN   <$> pN
         <|> RotacionP   <$> pP
         <|> RotacionR   <$> pR
         <|> RotacionEje <$> pEjesLineales

-- ===============================================================================
{-
Permite definir o redefinir en medio del programa la posicion del origen de
las coredenadas absolutas(cero programa) | G92 tambien puede recibir X Y Z ? <<<<==
-}
pOrigenTemporalG92 = OrigenTemporalG92 <$ pKeyword "G92" <*> pNSecuencia

-- ===============================================================================
--                  2. POSICIONAMIENTO Y TIEMPO DE ESPERA
-- ===============================================================================

pPosicionamiento = Movimiento <$> pInterpolacion <*> pList pBodyPosicion

              -- ======== Interpolacion Lineal ==========
pInterpolacion =  MovRapidoG00       <$ pKeyword "G00"  
              <|> LinealG01          <$ pKeyword "G01"
              <|> PuntoLineaG73      <$ pKeyword "G73"
              -- ========= Interpolacion Circular =======
              <|> ArcoHorarioG02     <$ pKeyword "G02"
              <|> ArcoAntiHorarioG03 <$ pKeyword "G03"
              -- ======== Tiempo de Permanencia =========
              <|> PermanenciaG04     <$ pKeyword "G04"
              -- ============ Arco Tangente =============
              <|> ArcoTangenteG05    <$ pKeyword "G05"

-- ================================= Body Posicion ===============================

pBodyPosicion =  PosAngulo   <$> pAngulo
             <|> PosD        <$> pD
             <|> PosE        <$> pE
             <|> PosF        <$> pF
             <|> PosH        <$> pH
             <|> PosCentro   <$> pPosicionCentro
             <|> PosL        <$> pL
             <|> PosM        <$> pM
             <|> PosN        <$> pN
             <|> PosO        <$> pO
             <|> PosP        <$> pP
             <|> PosQ        <$> pQ
             <|> PosR        <$> pR
             <|> PosS        <$> pS
             <|> PosT        <$> pT
             <|> PosEjesLin  <$> pEjesLineales
             <|> PosEjesRot  <$> pEjesRotacional

-- ===============================================================================
--                           3. Modo de lo Ejes
-- ===============================================================================

pModoEjes =  SeleccionPlano       <$> pSeleccionPlano
         <|> CompensacionRayoHerr <$> pCompensacionRayo
         <|> CorrectorFijacion    <$> pCorrector

-- ============================= Seleccion de Plano ==============================

pSeleccionPlano = Plano <$> pSeleccionPlan <*> pNSecuencia

pSeleccionPlan =  PlanoxyG17 <$ pKeyword "G17" 
              <|> PlanoxzG18 <$ pKeyword "G18"
              <|> PlanoyzG19 <$ pKeyword "G19"

-- ================= Compensacion de Rayo de la Herramienta ======================

pCompensacionRayo = CompensacionRayo <$> pCompensacion <*> pList pBodyCompensacion

pCompensacion =  CancelCompocicionG40 <$ pKeyword "G40"
             <|> HerramientaIzqG41    <$ pKeyword "G41"
             <|> HerramientaDerG42    <$ pKeyword "G42"

pBodyCompensacion =  CompensacionN    <$> pN
                 <|> CompensacionEjes <$> pEjesLineales

-- ======================= Corrector de Fijacion - G45 ===========================

pCorrector = CorrectorFijacionG45 <$ pKeyword "G45" <*> pList pBodyCorrector

pBodyCorrector =  CorrectorE <$> pE
              <|> CorrectorH <$> pH
              <|> CorrectorN <$> pN
              <|> CorrectorO <$> pO
              <|> CorrectorP <$> pP

-- ===============================================================================
--                           4. Ciclos Fijos
-- ===============================================================================
-- OJO falta G75 y G79

pCiclosFijos =  CancelacionCiclo   <$> pCancelCicloFijo 
            <|> CicloPersacion     <$> pFlujoPerforacion    <*> pList pBodyPerforacion
            <|> PerforacionDescaga <$> pPerforacionDescargas
            <|> RoscamientoMandri  <$> pRoscaMandri         <*> pList pBodyCiclos

-- ========================= Cancelacion de ciclo fijo ===========================

pCancelCicloFijo = CancelCicloFijoG80 <$ pKeyword "G80" <*> pNSecuencia

-- ================== Ciclo fijo de Perforacion | Permanente =====================

pFlujoPerforacion =  CicloFijoPerforacion <$ pKeyword "G81"
                 <|> CicloFijoPerfPerma   <$ pKeyword "G82"

pBodyPerforacion =  PerforacionV <$  pKeyword "V" <*> pNumero
                <|> PerforaCiclo <$> pBodyCiclos

-- =========== Ciclo Fijo de perforacion con Descargas ===========================

-- OJO -> Controlar que no entre la fucncion V de los EjesRotacionales
pPerforacionDescargas = PerforConDescaga <$ pKeyword "G83" <*> pList pBodyPerfDescarga

pBodyPerfDescarga =  DescargaCentro   <$> pPosicionCentro
                 <|> DescargaRotacion <$> pEjesRotacional
                 <|> DescargaCiclo    <$> pBodyCiclos

-- ========== Ciclos Fijo de Roscamiento | Mandrilamiento ========================

pRoscaMandri =  CicloRoscamiento   <$ pKeyword "G84"
            <|> CicloMandrilamien  <$ pKeyword "G85"
            <|> CicloMandrilParado <$ pKeyword "G86"

pBodyCiclos =  CicloD    <$> pD
           <|> CicloF    <$> pF
           <|> CicloN    <$> pN
           <|> CicloP    <$> pP
           <|> CicloR    <$> pR
           <|> CicloEjes <$> pEjesLineales

-- ===============================================================================
--                         5. Auto Rutinas
-- ===============================================================================
{-
pAutoRutina = Rutina <$> pHelice <*> pList pBodyRutina

pHelice =  HeliceHorario  <$ pKeyword "G22"
       <|> HeliceAntiHora <$ pKeyword "G23"
-}

pAutoRutina =  RutinaA <$ pKeyword "G22" <*> pList pBodyRutina
           <|> RutinaB <$ pKeyword "G23" <*> pList pBodyRutina

pHelice =  HeliceHorario  <$ pKeyword "G22"
       <|> HeliceAntiHora <$ pKeyword "G23"


pBodyRutina =  RutinaPosCentro <$> pPosicionCentro
           <|> RutinaN         <$> pN
           <|> RutinaEjeLineal <$> pEjesLineales
-- ===============================================================================
--                        6. Funciones
-- ===============================================================================

pFunciones =  Espejo         <$> pEspejos       <*> pList pBodyEspejo 
          <|> Reactiva       <$  pKeyword "G89" <*> pNSecuencia
          <|> Ejecutar       <$  pKeyword "G29" <*> pList pBodyEjecutar
          <|> CirculoAgujero <$  pKeyword "G24" <*> pList pBodyAgujero
          <|> PosRepite      <$  pKeyword "G25" <*> pList pBodyPosRepite
          <|> Alojamiento    <$> pAlojamiento
          <|> Escala         <$  pKeyword "G72" <*> pList pBodyEscala
          <|> Programable    <$  pKeyword "G79" <*> pList pBodyPosicion
          <|> Cavidad        <$  pKeyword "G75" <*> pList pBodyCavidad

-- ======================== Imagen de Espejo =====================================

pEspejos =  CancelaEspejo <$ pKeyword "G30"
        <|> EspejoDelEje  <$ pKeyword "G31"

pBodyEspejo =  EspejoN         <$> pN
           <|> EspejoPosCentro <$> pPosicionCentro
           <|> EspejoEjeLineal <$> pEjesLineales

-- ========================= Body Ejecutar =======================================

pBodyEjecutar =  EjecutarN <$> pN
             <|> EjecutarL <$> pL

-- =========================== Body Circulo Agujero ==============================

pBodyAgujero =  AgujeroAngulos <$> pAngulo
            <|> AgujeroE       <$> pE
            <|> AgujeroH       <$> pH
            <|> AgujeroI       <$  pKeyword "I" <*> pNumero
            <|> AgujeroJ       <$  pKeyword "J" <*> pNumero
            <|> AgujeroL       <$> pL
            <|> AgujeroP       <$> pP
            <|> AgujeroR       <$> pR
            <|> AgujeroW       <$  pKeyword "W" <*> pNumero
            <|> AgujeroX       <$  pKeyword "X" <*> pNumero
            <|> AgujeroY       <$  pKeyword "Y" <*> pNumero

-- ========================= Body G25 ============================================

pBodyPosRepite =  PosRepiteE <$> pE
              <|> PosRepiteF <$> pF
              <|> PosRepiteH <$> pH
              <|> PosRepiteI <$  pKeyword "I" <*> pNumero
              <|> PosRepiteJ <$  pKeyword "J" <*> pNumero
              <|> PosRepiteN <$> pN
              <|> PosRepiteP <$> pP
              <|> PosRepiteX <$  pKeyword "X" <*> pNumero
              <|> PosRepiteY <$  pKeyword "Y" <*> pNumero

-- ==================== Alojamiento G26 | Saliencia G27 ==========================

pAlojamiento = Alojamientos <$> pAloja <*> pList pBodyAlojamiento

pAloja =  AlojamientoG26 <$ pKeyword "G26"
      <|> AlojamientoG27 <$ pKeyword "G27"

pBodyAlojamiento =  AlojaD         <$> pD
                <|> AlojaF         <$> pF
                <|> AlojaH         <$> pH
                <|> AlojaPosCentro <$> pPosicionCentro
                <|> AlojaL         <$> pL
                <|> AlojaN         <$> pN
                <|> AlojaO         <$> pO
                <|> AlojaP         <$> pP
                <|> AlojaQ         <$> pQ
                <|> AlojaR         <$> pR
                <|> AlojaEjeRota   <$> pEjesRotacional
                <|> AlojaEjeLineal <$> pEjesLineales

-- =========================== Body Escala =======================================

pBodyEscala =  EscalaE          <$> pE
           <|> EscalaH          <$> pH
           <|> EscalaL          <$> pL
           <|> EscalaN          <$> pN
           <|> EscalaP          <$> pP
           <|> EscalaRotacional <$> pEjesRotacional
           <|> EscalaLineal     <$> pEjesLineales

-- ============================== Body Cavidad ===================================

pBodyCavidad =  CavidadD <$> pD
            <|> CavidadE <$> pE
            <|> CavidadH <$> pH
            <|> CavidadN <$> pN
            <|> CavidadP <$> pP
            <|> CavidadX <$  pKeyword "X" <*> pNumero
            <|> CavidadZ <$  pKeyword "Z" <*> pNumero

-- ===============================================================================
--                    Asignacion de Angulos
-- ===============================================================================

pAngulo = Angulo <$> pAngulos <*> pNumero

pAngulos =  PosicionActual  <$ pKeyword "A"
        <|> PuntoCentro     <$ pKeyword "C"
        <|> EspacionAngular <$ pKeyword "B"

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

pM = M <$> pCord
 --Controlar en las Condiciones de contexto
pCord =  ControlEjeArbol    <$> pControlArbol 
     <|> ControlDelPrograma <$> pControlPrograma
--     <|> CambioGrupo        <$> CambioGrupo

pControlArbol =  RotacionDer <$ pKeyword "M03"
             <|> RotacionIzq <$ pKeyword "M04"
             <|> ParadaArbol <$ pKeyword "M05"

pControlPrograma =  ParadaPrograma <$ pKeyword "M00"
                <|> ParadaOpcional <$ pKeyword "M01"
                <|> FinPrograma    <$ pKeyword "M02"
                <|> FinProgramaReb <$ pKeyword "M30"
                <|> CambioHerram   <$ pKeyword "M06"

-- ===============================================================================
--                     Ramificaciones en programa 
--                          N - H - E - L - P 
-- ===============================================================================

pNSecuencia =  N <$ pKeyword "N" <*>  pNumero
           <|> pSucceed EmptyNSecuencia

pN = Num <$ pKeyword "N" <*> pNumero

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

pR = R <$ pKeyword "R" <*> pNumero

-- ===============================================================================

--pEndBlock = EndB <$ pKeyword "#"

-- ===============================================================================
--                       Union Scanner con el Parser
-- ===============================================================================

instance (Eq Tipo) => (Eq Simbolo) where
   (Simbolo Error _ _ _ ) == (Simbolo Error _ _ _ ) = True
   (Simbolo Numero _ _ _) == (Simbolo Numero _ _ _) = True
   (Simbolo Keyword a _ _) == (Simbolo Keyword a1 _ _) =  a == a1
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
pEndBlock k = tSym EndBlock k
pNumero   = (\ x ->(read (addCero x))::Float) <$> tSym Numero ""

addCero x | last x == '.' = x++"0"
          | otherwise = x

