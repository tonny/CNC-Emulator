module Parser where
import UU.Parsing
import Scanner
import GramaticaAbstracta

pRaiz = pCnc

pCnc =  Movimientos <$> pFoldr ((:),[]) pMovimiento
       
{-
=====================================================================
    - Funciones G de Movimientos 
===================================================================== 
-}

pMovimiento =  Movimiento       <$> pTipoMovimiento <*> pFoldr ((:),[]) pDirMov
           <|> MovimientoN      <$> pCordenadas     <*  pKeyword "N"
           <|> MovimientoElice  <$> pElices         <*> pFoldr ((:),[]) pDirElice
           <|> CircAgujeroG24   <$  pKeyword "G24"  <*> pFoldr ((:),[]) pAgujeros
           <|> PosRepiteG25     <$  pKeyword "G25"  <*> pFoldr ((:),[]) pPosRepite
           <|> AlojaSale        <$> pMovAlojaSale   <*> pFoldr ((:),[]) pAlojaSale
           <|> EjecutarG29      <$  pKeyword "G29"  <*> pEjecutarAC
           <|> EspejoDeslLiga   <$> pEspejo         <*> pDirEspejo
           <|> EjesIzqDerCan    <$> pIzqDerCan      <*> pFoldr ((:),[]) pDirIzqDerCan
           <|> CorrectorFijG45  <$  pKeyword "G45"  <*> pFoldr ((:),[]) pDirCorrector
           <|> ZonaseguranzaG60 <$  pKeyword "G60"  <*> pFoldr ((:),[]) pDirZonaseguranza
           <|> ContrInibG62     <$  pKeyword "G62"  <*> pFoldr ((:),[]) pDirContr
           <|> ContrGraficoG66  <$  pKeyword "G66"  <*> pFoldr ((:),[]) pDirContrGraf
           <|> EscalaG72        <$  pKeyword "G72"  <*> pFoldr ((:),[]) pEscala 
           <|> RotacionG74      <$  pKeyword "G74"  <*> pFoldr ((:),[]) pRotacion
           <|> AbsolutoIncre    <$> pAbsoluto       <*> pFoldr ((:),[]) pAbsIncr
           <|> CavidadG75       <$  pKeyword "G75"  <*> pFoldr ((:),[]) pCavidad
           <|> PerforarRebajar  <$> pRebajar        <*> pFoldr ((:),[]) pPerforarRebajar
           <|> PerforarDescG83  <$  pKeyword "G83"  <*> pFoldr ((:),[]) pPerforarDes
           <|> RoscMandPrdEje   <$> pRoscMandPrd    <*> pFoldr ((:),[]) pRoscMandPrdEje

pTipoMovimiento =  MovRapidoG00       <$ pKeyword "G00"
               <|> MovLinealG01       <$ pKeyword "G01" 
               <|> MovArcoHorG02      <$ pKeyword "G02" 
               <|> MovArcoAHorG03     <$ pKeyword "G03" 
               <|> MovPermanenciaG04  <$ pKeyword "G04" 
               <|> MovArcoTangenteG05 <$ pKeyword "G05" 
               <|> MovPtoLineaG73     <$ pKeyword "G73" 
               <|> SubprogParamG39    <$ pKeyword "G39" 
               <|> ProgramaACG79      <$ pKeyword "G79" 
               <|> PalG59             <$ pKeyword "G59"
               <|> CancelarG92G99     <$ pKeyword "G99" 


pDirMov    =  GA <$ pKeyword "A"
          <|> GB <$ pKeyword "B"
          <|> GC <$ pKeyword "C"
          <|> GD <$ pKeyword "D"
          <|> GE <$ pKeyword "E"
          <|> GF <$ pKeyword "F"
          <|> GH <$ pKeyword "H"
          <|> GI <$ pKeyword "I"
          <|> GJ <$ pKeyword "J"
          <|> GK <$ pKeyword "K"
          <|> GL <$ pKeyword "L"
          <|> GM <$ pKeyword "M"
          <|> GN <$ pKeyword "N"
          <|> GO <$ pKeyword "O"
          <|> GP <$ pKeyword "P"
          <|> GQ <$ pKeyword "Q"
          <|> GR <$ pKeyword "R"
          <|> GS <$ pKeyword "S"
          <|> Gt <$ pKeyword "T"
          <|> GU <$ pKeyword "U"
          <|> GV <$ pKeyword "V"
          <|> GW <$ pKeyword "W"
          <|> GX <$ pKeyword "X"
          <|> GY <$ pKeyword "Y"
          <|> GZ <$ pKeyword "Z"

-------------------------------------------------------------------------------        

pCordenadas =  PlanoXYG17       <$ pKeyword "G17"
           <|> PlanoXZG18       <$ pKeyword "G18"
           <|> PlanoYZG19       <$ pKeyword "G19"
           <|> PulgadaG70       <$ pKeyword "G70"
           <|> MetricoG71       <$ pKeyword "G71"
           <|> DesactivaG80     <$ pKeyword "G80"
           <|> ReactivaACG89    <$ pKeyword "G89"
           <|> ReferenTiempoG92 <$ pKeyword "G92"

-------------------------------------------------------------------------------        

pElices =  EliceHor  <$ pKeyword "G22"
       <|> EliceAhor <$ pKeyword "G23"

pDirElice =  EliceI   <$ pKeyword "I"
         <|> EliceJ   <$ pKeyword "J"
         <|> EliceK   <$ pKeyword "K"
         <|> EliceN   <$ pKeyword "N"
         <|> EliceXyz <$> pXyz

pXyz =  X <$ pKeyword "X"         
    <|> Y <$ pKeyword "Y"
    <|> Z <$ pKeyword "Z"

-------------------------------------------------------------------------------

pAgujeros =  CircA <$ pKeyword "A"
         <|> CircB <$ pKeyword "B"
         <|> CircC <$ pKeyword "C"
         <|> CircE <$ pKeyword "E"
         <|> CircH <$ pKeyword "H"
         <|> CircI <$ pKeyword "I"
         <|> CircJ <$ pKeyword "J"
         <|> CircL <$ pKeyword "L"
         <|> CircP <$ pKeyword "P"
         <|> CircR <$ pKeyword "R"
         <|> CircW <$ pKeyword "W"
         <|> CircX <$ pKeyword "X"
         <|> CircY <$ pKeyword "Y"

-------------------------------------------------------------------------------

pPosRepite =  PosRepiteE <$ pKeyword "E"
          <|> PosRepiteF <$ pKeyword "F"
          <|> PosRepiteH <$ pKeyword "H"
          <|> PosRepiteI <$ pKeyword "I"
          <|> PosRepiteJ <$ pKeyword "J"
          <|> PosRepiteN <$ pKeyword "N"
          <|> PosRepiteP <$ pKeyword "P"
          <|> PosRepiteX <$ pKeyword "X"
          <|> PosRepiteY <$ pKeyword "Y"

-------------------------------------------------------------------------------

pMovAlojaSale =  AlojamientoG26 <$ pKeyword "G26"
             <|> SalenciaG27    <$ pKeyword "G27"

pAlojaSale =  AlojaSaleD <$ pKeyword "D"            
          <|> AlojaSaleF <$ pKeyword "F"
          <|> AlojaSaleH <$ pKeyword "H"
          <|> AlojaSaleI <$ pKeyword "I"
          <|> AlojaSaleJ <$ pKeyword "J"
          <|> AlojaSaleK <$ pKeyword "K"
          <|> AlojaSaleL <$ pKeyword "L"
          <|> AlojaSaleN <$ pKeyword "N"
          <|> AlojaSaleO <$ pKeyword "O"
          <|> AlojaSaleP <$ pKeyword "P"
          <|> AlojaSaleQ <$ pKeyword "Q"
          <|> AlojaSaleR <$ pKeyword "R"
          <|> AlojaSaleU <$ pKeyword "U"
          <|> AlojaSaleV <$ pKeyword "V"
          <|> AlojaSaleW <$ pKeyword "W"
          <|> AlojaSaleX <$ pKeyword "X"
          <|> AlojaSaleY <$ pKeyword "Y"
          <|> AlojaSaleZ <$ pKeyword "Z"

-------------------------------------------------------------------------------

pEjecutarAC =  EjecutarL <$ pKeyword "L"
           <|> EjecutarN <$ pKeyword "N"

-------------------------------------------------------------------------------
pEspejo =  DeslEspejoG30 <$ pKeyword "G30"
       <|> LigaEspejoG31 <$ pKeyword "G31"

pDirEspejo =  DirEspejoN <$ pKeyword "N"
          <|> DirEspejoU <$ pKeyword "U"
          <|> DirEspejoV <$ pKeyword "V"
          <|> DirEspejoW <$ pKeyword "W"
          <|> DirEspejoX <$ pKeyword "X"
          <|> DirEspejoY <$ pKeyword "Y"
          <|> DirEspejoZ <$ pKeyword "Z"
-------------------------------------------------------------------------------

pIzqDerCan =  CancelarCompG40 <$ pKeyword "G40"
          <|> HerramIzqG41    <$ pKeyword "G41"
          <|> HerramDerG42    <$ pKeyword "G42"

pDirIzqDerCan =  IzqDerCanN   <$  pKeyword "N"          
             <|> IzqDerCanXYZ <$> pXyz

-------------------------------------------------------------------------------

pDirCorrector =  CorrectorE <$ pKeyword "E"
             <|> CorrectorH <$ pKeyword "H"
             <|> CorrectorN <$ pKeyword "N"
             <|> CorrectorO <$ pKeyword "O"
             <|> CorrectorP <$ pKeyword "P"

-------------------------------------------------------------------------------

pDirZonaseguranza =  ZonaseguranzaA <$ pKeyword "A"
                 <|> ZonaseguranzaB <$ pKeyword "B"
                 <|> ZonaseguranzaC <$ pKeyword "C"
                 <|> ZonaseguranzaI <$ pKeyword "I"
                 <|> ZonaseguranzaJ <$ pKeyword "J"
                 <|> ZonaseguranzaK <$ pKeyword "K"
                 <|> ZonaseguranzaN <$ pKeyword "N"
                 <|> ZonaseguranzaR <$ pKeyword "R"
                 <|> ZonaseguranzaU <$ pKeyword "U"
                 <|> ZonaseguranzaV <$ pKeyword "V"
                 <|> ZonaseguranzaW <$ pKeyword "W"
                 <|> ZonaseguranzaX <$ pKeyword "X"
                 <|> ZonaseguranzaY <$ pKeyword "Y"
                 <|> ZonaseguranzaZ <$ pKeyword "Z"

-------------------------------------------------------------------------------

pDirContr = ContrInibF <$ pKeyword "F"
        <|> ContrInibN <$ pKeyword "N"
        <|> ContrInibQ <$ pKeyword "Q"
        <|> ContrInibS <$ pKeyword "S"
-------------------------------------------------------------------------------

pDirContrGraf =  ContrGrafN <$ pKeyword "N"
             <|> ContrGrafQ <$ pKeyword "Q" 
             <|> ContrGrafR <$ pKeyword "R"
             <|> ContrGrafW <$ pKeyword "W"
-------------------------------------------------------------------------------

pEscala =  EscalaE <$ pKeyword "E"
       <|> EscalaH <$ pKeyword "H"
       <|> EscalaL <$ pKeyword "L"
       <|> EscalaN <$ pKeyword "N"
       <|> EscalaP <$ pKeyword "P"
       <|> EscalaU <$ pKeyword "U"
       <|> EscalaV <$ pKeyword "V"
       <|> EscalaW <$ pKeyword "W"
       <|> EscalaX <$ pKeyword "X"
       <|> EscalaY <$ pKeyword "Y"
       <|> EscalaZ <$ pKeyword "Z"
-------------------------------------------------------------------------------

pRotacion =  RotacionA <$ pKeyword "A"
         <|> RotacionC <$ pKeyword "C"
         <|> RotacionE <$ pKeyword "E"
         <|> RotacionH <$ pKeyword "H"
         <|> RotacionI <$ pKeyword "I"
         <|> RotacionJ <$ pKeyword "J"
         <|> RotacionM <$ pKeyword "M"
         <|> RotacionN <$ pKeyword "N"
         <|> RotacionP <$ pKeyword "P"
         <|> RotacionR <$ pKeyword "R"
         <|> RotacionX <$ pKeyword "X"
         <|> RotacionY <$ pKeyword "Y"
         <|> RotacionZ <$ pKeyword "Z"

-------------------------------------------------------------------------------
pAbsoluto =  AbsolutoG90    <$ pKeyword "G90"
         <|> IncrementalG91 <$ pKeyword "G91"

pAbsIncr =  AbsIncA <$ pKeyword "A"
        <|> AbsIncB <$ pKeyword "B" 
        <|> AbsIncC <$ pKeyword "C" 
        <|> AbsIncI <$ pKeyword "I" 
        <|> AbsIncJ <$ pKeyword "J" 
        <|> AbsIncK <$ pKeyword "K" 
        <|> AbsIncN <$ pKeyword "N" 
        <|> AbsIncR <$ pKeyword "R" 
        <|> AbsIncU <$ pKeyword "U" 
        <|> AbsIncV <$ pKeyword "V" 
        <|> AbsIncW <$ pKeyword "W" 
        <|> AbsIncX <$ pKeyword "X" 
        <|> AbsIncY <$ pKeyword "Y" 
        <|> AbsIncZ <$ pKeyword "Z" 

-------------------------------------------------------------------------------

pCavidad =  CavidadD <$ pKeyword "D"
        <|> CavidadE <$ pKeyword "E"
        <|> CavidadH <$ pKeyword "H"
        <|> CavidadN <$ pKeyword "N"
        <|> CavidadP <$ pKeyword "P"
        <|> CavidadX <$ pKeyword "X"
        <|> CavidadZ <$ pKeyword "Z"
-------------------------------------------------------------------------------

pRebajar =  PerforarG81 <$ pKeyword "G81"
        <|> RebajarG82  <$ pKeyword "G82"

pPerforarRebajar =  PerforarRebajarD <$ pKeyword "D"        
                <|> PerforarRebajarF <$ pKeyword "F"
                <|> PerforarRebajarN <$ pKeyword "N"
                <|> PerforarRebajarP <$ pKeyword "P"
                <|> PerforarRebajarR <$ pKeyword "R"
                <|> PerforarRebajarV <$ pKeyword "V"
                <|> PerforarRebajarX <$ pKeyword "X"
                <|> PerforarRebajarY <$ pKeyword "Y"
                <|> PerforarRebajarZ <$ pKeyword "Z"
-------------------------------------------------------------------------------

pPerforarDes =  PerforarDes  <$> pPerforarRebajar 
            <|> PerforarDesI <$  pKeyword "I"  
            <|> PerforarDesJ <$  pKeyword "J"
            <|> PerforarDesK <$  pKeyword "K"
            <|> PerforarDesU <$  pKeyword "U"  
            <|> PerforarDesW <$  pKeyword "W"

-------------------------------------------------------------------------------

pRoscMandPrd =  RoscamientoG84 <$ pKeyword "G84"
            <|> MandirlarG85   <$ pKeyword "G85"
            <|> MandPrdEjeG86  <$ pKeyword "G86"

pRoscMandPrdEje =  RoscMandPrdEjeD <$ pKeyword "D" 
               <|> RoscMandPrdEjeF <$ pKeyword "F" 
               <|> RoscMandPrdEjeN <$ pKeyword "N" 
               <|> RoscMandPrdEjeP <$ pKeyword "P" 
               <|> RoscMandPrdEjeR <$ pKeyword "R" 
               <|> RoscMandPrdEjeX <$ pKeyword "X" 
               <|> RoscMandPrdEjeY <$ pKeyword "Y" 
               <|> RoscMandPrdEjeZ <$ pKeyword "Z" 




{--

{-=====================================================================
              - Funciones G de Auto Ciclos
=======================================================================-}

pAutoCiclo =  CavidadG75     <$ pKeyword "G75" <*> pList pDir75
--          <|> ProgramaACG79  <$ pKeyword "G79" <*> pList pDirMov
          <|> DesactivaG80   <$ pKeyword "G80" <*  pKeyword "N"
          <|> PerforarG81    <$ pKeyword "G81" <*> pList pDir812
          <|> RebajarG82     <$ pKeyword "G82" <*> pList pDir812
          <|> PerforarDesG83 <$ pKeyword "G83" <*> pList pDir83
          <|> RoscamientoG84 <$ pKeyword "G84" <*> pList pDir8456
          <|> MandrilarG85   <$ pKeyword "G85" <*> pList pDir8456
          <|> MandPrdEjeG86  <$ pKeyword "G86" <*> pList pDir8456
          <|> ReactivaACG89  <$ pKeyword "G89" <*  pKeyword "N"

pDir75 =  GD75 <$ pKeyword "D"
      <|> GE75 <$ pKeyword "E"
      <|> GH75 <$ pKeyword "H"
      <|> GN75 <$ pKeyword "N"
      <|> GP75 <$ pKeyword "P"
      <|> GX75 <$ pKeyword "X"
      <|> GZ75 <$ pKeyword "Z"

pDir812 =  GD812 <$ pKeyword "D" 
       <|> GF812 <$ pKeyword "F"
       <|> GN812 <$ pKeyword "N"     
       <|> GP812 <$ pKeyword "P"
       <|> GR812 <$ pKeyword "R"
       <|> GV812 <$ pKeyword "V"
       <|> GX812 <$ pKeyword "X"
       <|> GY812 <$ pKeyword "Y"
       <|> GZ812 <$ pKeyword "Z"

pDir83 =  GD83 <$ pKeyword "D"
      <|> GF83 <$ pKeyword "F"
      <|> GI83 <$ pKeyword "I"
      <|> GJ83 <$ pKeyword "J"
      <|> GK83 <$ pKeyword "K"
      <|> GN83 <$ pKeyword "N"
      <|> GP83 <$ pKeyword "P"
      <|> GR83 <$ pKeyword "R"
      <|> GU83 <$ pKeyword "U"
      <|> GW83 <$ pKeyword "W"
      <|> GX83 <$ pKeyword "X"
      <|> GY83 <$ pKeyword "Y"
      <|> GZ83 <$ pKeyword "Z"

pDir8456 =  GD8456 <$ pKeyword "D" 
        <|> GF8456 <$ pKeyword "F"
        <|> GN8456 <$ pKeyword "N"     
        <|> GP8456 <$ pKeyword "P"
        <|> GR8456 <$ pKeyword "R"
        <|> GX8456 <$ pKeyword "X"
        <|> GY8456 <$ pKeyword "Y"
        <|> GZ8456 <$ pKeyword "Z"

{- 
===============================================================================
    - Funciones G de Auto Rutinas 
=============================================================================== 
-}

pAutoRutinas =  HeliceHorG22    <$ pKeyword "G22" <*> pList pDirG223
            <|> HeliceAHorG23   <$ pKeyword "G23" <*> pList pDirG223
            <|> CircAgujerosG24 <$ pKeyword "G24" <*> pList pDirG24
            <|> PosRepiteG25    <$ pKeyword "G25" <*> pList pDirG25
            <|> AlojamientoG26  <$ pKeyword "G26" <*> pList pDirG267
            <|> SalienciaG27    <$ pKeyword "G27" <*> pList pDirG267
            <|> EjecutarACG29   <$ pKeyword "G29" <*> pList pDirG29
--            <|> SubprogParamG39 <$ pKeyword "G30" <*> pList pDirMov

pDirG223 =  GI223 <$ pKeyword "I"
        <|> GJ223 <$ pKeyword "J"
        <|> GK223 <$ pKeyword "K"
        <|> GN223 <$ pKeyword "N"
        <|> GX223 <$ pKeyword "X"
        <|> GY223 <$ pKeyword "Y"
        <|> GZ223 <$ pKeyword "Z"

pDirG24 =  GA24 <$ pKeyword "A"
       <|> GB24 <$ pKeyword "B"
       <|> GC24 <$ pKeyword "C"
       <|> GE24 <$ pKeyword "E"
       <|> GH24 <$ pKeyword "H"
       <|> GI24 <$ pKeyword "I"
       <|> GJ24 <$ pKeyword "J"
       <|> GL24 <$ pKeyword "L"
       <|> GP24 <$ pKeyword "P"
       <|> GR24 <$ pKeyword "R"
       <|> GW24 <$ pKeyword "W"
       <|> GX24 <$ pKeyword "X"
       <|> GY24 <$ pKeyword "Y"
       
pDirG25 = GE25 <$ pKeyword "E"
      <|> GF25 <$ pKeyword "F"
      <|> GH25 <$ pKeyword "H"
      <|> GI25 <$ pKeyword "I"
      <|> GJ25 <$ pKeyword "J"
      <|> GN25 <$ pKeyword "N"
      <|> GP25 <$ pKeyword "P"
      <|> GX25 <$ pKeyword "X"
      <|> GY25 <$ pKeyword "Y" 

pDirG267 =  GD267 <$ pKeyword "D"
        <|> GF267 <$ pKeyword "F"
        <|> GH267 <$ pKeyword "H"
        <|> GI267 <$ pKeyword "I"
        <|> GJ267 <$ pKeyword "J"
        <|> GK267 <$ pKeyword "K"
        <|> GL267 <$ pKeyword "L"
        <|> GN267 <$ pKeyword "N"
        <|> GO267 <$ pKeyword "O"
        <|> GP267 <$ pKeyword "P"
        <|> GQ267 <$ pKeyword "Q"
        <|> GR267 <$ pKeyword "R"
        <|> GU267 <$ pKeyword "U"
        <|> GV267 <$ pKeyword "V"
        <|> GW267 <$ pKeyword "W"
        <|> GX267 <$ pKeyword "X"
        <|> GY267 <$ pKeyword "Y"
        <|> GZ267 <$ pKeyword "Z"

pDirG29 =  GN29 <$ pKeyword "N"
       <|> GL29 <$ pKeyword "L"
{- 
===============================================================================
    - Funciones G de Variables
=============================================================================== 
-}   

--pVariables =  PalG59    <$ pKeyword "G59" <*> pList pDirMov
--          <|> GlobalG79 <$ pKeyword "G79" <*> pList pDirMov 

{- 
===============================================================================
    - Funciones G de Control de Cordenadas
=============================================================================== 
-}


pControlCordenadas =  PulgadaG70       <$ pKeyword "G70" <*  pKeyword "N"
                  <|> MetricoG71       <$ pKeyword "G71" <*  pKeyword "N"
                  <|> EscalaG72        <$ pKeyword "G72" <*> pFoldr ((:),[]) pDirG72
                  <|> RotacionG74      <$ pKeyword "G74" <*> pFoldr ((:),[]) pDirG74
                  <|> AbsolutoG90      <$ pKeyword "G90" <*> pFoldr ((:),[]) pDirG912
                  <|> IncrementalG91   <$ pKeyword "G91" <*> pFoldr ((:),[]) pDirG912
                  <|> RefenreTiempoG92 <$ pKeyword "G92" <*  pKeyword "N"
                  <|> AvancePTiempoG94 <$ pKeyword "G94" <*> pFoldr((:),[]) pDirG94
                  <|> InibeAcDesG97    <$ pKeyword "G97" <*> pFoldr((:),[]) pDirG97
--                  <|> CancelarG92G99   <$ pKeyword "G99" <*> pList pDirMov 

pDirG72 =  GE72 <$ pKeyword "E"
       <|> GH72 <$ pKeyword "H"
       <|> GL72 <$ pKeyword "L"
       <|> GN72 <$ pKeyword "N"
       <|> GP72 <$ pKeyword "P"
       <|> GU72 <$ pKeyword "U"
       <|> GV72 <$ pKeyword "V"
       <|> GW72 <$ pKeyword "W"
       <|> GX72 <$ pKeyword "X"
       <|> GY72 <$ pKeyword "Y"
       <|> GZ72 <$ pKeyword "Z"

pDirG74 =  GA74 <$ pKeyword "A"
       <|> GC74 <$ pKeyword "C"
       <|> GE74 <$ pKeyword "E"
       <|> GH74 <$ pKeyword "H"
       <|> GI74 <$ pKeyword "I"
       <|> GJ74 <$ pKeyword "J"
       <|> GM74 <$ pKeyword "M"
       <|> GN74 <$ pKeyword "N"
       <|> GP74 <$ pKeyword "P"
       <|> GR74 <$ pKeyword "R"
       <|> GX74 <$ pKeyword "X"
       <|> GY74 <$ pKeyword "Y"
       <|> GZ74 <$ pKeyword "Z"
               
pDirG912 =  GA912 <$ pKeyword "A"
        <|> GB912 <$ pKeyword "B"
        <|> GC912 <$ pKeyword "C"
        <|> GI912 <$ pKeyword "I"
        <|> GJ912 <$ pKeyword "J"
        <|> GK912 <$ pKeyword "K"
        <|> GN912 <$ pKeyword "N"
        <|> GR912 <$ pKeyword "R"
        <|> GU912 <$ pKeyword "U"
        <|> GV912 <$ pKeyword "V"
        <|> GW912 <$ pKeyword "W"
        <|> GX912 <$ pKeyword "X"
        <|> GY912 <$ pKeyword "Y"
        <|> GZ912 <$ pKeyword "Z"

pDirG94 =  GC94 <$ pKeyword "C"        
       <|> GF94 <$ pKeyword "F"
       <|> GN94 <$ pKeyword "N"

pDirG97 =  GN97 <$ pKeyword "N"       
       <|> GU97 <$ pKeyword "U"
       <|> GV97 <$ pKeyword "V"
       <|> GW97 <$ pKeyword "W"
       <|> GX97 <$ pKeyword "X"
       <|> GY97 <$ pKeyword "Y"
       <|> GZ97 <$ pKeyword "Z"

{- 
===============================================================================
    - Funciones G Modos de los Ejes
=============================================================================== 
-}   

pModosEjes =  PlanoXYG17    <$ pKeyword "G17" <*  pKeyword "N"
          <|> PlanoXZG18    <$ pKeyword "G18" <*  pKeyword "N"
          <|> PlanoYZG19    <$ pKeyword "G19" <*  pKeyword "N"
          <|> DeslEspejoG30 <$ pKeyword "G30" <*> pList pDirG97
          <|> LigaEspejoG31 <$ pKeyword "G31" <*> pList pDirG97
--}

