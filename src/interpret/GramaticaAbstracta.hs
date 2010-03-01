module GramaticaAbstracta where

--pRaiz = pCodigoCnc

data CodigoCnc = Cnc [Cnc]
      deriving Show 

data Cnc = Movimientos       [Movimiento]
                 
--         | AutoCiclos         [AutoCiclo]
--         | AutoRutinas       [AutoRutinas]
--         | Variables         [Variables]
--         | ControlCordenadas [ControlCordenadas]
--         | ModosEjes         [ModosEjes]
   deriving Show

{-
=====================================================================
    - Funciones G de Movimientos 
===================================================================== 
-}  

data Movimiento = Movimiento       TipoMovimiento     [DirMov]
                | MovimientoN      Cordenadas
                | MovimientoElice  Elices             [DirElice]
                | CircAgujeroG24   [Agujeros]
                | PosRepiteG25     [PosRepite]
                | AlojaSale        MovAlojaSale       [AlojaSale]
                | EjecutarG29      EjecutarAC
                | EspejoDeslLiga   Espejo             DirEspejo
                | EjesIzqDerCan    IzqDerCan          [DirIzqDerCan]
                | CorrectorFijG45  [DirCorrector]
                | ZonaseguranzaG60 [DirZonaseguranza]
                | ContrInibG62     [DirContr]
                | ContrGraficoG66  [DirContrGraf]
                | EscalaG72        [Escala]
                | RotacionG74      [Rotacion]
                | AbsolutoIncre    Absoluto           [AbsIncr]
                | CavidadG75       [Cavidad]
                | PerforarRebajar  Rebajar            [PerforarRebajar]
                | PerforarDescG83  [PerforarDes]
                | RoscMandPrdEje   RoscMandPrd        [RoscMandPrdEje]

          deriving Show

   
data TipoMovimiento = MovRapidoG00
                    | MovLinealG01 
                    | MovArcoHorG02
                    | MovArcoAHorG03
                    | MovPermanenciaG04
                    | MovArcoTangenteG05
                    | MovPtoLineaG73
                    | SubprogParamG39
                    | ProgramaACG79
                    | PalG59
                    | CancelarG92G99
             deriving Show

data DirMov = GA | GB | GC | GD | GE | GF | GH | GI | GJ | GK | GL | GM | GN 
            | GO | GP | GQ | GR | GS | Gt | GU | GV | GW | GX | GY | GZ    
       deriving Show

-------------------------------------------------------------------------------

--data N = Ene Cordenadas
--     deriving Show

data Cordenadas = PlanoXYG17
                | PlanoXZG18
                | PlanoYZG19
                | PulgadaG70
                | MetricoG71
                | DesactivaG80
                | ReactivaACG89
                | ReferenTiempoG92
        deriving Show
-------------------------------------------------------------------------------        

--data Elice = Elice Elices DirElice
--    deriving Show

data Elices = EliceHor
            | EliceAhor
      deriving Show

data DirElice = EliceI
              | EliceJ
              | EliceK
              | EliceN
              | EliceXyz Xyz
         deriving Show

data Xyz = X         
         | Y
         | Z
    deriving Show     

-------------------------------------------------------------------------------

data Agujeros = CircA | CircB | CircC | CircE | CircH | CircI | CircJ
              | CircL | CircP | CircR | CircW | CircX | CircY
    deriving Show

-------------------------------------------------------------------------------

data PosRepite = PosRepiteE | PosRepiteF | PosRepiteH | PosRepiteI 
               | PosRepiteJ | PosRepiteN | PosRepiteP | PosRepiteX 
               | PosRepiteY
        deriving Show

-------------------------------------------------------------------------------

data MovAlojaSale = AlojamientoG26
                  | SalenciaG27
         deriving Show

data AlojaSale = AlojaSaleD | AlojaSaleF | AlojaSaleH | AlojaSaleI | AlojaSaleJ
               | AlojaSaleK | AlojaSaleL | AlojaSaleN | AlojaSaleO | AlojaSaleP 
               | AlojaSaleQ | AlojaSaleR | AlojaSaleU | AlojaSaleV | AlojaSaleW 
               | AlojaSaleX | AlojaSaleY | AlojaSaleZ
        deriving Show

-------------------------------------------------------------------------------

data EjecutarAC = EjecutarL
                | EjecutarN 
        deriving Show

-------------------------------------------------------------------------------
data Espejo = DeslEspejoG30
            | LigaEspejoG31
        deriving Show

data DirEspejo = DirEspejoN | DirEspejoU | DirEspejoV | DirEspejoW 
               | DirEspejoX | DirEspejoY | DirEspejoZ
       deriving Show
-------------------------------------------------------------------------------

data IzqDerCan = CancelarCompG40 
               | HerramIzqG41
               | HerramDerG42
        deriving Show

data DirIzqDerCan = IzqDerCanN            
                  | IzqDerCanXYZ Xyz
           deriving Show
-------------------------------------------------------------------------------

data DirCorrector = CorrectorE | CorrectorH | CorrectorN
                  | CorrectorO | CorrectorP
            deriving Show

-------------------------------------------------------------------------------

data DirZonaseguranza = ZonaseguranzaA | ZonaseguranzaB | ZonaseguranzaC
                      | ZonaseguranzaI | ZonaseguranzaJ | ZonaseguranzaK 
                      | ZonaseguranzaN | ZonaseguranzaR | ZonaseguranzaU
                      | ZonaseguranzaV | ZonaseguranzaW | ZonaseguranzaX
                      | ZonaseguranzaY | ZonaseguranzaZ
            deriving Show
-------------------------------------------------------------------------------

data DirContr = ContrInibF | ContrInibN
              | ContrInibQ | ContrInibS
        deriving Show
-------------------------------------------------------------------------------

data DirContrGraf = ContrGrafN | ContrGrafQ 
                  | ContrGrafR | ContrGrafW
            deriving Show

-------------------------------------------------------------------------------

data Escala = EscalaE | EscalaH | EscalaL | EscalaN | EscalaP
            | EscalaU | EscalaV | EscalaW | EscalaX | EscalaY | EscalaZ
        deriving Show

-------------------------------------------------------------------------------

data Rotacion = RotacionA | RotacionC | RotacionE | RotacionH | RotacionI 
              | RotacionJ | RotacionM | RotacionN | RotacionP | RotacionR
              | RotacionX | RotacionY | RotacionZ
       deriving Show
-------------------------------------------------------------------------------

data Absoluto = AbsolutoG90
              | IncrementalG91
       deriving Show

data AbsIncr = AbsIncA | AbsIncB | AbsIncC | AbsIncI | AbsIncJ | AbsIncK
             | AbsIncN | AbsIncR | AbsIncU | AbsIncV | AbsIncW | AbsIncX
             | AbsIncY | AbsIncZ
        deriving Show
-------------------------------------------------------------------------------

data Cavidad = CavidadD | CavidadE | CavidadH | CavidadN 
             | CavidadP | CavidadX | CavidadZ 
    deriving Show
-------------------------------------------------------------------------------

data Rebajar = PerforarG81 
             | RebajarG82  
    deriving Show

data PerforarRebajar = PerforarRebajarD | PerforarRebajarF | PerforarRebajarN 
                     | PerforarRebajarP | PerforarRebajarR | PerforarRebajarV
                     | PerforarRebajarX | PerforarRebajarY | PerforarRebajarZ 
            deriving Show
-------------------------------------------------------------------------------

data PerforarDes = PerforarDes  PerforarRebajar 
                 | PerforarDesI 
                 | PerforarDesJ
                 | PerforarDesK
                 | PerforarDesU 
                 | PerforarDesW 
        deriving Show

-------------------------------------------------------------------------------

data RoscMandPrd = RoscamientoG84 
                 | MandirlarG85   
                 | MandPrdEjeG86 
        deriving Show

data RoscMandPrdEje = RoscMandPrdEjeD
                    | RoscMandPrdEjeF
                    | RoscMandPrdEjeN
                    | RoscMandPrdEjeP
                    | RoscMandPrdEjeR
                    | RoscMandPrdEjeX
                    | RoscMandPrdEjeY
                    | RoscMandPrdEjeZ
            deriving Show
{-
{-        
===============================================================================
    - Funcion G de Auto Ciclo
===============================================================================     
-}


data AutoCiclo = CavidadG75     [Dir75]
               | ProgramaACG79  [DirMov]
               | DesactivaG80
               | PerforarG81    [Dir812]
               | RebajarG82     [Dir812]
               | PerforarDesG83 [Dir83]
               | RoscamientoG84 [Dir8456]
               | MandrilarG85   [Dir8456]
               | MandPrdEjeG86  [Dir8456]
               | ReactivaACG89  
       deriving Show

data Dir75 = GD75 | GE75 | GH75 | GN75
           | GP75 | GX75 | GZ75
       deriving Show

data Dir812 = GD812 | GF812 | GN812 | GP812
            | GR812 | GV812 | GX812 | GY812 | GZ812 
        deriving Show

data Dir83 = GD83 | GF83 | GI83 | GJ83 | GK83 | GN83 | GP83
           | GR83 | GU83 | GW83 | GX83 | GY83 | GZ83 
        deriving Show

data Dir8456 = GD8456 | GF8456 | GN8456 | GP8456
             | GR8456 | GX8456 | GY8456 | GZ8456 
        deriving Show
{-
===============================================================================
    - Funcion G de Auto Rutinas
===============================================================================     
-}

data AutoRutinas = HeliceHorG22    [DirG223]
                 | HeliceAHorG23   [DirG223]
                 | CircAgujerosG24 [DirG24]
                 | PosRepiteG25    [DirG25]
                 | AlojamientoG26  [DirG267]
                 | SalienciaG27    [DirG267]
                 | EjecutarACG29   [DirG29]
                 | SubprogParamG39 [DirMov]
          deriving Show

data DirG223 = GI223 | GJ223 | GK223 | GN223 
             | GX223 | GY223 | GZ223 
       deriving Show

data DirG24 = GA24 | GB24 | GC24 | GE24 | GH24 | GI24 | GJ24 
            | GL24 | GP24 | GR24 | GW24 | GX24 | GY24 
       deriving Show

data DirG25 = GE25 | GF25 | GH25 | GI25 | GJ25
            | GN25 | GP25 | GX25 | GY25
       deriving Show

data DirG267 = GD267 | GF267 | GH267 | GI267 | GJ267 | GK267 | GL267 | GN267 | GO267
             | GP267 | GQ267 | GR267 | GU267 | GV267 | GW267 | GX267 | GY267 | GZ267     
        deriving Show

data DirG29 = GL29 | GN29
       deriving Show
{- 
===============================================================================
    - Funciones G de Control de Cordenadas
=============================================================================== 
-} 

data Variables = PalG59    [DirMov]
               | GlobalG79 [DirMov]
            deriving Show

{- 
===============================================================================
    - Funciones G de Control de Cordenadas
=============================================================================== 
-}   

data ControlCordenadas = PulgadaG70 
                       | MetricoG71 
                       | EscalaG72        [DirG72]
                       | RotacionG74      [DirG74]
                       | AbsolutoG90      [DirG912]
                       | IncrementalG91   [DirG912]
                       | RefenreTiempoG92 
                       | AvancePTiempoG94 [DirG94]
                       | InibeAcDesG97    [DirG97]
                       | CancelarG92G99   [DirMov]
                deriving Show

data DirG72 = GE72 | GH72 | GL72 | GN72 | GP72 | GU72 | GV72
            | GW72 | GX72 | GY72 | GZ72 
        deriving Show

data DirG74 = GA74 | GC74 | GE74 | GH74 | GI74 | GJ74 | GM74
            | GN74 | GP74 | GR74 | GX74 | GY74 | GZ74
       deriving Show
               
data DirG912 = GA912 | GB912 | GC912 | GI912 | GJ912 | GK912 | GN912
             | GR912 | GU912 | GV912 | GW912 | GX912 | GY912 | GZ912
       deriving Show

data DirG94 =  GC94 | GF94 | GN94
       deriving Show

data DirG97 =  GN97 | GU97 | GV97 | GW97 | GX97 | GY97 | GZ97
       deriving Show

{- 
===============================================================================
    - Funciones G Modos de Ejes 
=============================================================================== 
-}   

data ModosEjes = PlanoXYG17 
               | PlanoXZG18
               | PlanoYZG19
               | DeslEspejoG30 [DirG97]
               | LigaEspejoG31 [DirG97]
        deriving Show
-}    
