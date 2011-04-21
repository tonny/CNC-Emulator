module MainWindows where

import Qtc.Core.Base
import Qtc.Classes.Gui
import Qtc.Classes.Core
import Qtc.Gui.QMainWindow
import Qtc.Gui.QApplication
import Qtc.Gui.QLabel
import Qtc.Gui.Base
import Qtc.Gui.QAction
import Qtc.Gui.QMenu
import Qtc.Gui.QMenuBar
import Qtc.Gui.QDialog
import Qtc.Gui.QMessageBox
import Qtc.Gui.QKeySequence
import Qtc.Gui.QFileDialog
import Qtc.ClassTypes.Gui
import Qtc.Enums.Gui.QMessageBox
import Qtc.Classes.Qccs
import Qtc.Gui.QWidget
import Qtc.ClassTypes.Core
import Qtc.Gui.QTextEdit
import Qtc.Gui.QVBoxLayout
import Qtc.Gui.QGroupBox
import Qtc.Gui.QGridLayout
import Qtc.Classes.Object
import Qtc.Gui.QPushButton
import Qtc.Gui.QPushButton_h
import Qtc.Gui.QAbstractButton
import Qtc.Gui.QHBoxLayout 
import Qtc.Gui.QDial
import Qtc.Gui.QColor
import Qtc.Enums.Gui.QPalette

--import Qtc.Classes.QsetGeometry

-- precently.c

type MyQMainWindow = QMainWindowSc (CMyQMainWindow)
data CMyQMainWindow = CMyQMainWindow

myQMainWindow :: IO (MyQMainWindow)
myQMainWindow = qSubClass (qMainWindow())

type MyQDialog = QDialogSc (CMyQDialog)
data CMyQDialog = CMyQDialog

myQDialog :: IO (MyQDialog)
myQDialog = qSubClass $ qDialog ()

type MyQPushButton = QPushButtonSc (CMyQPushButton)
data CMyQPushButton = CMyQPushButton

myQPushButton :: String -> IO (MyQPushButton)
myQPushButton t = qSubClass $ qPushButton t

type MyQDial = QDial (CMyQDial)
data CMyQDial =  CMyQDial

myQDial :: IO (MyQDial)
myQDial = qSubClass $ qDial ()


-- Funcion principal que carga todos los mudulos que tienen que ver con 
-- la interfaz grafica
createMainWindow :: QApplication () -> IO (MyQMainWindow)
createMainWindow qapp
  = do
    mw <- myQMainWindow
    d  <- myQDialog
    mainLayout <- qGridLayout ()
    terminal <- qTextEdit ()

    negro <- qColor (0::Int,0::Int,0::Int)
    
    setPlainText terminal "Hola mundo" 
    setWindowTitle mw "CNC Mach-9MP"

    createMenus mw qapp d

    addWidget mainLayout (terminal,0::Int,0::Int,20::Int,20::Int)  
    addSoftkeys mainLayout
    addPalkeys mainLayout
    keywordAbc mainLayout
    addBotones mainLayout
    addNumeros mainLayout

    setColumnStretch mainLayout (1,1)

--    grid <- add_grid
--    kyword <- add_keyword
    
--    hor <- add_horizontal
       
--    addWidget mainLayout grid
 --   addWidget mainLayout kyword
--    addWidget mainLayout hor

    w <- qWidget ()
--    setForegroundRole w eForeground
    setLayout w mainLayout

    setCentralWidget mw w

    resize mw (500::Int,400::Int)
    return mw

addSoftkeys :: QGridLayout () -> IO ()
addSoftkeys qgl
 = do
   f1 <- myQPushButton "F1\n"
   f2 <- myQPushButton "F2\n"
   f3 <- myQPushButton "F3\n"
   f4 <- myQPushButton "F4\n"
   f5 <- myQPushButton "F5\n"
   f6 <- myQPushButton "F6\n"
   f7 <- myQPushButton "F7\n"
   f8 <- myQPushButton "F8\n"
   f9 <- myQPushButton "F9\n"

--   mapM_ (\x -> addWidget qgl (x, )) [f1,f2,f3,f4,f5,f6,f7,f8,f9]

   addWidget qgl (f1, 0::Int, 21::Int,2::Int,1::Int)
   addWidget qgl (f2, 2::Int, 21::Int,2::Int,1::Int)
   addWidget qgl (f3, 4::Int, 21::Int,2::Int,1::Int)
   addWidget qgl (f4, 6::Int, 21::Int,2::Int,1::Int)
   addWidget qgl (f5, 8::Int, 21::Int,2::Int,1::Int)
   addWidget qgl (f6, 10::Int, 21::Int,2::Int,1::Int)
   addWidget qgl (f7, 12::Int, 21::Int,2::Int,1::Int)
   addWidget qgl (f8, 14::Int, 21::Int,2::Int,1::Int)
   addWidget qgl (f9, 16::Int, 21::Int,2::Int,1::Int)
   return ()

addPalkeys :: QGridLayout () -> IO ()
addPalkeys qgl
 = do
   arr  <- myQPushButton "^^"
   f10  <- myQPushButton "F10"
   f11  <- myQPushButton "F11"
   f12  <- myQPushButton "F12"
   f13  <- myQPushButton "F13"
   f14  <- myQPushButton "F14"
   aba  <- myQPushButton "aba"
   exit <- myQPushButton "EXIT"

   addWidget qgl (arr, 21::Int, 0::Int,1::Int,1::Int)  
   addWidget qgl (f10, 21::Int, 1::Int,1::Int,1::Int)
   addWidget qgl (f11, 21::Int, 2::Int,1::Int,1::Int)
   addWidget qgl (f12, 21::Int, 3::Int,1::Int,1::Int)
   addWidget qgl (f13, 21::Int, 4::Int,1::Int,1::Int)
   addWidget qgl (f14, 21::Int, 5::Int,1::Int,1::Int)
   addWidget qgl (aba, 21::Int, 6::Int,1::Int,1::Int)
   addWidget qgl (exit, 21::Int, 8::Int,1::Int,3::Int)

   return ()

-- funcion que crea el teclado del Emuloador
keywordAbc :: QGridLayout () -> IO()
keywordAbc qgl
 = do
   a <- myQPushButton "A\n"
   b <- myQPushButton "B\n"
   c <- myQPushButton "C\n"
   d <- myQPushButton "D\n"
   e <- myQPushButton "E\n"
   f <- myQPushButton "F\n"
   g <- myQPushButton "G\n"

   addWidget qgl (a,0::Int,23::Int,2::Int,1::Int)
   addWidget qgl (b,0::Int,24::Int,2::Int,1::Int)
   addWidget qgl (c,0::Int,25::Int,2::Int,1::Int)
   addWidget qgl (d,0::Int,26::Int,2::Int,1::Int)
   addWidget qgl (e,0::Int,27::Int,2::Int,1::Int)
   addWidget qgl (f,0::Int,28::Int,2::Int,1::Int)
   addWidget qgl (g,0::Int,29::Int,2::Int,1::Int)

   h <- myQPushButton "H\n"
   i <- myQPushButton "I\n"
   j <- myQPushButton "J\n"
   k <- myQPushButton "K\n"
   l <- myQPushButton "L\n"
   m <- myQPushButton "M\n"
   n <- myQPushButton "N\n"
   addWidget qgl (h,2::Int,23::Int,2::Int,1::Int)
   addWidget qgl (i,2::Int,24::Int,2::Int,1::Int)
   addWidget qgl (j,2::Int,25::Int,2::Int,1::Int)
   addWidget qgl (k,2::Int,26::Int,2::Int,1::Int)
   addWidget qgl (l,2::Int,27::Int,2::Int,1::Int)
   addWidget qgl (m,2::Int,28::Int,2::Int,1::Int)
   addWidget qgl (n,2::Int,29::Int,2::Int,1::Int)

   o <- myQPushButton "O\n"
   p <- myQPushButton "P\n"
   q <- myQPushButton "Q\n"
   r <- myQPushButton "R\n"
   s <- myQPushButton "S\n"
   t <- myQPushButton "T\n"
   u <- myQPushButton "U\n"
   addWidget qgl (o,4::Int,23::Int,2::Int,1::Int)
   addWidget qgl (p,4::Int,24::Int,2::Int,1::Int)
   addWidget qgl (q,4::Int,25::Int,2::Int,1::Int)
   addWidget qgl (r,4::Int,26::Int,2::Int,1::Int)
   addWidget qgl (s,4::Int,27::Int,2::Int,1::Int)
   addWidget qgl (t,4::Int,28::Int,2::Int,1::Int)
   addWidget qgl (u,4::Int,29::Int,2::Int,1::Int)

   v <- myQPushButton "V\n"
   w <- myQPushButton "W\n"
   x <- myQPushButton "X\n"
   y <- myQPushButton "Y\n"
   z <- myQPushButton "Z\n"
   eob <- myQPushButton "EOB\n"
   addWidget qgl (v,6::Int,23::Int,2::Int,1::Int)
   addWidget qgl (w,6::Int,24::Int,2::Int,1::Int)
   addWidget qgl (x,6::Int,25::Int,2::Int,1::Int)
   addWidget qgl (y,6::Int,26::Int,2::Int,1::Int)
   addWidget qgl (z,6::Int,27::Int,2::Int,1::Int)
   addWidget qgl (eob,6::Int,28::Int,2::Int,2::Int)

   ins <- myQPushButton "INS\n"
   del <- myQPushButton "DEL\n"
   err <- myQPushButton "ERROR\nMSGS"
   hel <- myQPushButton "HELP\n"
   zoomin <- myQPushButton "ZOOM\nIN"
   zoomout <- myQPushButton "ZOOM\nOUT"
   space <- myQPushButton "SPACE\n"
   addWidget qgl (ins    ,8::Int,23::Int,2::Int,1::Int)
   addWidget qgl (del    ,8::Int,24::Int,2::Int,1::Int)
   addWidget qgl (err    ,8::Int,25::Int,2::Int,1::Int)
   addWidget qgl (hel    ,8::Int,26::Int,2::Int,1::Int)
   addWidget qgl (zoomin ,8::Int,27::Int,2::Int,1::Int)
   addWidget qgl (zoomout,8::Int,28::Int,2::Int,1::Int)
   addWidget qgl (space  ,8::Int,29::Int,2::Int,1::Int)

   return ()

-- Funcion que agrega los numeros al teclado
addNumeros :: QGridLayout () -> IO ()
addNumeros qgl 
 = do
   siete <- myQPushButton "@\n7"
   ocho  <- myQPushButton "(\n8"
   nueve <- myQPushButton ")\n9"
   slash <- myQPushButton " /\n/"
   arrib <- myQPushButton "^\n^"

   addWidget qgl (siete ,10::Int,23::Int,2::Int,1::Int)
   addWidget qgl (ocho  ,10::Int,24::Int,2::Int,1::Int)
   addWidget qgl (nueve ,10::Int,25::Int,2::Int,1::Int)
   addWidget qgl (slash ,10::Int,26::Int,2::Int,1::Int)
   addWidget qgl (arrib ,10::Int,28::Int,2::Int,1::Int)

   cuatro <- myQPushButton "$\n4"
   cinco  <- myQPushButton "%\n5"
   seis   <- myQPushButton "\&\n6"
   multi  <- myQPushButton "X\n" 
   izqui  <- myQPushButton "<<<<<"
   dere   <- myQPushButton ">>>>>"

   addWidget qgl (cuatro ,12::Int,23::Int,2::Int,1::Int)
   addWidget qgl (cinco  ,12::Int,24::Int,2::Int,1::Int)
   addWidget qgl (seis   ,12::Int,25::Int,2::Int,1::Int)
   addWidget qgl (multi  ,12::Int,26::Int,2::Int,1::Int)
   addWidget qgl (izqui  ,12::Int,27::Int,2::Int,1::Int)
   addWidget qgl (dere   ,12::Int,29::Int,2::Int,1::Int)

   uno   <- myQPushButton "*\n1"
   dos   <- myQPushButton "\"\n2"
   tres  <- myQPushButton "!\n3"
   menos <- myQPushButton " _\n/"
   abajo <- myQPushButton "aba\n^"

   addWidget qgl (uno   ,14::Int,23::Int,2::Int,1::Int)
   addWidget qgl (dos   ,14::Int,24::Int,2::Int,1::Int)
   addWidget qgl (tres  ,14::Int,25::Int,2::Int,1::Int)
   addWidget qgl (menos ,14::Int,26::Int,2::Int,1::Int)
   addWidget qgl (abajo ,14::Int,28::Int,2::Int,1::Int)

   coma  <- myQPushButton "<\n,"
   cero  <- myQPushButton ";\n0"
   punto <- myQPushButton ">\n."
   mas   <- myQPushButton "+\n/"
   igual <- myQPushButton "=\n^"
   shift <- myQPushButton "SHIFT\n"
   enter <- myQPushButton "ENTER\n"

   addWidget qgl (coma  ,16::Int,23::Int,2::Int,1::Int)
   addWidget qgl (cero  ,16::Int,24::Int,2::Int,1::Int)
   addWidget qgl (punto ,16::Int,25::Int,2::Int,1::Int)
   addWidget qgl (mas   ,16::Int,26::Int,2::Int,1::Int)
   addWidget qgl (igual ,16::Int,27::Int,2::Int,1::Int)
   addWidget qgl (shift ,16::Int,28::Int,2::Int,1::Int)
   addWidget qgl (enter ,16::Int,29::Int,2::Int,1::Int)

   return ()


-- Botones redondos
addBotones :: QGridLayout () -> IO()
addBotones qgl
  = do
--    horGroup <- qGroupBox ""
--    horLayout <- qHBoxLayout ()
    emergencia <- myQDial
    volElectro <- myQDial
    variacion1 <- myQDial
    variacion2 <- myQDial

    romi <- qLabel "ROMI MACH 9MP\n" 

    stop  <- myQPushButton "CYCLE\nSTOP"
    blk   <- myQPushButton "BLK\nBLK"
    start <- myQPushButton "CYCLE\nSTART"

    addWidget qgl (emergencia, 22::Int,0::Int,4::Int,6::Int)
    addWidget qgl (volElectro, 22::Int,3::Int,4::Int,6::Int)
    addWidget qgl (variacion1, 22::Int,6::Int,4::Int,6::Int)
    addWidget qgl (variacion2, 22::Int,10::Int,4::Int,13::Int)

    addWidget qgl (romi  , 22::Int,25::Int,2::Int,3::Int)
    addWidget qgl (stop  , 24::Int,25::Int,2::Int,1::Int)
    addWidget qgl (blk   , 24::Int,27::Int,2::Int,1::Int)
    addWidget qgl (start , 24::Int,29::Int,2::Int,1::Int)

--    setLayout horGroup horLayout
--    return $ objectCast horGroup 
    return ()

add_keyword :: IO (QWidget a)
add_keyword
 = do
   gridGroupBox <- qGroupBox ""
   gridLayout <- qGridLayout ()

   keywordAbc gridLayout
   setLayout gridGroupBox gridLayout

   return $ objectCast gridGroupBox




createMenus :: MyQMainWindow -> QApplication() -> MyQDialog -> IO ()
createMenus mw qapp d
   = do
     openAct  <- cfa "&Abrir" "Ctrl+O" True
     saveAct  <- cfa "&Grabar" "Ctrl+G" False
     printAct <- cfa "&Imprimir" "Ctrl+P" False
     exitAct  <- cfa "&Salir" "Ctrl+Q" True
     fileMenu <- qMenu ("&Archivo", mw)

     aam fileMenu True [openAct, saveAct, printAct, exitAct]

     aboutAct   <- qAction("&Acerca de",mw)
     aboutQtAct <- qAction ("About &Qt",mw)
     helpMenu   <- qMenu("&Ayuda",mw)
     aam helpMenu False [aboutAct, aboutQtAct]

     mb <- menuBar mw ()
     mapM_ (addMenu mb) [fileMenu,helpMenu]
     mapM_ (\(a,as,sf) -> connectSlot a "triggered()" mw as sf)
           [
--           (openAct,"open()",(myOpen l sa mw)),
--            (saveAct,"saveAs()",(saveAs saveAct iv)),
--            (printAct,"print()",myPrint),
            (aboutAct,"about()",myAbout)
           ]

     connectSlot exitAct "triggered()" mw "close()" ()
     connectSlot aboutQtAct "triggered()" qapp "aboutQt()" ()

  where
    cfa txt key abl = ca txt key abl False
    ca txt key abl chk
     = do
       na <- qAction(txt,mw)
       setShortcut na =<< qKeySequence key
       if (not abl) then setEnabled na False else return ()
       if (chk) then setCheckable na True else return ()
       return na

    aam _ _ [] = return ()
    aam mnu True (a:[])
     = do
       addSeparator mnu ()
       addAction mnu a

    aam mnu ps (a:as)
     = do
       addAction mnu a
       aam mnu ps as

{-
myOpen :: QLabel () -> QScrollArea () -> MyQMainWindow -> IO()
myOpen il sa mw
  = do
    fileName <- qFileDialogGetOpenFileName (mw,"Open File")
    if (fileName /= "")
     then
      do
      inp <- readFile fileName
      case inp of
       (Just a) -> return ()
       _        -> do 
                   rb <- qMessageBoxInformation (mw,"Archivo",("No podemos cargar " ++ fileName ++ "."))::IO QMessageBoxStandardButton
                   return ()
     else
        return ()
    returnGC
-}

myPrint :: MyQMainWindow -> IO ()
myPrint mw
 = do
   return ()


myAbout :: MyQMainWindow -> IO ()
myAbout mw
 = do
   let tex = "<p><h2>Emulador CNC-Mach-9MP </h2><br>Desarrollado por Antonio"++
             "Mamani<br> Mayor Información visitar"++
                "<a href=http://cnc-emulator.wordpress.com> Pagina Oficial </a></p>"
   about <- qMessageBoxAbout(mw,"CNC Mach-9MP",tex)
   return ()

