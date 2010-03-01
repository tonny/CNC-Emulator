module Main where

import Qtc.Classes.Qccs
import Qtc.Classes.Gui
import Qtc.Gui.Base
import Qtc.Gui.QApplication
import Qtc.Gui.QPushButton

main :: IO Int
main = do 
       qApplication()
       hello <- qPushButton "Welcom CNC-Emulator"
       resize hello(300::Int,400::Int)
       qshow hello ()
       qApplicationExec()
