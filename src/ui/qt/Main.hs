module Main where

-- ============================================================================
-- Author      :: to_On1
-- Email       :: antonio.mq@gmail.com
-- Description :: Manager CNC Mach-9MP machine control
-- ============================================================================

import Qtc.ClassTypes.Gui
import Qtc.Classes.Object
import Qtc.Classes.Gui
import Qtc.Classes.Core
import Qtc.Gui.QApplication
import Qtc.Core.Base
import Qtc.Gui.Base
import MainWindows

main :: IO()
  = do
    app <- qApplication()

    mainWindow <- createMainWindow app

    qshow mainWindow ()
    ok <- qApplicationExec ()
    returnGC
