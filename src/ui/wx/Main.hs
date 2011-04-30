module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore
import Mensajes
import Ventana 

main :: IO ()
main = start gui

gui :: IO()
gui =
 do 
   fram <- frameFixed [ text := "CNC-Emulator" ]
   pMain <- panel fram [ bgcolor := black, clientSize := sz 900 800]
   
   crearInterfaz fram pMain 
   
   ------ Definiendo la barra de menus y estado del simulador ------------------
   -----------------------------------------------------------------------------
   mArchivo <- menuPane          [text := menuArchivo]
   mNuevo   <- menuItem mArchivo [text := abrirArchivo]
   mSalir   <- menuItem mArchivo [text := salirArchivo]

   mOpciones <- menuPane [text := menuOpciones]
 
   mAyuda  <- menuPane        [text := menuAyuda]
   mAcerca <- menuItem mAyuda [text := acercaAyuda]

   ---------------------- Definiendo propiedades --------------------------------
   ------------------------------------------------------------------------------ 
   set mSalir [ on command := close fram ]

   set fram [ menuBar   := [mArchivo, mOpciones, mAyuda] ]
   repaint fram 
   return ()


