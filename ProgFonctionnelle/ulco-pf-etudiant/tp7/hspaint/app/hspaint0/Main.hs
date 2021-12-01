{-# LANGUAGE OverloadedStrings #-}

import           GI.Cairo.Render
import           GI.Cairo.Render.Connector (renderWithContext)
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import           GI.Gtk.Objects.Widget

handleDraw :: Gtk.DrawingArea -> Render Bool
handleDraw canvas = do
    moveTo 0 0
    lineTo 200 100
    stroke
    return True

handlePress :: Gdk.EventButton -> IO Bool
handlePress b = do
    Gtk.mainQuit
    return True

main :: IO ()
main = do
    _ <- Gtk.init Nothing
    window <- Gtk.windowNew Gtk.WindowTypeToplevel 
    _ <- Gtk.onWidgetDestroy window Gtk.mainQuit

    canvas <- Gtk.drawingAreaNew
    _ <- Gtk.onWidgetDraw canvas $ renderWithContext $ handleDraw canvas
    Gtk.containerAdd window canvas 

    Gtk.widgetSetEvents canvas [ Gdk.EventMaskButtonPressMask ]
    _ <- onWidgetButtonPressEvent canvas handlePress 

    Gtk.widgetShowAll window
    Gtk.main

