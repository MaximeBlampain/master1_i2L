{-# LANGUAGE OverloadedStrings #-}

import qualified GI.Gtk as Gtk
import Data.Text

main :: IO ()
main = do
    _ <- Gtk.init Nothing
    window <- Gtk.windowNew Gtk.WindowTypeToplevel 
    Gtk.windowSetDefaultSize window 300 100
    Gtk.windowSetTitle window "Hello World!"
    _ <- Gtk.onWidgetDestroy window Gtk.mainQuit

    label <- Gtk.labelNew (Just "Hello World!")
    Gtk.containerAdd window label

    Gtk.widgetShowAll window
    Gtk.main

