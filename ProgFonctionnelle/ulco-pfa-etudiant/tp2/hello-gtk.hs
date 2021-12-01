{-# LANGUAGE OverloadedStrings #-}

import qualified GI.Gtk as Gtk

main :: IO ()
main = do

    _ <- Gtk.init Nothing
    window <- Gtk.windowNew Gtk.WindowTypeToplevel
    Gtk.windowSetTitle window "hello-gtk"
    Gtk.windowSetDefaultSize window 320 240
    _ <- Gtk.onWidgetDestroy window Gtk.mainQuit

    label <- Gtk.labelNew (Just "hello gtk")
    Gtk.containerAdd window label

    Gtk.widgetShowAll window
    Gtk.main

