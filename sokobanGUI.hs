module SokobanGUI where

import Sokoban
import Prelude hiding (Either(..))
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Control.Concurrent.MVar

{-
sokobanLoop mapa = do
    print mapa
    input <- getInput
    let novoMapa = case modificarMapa mapa input of
            Just nm -> nm
            Nothing -> mapa
    if terminou novoMapa
        then print novoMapa >> print "terminou"
        else sokobanLoop novoMapa
-}

main :: IO ()
main = do
    initGUI