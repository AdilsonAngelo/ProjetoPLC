module SokobanGUI where

import Sokoban
import Prelude hiding (Either(..))
import Graphics.Rendering.Cairo as Cairo
import Graphics.UI.Gtk
import Control.Concurrent
import Data.Map as M
import Data.Maybe as Maybe
import Control.Monad (when, liftM, forM_, forM)
import Control.Monad.IO.Class (liftIO)
import Data.Text (unpack)
import Data.List (unfoldr)

data Estado = Estado { mapa :: Mapa, niveis :: [Mapa], atual :: Int }

carregarNiveis :: String -> IO [Mapa]
carregarNiveis file = do
    lns <- liftM lines . readFile $ file
    return $ unfoldr consume lns
    where 
        empty = all (' '==)
        consume [] = Nothing
        consume ls = let (a,b) = break empty ls
            in return (carregarNivel $ unlines a, drop 1 b)

estadoVazio = do
  nvs <- carregarNiveis "niveis.txt"
  return $ Estado { mapa = nvs!!0, niveis = nvs, atual = 0 }

proximoNivel estado = do
  e <- takeMVar estado
  if length (niveis e) <= (atual e) + 1
    then print "terminou"
    else putMVar estado $ e{mapa = (niveis e)!!((atual e)+1), atual = (atual e) + 1}

teclado window estado = do
  tryEvent $ do
    keyName <- eventKeyName
    liftIO $
        case unpack keyName of
            "Up" -> do
                checkInput Up
            "Down" -> do
                checkInput Down
            "Left" -> do
                checkInput Left
            "Right" -> do
                checkInput Right
            "r" -> do
              modifyMVar_ estado $ \e -> return e{mapa = (niveis e)!!(atual e)}
              widgetQueueDraw window
            "q" -> do
              mainQuit
  
  where checkInput i = liftIO $ do
          forkIO(atualizarMapa estado i)
          widgetQueueDraw window


atualizarMapa :: MVar Estado -> Input -> IO ()
atualizarMapa estado input = do
  e <- takeMVar estado
  let mp = mapa e
      novoMapa  = case modificarMapa mp input of
                  Nothing -> mp
                  Just x  -> x
  putMVar estado $ e{mapa = novoMapa}
  when (terminou novoMapa) $ proximoNivel estado


draw window estado tiles = liftIO $ do
  cr <- widgetGetDrawWindow window
  mp <- liftM mapa $ readMVar estado
  let (offsetX, offsetY) = (100, 85)
  renderWithDrawable cr $ do
    Cairo.scale 0.4 0.4

    let (maxX, maxY) = fimMapa mp
    let coords = [(x,y)  | x <- [0..maxX], y <- [0..maxY]]
    let lookup_ = Maybe.fromJust . (`M.lookup` tiles)
    let showAt what (x, y) = do
          Cairo.setSourceSurface  (lookup_ what) 
                              (fromIntegral x * offsetX)
                              (fromIntegral y * offsetY) 
          Cairo.paint
    let endpoint c = showAt "Selector" c
    let player c = showAt "batman2" c
    let caixa c = showAt "Wood Block" c
    let parede c = showAt "Stone Block" c
    let vazio c = return ()

    forM coords $ \c ->
      case () of () | ehEndpoint mp c && ehCaixa mp c -> 
                        vazio c >> endpoint c >> caixa c
                    | ehEndpoint mp c && ehPlayer mp c ->
                        vazio c >> endpoint c >> player c
                    | ehEndpoint mp c -> vazio c >> endpoint c
                    | ehCaixa   mp c -> vazio c >> caixa   c
                    | ehPlayer  mp c -> vazio c >> player  c
                    | ehParede    mp c -> parede  c
                    | otherwise         -> vazio c
  return True


carregarTiles strs = do
  let strings = Prelude.map (\s -> "imagens/" ++ s ++ ".png") strs
  surfaces <- mapM Cairo.imageSurfaceCreateFromPNG strings
  return $ M.fromList $ zip strs surfaces

main :: IO ()
main = do
  initGUI

  estado <- newMVar =<< estadoVazio
  tiles <- carregarTiles  ["batman2", "Wood Block", "Selector", "Stone Block"]

  window <- windowNew
  window `on` sizeRequest   $ return (Requisition 800 600)
  window `on` keyPressEvent $ teclado window estado
  window `on` exposeEvent   $ draw window estado tiles

  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
