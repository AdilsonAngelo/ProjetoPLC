module SokobanConsole where

import Sokoban
import Prelude hiding (Either(..))
import System.IO (stdin, stdout, hSetEcho, hSetBuffering, BufferMode(..))

getInput :: IO Input
getInput = do
    char <- getChar
    case char of
        'w' -> return Up
        'a' -> return Left
        's' -> return Down
        'd' -> return Right
        otherwise -> getInput

sokobanLoop mapa = do
    print mapa
    input <- getInput
    let novoMapa = case modificarMapa mapa input of
            Just nm -> nm
            Nothing -> mapa
    if terminou novoMapa
        then print novoMapa >> print "terminou"
        else sokobanLoop novoMapa
        
main :: IO()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    sokobanLoop $ carregarNivel nivel
