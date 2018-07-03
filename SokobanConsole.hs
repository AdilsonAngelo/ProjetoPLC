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
        'r' -> return Reset
        'q' -> return Quit
        otherwise -> getInput

sokobanLoop mapa = do
    print mapa
    input <- getInput
    if input == Quit
        then return()
        else do
            let novoMapa = case modificarMapa mapa input of
                        Just nm -> nm
                        Nothing -> mapa
            if terminou novoMapa
                then print novoMapa >> print "terminou"
                else sokobanLoop novoMapa

iterarNiveis ::[String] -> IO ()
iterarNiveis [] = return ()
iterarNiveis (x:xs) = do
        sokobanLoop $ carregarNivel x
        iterarNiveis xs

main :: IO()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    iterarNiveis niveis