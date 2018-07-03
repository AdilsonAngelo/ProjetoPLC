module Sokoban (modificarMapa,
                terminou,
                carregarNivel,
                Mapa,
                Input(..)) where

import Prelude hiding (Either(..))
import Data.List (sort, delete)
import Control.Monad (forM_)
import System.IO (stdin, stdout, hSetEcho, hSetBuffering, BufferMode(..))

type Coord = (Int, Int)

data Input = Up | Down | Left | Right deriving (Show, Eq, Ord)

data Mapa = Mapa {
        paredes,
        caixas,
        endpoints :: [Coord],
        fim,
        player :: Coord,
        passos :: Int
    } deriving (Show)

mapaVazio :: Mapa
mapaVazio = Mapa {
        paredes = [],
        caixas = [],
        endpoints = [],
        fim = (0, 0),
        player = (0, 0),
        passos = 0
    }

somaCoord :: Coord -> Input -> Coord
somaCoord (x, y) input
    | input == Up = (x, y-1)
    | input == Down = (x, y+1)
    | input == Left = (x-1, y)
    | input == Right = (x+1, y)

nivel = unlines [
    "######",
    "#  @ #",
    "#o # #",
    "#    #",
    "#.   #",
    "######"
    ]

carregarNivel :: String -> Mapa
carregarNivel str = foldl consume (mapaVazio{fim = (maxX, maxY)}) elems
    where 
        lns = lines str
        elems = concat $ zipWith zip coords lns
        coords = [[(x, y) | x <- [0..]] | y <- [0..]]
        maxX = maximum . map (fst . fst) $ elems
        maxY = maximum . map (snd . fst) $ elems
        consume m (c, e) =
            case e of
                '@' -> m{player = c}
                'o' -> m{caixas = c:caixas m}
                '#' -> m{paredes = c:paredes m}
                '.' -> m{endpoints = c:endpoints m}
                ' ' -> m
                otherwise -> error (show e ++ ": char não reconhecido")

getInput :: IO Input
getInput = do
    char <- getChar
    case char of
        'w' -> return Up
        'a' -> return Left
        's' -> return Down
        'd' -> return Right
        otherwise -> getInput

ehParede :: Mapa -> Coord -> Bool
ehParede mapa coord = elem coord (paredes mapa)

ehCaixa :: Mapa -> Coord -> Bool
ehCaixa mapa coord = elem coord (caixas mapa)

ehEndpoint :: Mapa -> Coord -> Bool
ehEndpoint mapa coord = elem coord (endpoints mapa)

ehPlayer :: Mapa -> Coord -> Bool
ehPlayer mapa coord = player mapa == coord

ehValido :: Mapa -> Input -> Bool
ehValido mapa input
        | ehParede mapa novaPos = False
        | (ehParede mapa novaPos' || ehCaixa mapa novaPos') && ehCaixa mapa novaPos = False
        | otherwise = True
    where
        pos = player mapa
        novaPos = somaCoord pos input
        novaPos' = somaCoord novaPos input

modificarMapa :: Mapa -> Input -> Mapa
modificarMapa mapa input
    | ehCaixa mapa novaPos = moverCaixa novoMapa novaPos novaPos'
    | otherwise = novoMapa
    where
        pos = player mapa
        novaPos = somaCoord pos input
        novaPos' = somaCoord novaPos input
        novoMapa = mapa{player = novaPos, passos = passos mapa + 1}
        moverCaixa m p p' = m{caixas = p' : delete p (caixas m)}

mostrarMapa :: Mapa -> IO ()
mostrarMapa mapa = putStrLn . unlines . map (map aux) $ coords
    where
        aux c 
            | ehCaixa mapa c && ehEndpoint mapa c = '*'
            | ehPlayer mapa c && ehEndpoint mapa c = '+'
            | ehParede mapa c = '#'
            | ehPlayer mapa c = '@'
            | ehCaixa mapa c = 'o'
            | ehEndpoint mapa c = '.'
            | otherwise = ' '
        (maxX, maxY) = fim mapa
        coords = [[(x, y) | x <- [0..maxX]] | y <- [0..maxY]]

terminou :: Mapa -> Bool
terminou mapa = sort (caixas mapa) == sort (endpoints mapa)

main :: IO()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    sokobanLoop $ carregarNivel nivel

sokobanLoop mapa = do
    mostrarMapa mapa
    input <- getInput
    let novoMapa =  if ehValido mapa input
                    then modificarMapa mapa input
                    else mapa
    if terminou novoMapa
        then mostrarMapa novoMapa >> print "terminou"
        else sokobanLoop novoMapa