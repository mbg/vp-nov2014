{--------------------------------------------------------------------------------------------------
                                        Noughts and Crosses                                        
--------------------------------------------------------------------------------------------------}

module Main where

{----------------------------------------------------------------------}
{-- Module Imports                                                    -}
{----------------------------------------------------------------------}

import System.Exit

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Noughts as N

{----------------------------------------------------------------------}
{-- Constants                                                         -}
{----------------------------------------------------------------------}

type GlossState = (Player, Board)
type V2D a      = (a,a)

width :: Float
width = 800.0

height :: Float
height = 600.0

cellW :: Float
cellW = width / fromIntegral size

cellH :: Float
cellH = height / fromIntegral size

{----------------------------------------------------------------------}
{-- Grid                                                              -}
{----------------------------------------------------------------------}

verticalLine :: Int -> Picture
verticalLine n = line [(i * cellW, 0.0), (i * cellW, height)]
    where
        i = fromIntegral n

horizontalLine :: Int -> Picture
horizontalLine n = line [(0.0, i * cellH), (width,i * cellH)]
    where
        i = fromIntegral n

makeLines :: Int -> Picture
makeLines n = pictures [verticalLine n, horizontalLine n]

grid :: Picture
grid = pictures [makeLines n | n <- [1..size-1]]

{----------------------------------------------------------------------}
{-- Rows                                                              -}
{----------------------------------------------------------------------}

radius :: Float
radius = min cellW cellH / 2.0

margin :: Float
margin = (radius / 100.0) * 10.0

cross :: Picture
cross = pictures [
    line [(0.0,0.0),(cellW,cellH)], 
    line [(cellW,0.0),(0.0,cellH)]]

drawCell :: Player -> Picture
drawCell N.Blank  = blank
drawCell N.Cross  = cross
drawCell N.Nought = translate (cellW/2) (cellH/2) $ circle (radius - margin)

{----------------------------------------------------------------------}
{-- Boards                                                            -}
{----------------------------------------------------------------------}

row :: Row -> Picture
row cs = pictures [translate (n * cellW) 0.0 (drawCell c) | (c,n) <- zip cs [0..]]

drawBoard :: GlossState -> IO Picture
drawBoard (_,b) = return $ translate ((width/2) * (-1)) ((height/2) * (-1)) pic
    where
        cells = pictures [translate 0.0 (n * cellH) r | (r,n) <- zip (map row $ reverse b) [0..]]
        pic   = pictures [grid, cells]

{----------------------------------------------------------------------}
{-- Input                                                             -}
{----------------------------------------------------------------------}

normaliseCoords :: V2D Float -> V2D Float
normaliseCoords (x,y) = (x + width / 2.0, (y - height / 2.0) * (-1.0))

coordsToCell :: V2D Float -> V2D Int
coordsToCell (x,y) = (floor y `div` floor cellH, floor x `div` floor cellW)

inBounds :: V2D Int -> Bool
inBounds (r,c) = r >= 0 && c >= 0 && r < size && c < size

canMove :: Board -> V2D Int -> Bool
canMove b (r,c) = inBounds (r,c) && isBlank ((b !! r) !! c)

updateBoard :: Board -> V2D Int -> N.Board
updateBoard b (r,c) = init bhs ++ [init rhs ++ [human] ++ rts] ++ bts
    where
        (bhs, bts) = splitAt (r+1) b
        (rhs, rts) = splitAt (c+1) (last bhs)

handleInput :: Event -> GlossState -> IO GlossState
handleInput (EventKey (MouseButton LeftButton) Down _ p) (t,s) 
 | t == human = let pos = coordsToCell $ normaliseCoords p in
        if canMove s pos 
        then checkEnded computer $ updateBoard s pos
        else return (human, s)
handleInput _ s = return s

{----------------------------------------------------------------------}
{-- Game                                                              -}
{----------------------------------------------------------------------}

checkEnded :: Player -> Board -> IO GlossState
checkEnded t b 
 | ended b   = exitSuccess
 | otherwise = return (t,b)

gameStep :: Float -> GlossState -> IO GlossState
gameStep _ (t,b) 
 | t == computer = checkEnded human $ search computer b
 | otherwise     = return (human, b)

displaySettings :: Display
displaySettings = InWindow
    "Noughts and Crosses"
    (floor width, floor height)
    (0, 0)

main :: IO ()
main = playIO displaySettings white 1 (human, emptyBoard) drawBoard handleInput gameStep

{--------------------------------------------------------------------------------------------------
                                                Fin                                                
--------------------------------------------------------------------------------------------------} 
