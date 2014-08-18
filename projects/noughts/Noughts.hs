{--------------------------------------------------------------------------------------------------
                                        Noughts and Crosses                                        
--------------------------------------------------------------------------------------------------}

module Noughts where

size :: Int
size =  3

type Row = [Player]

type Board = [Row]

data Player = Nought | Blank | Cross
              deriving (Ord, Eq, Show)

human :: Player
human = Cross

computer :: Player
computer = opponentOf human

emptyBoard :: Board
emptyBoard = replicate size $ replicate size Blank

opponentOf :: Player -> Player
opponentOf Nought = Cross
opponentOf Cross  = Nought
opponentOf _      = undefined

isBlank :: Player -> Bool
isBlank = (==) Blank

ended :: Board -> Bool
ended b = undefined

search :: Player -> Board -> Board
search p b = undefined

{--------------------------------------------------------------------------------------------------
                                                Fin                                                
--------------------------------------------------------------------------------------------------} 

