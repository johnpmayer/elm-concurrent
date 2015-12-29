
module Concurrent.Chan
  ( Chan
  , newChan
  , readChan, writeChan
  ) where

{-| 

Unbounded Channels 

@docs Chan

@docs newChan, readChan, writeChan

-}

import Task exposing (Task, andThen, succeed)
import TaskUtils exposing (andThen_)
import Concurrent.MVar exposing (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)

{-| Chan -}
type Chan a = Chan { 
  readVar : MVar (Stream a), 
  writeVar : MVar (Stream a) }

type alias Stream a = MVar (ChItem a)

type ChItem a = ChItem a (Stream a)

{-| Create a new channel -}
newChan : Task x (Chan a)
newChan = 
  newEmptyMVar  `andThen` (\hole ->
  newMVar hole  `andThen` (\readVar ->
  newMVar hole  `andThen` (\writeVar ->
  succeed (Chan { readVar = readVar, writeVar = writeVar}))))
  
{-| Write a value to the channel -}
writeChan : Chan a -> a -> Task x () 
writeChan (Chan {writeVar}) val = 
  newEmptyMVar                            `andThen` (\new_hole ->
  modifyMVar writeVar <| (\old_hole -> 
    putMVar old_hole (ChItem val new_hole) `andThen_` succeed (new_hole, ())))

{-| Read the next value from the channel -}
readChan : Chan a -> Task x a
readChan (Chan {readVar}) =
  modifyMVar readVar <| (\read_end ->
    readMVar read_end `andThen` (\(ChItem val new_read_end) ->
    succeed (new_read_end, val)))


