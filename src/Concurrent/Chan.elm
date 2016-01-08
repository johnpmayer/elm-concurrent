
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
import Concurrent.Pigeonhole exposing (Pigeonhole, modify, newEmpty, new, put, read, take)

{-| Chan -}
type Chan a = Chan { 
  readVar  : Pigeonhole (Stream a), 
  writeVar : Pigeonhole (Stream a) }

type alias Stream a = Pigeonhole (ChItem a)

type ChItem a = ChItem a (Stream a)

{-| Create a new channel -}
newChan : Task x (Chan a)
newChan = 
  newEmpty  `andThen` \hole ->
  new hole  `andThen` \readVar ->
  new hole  `andThen` \writeVar ->
  succeed (Chan { readVar = readVar, writeVar = writeVar})
  
{-| Write a value to the channel -}
writeChan : Chan a -> a -> Task x () 
writeChan (Chan {writeVar}) val = 
  newEmpty `andThen` \new_hole ->
  modify writeVar <| \old_hole -> 
    put old_hole (ChItem val new_hole) `andThen_` succeed (new_hole, ())

{-| Read the next value from the channel -}
readChan : Chan a -> Task x a
readChan (Chan {readVar}) =
  modify readVar <| \read_end ->
    read read_end `andThen` \(ChItem val new_read_end) ->
    succeed (new_read_end, val)

{-| Duplicate a chan. The duplicate begins empty, 
but consecutive writes are available by both, creating
a 'broadcast' effect -}
dupChan : Chan a -> Task x (Chan a)
dupChan (Chan {writeVar}) =
  read writeVar `andThen` \hole ->
  new hole `andThen` \newReadVar ->
  succeed (Chan { readVar = newReadVar, writeVar = writeVar })
