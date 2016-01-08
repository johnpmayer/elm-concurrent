
module Concurrent.Chan
  ( Chan
  , new
  , read, write
  ) where

{-| 

Unbounded Channels 

@docs Chan

@docs new, read, write

-}

import Task exposing (Task, andThen, succeed)
import TaskUtils exposing (andThen_)
import Concurrent.Pigeonhole as PH
import Concurrent.Pigeonhole exposing (Pigeonhole, modify, newEmpty, put, read, take)

{-| Chan -}
type Chan a = Chan { 
  readVar  : Pigeonhole (Stream a), 
  writeVar : Pigeonhole (Stream a) }

type alias Stream a = Pigeonhole (ChItem a)

type ChItem a = ChItem a (Stream a)

{-| Create a new channel -}
new : Task x (Chan a)
new = 
  PH.newEmpty `andThen` \hole ->
  PH.new hole `andThen` \readVar ->
  PH.new hole `andThen` \writeVar ->
  succeed (Chan { readVar = readVar, writeVar = writeVar})
  
{-| Write a value to the channel -}
write : Chan a -> a -> Task x () 
write (Chan {writeVar}) val = 
  newEmpty `andThen` \new_hole ->
  modify writeVar <| \old_hole -> 
    put old_hole (ChItem val new_hole) `andThen_` succeed (new_hole, ())

{-| Read the next value from the channel -}
read : Chan a -> Task x a
read (Chan {readVar}) =
  modify readVar <| \read_end ->
    PH.read read_end `andThen` \(ChItem val new_read_end) ->
    succeed (new_read_end, val)

{-| Duplicate a chan. The duplicate begins empty, 
but consecutive writes are available by both, creating
a 'broadcast' effect -}
dupChan : Chan a -> Task x (Chan a)
dupChan (Chan {writeVar}) =
  PH.read writeVar `andThen` \hole ->
  PH.new hole      `andThen` \newReadVar ->
  succeed (Chan { readVar = newReadVar, writeVar = writeVar })
