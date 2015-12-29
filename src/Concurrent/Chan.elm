
module Concurrent.Chan
  ( Chan
  ) where

{-| 

Unbounded Channels 

@docs Chan

-}

import Task exposing (Task, andThen, succeed)
import Concurrent.MVar exposing (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)

{-| Chan -}
type Chan a = Chan { 
  readVar : MVar (Stream a), 
  writeVar : MVar (Stream a) }

type alias Stream a = MVar (ChItem a)

type ChItem a = ChItem a (Stream a)

newChan : Task x (Chan a)
newChan = 
  newEmptyMVar  `andThen` (\hole ->
  newMVar hole  `andThen` (\readVar ->
  newMVar hole  `andThen` (\writeVar ->
  succeed (Chan { readVar = readVar, writeVar = writeVar}))))
  
writeChan : Chan a -> a -> Task x () 
writeChan (Chan {writeVar}) val = 
  newEmptyMVar                            `andThen` (\new_hole ->
  takeMVar writeVar                       `andThen` (\old_hole ->
  putMVar old_hole (ChItem val new_hole)  `andThen` (\_ ->
  putMVar writeVar new_hole)))
  