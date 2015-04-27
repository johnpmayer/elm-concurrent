
module Concurrent 
  ( MVar
  , newEmptyMVar, newMVar
  , takeMVar, putMVar
  ) where

{-| Concurrency primitives for working with Tasks

# Mutable variables

@docs MVar

Mutable variables either have a value or do not have a value. They are directly modelled after Haskell's MVar.

https://hackage.haskell.org/package/base/docs/Control-Concurrent-MVar.html

# Creating a mutable variable

@docs newEmptyMVar, newMVar

# Atomic actions

@docs takeMVar, putMVar

-}
  
import Task exposing (Task, andThen, succeed)
  
type MVar a = OpaqueMVar

newEmptyMVar : Task x (MVar a)
newEmptyMVar = Native.Concurrent.newEmptyMVar

newMVar : a -> Task x (MVar a)
newMVar value = 
  newEmptyMVar        `andThen` (\mvar ->
  putMVar mvar value  `andThen` (\_ ->
  succeed mvar))

takeMVar : MVar a -> Task x a
takeMVar = Native.Concurrent.takeMVar
  
putMVar : MVar a -> a -> Task x ()
putMVar = Native.Concurrent.putMVar