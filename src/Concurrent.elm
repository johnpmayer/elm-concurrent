
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
import Native.Concurrent
import Queue -- Required by Native Code

{-| Type should not be instantiated, handled in Native Code -}
type MVar a = OpaqueMVar

{-| Create an MVar "empty", without an initial value -}
newEmptyMVar : Task x (MVar a)
newEmptyMVar = Native.Concurrent.newEmptyMVar ()

{-| Create an MVar holding the provided initial value -}
newMVar : a -> Task x (MVar a)
newMVar value = 
  newEmptyMVar        `andThen` (\mvar ->
  putMVar mvar value  `andThen` (\_ ->
  succeed mvar))

{-| Take the value held by the MVar, blocking if empty. Multiple blocking consumers are woken in FIFO order. -}
takeMVar : MVar a -> Task x a
takeMVar = Native.Concurrent.takeMVar

{-| Put the provided value in the MVar, blocking if occupied. Multiple blocking producers are woken in FIFO order. -}  
putMVar : MVar a -> a -> Task x ()
putMVar = Native.Concurrent.putMVar