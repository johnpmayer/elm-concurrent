
module Concurrent.MVar
  ( MVar
  , newEmptyMVar, newMVar
  , takeMVar, putMVar
  , readMVar, modifyMVar
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

# Compound actions

@docs readMVar, modifyMVar

-}

import Task exposing (Task, andThen, fail, succeed, toResult)
import TaskUtils exposing (andThen_)
import Native.Concurrent.MVar
import Queue -- Required by Native Code

{-| Type should not be instantiated, handled in Native Code -}
type MVar a = OpaqueMVar

{-| Create an MVar "empty", without an initial value -}
newEmptyMVar : Task x (MVar a)
newEmptyMVar = Native.Concurrent.MVar.newEmptyMVar

{-| Create an MVar holding the provided initial value -}
newMVar : a -> Task x (MVar a)
newMVar value = 
  newEmptyMVar        `andThen` \mvar ->
  putMVar mvar value  `andThen_` 
  succeed mvar

{-| Take the value held by the MVar, blocking if empty. Multiple blocking consumers are woken in FIFO order. -}
takeMVar : MVar a -> Task x a
takeMVar = Native.Concurrent.MVar.takeMVar

{-| Put the provided value in the MVar, blocking if occupied. Multiple blocking producers are woken in FIFO order. -}  
putMVar : MVar a -> a -> Task x ()
putMVar = Native.Concurrent.MVar.putMVar

{-| Read the MVar -}
readMVar : MVar a -> Task x a
readMVar var =
  takeMVar var    `andThen` \val ->
  putMVar var val `andThen_`
  succeed val

{-| Modify the MVar, replacing if the replacement task fails -}
modifyMVar : MVar a -> (a -> Task x (a,b)) -> Task x b
modifyMVar var task =
  takeMVar var        `andThen` \val ->
  toResult (task val) `andThen` \result ->
    case result of
      Ok (new_val, ret_val) -> putMVar var new_val `andThen_` succeed ret_val
      Err error -> putMVar var val `andThen_` fail error
