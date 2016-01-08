
module Concurrent.Pigeonhole
  ( Pigeonhole
  , newEmpty, new
  , take, put
  , read, with
  , modify_, modify
  ) where

{-| Concurrency primitives for working with Tasks

# Mutable variables

@docs Pigeonhole

Pigeonholes either have a value or do not 
have a value. They are directly modelled after 
Haskell's MVar.

https://hackage.haskell.org/package/base/docs/Control-Concurrent-MVar.html

# Creating a new hole

@docs newEmpty, new

# Atomic actions

The operations take and put have FIFO characteristics, which is useful for
fairness.

@docs take, put

# Compound actions

These operations are not atomic and their use
may introduce race conditions

@docs read, with, modify_, modify

-}

import Task exposing (Task, andThen, fail, succeed, toResult)
import TaskUtils exposing (andThen_, bracket, bracketOnError)
import Native.Concurrent.Pigeonhole
import Queue -- Required by Native Code

{-| Type should not be instantiated, handled in Native Code -}
type Pigeonhole a = OpaquePigeonhole

{-| Create an Pigeonhole "empty", without an initial value -}
newEmpty : Task x (Pigeonhole a)
newEmpty = Native.Concurrent.Pigeonhole.newEmptyPigeonhole

{-| Create an Pigeonhole holding the provided initial value -}
new : a -> Task x (Pigeonhole a)
new value = 
  newEmpty        `andThen` \hole ->
  put hole value  `andThen_` 
  succeed hole

{-| Take the value held by the Pigeonhole, blocking if empty. Multiple blocking consumers are woken in FIFO order. -}
take : Pigeonhole a -> Task x a
take = Native.Concurrent.Pigeonhole.takePigeonhole

{-| Put the provided value in the Pigeonhole, blocking if occupied. Multiple blocking producers are woken in FIFO order. -}  
put : Pigeonhole a -> a -> Task x ()
put = Native.Concurrent.Pigeonhole.putPigeonhole

{-| Read the Pigeonhole -}
read : Pigeonhole a -> Task x a
read var =
  take var    `andThen` \val ->
  put var val `andThen_`
  succeed val

{-| Operate on the contents of the Pigeonhole -}
with : Pigeonhole a -> (a -> Task x b) -> Task x b
with var work =
  bracket (take var) (put var) work

{-| Modify the Pigeonhole, replacing if the replacement task fails -}
modify_ : Pigeonhole a -> (a -> Task x a) -> Task x ()
modify_ var update =
  let acquire = take var
      release = put var
      work = \val ->
        update val `andThen` put var
  in bracketOnError acquire release work

{-| Modify the Pigeonhole, replacing if the replacement task fails. -}
modify : Pigeonhole a -> (a -> Task x (a,b)) -> Task x b
modify var update =
  let acquire = take var
      release = put var
      work = \val -> 
        update val      `andThen` \(new_val, ret_val) ->
        put var new_val `andThen_` succeed ret_val
  in bracketOnError acquire release work
