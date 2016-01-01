
module Concurrent.Task
  ( waitBoth
  , map2
  ) where

{-|

@docs waitBoth

@docs map2

-}

import Result exposing (Result)
import Task exposing (Task, andThen, map, succeed)
import Concurrent.Future exposing (Future, future, wait)

{-| 
Run both tasks in parallel, returning both results
If either task fails, this function fails with that error
If both tasks fail, this function fails with the error of the first task
-}
waitBoth : Task x a -> Task x b -> Task x (a,b)
waitBoth taskA taskB = 
  future taskA `andThen` \future1 ->
  future taskB `andThen` \future2 ->
  wait future1 `andThen` \a ->
  wait future2 `andThen` \b ->
  succeed (a, b)

{-| 
Put the results of two tasks together. If either task fails, the whole
thing fails. Unlike Task.map2 from the core library, the tasks run in 
parallel.
-}
map2 : (a -> b -> c) -> Task x a -> Task x b -> Task x c
map2 f taskA taskB = map (uncurry f) <| waitBoth taskA taskB