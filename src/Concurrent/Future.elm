
module Concurrent.Future
  ( Future, future, wait
  , onSuccess, onFailure
  ) where

{-|

@docs Future, future, wait

@docs onSuccess, onFailure

-}

import Result exposing (Result)
import Task exposing (Task, ThreadID, andThen, fail, fromResult, onError, spawn, succeed, toResult)
import TaskUtils exposing (Never, andThen_, unsafeFromNever)
import Concurrent.MVar exposing (MVar, newEmptyMVar, putMVar, readMVar, takeMVar)

{-| A handle to a result that may be fulfilled -}
type Future x a = Future { await : Task Never (Result x a) }

{-| Convert a task into a future -}
future : Task x a -> Task x (Future x a)
future task =
  newEmptyMVar `andThen` \var ->
  spawn (toResult task `andThen` putMVar var) `andThen_`
  let await = readMVar var
  in succeed (Future { await = await })

{-| Wait for a future to resolve -}
wait : Future x a -> Task x a
wait (Future {await}) = 
  unsafeFromNever await `andThen` fromResult
    
{-| 
Register a task to run if the future succeeds
Nothing is done if the registered task itself fails
-}
onSuccess : Future x a -> (a -> Task y b) -> Task z ThreadID
onSuccess (Future {await}) task =
  spawn <|
    unsafeFromNever await `andThen` \result ->
    case result of
      Ok a -> task a `andThen_` succeed ()
      Err e -> succeed ()

{-| 
Register a task to run if the future fails
Nothing is done if the registered task itself fails
-}
onFailure : Future x a -> (x -> Task y b) -> Task z ThreadID
onFailure (Future {await}) task =
  spawn <|
    unsafeFromNever await `andThen` \result ->
    case result of
      Ok a -> succeed ()
      Err e -> task e `andThen_` succeed ()

{-| 'Functor' interface for Future -}
map : (a -> b) -> Future x a -> Future x b
map fun (Future {await}) = Future { await = Task.map (Result.map fun) await }
