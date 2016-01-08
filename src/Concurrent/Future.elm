
module Concurrent.Future
  ( Future, future, wait
  , onSuccess, onFailure
  , map, succeed, map2
  ) where

{-|

@docs Future, future, wait

@docs onSuccess, onFailure

@docs map, succeed, map2

-}

import Result exposing (Result)
import Task exposing (Task, ThreadID, andThen, fail, fromResult, onError, spawn, toResult)
import TaskUtils exposing (Never, andThen_, unsafeFromNever)
import Concurrent.Pigeonhole exposing (Pigeonhole, newEmpty, put, read, take)

{-| A handle to a result that may be fulfilled -}
type Future x a = Future { await : Task Never (Result x a) }

{-| Spawn a task, and return a Future -}
future : Task x a -> Task x (Future x a)
future task =
  newEmpty `andThen` \var ->
  spawn (toResult task `andThen` put var) `andThen_`
  let await = read var
  in Task.succeed (Future { await = await })

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
      Ok a -> task a `andThen_` Task.succeed ()
      Err e -> Task.succeed ()

{-| 
Register a task to run if the future fails
Nothing is done if the registered task itself fails
-}
onFailure : Future x a -> (x -> Task y b) -> Task z ThreadID
onFailure (Future {await}) task =
  spawn <|
    unsafeFromNever await `andThen` \result ->
    case result of
      Ok a -> Task.succeed ()
      Err e -> task e `andThen_` Task.succeed ()

{-| 'Functor' interface for Future -}
map : (a -> b) -> Future x a -> Future x b
map fun (Future {await}) = 
  Future { await = Task.map (Result.map fun) await }

{-| 'Applicative' interface for Future -}
succeed : a -> Future x a
succeed a = Future { await = Task.succeed (Ok a) }

{-| 'Applicative' interface for Future -}
map2 : (a -> b -> c) -> Future x a -> Future x b -> Future x c
map2 fun (Future futureA) (Future futureB) =
  let task = Task.map2 (Result.map2 fun) futureA.await futureB.await
  in Future { await = task }

