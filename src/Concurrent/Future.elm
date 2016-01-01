
module Concurrent.Future
  ( Future, future, wait
  , onSuccess, onFailure
  ) where

{-|

@docs Future, future, wait

@docs onSuccess, onFailure

-}

import Result exposing (Result)
import Task exposing (Task, ThreadID, andThen, fail, fromResult, map, onError, spawn, succeed, toResult)
import TaskUtils exposing (andThen_)
import Concurrent.MVar exposing (MVar, newEmptyMVar, putMVar, readMVar, takeMVar)

{-| A handle to a result that may be fulfilled -}
type Future x a = Future (MVar (Result x a))

{-| Convert a task into a future -}
future : Task x a -> Task x (Future x a)
future task =
  newEmptyMVar `andThen` \var ->
  spawn (toResult task `andThen` putMVar var) `andThen_`
  succeed (Future var)

{-| Wait for a future to resolve -}
wait : Future x a -> Task x a
wait (Future var) = 
  readMVar var `andThen` fromResult

{-| 
Register a task to run if the future succeeds
Nothing is done if the registered task itself fails
-}
onSuccess : Future x a -> (a -> Task y b) -> Task z ThreadID
onSuccess (Future var) task =
  spawn <|
    readMVar var `andThen` \result ->
    case result of
      Ok a -> task a `andThen_` succeed ()
      Err e -> succeed ()

{-| 
Register a task to run if the future fails
Nothing is done if the registered task itself fails
-}
onFailure : Future x a -> (x -> Task y b) -> Task z ThreadID
onFailure (Future var) task =
  spawn <|
    readMVar var `andThen` \result ->
    case result of
      Ok a -> succeed ()
      Err e -> task e `andThen_` succeed ()
