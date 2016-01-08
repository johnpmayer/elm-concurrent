
module TaskUtils (Never, andThen_, bracket, bracketOnError, unsafeFromNever) where

import Debug exposing (crash)
import Task exposing (Task, andThen, fail, fromResult, onError, succeed, toResult)

andThen_ : Task x a -> Task x b -> Task x b
andThen_ t1 t2 = t1 `andThen` (\_ -> t2)

bracket : Task x a -> (a -> Task x b) -> (a -> Task x c) -> Task x c
bracket acquire release work = 
  acquire `andThen` \resource ->
  toResult (work resource) `andThen` \result ->
  release resource `andThen_` 
  fromResult result

bracketOnError : Task x a -> (a -> Task x b) -> (a -> Task x c) -> Task x c
bracketOnError acquire release work =
  acquire `andThen` \resource ->
  toResult (work resource) `andThen` \result ->
    case result of
      Ok c -> succeed c
      Err error -> release resource `andThen_` fail error

type Never = Never Never

unsafeFromNever : Task Never a -> Task x a
unsafeFromNever wontFail = onError wontFail (\impossibleError -> crash "Task Never a produced an error")