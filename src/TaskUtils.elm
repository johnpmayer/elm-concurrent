
module TaskUtils (Never, andThen_, unsafeFromNever) where

import Task exposing (Task, andThen, fail, onError)
import Native.TaskUtils exposing (fromNever)

andThen_ : Task x a -> Task x b -> Task x b
andThen_ t1 t2 = t1 `andThen` (\_ -> t2)

type Never = Never_

unsafeFromNever : Task Never a -> Task x a
unsafeFromNever = Native.TaskUtils.fromNever
