
module TaskUtils (andThen_) where

import Task exposing (Task, andThen)

andThen_ : Task x () -> Task x b -> Task x b
andThen_ t1 t2 = t1 `andThen` (\_ -> t2)

