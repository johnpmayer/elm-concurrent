
module Handoff where

import Concurrent exposing (MVar, newEmptyMVar, takeMVar, putMVar)
import Graphics.Element exposing (show)
import Task exposing (Task, andThen, succeed, spawn)

main = show "Test"

producer : MVar Int -> Task x ()
producer sync = putMVar sync 5

consumer : MVar Int -> Task x ()
consumer sync = succeed ()

port startup : Task x ()
port startup = 
  newEmptyMVar          `andThen` \sync ->
  spawn (producer sync) `andThen` \_ ->
  spawn (consumer sync) `andThen` \_ ->
  succeed ()