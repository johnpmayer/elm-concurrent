
module Handoff where

import Concurrent.Pigeonhole exposing (Pigeonhole, newEmpty, take, put)
import Graphics.Element exposing (Element, show)
import Signal exposing (constant, Mailbox, mailbox, map, send)
import Task exposing (Task, andThen, spawn, succeed)

handoffResult : Mailbox (Maybe Int)
handoffResult = mailbox Nothing

main : Signal Element
main = map show handoffResult.signal

producer : Pigeonhole Int -> Task x ()
producer sync = 
  put sync 5

consumer : Pigeonhole Int -> Task x ()
consumer sync = 
  take sync `andThen` \x ->
  send handoffResult.address <| Just x

port startup : Task x ()
port startup = 
  newEmpty              `andThen` \sync ->
  spawn (producer sync) `andThen` \_ ->
  spawn (consumer sync) `andThen` \_ ->
  succeed ()
