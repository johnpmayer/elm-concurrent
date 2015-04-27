
module Handoff where

import Concurrent exposing (MVar, newEmptyMVar, takeMVar, putMVar)
import Graphics.Element exposing (Element, show)
import Signal exposing (constant, Mailbox, mailbox, send, (<~))
import Task exposing (Task, andThen, spawn, succeed)

handoffResult : Mailbox (Maybe Int)
handoffResult = mailbox Nothing

main : Signal Element
main = show <~ handoffResult.signal

producer : MVar Int -> Task x ()
producer sync = 
  putMVar sync 5

consumer : MVar Int -> Task x ()
consumer sync = 
  takeMVar sync `andThen` \x ->
  send handoffResult.address <| Just x

port startup : Task x ()
port startup = 
  newEmptyMVar          `andThen` \sync ->
  spawn (producer sync) `andThen` \_ ->
  spawn (consumer sync) `andThen` \_ ->
  succeed ()