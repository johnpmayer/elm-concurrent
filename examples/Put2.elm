
module Put2 where

import Concurrent.MVar exposing (MVar, newEmptyMVar, takeMVar, putMVar)
import Graphics.Element exposing (Element, show)
import Signal exposing (constant, Mailbox, mailbox, map, send)
import Task exposing (Task, andThen, spawn, succeed)

handoffResult : Mailbox (Maybe String)
handoffResult = mailbox Nothing

main : Signal Element
main = map show handoffResult.signal

producer : MVar Int -> Task x ()
producer sync = 
  send handoffResult.address (Just "Put none") `andThen` \_ ->
  putMVar sync 5 `andThen` \_ ->
  send handoffResult.address (Just "Put first") `andThen` \_ ->
  putMVar sync 2 `andThen` \_ ->
  send handoffResult.address (Just "Put second")

port startup : Task x ()
port startup = 
  newEmptyMVar          `andThen` \sync ->
  spawn (producer sync) `andThen` \_ ->
  succeed ()
