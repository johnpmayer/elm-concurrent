
module Put2 where

import Concurrent.Pigeonhole exposing (Pigeonhole, newEmpty, take, put)
import Graphics.Element exposing (Element, show)
import Signal exposing (constant, Mailbox, mailbox, map, send)
import Task exposing (Task, andThen, spawn, succeed)
import TaskUtils exposing (andThen_)

handoffResult : Mailbox (Maybe String)
handoffResult = mailbox Nothing

main : Signal Element
main = map show handoffResult.signal

producer : Pigeonhole Int -> Task x ()
producer sync = 
  send handoffResult.address (Just "Put none")  `andThen_`
  put sync 5                                    `andThen_`
  send handoffResult.address (Just "Put first") `andThen_`
  put sync 2                                    `andThen_`
  send handoffResult.address (Just "Put second")

port startup : Task x ()
port startup = 
  newEmpty              `andThen` \sync ->
  spawn (producer sync) `andThen` \_ ->
  succeed ()
