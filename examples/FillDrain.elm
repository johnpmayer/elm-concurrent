
module FillDrain where

import Concurrent.Chan exposing (Chan, new, write, read)
import Debug exposing (log)
import Graphics.Element exposing (Element, down, flow, show)
import Signal exposing (constant, Mailbox, mailbox, map, map2, send)
import Task exposing (Task, andThen, sleep, spawn, succeed)

fillResult : Mailbox (Maybe String)
fillResult = mailbox Nothing

drainResult : Mailbox (Maybe String)
drainResult = mailbox Nothing

main : Signal Element
main = map (\(fill, drain) -> flow down [show fill, show drain]) (map2 (,) fillResult.signal drainResult.signal)

produceN : Int -> Int -> Chan Int -> Task x ()
produceN x max chan = 
  send fillResult.address (Just ("Fill " ++ toString x))  `andThen` \_ ->
  write chan x                                            `andThen` \_ ->
  sleep 100                                               `andThen` \_ ->
    if x < max
    then produceN (x + 1) max chan
    else succeed ()

consumer : Chan Int -> Task x ()
consumer chan = 
  read chan                                                 `andThen` \x ->
  send drainResult.address (Just ("Drain " ++ toString x))  `andThen` \_ ->
  consumer chan

-- Demonstrate that the chan can fill up without blocking, and then drain & keep filling concurrently

port startup : Task x ()
port startup = 
  new                         `andThen` \chan ->
  produceN 0 15 chan          `andThen` \_ ->
  spawn (consumer chan)       `andThen` \_ ->
  spawn (produceN 15 50 chan) `andThen` \_ ->
  succeed ()
