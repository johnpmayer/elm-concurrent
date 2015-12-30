
module FillDrain where

import Concurrent.Chan exposing (Chan, newChan, writeChan, readChan)
import Debug exposing (log)
import Graphics.Element exposing (Element, show)
import Signal exposing (constant, Mailbox, mailbox, map, send)
import Task exposing (Task, andThen, sleep, spawn, succeed)

drainResult : Mailbox (Maybe Int)
drainResult = mailbox Nothing

main : Signal Element
main = map show drainResult.signal

producer : Int -> Chan Int -> Task x ()
producer x chan = 
  writeChan chan x  `andThen` \_ ->
  sleep 100         `andThen` \_ ->
  producer (x + 1) chan

consumer : Chan Int -> Task x ()
consumer chan = 
  readChan chan                     `andThen` \x ->
  send drainResult.address (Just x) `andThen` \_ ->
  consumer chan

port startup : Task x ()
port startup = 
  newChan               `andThen` \chan ->
  spawn (producer 0 chan) `andThen` \_ ->
  spawn (consumer chan) `andThen` \_ ->
  succeed ()
