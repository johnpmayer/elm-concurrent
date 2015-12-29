
module FillDrain where

--import Concurrent.MVar exposing (MVar, newEmptyMVar, takeMVar, putMVar)
import Concurrent.Chan exposing (Chan, newChan, writeChan, readChan)
import Graphics.Element exposing (Element, show)
import Signal exposing (constant, Mailbox, mailbox, map, send)
import Task exposing (Task, andThen, spawn, succeed)

handoffResult : Mailbox (Maybe Int)
handoffResult = mailbox Nothing

main : Signal Element
main = map show handoffResult.signal

consumer : Chan Int -> Task x ()
consumer chan = 
  readChan chan `andThen` \x ->
  send handoffResult.address (Just x) `andThen` \_ ->
  consumer chan

port startup : Task x ()
port startup = 
  newChan         `andThen` \chan ->
  writeChan chan 0  `andThen` \_ ->
  writeChan chan 1  `andThen` \_ ->
  writeChan chan 2  `andThen` \_ ->
  writeChan chan 3  `andThen` \_ ->
  writeChan chan 4  `andThen` \_ ->
  spawn (consumer chan) `andThen` \_ ->
  succeed ()
