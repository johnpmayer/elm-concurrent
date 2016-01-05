
module ParallelImages where

import Graphics.Element exposing (Element, down, flow, show)
import Http exposing (getString)
import List exposing (map)
import Signal exposing (Mailbox, foldp, mailbox, send)
import String exposing (left)
import Task exposing (Task, andThen, onError)

import Concurrent.Future exposing (future, wait)

loadResult : Mailbox (Maybe String)
loadResult = mailbox Nothing

main : Signal Element
main = Signal.map (flow down << map show) <| foldp (::) [] loadResult.signal

log : String -> Task x ()
log msg = send loadResult.address <| Just msg

download : String -> Task Http.Error String
download url = 
  log ("Initiate " ++ url) `andThen` \_ ->
  getString url

port startup : Task Http.Error ()
port startup = 
  log "Start" `andThen` \_ ->
  future (download "FillDrain.elm") `andThen` \res1 ->
  future (download "Put2.elm") `andThen` \res2 ->
  wait res1 `andThen` (log << (++) "Result 1 " << left 20) `andThen` \_ ->
  wait res2 `andThen` (log << (++) "Result 2 " << left 20)