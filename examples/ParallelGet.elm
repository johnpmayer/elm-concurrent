
module ParallelImages where

import Graphics.Element exposing (Element, flow, show, up)
import Http exposing (getString)
import List exposing (map)
import Signal exposing (Mailbox, foldp, mailbox, send)
import String exposing (left)
import Task exposing (Task, andThen, succeed)

import Concurrent.Task exposing (waitBoth)

loadResult : Mailbox (Maybe String)
loadResult = mailbox Nothing

main : Signal Element
main = Signal.map (flow up << map show) <| foldp (::) [] loadResult.signal

log : String -> Task x ()
log msg = send loadResult.address <| Just msg

download : String -> Task Http.Error String
download url = 
  log ("Initiate " ++ url) `andThen` \_ ->
  getString url `andThen` \res ->
  log ("Got response") `andThen` \_ ->
  succeed res

port startup : Task Http.Error ()
port startup = 
  log "Start" `andThen` \_ ->
  waitBoth (download "FillDrain.elm") (download "Put2.elm") `andThen` \(res1, res2) ->
  log ("Result1: '" ++ left 20 res1 ++ "...'; Result2: '" ++ left 20 res2 ++ "...'")
