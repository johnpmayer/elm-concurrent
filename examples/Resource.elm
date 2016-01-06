
module Resource where

import Concurrent.MVar exposing (MVar, newMVar, withMVar)
import Graphics.Element exposing (Element, down, flow, show, up)
import Signal exposing (Mailbox, foldp, mailbox, map, send)
import Task exposing (Task, andThen, spawn, succeed)
import TaskUtils exposing (andThen_)

log : Mailbox (Maybe String)
log = mailbox Nothing

logDisplay : Signal Element
logDisplay = map (flow up << List.map show) (foldp (::) [] log.signal)

main = map (\ld -> flow down [show "Demonstrate mutex", show "Messages:", ld]) logDisplay

doLogging : MVar a -> String -> Task x ()
doLogging mutex name =
  let doLog msg = send log.address <| Just (name ++ ": " ++ msg)
      criticalSection = withMVar mutex <| \_ ->
        doLog "Doing step 1 in critical section" `andThen_`
        doLog "Doing step 2 in critical section" `andThen_`
        doLog "Doing step 3 in critical section" `andThen_`
        doLog "Doing step 4 in critical section"
  in criticalSection

port startup : Task x ()
port startup =
  newMVar () `andThen` \mutex ->
  spawn (doLogging mutex "A") `andThen_`
  spawn (doLogging mutex "B") `andThen_`
  spawn (doLogging mutex "C") `andThen_`
  succeed ()
