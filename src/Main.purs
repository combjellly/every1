module Main where
import Parser
import AST
import Tempo

import Prelude

import Data.Either
import Data.Maybe

import Parsing
import Data.Identity
import RenderEngine
import Effect
import Data.List
import Effect.Class.Console
import Effect.Aff
import Data.Time.Duration
import Effect.Class
import Effect.Ref
import Data.Enum
import Effect.Now (nowDateTime)
import Data.DateTime
import Data.Time.Duration 
import Data.Newtype (unwrap)
import WebDirt
import Data.Traversable


type EngineRecord = 
  {
    programRef :: Ref Program
  , tempo :: Ref Tempo -- Number 
  , wStart :: Ref DateTime
  , wEnd :: Ref DateTime
  }

main :: Effect Unit 
main = pure unit

launch :: Effect EngineRecord 
launch = do 
  launchDateTime <- nowDateTime
  tempo <- new $ launchDateTime 
  wStart <- new $ launchDateTime
  wEnd <- new $ launchDateTime
  programRef <- new $ LoopElement (Loop 3.0 (Play "hp808":Nil)):Nil
  let er = {programRef,tempo,wStart,wEnd}
  -- _ <- launchAff $ crudeScheduler 0.03 er
  pure er

launchDirt :: Effect WebDirt
launchDirt = do
  wd <- newWebDirt { sampleMapUrl: "samples/sampleMap.json", sampleFolder: "samples" }
  initializeWebAudio wd
  pure wd

renderStandalone :: EngineRecord -> WebDirt -> Effect Unit
renderStandalone er wd = do 
  t <- nowDateTime -- only needed to please fromMaybe...default value
  wS <- read $ er.wEnd
  let wE = fromMaybe t $ adjust (Seconds 0.25) wS 
  write wS er.wStart
  write wE er.wEnd
  t <- read $ er.tempo
  let cycleStart = timeToCount t wS
  let cycleEnd = timeToCount t wE
  p <- liftEffect $ read $ er.programRef
  let es = renderEvents p cycleStart cycleEnd t
  printToConsole es
  play wd es



play :: WebDirt ->List SampleEvent -> Effect Unit
play wd es = for_ es \i -> do
  playSample wd {s: i.s, n: "1", whenPosix: i.whenPosix}
  pure unit


{-
maybeSampleRecord :: List SampleEvent -> Record
maybeSampleRecord wd (SampleEvent es) =  
  let first = head es
  in case first of
    Nothing -> playSample wd  {s: "BLANK", when: 0.0}
    Just x ->  {s: es.s, when: es.when}
-}

printToConsole :: List SampleEvent -> Effect Unit
printToConsole es = log (show es)

parse :: EngineRecord -> String -> Effect String
parse er x = case (runParser x program) of
    Left err -> pure $ show err 
    Right p -> do 
        write p er.programRef
        pure $ show "success"