module RenderEngine where

import AST
import Parser

import Prelude
import Data.Int (toNumber,ceil)
import Data.List
import Data.Either
import Control.Plus (empty)
import Tempo
import Data.DateTime
import Prelude 
import Data.Newtype
import Data.Time.Duration
import Data.Maybe
import Data.DateTime.Instant


--EventBank for troubleshooting
type EventBank = (List Event) 
data Event = Number
type Cycles = Number


type SampleEvent = 
  {
  s :: String,
  whenPosix :: Number
  }

{-
instance showEvent :: Show SampleEvent where
  show x =  x.s <> " " <> show x.when
-}

renderEvents :: Program -> Cycles -> Cycles -> Tempo -> List SampleEvent
renderEvents p wStart wEnd t =
 concat $ map (renderElement t wStart wEnd) p 

renderElement :: Tempo -> Cycles -> Cycles -> Element -> List SampleEvent
renderElement _ _ _ (VariableGlobal _) = Nil
renderElement t wStart wEnd (LoopElement x) = renderLoop t wStart wEnd x

renderLoop :: Tempo -> Cycles -> Cycles -> Loop -> List SampleEvent
renderLoop t wStart wEnd (Loop n xs) = concat $ map (renderAction t wStart wEnd n) xs

renderAction :: Tempo -> Cycles -> Cycles -> Number -> Action -> List SampleEvent
renderAction _ _ _ _ (VariableAction _) = Nil
renderAction t wStart wEnd n (Play s) = samplePlayEvents t wStart wEnd n s

samplePlayEvents ::Tempo ->  Number -> Number -> Number -> String -> List SampleEvent
samplePlayEvents t wStart wEnd n s = 
  let xs = cycleIntervalList wStart wEnd n
  in
  map (sampleNaming t s) xs

cycleIntervalList :: Number -> Number -> Number -> List Number
cycleIntervalList wStart wEnd n = 
  let pFirst= (toNumber $ ceil (wStart/n)) * n
  in if pFirst < wEnd then
    (pFirst : cycleIntervalList (pFirst+n) wEnd n )
  else
    Nil 

sampleNaming :: Tempo -> String -> Number -> SampleEvent
sampleNaming t s wCycles = {s : s, whenPosix: (unwrap (unInstant (fromDateTime (countToTime t wCycles))))/1000.0} 
