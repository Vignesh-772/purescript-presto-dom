module PrestoDOM.Utils
  ( continue
  , exit
  , update
  , updateAndExit
  , continueWithCmd
  , updateWithCmdAndExit
  , concatPropsArrayRight
  , concatPropsArrayLeft
  , (<>>)
  , (<<>)
  , storeToWindow
  , getFromWindow
  , debounce
  , logAction
  , addTime2
  , performanceMeasure
  , isGenerateVdom
  , initMeasuringDuration
  , endMeasuringDuration
  )where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Uncurried as EFn

import PrestoDOM.Types.Core (class Loggable, performLog, Eval, Cmd)
import Effect(Effect)
import Effect.Ref as Ref
import Foreign(Foreign)
import Foreign.Object as Object

foreign import addTime2 :: String -> Effect Unit
foreign import getTime :: Effect Int

foreign import performanceMeasure :: String -> String -> String -> Effect Unit
foreign import isGenerateVdom :: Effect Boolean

foreign import initMeasuringDuration :: String -> Effect Unit
foreign import endMeasuringDuration :: String -> Effect Unit

continue
  :: forall state action returnType
   . state
  -> Eval action returnType state
continue state = Right (Right (Tuple state []))

exit
  :: forall state action returnType
   . returnType
  -> Eval action returnType state
exit = Left <<< Tuple Nothing

updateAndExit
  :: forall state action returnType
   . state
  -> returnType
  -> Eval action returnType state
updateAndExit state = Left <<< Tuple (Just $ Tuple state [])

updateWithCmdAndExit
  :: forall state action returnType
   . state
  -> Cmd action
  -> returnType
  -> Eval action returnType state
updateWithCmdAndExit state cmds = Left <<< Tuple (Just $ Tuple state cmds)

continueWithCmd
  :: forall state action returnType
   . state
  -> Cmd action
  -> Eval action returnType state
continueWithCmd state cmds = Right (Right (Tuple state cmds))

update
  :: forall state action returnType
   . state
  -> Eval action returnType state
update state = Right (Left state)


foreign import concatPropsArrayImpl :: forall a. Array a -> Array a -> Array a
foreign import debounce :: (String -> Foreign -> Object.Object Foreign -> Effect Unit) -> String -> Foreign -> (Object.Object Foreign) -> Effect Unit


concatPropsArrayRight :: forall a. Array a -> Array a -> Array a
concatPropsArrayRight = concatPropsArrayImpl

concatPropsArrayLeft :: forall a. Array a -> Array a -> Array a
concatPropsArrayLeft = flip concatPropsArrayImpl

infixr 5 concatPropsArrayRight as <>>

infixr 5 concatPropsArrayLeft as <<>

foreign import storeToWindow_ :: forall a. EFn.EffectFn2 String a Unit
foreign import getFromWindow_ :: forall a. String -> (a -> Maybe a) -> (Maybe a) -> a

storeToWindow :: forall a. String -> a -> Effect Unit
storeToWindow = EFn.runEffectFn2 storeToWindow_

getFromWindow :: forall a. String ->  Maybe a
getFromWindow key = getFromWindow_ key Just Nothing


logAction :: forall a. Loggable a => Show a => Ref.Ref Int -> (Maybe a) -> (Maybe a) -> Boolean -> (Object.Object Foreign)-> Effect Unit
logAction timerRef (Just prevAct) (Just currAct) false json = do
  currTime <- getTime
  prevTime <- Ref.read timerRef
  _ <- Ref.write currTime timerRef
  if show prevAct == show currAct && (currTime - prevTime < 300)
    then pure unit
    else loggerFunction currAct json 
logAction _ Nothing (Just currAct) false json = loggerFunction currAct json 
logAction _ _ (Just currAct) true json = loggerFunction currAct json 
logAction _ _ _ _ _ = pure unit

loggerFunction :: forall a. Loggable a => Show a => a -> (Object.Object Foreign) -> Effect Unit
loggerFunction action json = do
  performLog action json
