
module PrestoDOM.Core.Types.Language.Flow where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Presto.Core.Flow (Flow, doAff)
import Presto.Core.Types.Language.Flow (getLogFields)
import PrestoDOM.Core as PrestoDOM
import PrestoDOM.Types.Core (class Loggable, ScopedScreen, Controller, Screen, LoggableScreen)
import PrestoDOM.Utils (addTime2)

initUI :: forall a. Flow a Unit
initUI  = do
  ns <- doAff do liftEffect $ PrestoDOM.sanitiseNamespace $ Just "default"
  doAff do liftEffect $ PrestoDOM.initUIWithNameSpace ns Nothing

initUIWithNameSpace :: String -> Maybe String -> Effect Unit
initUIWithNameSpace = PrestoDOM.initUIWithNameSpace

-- deprecated
initUIWithScreen
  :: forall action state a
   . Screen action state Unit
  -> Flow a Unit
initUIWithScreen screen = do
  json <- getLogFields
  doAff do PrestoDOM.initUIWithScreen "default" Nothing (mapToLoggableScreen $ mapToScopedScreen screen) json

runScreen :: forall action state retType a. Show action => Loggable action => Screen action state retType -> Flow a retType
runScreen screen = do
  _ <- doAff $ liftEffect $ addTime2 "Process_Eval_End"
  _ <- doAff $ liftEffect $ addTime2 "Render_runScreen_Start"
  PrestoDOM.setScreenInLog Nothing screen.name
  json <- getLogFields
  doAff $ PrestoDOM.runScreen (mapToLoggableScreen $ mapToScopedScreen screen) json

runLoggableScreen :: forall action state retType a. Show action => Loggable action => LoggableScreen action state retType -> Flow a retType
runLoggableScreen screen = do
  _ <- doAff $ liftEffect $ addTime2 "Process_Eval_End"
  _ <- doAff $ liftEffect $ addTime2 "Render_runScreen_Start"
  PrestoDOM.setScreenInLog Nothing screen.name
  json <- getLogFields
  doAff $ PrestoDOM.runScreen screen json

runScreenWithNameSpace :: forall action state retType a. Show action => Loggable action => ScopedScreen action state retType -> Flow a retType
runScreenWithNameSpace screen = do
  _ <- doAff $ liftEffect $ addTime2 "Process_Eval_End"
  _ <- doAff $ liftEffect $ addTime2 "Render_runScreen_Start"
  PrestoDOM.setScreenInLog screen.parent screen.name
  json <- getLogFields
  doAff $ PrestoDOM.runScreen (mapToLoggableScreen screen) json

prepareScreenWithNameSpace
  :: forall action state retType a.  Show action => Loggable action => ScopedScreen action state retType -> Flow a Unit
prepareScreenWithNameSpace screen = do
  json <- getLogFields
  doAff $ PrestoDOM.prepareScreen (mapToLoggableScreen screen) json

prepareScreen
  :: forall action state retType a. Show action => Loggable action => Screen action state retType -> Flow a Unit
prepareScreen screen = do
  json <- getLogFields
  doAff $ PrestoDOM.prepareScreen (mapToLoggableScreen $ mapToScopedScreen screen) json

showScreen :: forall action state retType a. Show action => Loggable action => Screen action state retType -> Flow a retType
showScreen screen = do
  PrestoDOM.setScreenInLog Nothing screen.name
  json <- getLogFields
  doAff $ PrestoDOM.showScreen (mapToLoggableScreen $ mapToScopedScreen screen) json

showScreenWithNameSpace :: forall action state retType a. Show action => Loggable action => ScopedScreen action state retType -> Flow a retType
showScreenWithNameSpace screen = do
  PrestoDOM.setScreenInLog screen.parent screen.name
  json <- getLogFields
  doAff $ PrestoDOM.showScreen (mapToLoggableScreen screen) json

runController :: forall action state retType a. Show action => Loggable action => Controller action state retType -> Flow a retType
runController controller = do
  json <- getLogFields
  doAff $ PrestoDOM.runController controller json

updateScreen :: forall action state retType a. Show action => Loggable action => Screen action state retType -> Flow a Unit
updateScreen screen = doAff do liftEffect $ PrestoDOM.updateScreen (mapToLoggableScreen $ mapToScopedScreen screen)

updateScreenWithNameSpace :: forall action state retType a. Show action => Loggable action => ScopedScreen action state retType -> Flow a Unit
updateScreenWithNameSpace screen = doAff do liftEffect $ PrestoDOM.updateScreen (mapToLoggableScreen screen)

mapToScopedScreen :: forall action state retType. Screen action state retType -> ScopedScreen action state retType
mapToScopedScreen screen =
  { initialState : screen.initialState
  , name : screen.name
  , globalEvents : screen.globalEvents
  , view : screen.view
  , eval : screen.eval
  , parent : Nothing
  }

mapToLoggableScreen :: forall action state retType. ScopedScreen action state retType -> LoggableScreen action state retType
mapToLoggableScreen screen =
  { initialState : screen.initialState
  , name : screen.name
  , globalEvents : screen.globalEvents
  , view : screen.view
  , eval : screen.eval
  , parent : screen.parent
  , logWhitelist : []
  }

terminateUI :: forall a. Flow a Unit
terminateUI = doAff do liftEffect $ PrestoDOM.terminateUI Nothing