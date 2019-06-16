module Test.Button (Slot, Query(..), Message(..), component) where

import Prelude

import Affjax.RequestBody (RequestBody(..))
import Data.Maybe (Maybe(..))
import Debug.Trace as D
import Effect.Console (log)
import Effect.Console as EC
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Slot = H.Slot Query Message

type State = { enabled :: Boolean }

data Message = Toggled Boolean

data Query a = IsOn (Boolean -> a)

data Action = Toggle

component :: forall q i o m. H.Component HH.HTML Query i Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction,
          handleQuery = handleQuery
        }
    }

initialState :: forall i. i -> State
initialState _ = { enabled: false }

labelValue :: String
labelValue = " <-- Button State"

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div [HE.onClick \_ -> Just Toggle]
                      [HH.text ("BUTTON STATUS:" <> " " <> buttonLabel)]
  where
    buttonLabel = if state.enabled then "ON" else "OFF"


handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Toggle -> do
    newStateD <- D.trace "Inside Handle Action" \_ -> H.modify \st -> st { enabled = not st.enabled }
    H.raise (Toggled newStateD.enabled)

handleQuery :: forall m a. Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery = case _ of
  IsOn k -> do
    enabled <- H.gets _.enabled
    D.trace "Inside Query Handler" \_ -> pure (Just (k enabled))
