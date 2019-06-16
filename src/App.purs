module Test.App (Message(..), component) where

import Prelude

import Affjax.RequestBody (RequestBody(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Data.Symbol (SProxy(..))
import Debug.Trace as D
import Effect.Aff (Aff)
import Effect.Random (randomInt)
import Effect.Random as R
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Test.Button as Button
import Data.HTTP.Method (Method(..))
import Affjax as AX
import Data.Argonaut.Core as J
import Affjax.ResponseFormat as ResponseFormat

type State = { toggleCount :: Int,
               buttonState :: Maybe Boolean,
               rnd :: Int,
               ajaxLoading:: Boolean,
               ajaxResp:: String }

data Message = Toggled Boolean

data Action
  = HandleButton Button.Message
  | CheckButtonState
  | RandomNumber
  | AjaxCall

_button :: SProxy "button"
_button = SProxy

type ChildSlots = ( button :: Button.Slot Unit )

initialState :: forall i. i -> State
initialState _ = {
                  toggleCount: 0,
                  buttonState: Nothing,
                  rnd: 0,
                  ajaxLoading: false,
                  ajaxResp: " -- empty -- "
                  }

component :: forall q i o m. H.Component HH.HTML q i o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }


getButtonStatus :: Maybe Boolean -> String
getButtonStatus (Just a) = if a then "ON" else "OFF"
getButtonStatus Nothing = "Undefined"

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state =  HH.div_
                      [
                        HH.header_ [HH.h1_ [HH.text ("PureScript Application")]],
                        HH.div_ [
                          HH.text ("-- embeded button bellow -- status change: " <> show state.toggleCount),
                          HH.slot _button unit Button.component unit (Just <<< HandleButton)
                        ],
                        HH.div [HE.onClick \_ -> Just CheckButtonState,
                                HP.classes [ClassName "buttonStatus"]] [
                          HH.text ("** CLICK ** to check button status"),
                          HH.text ("Button Status Is: " <> getButtonStatus state.buttonState )
                        ],
                        HH.div [HE.onClick \_ -> Just RandomNumber,
                                HP.classes [ClassName "randomNum"]] [
                          HH.text ("Random Number: " <> show state.rnd)
                        ],
                        HH.div [HE.onClick \_ -> Just AjaxCall,
                                HP.classes [ClassName "ajaxCall"]]
                               [
                                  HH.p_ [ HH.text ("Ajax Loading: " <> show state.ajaxLoading) ],
                                  HH.p_ [ HH.text ("Ajax Response: " <> show state.ajaxResp) ]
                               ]
                      ]

handleAction :: forall o m. Action -> H.HalogenM State Action ChildSlots o Aff Unit
handleAction = case _ of
  HandleButton (Button.Toggled _) -> do
      D.trace "Button emitted status" \_ -> H.modify_ (\st -> st { toggleCount = st.toggleCount + 1 })
  CheckButtonState -> do
      _buttonState <- H.query _button unit $ H.request Button.IsOn
      D.trace "checking button state" \_ -> H.modify_ (_ { buttonState = _buttonState })
  RandomNumber -> do
      newRnd <- H.liftEffect $ randomInt 1 100
      D.trace "checking button state" \_ -> H.modify_ (_ { rnd = newRnd })
  AjaxCall -> do
      H.modify_ (_ { ajaxLoading = true })
      response <- H.liftAff $ AX.request (AX.defaultRequest { url = "https://api.github.com/users/lukaszkujawa", method = Left GET, responseFormat = ResponseFormat.json })
      case response.body of
          Left err -> H.modify_ (_ { ajaxLoading = false, ajaxResp = "ERROR: " <> AX.printResponseFormatError err })
          Right json -> H.modify_ (_ { ajaxLoading = false, ajaxResp = J.stringify json })
