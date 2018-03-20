module Component where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a = ToggleState a

type State = { on :: Boolean }

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { on: false }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.div
          [ HE.onClick (HE.input_ ToggleState) ]
          [ if not state.on
              then (HH.h1_ [HH.text "Welcome!"])
              else (HH.img [HP.src "../images/raccoon-icon.png"])
          ]
      , HH.a
          [ HP.href "https://www.linkedin.com/in/steven-maccoun-b4448b38/" ]
          [ HH.text "Linkedin"]
      , personalLinkView "https://github.com/smaccoun" "Github"

      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    ToggleState next -> do
      H.modify (\state -> { on: not state.on })
      pure next


personalLinkView :: forall t3 t4. String -> String -> HH.HTML t4 t3 
personalLinkView srcUrl displayText = 
   HH.a
    [ HP.href srcUrl ]
    [ HH.text displayText]
