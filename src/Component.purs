module Component where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Array (mapMaybe, concatMap, singleton)
import Data.Either (Either)
import Data.Foldable (foldMap)
import Data.StrMap as SM
import Data.String (joinWith)
import Data.Tuple (Tuple(..))

import CSS.Property (Key, Value)
import CSS.Render (collect)
import CSS.Stylesheet (CSS, Rule(..), runS)
import CSS (paddingLeft, px, fontSize)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core as HC

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
      , personalLinkIcon linkedinIcon
      , personalLinkIcon githubLinkIcon

      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    ToggleState next -> do
      H.modify (\state -> { on: not state.on })
      pure next


type PersonalLinkIcon = 
    { srcUrl :: String
    , displayText :: String
    , className' :: String  
    }

githubLinkIcon :: PersonalLinkIcon
githubLinkIcon = 
    { srcUrl : "https://github.com/smaccoun"
    , displayText : "Github"
    , className' : "fa fa-github"
    }

linkedinIcon :: PersonalLinkIcon
linkedinIcon = 
    { srcUrl : "https://www.linkedin.com/in/steven-maccoun-b4448b38/"
    , displayText : "Linkedin"
    , className' : "fa fa-linkedin"
    }


personalLinkIcon :: forall t3 t4. PersonalLinkIcon -> HH.HTML t4 t3 
personalLinkIcon {srcUrl, displayText, className'} = 
  HH.a
    [ HP.href srcUrl, style do paddingLeft (pxS 10) ]
    [ HH.i 
       [HP.class_ (H.ClassName className'), style do fontSize (pxS 50)] 
       []
    ]
  where
    pxS i = px $ toNumber i



style ∷ ∀ i r. CSS → HP.IProp (style ∷ String|r) i
style =
  HP.attr (HC.AttrName "style")
    <<< toString
    <<< rules
    <<< runS
  where
  toString ∷ SM.StrMap String → String
  toString = joinWith "; " <<< SM.foldMap (\key val → [ key <> ": " <> val])

  rules ∷ Array Rule → SM.StrMap String
  rules rs = SM.fromFoldable properties
    where
    properties ∷ Array (Tuple String String)
    properties = mapMaybe property rs >>= collect >>> rights

  property ∷ Rule → Maybe (Tuple (Key Unit) Value)
  property (Property k v) = Just (Tuple k v)
  property _              = Nothing

  rights ∷ ∀ a b. Array (Either a b) → Array b
  rights = concatMap $ foldMap singleton
