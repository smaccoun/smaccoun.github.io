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
import CSS (Abs, Size, Rel, display, flex, fontSize, margin, px, height, vh)
import CSS.Flexbox (column, flexDirection, justifyContent, alignItems, spaceBetween)
import CSS.Common (center)

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
    HH.div
      [style 
         do display flex
            justifyContent center
            alignItems center
            flexDirection column
            height (vhS 60)
      ]
      [ HH.div
          [ HE.onClick (HE.input_ ToggleState)
          , style do display flex
                     justifyContent center
                     flexDirection column
          ]
          [ if not state.on
              then (HH.h1_ [HH.text "Welcome!"])
              else (HH.img [HP.src "../images/raccoon-icon.png"])
          , viewPersonalLinks
          ]
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

viewPersonalLinks :: forall t74 t75. H.HTML t75 t74 
viewPersonalLinks =
  HH.div
    [ style 
       do display flex
          justifyContent spaceBetween
    ]
    [ personalLinkIcon linkedinIcon
    , personalLinkIcon githubLinkIcon
    ]

personalLinkIcon :: forall t3 t4. PersonalLinkIcon -> HH.HTML t4 t3 
personalLinkIcon {srcUrl, displayText, className'} = 
  HH.a
    [ HP.href srcUrl, style do margin (pxS 0) (pxS 10) (pxS 0) (pxS 10) ]
    [ HH.i 
       [HP.class_ (H.ClassName className'), style do fontSize (pxS 50)] 
       []
    ]

pxS :: Int -> Size Abs
pxS i = 
  px $ toNumber i


vhS :: Int -> Size Rel
vhS i = 
  vh $ toNumber i


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
