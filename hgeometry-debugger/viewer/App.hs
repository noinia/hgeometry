{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module App(main) where

import           Control.Monad.State (evalState, gets, modify)
import           Control.Lens hiding (view, element)
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NonEmpty
-- import           GHC.TypeNats
-- import           HGeometry.ConvexHull.GrahamScan
-- import           HGeometry.Ext
-- import           HGeometry.Miso.OrphanInstances ()
-- import           HGeometry.Miso.Svg
-- import           HGeometry.Miso.Svg.Canvas (Canvas, blankCanvas, mouseCoordinates)
-- import qualified HGeometry.Miso.Svg.Canvas as Canvas
-- import           HGeometry.Number.Real.Rational
-- import           HGeometry.Point
-- import           HGeometry.Polygon.Convex
import           Miso hiding (text_)
import           Miso.String (ToMisoString(..), toLower)
import           Miso.CSS (style_, border)
import           Miso.Svg hiding (style_, script_)
import           Miso.Svg.Property hiding (path_)
import qualified Miso.Svg.Property as SvgProp
import           Miso.Html.Element hiding (style_)
import           Miso.Html.Property
import qualified Miso.Html.Property as Prop
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Proxy
import           Servant.Miso.Client
import           Servant.API ((:<|>)(..), PlainText)
import           Debugger.API
import qualified Miso.JSON
import           Text.Pretty.Simple (defaultOutputOptionsLightBg)
import           Text.Pretty.Simple.Internal (Annotation (..), layoutStringAbstract)
import           Prettyprinter.Render.Util.SimpleDocTree (SimpleDocTree (..), treeForm)
import           Text.Pretty.Simple (OutputOptions(..))
import           Miso.Html.Parse
-- import           SideBar

--------------------------------------------------------------------------------

outputOptions :: OutputOptions
outputOptions = defaultOutputOptionsLightBg
                 { outputOptionsPageWidth     = 120
                 , outputOptionsCompact       = True
                 , outputOptionsCompactParens = True
                 }

type R = Double

data Item = Item { _content   :: String
                 , _drawing   :: Drawing
                 , _isVisible :: Bool
                 }
            deriving (Show,Eq)

makeLenses ''Item


data Model = Model { _drawings     :: Map LayerName (Seq Item)
                   , _currentLayer :: Maybe LayerName
                   }
           deriving (Eq)

makeLenses ''Model

initialModel :: Model
initialModel = Model mempty (Just "myLayer")

-- dummy = Map.fromList [ ("myLayer", Seq.fromList [Item "Item \"bar\" (Drawing \"bar\") True" (Drawing "foodrawing") True])
--                      , ("bar", Seq.fromList [ Item "5" (Drawing "bar") True
--                                             , Item "Just (5,\"True\")" (Drawing "bazz") False
--                                             ])
--                      ]

instance MimeRender PlainText String where
  type MimeRenderType String = IO JSVal
  mimeRender Proxy = toJSVal

instance MimeUnrender PlainText String where
  type MimeUnrenderType String = JSVal
  mimeUnrenderType Proxy Proxy = TEXT
  mimeUnrender Proxy = fmap pure . fromJSValUnchecked

deriving instance ToJSVal Drawing


--------------------------------------------------------------------------------

data Action = AcquireData
            | LoadDrawing Drawings
            | ClearLayer LayerName
            | FetchError (Response MisoString)
            | Clear
            | SetCurrentLayer LayerName
            | ToggleVisibility (LayerName, Int)
                -- toggle the visibility of the item indicated by the given location

mkItem       :: (String, Drawing) -> Item
mkItem (s,d) = Item s d True

updateModel   :: Action -> Effect parent props Model Action
updateModel = \case
  AcquireData      -> withSink $ \sink ->
    clientDrawing (sink . LoadDrawing . body) (sink . FetchError)
  LoadDrawing ds   -> drawings .= fmap (fmap mkItem) ds
  FetchError err   -> io_ $ consoleError $ ms (show $ errorMessage err)
  Clear            -> pure ()
  ClearLayer layer -> pure ()

  SetCurrentLayer layer      -> currentLayer ?= layer
  ToggleVisibility (layer,i) -> drawings.ix layer.traverse.isVisible %= not

-- TODO: Add the types of these things;
clientDrawing
  :<|> clientDrawLayer
  :<|> clientClearLayer
  :<|> clientClear
  = toClient baseUrl (Proxy @API)





baseUrl :: MisoString
baseUrl = toMisoString $ "http://" <> defaultHost <> ":" <> show defaultPort
--------------------------------------------------------------------------------0

viewModel         :: props -> Model -> View Model Action
viewModel _ model = div_ []
    [ svg_ [ width_  "1024px"
             , height_ "700px"
             ,   style_ [border "1px solid black"]
             ]
             content'
    , div_ [ class_ "flex columns-2"]
           [ theLayers
           , itemsOnTheLayer
           ]
    , flowBite
    ]
  where
    theLayers = ul_ [ class_ "w-48 text-sm font-medium text-heading bg-neutral-primary-soft border border-default rounded-base"
                    ]
                    [ li_ [ class_ "w-full px-4 py-2 border-b border-default cursor-pointer"
                          , classes_ [ "bg-blue-500"
                                     | model^.currentLayer == Just layer
                                     ]
                          , onClick $ SetCurrentLayer layer
                          ]
                          [ text . ms $ layer
                          ]
                    | layer <- model^..drawings.ifolded.asIndex
                    ]
    itemsOnTheLayer = ul_ [ class_ "flex-1"]
                          [ li_ [ class_ "w-full px-4 py-2 border-b border-default cursor-pointer"
                                , style_ [ border "1px solid blue"
                                         | item^.isVisible
                                         ]
                                , onClick $ ToggleVisibility (currentLayer,i)
                                ]
                                [ pPrintStringHtml [] outputOptions $
                                  item^.content
                                ]
                          | currentLayer <- model^..currentLayer.folded
                          , (i, item)    <- model^..drawings.ix currentLayer.ifolded.withIndex
                          ]




-- dl_ [ class_ "max-w-md text-heading divide-y divide-default"]
      --                 --      drawingItems





    -- mainContent = div_ [class_ "p-4 sm:ml-64"]
    --   [
    --   , div_ [class_ "flex items-center justify-center h-48 rounded-base bg-neutral-secondary-soft mb-4" ]
    --          [ text "Layers"
    --          ]
    --   ]



      -- [ div_ [ class_ "p-4 border-1 border-default border-dashed rounded-base"]
      --        [ div_ [class_ "flex items-center justify-center h-48 rounded-base bg-neutral-secondary-soft mb-4" ]
      --               [ p_ [class_ "text-fg-disabled"]
      --                    [ svg_ [ width_  "1024px"
      --                           , height_ "700px"
      --                           ,   style_ [border "1px solid black"]
      --                           ]
      --                          content'
      --               -- , layers
      --                 -- dl_ [ class_ "max-w-md text-heading divide-y divide-default"]
      --                 --      drawingItems

      --                   ]

      --               ]
      --        ]
      -- ]

    content' = concat [ rawSVG d
                      | Drawing d <- model^..drawings.folded.folded.filteredBy isVisible.drawing
                      ]


    -- layers = div_ [ class_ "grid grid-cols-3 gap-4 mb-4" ]
    --               [ left
    --               , right
    --               ]
    --   where
    --     left = div_ [ class_ "flex items-center justify-center h-24 rounded-base bg-neutral-secondary-soft"
    --                 ]
    --                 [ p_ [class_ "text-fg-disabled"]
    --                      [ text "layers go here " ]
    --                 ]

    --     right  = div_ [ class_ "flex items-center justify-center h-24 rounded-base bg-neutral-secondary-soft"
    --                 ]
    --                 [ p_ [class_ "text-fg-disabled"]
    --                      [ text "more info goes here " ]
    --                 ]

    -- drawingItems = [ div_ [class_ "flex flex-col pb-3"]
    --                       [ dt_ [ class_ "mb-1 text-body"
    --                             ]
    --                             [ text $ ms layer
    --                             ]
    --                       , dd_ [ class_ "text-lg font-medium"]
    --                             [ text $ ms $ show items ]
    --                       ]
    --                | (layer,items) <- model^..drawings.ifolded.withIndex
    --                ]





  -- div_ []
  --                      [ svg_ [ width_  "1024px"
  --                             , height_ "700px"
  --                             , style_ [border "1px solid black"]
  --                             ]
  --                             content'
  --                      , dl_ [ class_ "max-w-md text-heading divide-y divide-default"]
  --                            drawingItems
  --                      , flowBite
  --                      ]



flowBite :: View model action
flowBite = script_ [src_ "https://cdn.jsdelivr.net/npm/flowbite@4.0.1/dist/flowbite.min.js"
                   ] ""



-- <dl class="max-w-md text-heading divide-y divide-default">
--     <div class="flex flex-col pb-3">
--         <dt class="mb-1 text-body">Email address</dt>
--         <dd class="text-lg font-medium">yourname@flowbite.com</dd>
--     </div>
--     <div class="flex flex-col py-3">
--         <dt class="mb-1 text-body">Home address</dt>
--         <dd class="text-lg font-medium">92 Miles Drive, Newark, NJ 07103, California, USA</dd>
--     </div>
--     <div class="flex flex-col pt-3">
--         <dt class="mb-1 text-body">Phone number</dt>
--         <dd class="text-lg font-medium">+00 123 456 789 / +12 345 678</dd>
--     </div>
-- </dl>


--------------------------------------------------------------------------------
-- * Stealing this from the pretty-simple web demo

data ParensLevel
    = Parens0
    | Parens1
    | Parens2
    deriving (Eq, Show, Bounded, Enum)

pPrintStringHtml          :: [Attribute action] -> OutputOptions -> String -> View model action
pPrintStringHtml as opts = renderHtml as . treeForm . annotateWithIndentation . layoutStringAbstract opts
  where
    annotateWithIndentation =
        flip evalState (prev Parens0) . traverse (\ann ->
            (++ [Class "annotation", toClassName @Annotation ann]) <$> case ann of
                Open -> modify next *> g
                Close -> g <* modify prev
                Comma -> g
                _ -> pure []
                                                 )
      where
        g = gets (pure . toClassName @ParensLevel)
        toClassName :: Show a => a -> Class
        toClassName = Class . toLower . ms . show

newtype Class = Class {unClass :: MisoString}

renderHtml :: [Attribute action] -> SimpleDocTree [Class] -> View model action
renderHtml as =
    let go = \case
            STEmpty -> [text ""]
            STChar c -> [text $ ms $ Text.singleton c]
            STText _ t -> [text $ ms t]
            STLine i -> [br_ [], text $ ms $ Text.replicate i $ Text.singleton ' ']
            STAnn cs content -> [span_ [classes_ $ map unClass cs] $ go content]
            STConcat contents -> foldMap go contents
     in pre_ as . go

-- | Safe, wrapping around, as in 'relude'
next, prev :: (Eq a, Bounded a, Enum a) => a -> a
next e
    | e == maxBound = minBound
    | otherwise = succ e
prev e
    | e == minBound = maxBound
    | otherwise = pred e

--------------------------------------------------------------------------------

main :: IO ()
main = startApp defaultEvents $
         (component initialModel updateModel viewModel)
           { mount = Just AcquireData
           }
