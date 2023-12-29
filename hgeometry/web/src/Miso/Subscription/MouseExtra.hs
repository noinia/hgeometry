{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Miso.Subscription.MouseExtra
  ( relativeMouseSub
  ) where

import Control.Monad.IO.Class
import GHCJS.Marshal
import JavaScript.Object
import JavaScript.Object.Internal
import Miso
import Miso.FFI.Extra
import Miso.String (MisoString)
import HGeometry.Point
import Language.Javascript.JSaddle (JSVal)

--------------------------------------------------------------------------------

-- | Gets the mouse position relative to the element named by the first argument
relativeMouseSub          :: MisoString -> (Maybe (Point 2 Int) -> action) -> Sub action
relativeMouseSub elemId f = \sink -> do
    windowAddEventListener "mousemove" $
      \mouseEvent -> do
        elem'  <- getElementById elemId
        rect   <- getInnerRect elem'
        Just x <- fromJSVal =<< getProp "clientX" (Object mouseEvent)
        Just y <- fromJSVal =<< getProp "clientY" (Object mouseEvent)
        let mp = inDOMRect (Point2 x y) rect
        -- see https://stackoverflow.com/questions/10298658/mouse-position-inside-autoscaled-svg
        liftIO (sink $ f mp)

-- | A DOMRect
data DOMRect = DOMRect { top    :: {-# UNPACK #-} !Int
                       , left   :: {-# UNPACK #-} !Int
                       , width  :: {-# UNPACK #-} !Int
                       , height :: {-# UNPACK #-} !Int
                       } deriving (Show,Eq)

-- | Given a point and a rect, both in global coordinates, returns the relative
-- coordinates of the point inside the rect (if the point lies inside the rectangle) (with
-- respect to the top-left corner of the rectangle).
inDOMRect :: Point 2 Int -> DOMRect -> Maybe (Point 2 Int)
inDOMRect (Point2 x y) (DOMRect t l w h)
  | inInterval x l w && inInterval y t h = Just $ Point2 (x-l) (y-t)
  | otherwise                            = Nothing
  where
    inInterval q s len = s <= q && q <= s+len


-- -- | Get the bounding rectangle of the given element, relative to the viewport
-- getBoundingBoxOf        :: MisoString -> JSM DOMRect
-- getBoundingBoxOf elemId = do
--     elem' <- getElementById elemId
--     getBoundingRect elem'

-- | Get the bounding rectangle of the given element, relative to the viewport

getBoundingRect       :: JSVal -> JSM DOMRect
getBoundingRect elem' = do
    rect  <- getBoundingClientRect elem'
    Just l <- fromJSVal =<< getProp "left"    (Object rect)
    Just t <- fromJSVal =<< getProp "top"     (Object rect)
    Just w <- fromJSVal =<< getProp "width"   (Object rect)
    Just h <- fromJSVal =<< getProp "height"  (Object rect)
    pure $ DOMRect l t w h

-- | Get the inner rectangle of an element (i.e. without its border) relative to the
-- viewport.
getInnerRect       :: JSVal -> JSM DOMRect
getInnerRect elem' = do
  Just cl <- fromJSVal =<< getProp "clientLeft"   (Object elem')
  Just ct <- fromJSVal =<< getProp "clientTop"    (Object elem')
  Just cr <- fromJSVal =<< getProp "clientRight"  (Object elem')
  Just cb <- fromJSVal =<< getProp "clientBottom" (Object elem')
  DOMRect l t w h <- getBoundingRect elem'
  pure $ DOMRect (l-cl) (t-ct) (w - cr) (h - cb)


--  relativePositionIn          :: MisoString
--                             ->
-- relativePositionIn elemId f =
