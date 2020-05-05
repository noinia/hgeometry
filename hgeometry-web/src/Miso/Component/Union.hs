{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE OverloadedStrings          #-}
module Miso.Component.Union where


import           Data.Vinyl
import           Data.Vinyl.CoRec
import qualified Data.Vinyl.Functor as VF
import           Data.Vinyl.Notation
import           Data.Vinyl.TypeLevel
import           Miso hiding (update, view)

--------------------------------------------------------------------------------

type Action = CoRec VF.Identity

--------------------------------------------------------------------------------

type family ModelOf a :: *

newtype ActionHandler a = ActionHandler (a -> Effect a (ModelOf a))

update' :: Rec VF.Identity as -> Rec ActionHandler as -> Effect (CoRec ActionHandler as) (Rec VF.Identity as)
update' = undefined


class HasActionHandler model a where
  handleAction :: model -> a -> Effect action model

update   :: forall model as action. RPureConstrained (HasActionHandler model) as
         => model -> Action as -> Effect action model
update m = onCoRec @(HasActionHandler model) (handleAction m . VF.getIdentity)



view :: ()
     => (Action as -> action)
     -> model
     -> View action
view = undefined


viewWithAction :: a ∈ as => (Action as -> action) -> model -> View action
viewWithAction = undefined

attrsWithAction           :: a ∈ as => proxy as -> (a -> action) -> model -> [Attribute action]
attrsWithAction _ embed m = undefined

--------------------------------------------------------------------------------





-- view            :: forall model as action.
--                    ( RecApplicative as, RMap as
--                    , as ⊆ CanvasCapabilities
--                    )
--                 => (CanvasAction cs -> action)
--                 -> ICanvas r
--                 -> [Attribute action] -> [View action] -> View action
-- view f m ats vs = staticCanvas_ (m^.canvas)
--                                 (mconcat [
--                                     [ onMouseLeave (f (coRec MouseLeave))                       ]
--                                   , supports (Proxy @Pan)  [onMouseDown $ f (coRec StartPan)    ]
--                                   , supports (Proxy @Pan)  [onMouseUp   $ f (coRec StopPan)     ]
--                                   , supports (Proxy @Zoom) [onWheel     $ f . coRec . ZoomAction]
--                                   , ats
--                                   ]
--                                 ) vs
--   where
--     capabilities :: Rec Support AllCanvasActions
--     capabilities = construct (Proxy @ cs)

--     -- mouseDown = supports (Proxy @Pan)  [onMouseDown $ f (coRec StartPan)]
--     -- mouseUp   = supports (Proxy @Pan)  [onMouseUp   $ f (coRec StopPan)]
--     -- wheel     = supports (Proxy @Zoom) [onWheel     $ f . coRec . ZoomAction]

--     -- TODO: the implementation of the [a] need that (c elem cs). Somehow make sure that
--     -- we can use that info
--     supports     :: (c ∈ AllCanvasActions) => proxy c -> [a] -> [a]
--     supports _ l = case rget capabilities :: Support c of
--                      Support   -> l
--                      NoSupport -> []

-- data Support (a :: k) = NoSupport | Support deriving (Show,Eq)


-- construct   :: forall pxy as bs. (RecApplicative as, RecApplicative bs, RMap as, RMap bs, as ⊆ bs)
--             => pxy as -> Rec Support bs
-- construct _ = rmap (fromMaybe NoSupport . VF.getCompose) . rdowncast $ r
--   where
--     r :: Rec Support as
--     r = rpure Support
