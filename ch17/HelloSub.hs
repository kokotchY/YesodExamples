{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module HelloSub (
    module HelloSub.Data,
    module HelloSub
    ) where

import HelloSub.Data
import Yesod

getSubHomeR :: Yesod master => HandlerT HelloSub (HandlerT master IO) Html
getSubHomeR = lift $ defaultLayout [whamlet|Welcome to the subsite!|]

instance Yesod master => YesodSubDispatch HelloSub (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesHelloSub)
