{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses #-}
import Yesod
import Data.Text

data App = App

mkYesod "App" [parseRoutes|
/ RootR GET
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getRootR :: Handler Html
getRootR = defaultLayout [whamlet|
    <p>Test
    |]

main :: IO ()
main = warp 3000 App
