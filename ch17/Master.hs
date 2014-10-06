{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

import HelloSub
import Yesod

data Master = Master {
    getHelloSub :: HelloSub
}

mkYesod "Master" [parseRoutes|
/ HomeR GET
/subsite SubsiteR HelloSub getHelloSub
|]

instance Yesod Master

getHomeR :: HandlerT Master IO Html
getHomeR = defaultLayout [whamlet|
    <h1>Welcome to the homepage
    <p>
        Feel free to visit the #
        <a href=@{SubsiteR SubHomeR}>subsite
        \ as well.
    |]

main :: IO ()
main = warp 3000 $ Master HelloSub
