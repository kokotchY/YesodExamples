{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}
import Yesod

data MyWiki = MyWiki

mkYesod "MyWiki" [parseRoutes|
/bla SomePathR GET
|]



getSomePathR :: Handler Html
getSomePathR = defaultLayout [whamlet|Hello World! @{SomePathR}|]

instance Yesod MyWiki where
    approot = ApprootStatic "http://localhost:3000/wiki"

main :: IO ()
main = warp 3000 MyWiki
