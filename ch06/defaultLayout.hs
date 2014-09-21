{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}

import Yesod

data ModifiedDefaultLayout = ModifiedDefaultLayout

mkYesod "ModifiedDefaultLayout" [parseRoutes|
/ RootR GET
|]

instance Yesod ModifiedDefaultLayout where
    defaultLayout contents = do
        PageContent title headTags bodyTags <- widgetToPageContent $ do
            toWidget [cassius|
#body
    font-family: sans-serif
#wrapper
    width: 760ppx
    margin: 0 auto
    color: red
|]
            toWidget contents
        withUrlRenderer [hamlet|
$doctype 5
<html>
    <head>
        <title>#{title}
        ^{headTags}
    <body>
        <div id="wrapper">
            ^{bodyTags}
|]

getRootR = defaultLayout [whamlet|
<p>
    <a href=@{RootR}>RootR
    Youpie
|]

main = warp 3000 ModifiedDefaultLayout
