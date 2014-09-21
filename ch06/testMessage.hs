{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}

import Yesod

data Message = Message

mkYesod "Message" [parseRoutes|
/ RootR GET
|]

instance Yesod Message where
    defaultLayout contents = do
        PageContent title headTags bodyTags <- widgetToPageContent contents
        mmsg <- getMessage
        withUrlRenderer [hamlet|
$doctype 5
<html>
    <head>
        <title>#{title}
        ^{headTags}
    <body>
        $maybe msg <- mmsg
            <div #message>#{msg}
        ^{bodyTags}
|]

getRootR = defaultLayout [whamlet|
<p>
    <a href=@{RootR}>RootR
    Youpie
|]

main = warp 3000 Message
