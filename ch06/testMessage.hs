{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}

import Yesod
import Data.Time (getCurrentTime)

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

getRootR :: Handler Html
getRootR = do
    now <- liftIO getCurrentTime
    setMessage $ toHtml $ "You previously visited at: " ++ show now
    defaultLayout [whamlet|<p>Try refreshing|]

main :: IO ()
main = warp 3000 Message
