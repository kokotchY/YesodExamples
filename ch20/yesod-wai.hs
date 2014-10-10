{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
import Data.Text (Text)
import Network.Wai (pathInfo)
import qualified Text.Blaze.Html5 as H
import Yesod.Core (HandlerT, Html, RenderRoute (..),
                   TypedContent, Value, Yesod,
                   YesodDispatch (..), getYesod,
                   notFound, object, provideRep,
                   selectRep, toWaiApp, yesodRunner,
                   (.=))
import Yesod.Core.Dispatch (warp)

data App = App {
    welcomeMessageHtml :: !Html,
    welcomeMessageText :: !Text,
    welcomeMessageJson :: !Value

}

instance Yesod App

instance RenderRoute App where
    data Route App = HomeR
        deriving (Show, Read, Eq, Ord)

    renderRoute HomeR = ( [], [] )

instance YesodDispatch App where
    yesodDispatch yesodRunnerEnv req sendResponse =
        let maybeRoute = case pathInfo req of
                [] -> Just HomeR
                _ -> Nothing
            handler =
                case maybeRoute of
                    Nothing -> notFound
                    Just HomeR -> getHomeR
        in yesodRunner handler yesodRunnerEnv maybeRoute req sendResponse

getHomeR :: HandlerT App IO TypedContent
getHomeR = do
    site <- getYesod
    selectRep $ do
        provideRep $ return $ welcomeMessageHtml site
        provideRep $ return $ welcomeMessageText site
        provideRep $ return $ welcomeMessageJson site

main :: IO ()
main = do
    warp 3000 App {
        welcomeMessageHtml = H.p "Welcome to Yesod!",
        welcomeMessageText = "Welcome to Yesod!",
        welcomeMessageJson = object [ "msg" .= ("Welcome to Yesod!" :: Text)]
    }
