{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
import Network.HTTP.Types (status200)
import Network.Wai (pathInfo)
import Network.Wai.Handler.Warp (run)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import qualified Text.Blaze.Html5 as H
import Yesod.Core (HandlerT, Html, RenderRoute (..), Yesod, YesodDispatch (..), getYesod, notFound, toWaiApp, yesodRunner)

data App = App {
    welcomeMessage :: !Html
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

getHomeR :: HandlerT App IO Html
getHomeR = do
    site <- getYesod
    return $ welcomeMessage site

main :: IO ()
main = do
    let welcome = H.p "Welcome to Yesod!"
    waiApp <- toWaiApp App {
        welcomeMessage = welcome
    }
    run 3000 waiApp
