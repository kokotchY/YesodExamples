{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
import Data.Text (Text)
import Network.Wai (pathInfo)
import Control.Applicative ((<$>))
import Yesod

data App = App

instance Yesod App

instance RenderRoute App where
    data Route App = HomeR | FibR Int
        deriving (Show, Read, Eq, Ord)

    renderRoute HomeR = ( [], [] )
    renderRoute (FibR i) = (["fib", toPathPiece i], [])

instance YesodDispatch App where
    yesodDispatch yesodRunnerEnv req sendResponse =
        let maybeRoute = parseRoute' (pathInfo req)
            handler =
                case maybeRoute of
                    Nothing -> notFound
                    Just HomeR -> getHomeR
                    Just (FibR i) -> getFibR i
        in yesodRunner handler yesodRunnerEnv maybeRoute req sendResponse

parseRoute' [] = Just HomeR
parseRoute' ["fib", i] = FibR <$> fromPathPiece i
parseRoute' _ = Nothing

getHomeR = redirect (FibR 1)

fibs :: [Int]
fibs = 0 : scanl (+) 1 fibs

getFibR i = return $ show $ fibs !! i

main :: IO ()
main = do
    warp 3000 App
