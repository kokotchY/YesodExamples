{-# LANGUAGE OverloadedStrings, RecordWildCards, QuasiQuotes, TemplateHaskell, TypeFamilies #-}
import Yesod
import Data.Text (Text)

data Person = Person {
    name :: Text,
    age :: Int
}

instance ToJSON Person where
    toJSON Person {..} = object
        [ "name" .= name,
          "age" .= age
        ]

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Value
getHomeR = returnJson $ Person "Bla" 21

main :: IO ()
main = warp 3000 App
