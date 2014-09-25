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
/second Home2R GET
|]

instance Yesod App

getHomeR :: Handler Value
getHomeR = returnJson $ Person "Bla" 21

getHome2R :: Handler TypedContent
getHome2R = selectRep $ do
    provideRep $ return [shamlet|
    <p>Hello, my name is #{name} and I am #{age} years old.
    |]
    provideJson person
    where
        person@Person {..} = Person "Bla" 21

main :: IO ()
main = warp 3000 App
