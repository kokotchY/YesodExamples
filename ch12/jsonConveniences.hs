{-# LANGUAGE OverloadedStrings, RecordWildCards, QuasiQuotes, TemplateHaskell, TypeFamilies #-}
import Yesod
import Data.Text (Text)

data Person = Person {
    name :: Text,
    age :: Int
} deriving Show

instance ToJSON Person where
    toJSON Person {..} = object
        [ "name" .= name,
          "age" .= age
        ]

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/second Home2R GET
/third Home3R GET
|]

instance Yesod App

mimeType :: ContentType
mimeType = "text/haskell-show"

getHomeR :: Handler Value
getHomeR = returnJson $ Person "Bla" 21

getHome2R :: Handler TypedContent
getHome2R = selectRep $ do
    provideRep $ return [shamlet|
    <p>Hello, my name is #{name} and I am #{age} years old.
    |]
    provideJson personJson
    where
        person = Person "Bla" 21
        personJson@Person {..} = person

getHome3R :: Handler TypedContent
getHome3R =
    return $ TypedContent mimeType $ toContent $ show person
    where
        person = Person "Michael" 28

main :: IO ()
main = warp 3000 App
