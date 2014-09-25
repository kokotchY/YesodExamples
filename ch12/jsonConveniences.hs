{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
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

main :: IO ()
main = L.putStrLn $ encode $ Person "Bla" 21