{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Data.Time
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    created UTCTime default=CURRENT_TIME
    language String default='Haskell'
    country String "default='El Salvador'"
    deriving Show
|]

peoples time language country = [
    Person "bla" (Just 26) time language country,
    Person "blo" Nothing time language country,
    Person "ha" (Just 43) time language country,
    Person "hihi" (Just 50) time language country,
    Person "Adam" (Just 34) time language country]

main :: IO ()
main = runSqlite ":memory:" $ do
    time <- liftIO getCurrentTime
    runMigration migrateAll
    mapM_ insert $ peoples time "Haskell" "Belgium"
    people <- selectList ([] :: [Filter Person]) []
    liftIO $ mapM_ (print . entityVal) people
