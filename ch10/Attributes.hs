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
    deriving Show
|]

peoples time = [
    Person "bla" (Just 26) time,
    Person "blo" Nothing time,
    Person "ha" (Just 43) time,
    Person "hihi" (Just 50) time,
    Person "Adam" (Just 34) time]

main :: IO ()
main = runSqlite ":memory:" $ do
    time <- liftIO getCurrentTime
    runMigration migrateAll
    mapM_ insert $ peoples time
    people <- selectList ([] :: [Filter Person]) []
    liftIO $ mapM_ (print . entityVal) people
