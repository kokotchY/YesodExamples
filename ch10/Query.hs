{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Data.Time
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    firstName String
    lastName String
    age Int
    PersonName firstName lastName
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll
    personId <- insert $ Person "Michael" "Snoyman" 26
    maybePerson <- get personId
    case maybePerson of
        Nothing -> liftIO $ putStrLn "Just kidding, not really there"
        Just person -> liftIO $ print person
    maybePerson2 <- getBy $ PersonName "Michael" "Snoyman2"
    case maybePerson2 of
        Nothing -> liftIO $ putStrLn "Just kidding, not really there"
        Just person -> liftIO $ print person
