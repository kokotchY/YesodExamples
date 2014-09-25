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
    liftIO $ putStrLn "One guy with get"
    case maybePerson of
        Nothing -> liftIO $ putStrLn "Just kidding, not really there"
        Just person -> liftIO $ print person
    maybePerson2 <- getBy $ PersonName "Michael" "Snoyman2"
    liftIO $ putStrLn "One guy with getBy"
    case maybePerson2 of
        Nothing -> liftIO $ putStrLn "Just kidding, not really there"
        Just person -> liftIO $ print person
    liftIO $ putStrLn "List of guys"
    people <- selectList [PersonAge >. 25, PersonAge <=. 30] []
    liftIO $ print people
    liftIO $ putStrLn "Another list of guys"
    people <- selectList ([PersonAge >. 25, PersonAge <=. 30]
        ||. [PersonFirstName /<-. ["Adam", "Bonny"]]
        ||. ([PersonAge ==. 50] ||. [PersonAge ==. 60])
        ) []
    liftIO $ print people
    liftIO $ putStrLn "Paginated list"
    people <- resultsForPage 1
    liftIO $ print people

resultsForPage pageNumber = do
    let resultsPerPage = 10
    selectList
        [PersonAge >=. 18]
        [
            Desc PersonAge,
            Asc PersonLastName,
            Asc PersonFirstName,
            LimitTo resultsPerPage,
            OffsetBy $ (pageNumber - 1) * resultsPerPage
        ]
