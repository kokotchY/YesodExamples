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
    displayMaybeForQuery "One guy with get" $ get personId
    displayMaybeForQuery "Ony guy with getBy" $ getBy $ PersonName "Michael" "Snoyman2"
    displayListForSelect "List of guys" $ selectList [PersonAge >. 25, PersonAge <=. 30] []
    displayListForSelect "Another list of guys" $ selectList ([PersonAge >. 25, PersonAge <=. 30]
        ||. [PersonFirstName /<-. ["Adam", "Bonny"]]
        ||. ([PersonAge ==. 50] ||. [PersonAge ==. 60])
        ) []
    displayListForSelect "Paginated list" $ resultsForPage 1

displayMaybeForQuery message query = do
    liftIO $ putStrLn message
    maybePerson <- query
    case maybePerson of
        Nothing -> liftIO $ putStrLn "Just kidding, not really there"
        Just person -> liftIO $ print person

displayListForSelect message select = do
    liftIO $ putStrLn message
    people <- select
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
