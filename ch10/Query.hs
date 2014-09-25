{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Data.Time
import Control.Monad.IO.Class (liftIO)
import Text.Printf

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    firstName String
    lastName String
    age Int
    PersonName firstName lastName
    deriving Show
|]

peoples = [
    Person "bla" "bli" 26,
    Person "blo" "blu" 29,
    Person "ha" "hi" 43,
    Person "hihi" "hoho" 50,
    Person "Adam" "bla" 34]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll
    personId <- insert $ Person "Michael" "Snoyman" 26
    mapM_ insert peoples
    displayMaybeForQuery "One guy with get" $ get personId
    displayMaybeForQuery "Ony guy with getBy" $ getBy $ PersonName "Michael" "Snoyman2"
    displayListForSelect "List of guys" $ selectList [PersonAge >. 25, PersonAge <=. 30] []
    displayListForSelect "Another list of guys" $ selectList ([PersonAge >. 25, PersonAge <=. 30]
        ||. [PersonFirstName /<-. ["Adam", "Bonny"]]
        ||. ([PersonAge ==. 50] ||. [PersonAge ==. 60])
        ) []

    displayNumberPage "Paginated list (page %d)" resultsForPage

    update personId [PersonAge =. 27]
    displayMaybeForQuery "Modified guy" $ get personId
    haveBirthday personId
    displayMaybeForQuery "Birthday guy" $ get personId

    updateWhere [PersonFirstName ==. "bla"] [PersonAge *=. 2]
    displayMaybeForQuery "Long day for bla" $ getBy $ PersonName "bla" "bli"

    replace personId $ Person "John" "Doe" 20
    displayMaybeForQuery "Replaced guy by a John Doe" $ get personId

    deleteWhere [PersonAge >=. 30]
    displayListForSelect "All guys" allGuysWithoutOptions

allGuys = selectList ([] :: [Filter Person])

allGuysWithoutOptions = allGuys []

displayNumberPage message func = mapM_ (\x -> displayListForSelect (formattedMessage x) (func x)) [1..4]
    where
        formattedMessage x = printf message x


displayMaybeForQuery message query = do
    liftIO $ putStrLn message
    maybePerson <- query
    case maybePerson of
        Nothing -> liftIO $ putStrLn "Just kidding, not really there"
        Just person -> liftIO $ print person

displayListForSelect message select = do
    liftIO $ putStrLn message
    people <- select
    liftIO $ mapM_ ((\x -> putStrLn $ "\t-" ++ show x) . entityVal) people


resultsForPage pageNumber = do
    let resultsPerPage = 3
    selectList
        [PersonAge >=. 18]
        [
            Desc PersonAge,
            Asc PersonLastName,
            Asc PersonFirstName,
            LimitTo resultsPerPage,
            OffsetBy $ (pageNumber - 1) * resultsPerPage
        ]

haveBirthday personId = update personId [PersonAge +=. 1]
