{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Data.Time
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    deriving Show
Store
    name String
PersonStore
    personId PersonId
    storeId StoreId
    UniquePersonStore personId storeId
Car
    ownerId PersonId
    name String
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll
    bruce <- insert $ Person "Bruce Wayne"
    michael <- insert $ Person "Michael"
    target <- insert $ Store "Target"
    gucci <- insert $ Store "Gucci"
    sevenEleven <- insert $ Store "7-11"
    insert $ Car bruce "Bat Mobile"
    insert $ Car bruce "Porsche"

    insert $ PersonStore bruce gucci
    insert $ PersonStore bruce sevenEleven

    insert $ PersonStore michael target
    insert $ PersonStore michael sevenEleven

    cars <- selectList [CarOwnerId ==. bruce] []
    liftIO $ displayList cars

    store <- selectList [PersonId ==. bruce] []
    liftIO $ displayList store

displayList :: (Show a, PersistEntity a) => [Entity a] -> IO ()
displayList = mapM_ ((\x -> putStrLn $ "\t- " ++ show x) . entityVal)
