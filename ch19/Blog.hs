{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ViewPatterns #-}
import Control.Monad.Logger
import Data.Text (Text, unpack)
import Database.Persist.Sqlite
import Yesod

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Author
    name Text
Blog
    author AuthorId
    title Text
    content Html
|]

data App = App {
    persistConfig :: SqliteConf,
    connPool :: ConnectionPool
}

instance Yesod App
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

mkYesod "App" [parseRoutes|
/ HomeR GET
/blog/#BlogId BlogR GET
|]

getHomeR :: Handler Html
getHomeR = do
    blogs <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Blog posts"
        [whamlet|
            <ul>
                $forall blogEntity <- blogs
                    ^{showBlogLink blogEntity}
        |]

showBlogLink :: Entity Blog -> Widget
showBlogLink (Entity blogid blog) = do
    author <- handlerToWidget $ runDB $ get404 $ blogAuthor blog
    [whamlet|
    <li>
        <a href=@{BlogR blogid}>
            #{blogTitle blog} by #{authorName author}
    |]

getBlogR :: BlogId -> Handler Html
getBlogR _ = error "todo"

main :: IO ()
main = do
    let conf = SqliteConf ":memory:" 1
    pool <- createPoolConfig conf
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll
        alice <- insert $ Author "Alice"
        bob <- insert $ Author "Bob"

        insert_ $ Blog alice "Alice's first post" "Hello World!"
        insert_ $ Blog bob "Bob's first post" "Hello World!"
        insert_ $ Blog alice "Alice's second post" "Goodbye World!"
    warp 3000 App {
        persistConfig = conf,
        connPool = pool
    }
