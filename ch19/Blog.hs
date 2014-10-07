{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ViewPatterns #-}
import Control.Monad.Logger
import Data.Text (Text, unpack)
import Database.Persist.Sqlite
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import Yesod
import qualified Data.Conduit.List as CL
import Data.Conduit (($=))

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
/raw Home2R GET
/stream StreamR GET
/esqueleto EsqueletoR GET
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

getHome2R :: Handler Html
getHome2R = do
    blogs <- runDB $ rawSql
        "SELECT ??, ?? \
        \FROM blog INNER JOIN author \
        \ON blog.author=author.id"
        []
    defaultLayout $ do
        setTitle "Blog posts"
        [whamlet|
            <ul>
                $forall (Entity blogid blog, Entity _ author) <- blogs
                    <li>
                        <a href=@{BlogR blogid}>
                            #{blogTitle blog} by #{authorName author}
        |]

getEsqueletoR :: Handler Html
getEsqueletoR = do
    blogs <- runDB
        $ E.select
        $ E.from $ \(blog `E.InnerJoin` author) -> do
            E.on $ blog ^. BlogAuthor E.==. author ^. AuthorId
            return
                ( blog   ^. BlogId
                , blog   ^. BlogTitle
                , author ^. AuthorName
                )

    defaultLayout $ do
        setTitle "Blog posts"
        [whamlet|
            <ul>
                $forall (E.Value blogid, E.Value title, E.Value name) <- blogs
                    <li>
                        <a href=@{BlogR blogid}>#{title} by #{name}
        |]

getStreamR :: Handler TypedContent
getStreamR = do
    let blogsSrc = E.selectSource
            $ E.from $ \(blog `E.InnerJoin` author) -> do
                E.on $ blog ^. BlogAuthor E.==. author ^. AuthorId
                return
                    ( blog ^. BlogId
                    , blog ^. BlogTitle
                    , author ^. AuthorName
                    )
    render <- getUrlRenderParams
    respondSourceDB typeHtml $ do
        sendChunkText "<html><head><title>Blog posts</title></head><body><ul>"
        blogsSrc $= CL.map (\(E.Value blogid, E.Value title, E.Value name) ->
            toFlushBuilder $
            [hamlet|
                <li>
                    <a href=@{BlogR blogid}>#{title} by #{name}
            |] render
            )
        sendChunkText "</ul></body></html>"

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
