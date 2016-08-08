#!/usr/bin/env stack
-- stack runghc
{-
Coder Radio Challenge for Episode #216
https://www.reddit.com/r/CoderRadio/comments/4wa09e/216_coding_challenge/

Run with Docker Compose: ./start.bash

Run without Docker: ./jarjar_quotes.hs

Requirements:
  - GHC
  - runhaskell
  - yesod

This program creates a server side web application which stores
text into a Sqlite database. A form on the Home page is for adding new
quotes. The Random page is for showing a random quote.

I created this mostly by reading through the Yesod book.
http://www.yesodweb.com/book

-}

{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}

import Yesod
import Database.Persist.Sqlite
import Text.Hamlet                  (hamletFile)
import Data.Text                    (Text)
import Control.Applicative          ((<$>), (<*>))
import Text.Blaze                   (ToMarkup)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger         (runStderrLoggingT)

data JarJarQuotes = JarJarQuotes ConnectionPool

-- The Quote datatype to receive from QuoateForm
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Quote
  text Text
  deriving Show
SiteVariable
  key String
  value String
  VarKey key
  deriving Show
|]
--data Quote = Quote { quoteText :: Text } deriving Show

-- Routes
mkYesod "JarJarQuotes" [parseRoutes|
/               HomeR   GET POST
/about          AboutR  GET
-- /random         RandomR GET
/quote/#QuoteId QuoteR  GET
|]

instance Yesod JarJarQuotes where
  -- Default layout for each page
  defaultLayout widget = do
    mmsg <- getMessage
    pc <- widgetToPageContent widget
    withUrlRenderer $(hamletFile "default-layout.hamlet")

instance YesodPersist JarJarQuotes where
  type YesodPersistBackend JarJarQuotes = SqlBackend
  runDB action = do
    JarJarQuotes pool <- getYesod
    runSqlPool action pool

instance RenderMessage JarJarQuotes FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Quote form
quoteForm :: Html -> MForm Handler (FormResult Quote, Widget)
quoteForm = renderDivs $ Quote <$> areq textField "Quote" Nothing

quotesListWidget :: [Entity Quote] -> Widget
quotesListWidget quotes = do
  toWidget [hamlet|
  <ul>
    $forall Entity quoteid quote <- quotes
      <li>#{quoteText quote}
  |]

addQuoteWidget :: (Text.Blaze.ToMarkup a) =>  Widget -> a -> Widget
addQuoteWidget w e = [whamlet|
<p>
  Add words of wisdom from Jar Jar Binks
  <form method=post action=@{HomeR} enctype=#{e}>
    ^{w}
    <button>Submit
|]

homeListWidget :: (Text.Blaze.ToMarkup a) =>  Widget -> a ->
  [Entity Quote] -> Widget
homeListWidget w e qs = addQuoteWidget w e >> quotesListWidget qs

-- Home page post handler
postHomeR :: Handler Html
postHomeR = do
  ((result, widget), enctype) <- runFormPost quoteForm
  case result of
    FormSuccess quote -> do
      quoteId <- runDB $ insert quote
      quotes <- runDB $ selectList [] [Asc QuoteId]
      defaultLayout $ do
        [whamlet|<p>Added '#{quoteText quote}'|]
        homeListWidget widget enctype quotes
    _ -> do
      quotes <- runDB $ selectList [] [Asc QuoteId]
      defaultLayout $ do
      [whamlet|<p>Invalid input, let's try again.|]
      homeListWidget widget enctype quotes

-- Home page
getHomeR :: Handler Html
getHomeR = do
  quotes <- runDB $ selectList [] [Asc QuoteId]
  (widget, enctype) <- generateFormPost quoteForm
  defaultLayout $ do
    homeListWidget widget enctype quotes

-- Quote page
getQuoteR :: QuoteId -> Handler Html
getQuoteR quoteId = do
  quote <- runDB $ get404 quoteId
  --return $ show quote
  defaultLayout [whamlet|#{quoteText quote}|]

-- Random page
-- getRandomR :: Handler Html
-- getRandomR = defaultLayout [whamlet|TODO: Show a random quote here|]

-- About page
getAboutR :: Handler Html
getAboutR = defaultLayout [whamlet|Small web app for saving your favorite Jar Jar Binks quotes!|]

openConnectionCount :: Int
openConnectionCount = 10

-- Returns current migration version
-- This function is for adding default data one time only
runCustomMigration = do
  versions <- selectList [SiteVariableKey ==. "Version"] []
  case versions of
    [] -> do
      insert $ SiteVariable "Version" "1"
      addDefaultData
    (version:_) -> return $ 1
  where
    addDefaultData = do
      insert $ Quote "Mesa called Jar-Jar Binks. Mesa your humble servant."
      insert $ Quote "My forgotten, da Bosses will do terrible tings to me TERRRRRIBLE is me going back der!"
      insert $ Quote "Ohh, maxi big da Force. Well dat smells stinkowiff."
      return 1


main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test.db3"
  openConnectionCount $ \pool -> liftIO $ do
    putStrLn "Starting database migration ..."
    runResourceT $ flip runSqlPool pool $ do
      runMigration migrateAll
      runCustomMigration
    putStrLn "View website by going to http://localhost:3000/ in your web browser"
    putStrLn "Starting web server on port 3000 ..."
    warp 3000 $ JarJarQuotes pool
