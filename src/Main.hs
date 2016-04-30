{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap
import Snap.Blaze (blaze)

import Text.Hamlet
import Text.Blaze.Html5 (toHtml)
import Text.Digestive
import Text.Digestive.Blaze.Html5
import Text.Digestive.Util (readMaybe)
import Text.Digestive.Snap

import Data.Text (Text)
import qualified Data.Text as T

import Data.Maybe

-- CODE FOLLOWS

data User = User
  { userName :: Text
  , userMail :: Text
  } deriving (Show)

userForm :: Monad m => Form Text m User
userForm = User
  <$> "name" .: text Nothing
  <*> "mail" .: check "Not a valid email address" checkEmail (text Nothing)

checkEmail :: Text -> Bool
checkEmail = isJust . T.find (== '@')

type Version = [Int]

validateVersion :: Text -> Result Text Version
validateVersion = maybe (Error "Cannot parse version") Success .
    mapM (readMaybe . T.unpack) . T.split (== '.')

data Category = Web | Text | Math
    deriving (Bounded, Enum, Eq, Show)

data Package = Package Text Version Category
    deriving (Show)


packageForm :: Monad m => Form Text m Package
packageForm = Package
    <$> "name"     .: text Nothing
    <*> "version"  .: validate validateVersion (text (Just "0.0.0.1"))
    <*> "category" .: choice categories Nothing
  where
    categories = [(x, T.pack (show x)) | x <- [minBound .. maxBound]]

data Release = Release User Package
    deriving (Show)

releaseForm :: Monad m => Form Text m Release
releaseForm = Release
    <$> "author"  .: userForm
    <*> "package" .: packageForm

userView :: View Html -> Html
userView view = [shamlet|
  <label name="name"> Name:
  ^{inputText "name" view}
  <br>

  ^{errorList "mail" view}
  <label name="mail"> Email address:
  ^{inputText "mail" view}
  <br>
|]


releaseView :: View Html -> Html
releaseView view = [shamlet|
  <form name=release action=\ method=POST>
    <h2>Author
    ^{userView $ subView "author" view}

    <h2>Package
    ^{childErrorList "package" view}

    <label name="package.name"> Name:
    ^{inputText "package.name" view}
    <br>

    <label name="package.version"> Version:
    ^{inputText "package.version" view}
    <br>

    <label name="package.category"> Category:
    ^{inputSelect "package.category" view}
    <br>

    ^{inputSubmit "Submit"}
|]

releaseReceivedView :: Release -> View Html -> Html
releaseReceivedView release view = [shamlet|
  <h1> Release received
  <pre> #{show $ release}
  ^{releaseView view}
|]

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = do
  (view, result) <- runForm "release" releaseForm
  let view' = fmap toHtml view
  blaze $ case result of
    Nothing -> releaseView view'
    Just release -> releaseReceivedView release view'
