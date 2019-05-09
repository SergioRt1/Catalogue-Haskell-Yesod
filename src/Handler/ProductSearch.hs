{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Handler.ProductSearch where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

data Search = Search {
    searchProductName :: Maybe Text,
    category :: Maybe Text
 }

--Aform From Entity product
searchForm :: Maybe Search -> AForm Handler Search
searchForm   text = Search
    <$> aopt textField "Name" Nothing
    <*> aopt (selectField categories) "Category" Nothing
   where
    categories = do
       entities <- runDB $ selectList [] [Asc CategoryName]
       optionsPairs $ map (\p -> (categoryName $ entityVal p, categoryName $ entityVal p)) entities

--CRUD
--Create

getProductSearchR ::  Handler Html
getProductSearchR  = do
                    ( widget , encoding ) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ searchForm Nothing
                    let actionR = ProductSearchR
                    defaultLayout $ do
                       $(widgetFile "Product/Form")

postProductSearchR :: Handler Html
postProductSearchR = do
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ searchForm Nothing
                case result of
                    FormSuccess values -> do
                              if searchProductName values /= Nothing && category values /= Nothing then do
                                 products <- runDB $ selectList [ProductName ==. (des (searchProductName values)), ProductCategory ==. (des (category values))] []
                                 defaultLayout $ do
                                   $(widgetFile "Product/List")
                              else if searchProductName values /= Nothing then do
                                 products <- runDB $ selectList [ProductName ==. (des (searchProductName values))] []
                                 defaultLayout $ do
                                   $(widgetFile "Product/List")
                              else do
                                 products <- runDB $ selectList [ProductCategory ==. (des (category values))] []
                                 defaultLayout $ do
                                   $(widgetFile "Product/List")
                    _ -> defaultLayout $ do
                        let actionR = ProductSearchR
                        $(widgetFile "Product/Form")

des :: Maybe a -> a
des (Just a) = a

