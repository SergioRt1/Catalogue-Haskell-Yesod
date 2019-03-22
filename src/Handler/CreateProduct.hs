{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.CreateProduct where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

--Aform From Entity product
productForm :: Maybe Product -> AForm Handler Product
productForm   product = Product
    <$> areq textField "Name" (productName <$> product)
    <*> areq textField "Description" (productDescription <$> product)
    <*> areq textField "Specifications" (productSpecifications <$> product)
    <*> areq doubleField "price" (productPrice <$> product)
    <*> areq (selectField categories) "Category" (productCategory <$> product)
   where
      categories = do
         entities <- runDB $ selectList [] [Asc CategoryName]
         optionsPairs $ map (\p -> (categoryName $ entityVal p, categoryName $ entityVal p)) entities
--CRUD
--Create
getCreateProductR ::  Handler Html
getCreateProductR = do
               (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ productForm Nothing
               defaultLayout $ do
                    let actionR = CreateProductR
                    $(widgetFile "Product/Form")

postCreateProductR :: Handler Html
postCreateProductR = do
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ productForm  Nothing
                case result of
                    FormSuccess product -> do
                                _ <- runDB $ insert product
                                redirect CreateProductR
                    _ -> defaultLayout $ do
                        let actionR = CreateProductR
                        $(widgetFile "Product/Form")