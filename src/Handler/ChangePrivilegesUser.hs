{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.ChangePrivilegesUser where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

data PrivilegesUser = PrivilegesUser {
    privileges :: Text
 }

--Aform From Entity PrivilegesUser
productForm :: Maybe PrivilegesUser -> AForm Handler PrivilegesUser
productForm   privilegesUser = privilegesUser
    <$> areq (selectField privileges) "Privileges" Nothing
   where
      privileges :: [(Text, Text)]
      privileges = [("CreateProduct", "PrvCreateProduct"),("CreateCategory", "PrvCreateCategory"), ("Search", "PrvSearch"), ("ChangePrivilegesUser", "PrvChangePrivilegesUser")]

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