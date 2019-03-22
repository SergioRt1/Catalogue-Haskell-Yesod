{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.CreateUser where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

--Aform From Entity Card
userForm :: Maybe UserAccount -> AForm Handler UserAccount
userForm   userAccount = UserAccount
    <$> areq textField "Email" (userAccountEmail <$> userAccount)
    <*> areq textField "name" (userAccountName <$> userAccount)
    <*> areq textField "Password" (userAccountPassword <$> userAccount)

--CRUD
--Create
getCreateUserR ::  Handler Html
getCreateUserR = do
               (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ userForm Nothing
               defaultLayout $ do
                    let actionR = CreateUserR
                    $(widgetFile "Product/Form")

postCreateUserR :: Handler Html
postCreateUserR = do
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ userForm  Nothing
                case result of
                    FormSuccess userAccount -> do
                                _ <- runDB $ insert userAccount
                                redirect HomeR
                    _ -> defaultLayout $ do
                        let actionR = CreateUserR
                        $(widgetFile "Product/Form")