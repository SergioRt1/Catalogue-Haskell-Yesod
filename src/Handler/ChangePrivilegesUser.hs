{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.ChangePrivilegesUser where

import Import
import Database.Persist.Sql (rawSql)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

data PrivilegesUser = PrivilegesUser
 { user :: Text
 , privilege :: [Privileges]
 }

--Aform From Entity PrivilegesUser
privilegesUserForm :: Maybe PrivilegesUser -> AForm Handler PrivilegesUser
privilegesUserForm   privilegesUser = PrivilegesUser
    <$> areq (selectField users) "User" Nothing
    <*> areq (multiSelectFieldList privileges) "Privileges" Nothing
   where
      privileges :: [(Text, Privileges)]
      privileges = [("CreateProduct", PrvCreateProduct),("CreateCategory", PrvCreateCategory), ("Search", PrvSearch), ("ChangePrivilegesUser", PrvChangePrivilegesUser)]
      users = do
         entities <- runDB $ selectList [] [Asc UserIdent]
         optionsPairs $ map (\p -> (userIdent $ entityVal p, userIdent $ entityVal p)) entities

--CRUD
--Create
getChangePrivilegesUserR ::  Handler Html
getChangePrivilegesUserR = do
               (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ privilegesUserForm Nothing
               defaultLayout $ do
                    let actionR = ChangePrivilegesUserR
                    $(widgetFile "Product/Form")

postChangePrivilegesUserR :: Handler Html
postChangePrivilegesUserR = do
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ privilegesUserForm  Nothing
                case result of
                    FormSuccess newPrivileges -> do
                                _ <- runDB $ updateWhere [UserIdent ==. (user newPrivileges)] [UserPerms =. (privilege newPrivileges)]
                                redirect ChangePrivilegesUserR
                    _ -> defaultLayout $ do
                        let actionR = ChangePrivilegesUserR
                        $(widgetFile "Product/Form")