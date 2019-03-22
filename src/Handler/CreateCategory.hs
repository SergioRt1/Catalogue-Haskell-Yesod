{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.CreateCategory where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

--Aform From Entity Category
categoryForm :: Maybe Category -> AForm Handler Category
categoryForm   category = Category
    <$> areq textField "Name" (categoryName <$> category)
    <*> areq textField "Description" (categoryDescription <$> category)

--CRUD
--Create
getCreateCategoryR ::  Handler Html
getCreateCategoryR = do
               (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ categoryForm Nothing
               defaultLayout $ do
                    let actionR = CreateCategoryR
                    $(widgetFile "Product/Form")

postCreateCategoryR :: Handler Html
postCreateCategoryR = do
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ categoryForm  Nothing
                case result of
                    FormSuccess category -> do
                                _ <- runDB $ insert category
                                redirect HomeR
                    _ -> defaultLayout $ do
                        let actionR = CreateCategoryR
                        $(widgetFile "Product/Form")