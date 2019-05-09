{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.AddFavorite where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)


data FavoriteData = FavoriteData { productData :: Text }

--Aform From Entity product
addFavoriteForm :: Maybe FavoriteData -> AForm Handler FavoriteData
addFavoriteForm   favoriteData = FavoriteData
    <$> areq (selectField products) "Add Product" Nothing
   where
      products = do
         entities <- runDB $ selectList [] [Asc ProductName]
         optionsPairs $ map (\p -> (productName $ entityVal p, productName $ entityVal p)) entities
--CRUD
--Create
getAddFavoriteR ::  Handler Html
getAddFavoriteR = do
               (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ addFavoriteForm Nothing
               defaultLayout $ do
                    let actionR = AddFavoriteR
                    $(widgetFile "Product/Form")


postAddFavoriteR :: Handler Html
postAddFavoriteR = do
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ addFavoriteForm  Nothing
                case result of
                    FormSuccess favoriteData -> do
                                Just maybeCurrentUserId <- maybeAuthId
                                Just (Entity productId _) <- runDB $ getBy $ UniqueProduct (productData favoriteData)
                                let favorite = Favorites { favoritesUserId = maybeCurrentUserId, favoritesProductId = productId }
                                _ <- runDB $ insertEntity favorite
                                redirect FavoritesR
                    _ -> defaultLayout $ do
                        let actionR = AddFavoriteR
                        $(widgetFile "Product/Form")