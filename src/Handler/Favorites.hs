{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Handler.Favorites where

import Import
import Database.Persist.Sql (rawSql,fromSqlKey)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

--CRUD
--Create

getFavoritesR ::  Handler Html
getFavoritesR  = do
      Just currentUserId <- maybeAuthId
      let query = "SELECT ?? \
                  \FROM favorites \
                  \LEFT JOIN product \
                  \ON favorites.product_id = product.id \
                  \LEFT JOIN \"user\" \
                  \ON favorites.user_id = \"user\".id \
                  \ WHERE \"user\".id = ?"
      products <- runDB $ rawSql query [PersistInt64 $ fromSqlKey currentUserId]
      defaultLayout $ do
        $(widgetFile "Product/List")