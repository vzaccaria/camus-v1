{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Tree
import Data.Maybe


data Concept = Customer Int
               | Restaurant
               | Orders Int
               | Food 
               | Web 
               | SmartPhone deriving (Show)

data View        = E String | Empty deriving (Show)
data Dimension   = Role | Location | InterestTopic | Interface deriving (Show)

data Context     = Ctx [ Concept ] deriving (Show)
data NodeData    = D Dimension | C (Context -> Maybe View) | Root


instance Monoid View where
  mempty              = Empty
  mappend Empty x     = x
  mappend y Empty     = y
  mappend (E x) (E y) = E (x ++ " doubleintersection " ++ y)

tailor :: Context -> NodeData -> View
tailor _ Root             = mempty
tailor _ (D _)            = mempty
tailor cx (C c)           = fromMaybe mempty (c cx) 

customerView :: Context -> Maybe View
customerView (Ctx []) = Nothing
customerView (Ctx (Customer n:_)) = Just $ E $ "select customers where id=" ++ show n
customerView (Ctx (_:xs)) = customerView (Ctx xs)

webView :: Context -> Maybe View
webView (Ctx []) = Nothing
webView (Ctx (Web:_)) = Just $ E "select _ where type=web"
webView (Ctx (_:xs)) = webView (Ctx xs)

leaf :: (Context -> Maybe View) -> Tree NodeData
leaf v = Node (C v) []

dim :: Dimension -> Forest NodeData -> Tree NodeData
dim d = Node (D d)

cdt :: Tree NodeData
cdt = Node Root [
        dim Role [ leaf customerView ],
        dim InterestTopic [],
        dim Interface [ leaf webView ]
  ]

context = Ctx [ Web, Customer 3 ]

getViews :: Foldable t => Context -> t NodeData -> View
getViews ctx = foldMap (tailor ctx)

main = putStrLn "Hello!"
