{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Tree
import Data.Maybe


data Concept = Customer Int
               | Restaurant
               | Orders Int
               | Food 
               | Web 
               | SmartPhone View deriving (Show)

data View        = E String | Empty deriving (Show)
data Dimension   = Role | Location | InterestTopic | Interface deriving (Show)

data Context     = Ctx [ Concept ] deriving (Show)
data NodeData    = D Dimension | C (Concept -> Maybe View) | Root


instance Monoid View where
  mempty              = Empty
  mappend Empty x     = x
  mappend y Empty     = y
  mappend (E x) (E y) = E (x ++ " doubleintersection " ++ y)

tailor :: Context -> NodeData -> View
tailor _ Root             = mempty
tailor _ (D _)            = mempty
tailor (Ctx []) (C c)     = mempty
tailor (Ctx (x:xs)) (C c) = case c x of
  Just v -> v
  Nothing -> tailor (Ctx xs) (C c)

customerView :: Concept -> Maybe View
customerView (Customer n) = Just $ E $ "select customers where id=" ++ show n
customerView _ = Nothing

webView :: Concept -> Maybe View
webView Web = Just $ E "select _ where type=web"
webView _ = Nothing

leaf :: (Concept -> Maybe View) -> Tree NodeData
leaf v = Node (C v) []

dim :: Dimension -> Forest NodeData -> Tree NodeData
dim d = Node (D d)

cdt :: Tree NodeData
cdt = Node Root [
        dim Role [ leaf customerView ],
        dim InterestTopic [],
        dim Interface [ leaf webView ]
  ]

context = Ctx [ Web ]

getViews ctx = foldMap (tailor ctx)

main = putStrLn "Hello!"
