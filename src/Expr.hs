module Expr (
    Expr(..),
    ExprContext(..),
    getContext,

    addDependency,
    collectDependencies,

    exprIO,
) where

import Data.Kind


data Expr c a where
    Pure :: a -> Expr c a
    App :: Expr c (a -> b) -> Expr c a -> Expr c b
    GetContext :: Expr c c
    AddDependency :: ExprContext c => ExprDependency c -> Expr c a -> Expr c a
    ExprIO :: IO a -> Expr c a

instance Functor (Expr c) where
    fmap f x = Pure f <*> x

instance Applicative (Expr c) where
    pure = Pure
    (<*>) = App


class Monoid (ExprDependency c) => ExprContext c where
    type ExprDependency c :: Type


getContext :: Expr c c
getContext = GetContext


addDependency :: ExprContext c => ExprDependency c -> Expr c a -> Expr c a
addDependency = AddDependency

collectDependencies :: ExprContext c => Expr c a -> ExprDependency c
collectDependencies = \case
    Pure {} -> mempty
    App f x -> collectDependencies f <> collectDependencies x
    GetContext {} -> mempty
    AddDependency d x -> d <> collectDependencies x
    ExprIO {} -> mempty


exprIO :: IO a -> Expr c a
exprIO = ExprIO
