{-# LANGUAGE FlexibleInstances #-}

-- | A car analogy.
module Car where

import Deptrack.Applicative
import Data.Traversable (sequenceA)

-- | A representation of our dependency
--
-- This Dependency object is quite dull, but we could augment it to carry more
-- information. One example of possible iteration is to add a coloring
-- information ot the node. Another example is to add an action to observe
-- whether or not the dependency is fullfilled.
data Dependency = Dependency String
  deriving (Show, Eq, Ord)

-------------------- some helpers, pass if you are a beginer ---------------

type Builder a = DepTrack Dependency a

-- | A method to project objects to a dependency
class (Show a) => IsDep a where
  asDep :: a -> Dependency
  asDep = Dependency . show

-- | Shorter nesting function
dep :: IsDep a => Builder a -> Builder a
dep = nest asDep

-------------------- modelization of our business problem -------------------

data Car = Car Brand Gaz (Four Wheel)
  deriving (Show, Eq, Ord)

newtype Brand = Brand String
  deriving (Show, Eq, Ord)

data Gaz = Diesel | Gasoline | Electrical
  deriving (Show, Eq, Ord)

type Four a = (a,a,a,a)

data Wheel = Wheel
  deriving (Show, Eq, Ord)

instance (Show a) => IsDep (Four a)
instance IsDep Wheel
instance IsDep Gaz
instance IsDep Car

-------------------- concise DSL -------------------

wheel = dep (pure Wheel)
diesel = dep (pure Diesel)
gasoline = dep (pure Gasoline)
wheels = dep ((,,,) <$> wheel <*> wheel <*> wheel <*> wheel)
car brand g ws  = dep (Car (Brand brand) <$> g <*> ws)

pigeot = car "pigeot" diesel wheels
rinault = car "rinault" gasoline wheels
fleet = sequenceA [pigeot, pigeot, rinault]

-------------------- end usage -------------------

example :: IO ()
example = drawDeps fleet
