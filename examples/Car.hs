{-# LANGUAGE FlexibleInstances #-}

-- | A car analogy.
module Car where

import Deptrack
import Data.Traversable (sequenceA)
import Text.Dot (showDot)

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

data Wheel = Wheel Int
  deriving (Show, Eq, Ord)

instance (Show a) => IsDep (Four a)
instance IsDep Wheel
instance IsDep Gaz
instance IsDep Car

-------------------- concise DSL -------------------

wheel n = dep (pure $ Wheel n)
diesel = dep (pure Diesel)
gasoline = dep (pure Gasoline)
wheels = dep ((,,,) <$> wheel 1 <*> wheel 2 <*> wheel 3 <*> wheel 4)
car brand g ws  = dep (Car (Brand brand) <$> g <*> ws)

pigeot = car "pigeot" diesel wheels
rinault = car "rinault" gasoline wheels
fleet = sequenceA [pigeot, pigeot, rinault]

-------------------- end usage -------------------

instance (ToDotInfos Dependency) where
  toDotInfos x = [("label", show x)]

data Example = PrintDot | PrintText
  deriving (Show, Eq, Ord)

example :: Example -> IO ()
example PrintText = drawDeps fleet
example PrintDot  = let (g,_) = evalGraph fleet in
  putStrLn $ showDot $ dotGraph g
