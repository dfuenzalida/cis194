module Party where

import Data.Monoid
import Data.Tree
import Employee

------------------------------------------------------------
-- Exercise 1.1
------------------------------------------------------------

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL ([e] ++ es) (f + (empFun e))

------------------------------------------------------------
-- Exercise 1.2
------------------------------------------------------------

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es f) (GL es' f') = GL (es ++ es') (f + f')

------------------------------------------------------------
-- Exercise 1.3
------------------------------------------------------------

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = if (a<b) then b else a

------------------------------------------------------------
-- Exercise 2
------------------------------------------------------------

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold z _ (Node _ []) = z
treeFold z f t@(Node r ts) = f (treeFold z f t)
