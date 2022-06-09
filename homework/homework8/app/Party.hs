module Party where

import Data.Tree
  ( Tree(Node)
  )

import Employee
  ( Employee(empName, empFun)
  , GuestList(GL)
  )

-- exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) ((empFun e) + f)

instance Semigroup GuestList where
  (GL left leftFun) <> (GL right rightFun) =
    GL (left ++ right) (leftFun + rightFun)

instance Monoid GuestList where
  mempty = GL mempty 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun left@(GL leftE leftFun) right@(GL rightE rightFun) =
  if leftFun >= rightFun
  then left
  else right

-- exercise 2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node rootLabel xs) = f rootLabel (map (treeFold f) xs)

-- exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = (mempty, glCons boss mempty)
nextLevel boss subdivisions =
  (glCons boss withBoss, withoutBoss)
  where
    (withBoss, withoutBoss) = mconcat subdivisions

-- exercise 4

maxFun :: Tree Employee -> GuestList
maxFun tree =
  moreFun withBoss withoutBoss
  where
    (withBoss, withoutBoss) = treeFold nextLevel tree
