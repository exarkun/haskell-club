module Main where

import qualified Party
import Employee
  ( Employee(Emp, empName)
  , GuestList(GL)
  , testCompany
  , testCompany2
  )
import Data.Tree
  ( Tree(Node)
  )
import Data.List
  ( intersperse
  , sort
  )

main :: IO ()
main = do
  -- print (Party.nextLevel (Emp "John" 1) [])
  -- print (Party.nextLevel (Emp "Sue" 5) [])
  -- print (Party.nextLevel (Emp "Sam" 4) [])
  -- print (Party.nextLevel (Emp "Sarah" 17) [Party.nextLevel (Emp "Sam" 4) []])

  -- print (Party.maxFun Employee.testCompany)
  -- print (Party.maxFun Employee.testCompany2)

  -- exercise 5
  putStr . showGuestList . Party.maxFun . read =<< getContents

showGuestList :: GuestList -> String
showGuestList (Employee.GL employees fun) =
  "Total fun: " ++ show fun ++ "\n" ++
  (concat . intersperse "\n" . sort . map empName $ employees) ++ "\n"
