module Party where

import Data.List
import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons emp@Emp {empFun = eFun} (GL es fun) = GL (emp : es) (fun + eFun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es1 fun1) (GL es2 fun2) = GL (es1 ++ es2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL es1 fun1) g2@(GL es2 fun2)
  | fun1 >= fun2 = g1
  | otherwise = g2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f Node {rootLabel = rl, subForest = sf} = f rl $ map (treeFold f) sf

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss employees =
  let withSubBoss = map fst employees
      withoutSubBoss = map snd employees
  in (glCons boss (foldr moreFun mempty withoutSubBoss), foldr moreFun mempty withSubBoss)

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel'

nextLevel' :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel' employee [] = nextLevel employee [(mempty, mempty)]
nextLevel' employee gl = nextLevel employee gl

main :: IO ()
main = do
  inputStr <- readFile "company.txt"
  putStrLn (formatGuestList . parseGuestList $ inputStr)

parseGuestList :: String -> GuestList
parseGuestList = maxFun . read

formatGuestList :: GuestList -> String
formatGuestList (GL employees fun) =
  "Total fun: " ++ show fun ++ "\n" ++ unlines (sortByFirstName $ map empName employees)

sortByFirstName :: [String] -> [String]
sortByFirstName = sortBy (\n1 n2 -> compare (fName n1) (fName n2))
  where
    fName = head . words