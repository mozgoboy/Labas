data Formula =V Int
               | C Int
               | Not Formula
               | Conj {
                        l :: Formula ,
                        r :: Formula
                      }
               | Disj {
                        l :: Formula ,
                        r ::  Formula
                      }
      deriving (Show,Eq,Read)


data Equation = Eq {
                      le :: Formula ,
                      re :: Formula
                   }
      deriving (Show,Eq,Read)


data Instruction = L | R | N
      deriving(Show,Eq,Read)

type Path = [Instruction]

data Tree = Condition {
                          c :: Formula,
                          p :: Path,
                          left :: Tree,
                          right :: Tree
                      }
           | Leaf Path
           | Node {
                       c:: Formula,
                       p:: Path,
                       next:: Tree,
                       e:: [Equation]

                  }
       deriving (Show,Eq,Read)
goByPath :: Path -> Tree -> Tree
goByPath [] a = a
goByPath (a:as) (Condition _ _ l r)  | a==L = goByPath as l
                                     | a==R = goByPath as r
goByPath (a:as) (Node _ _ n _) | a==N = goByPath as n


dropLast [a] = []
dropLast (a:as) = [a] ++ dropLast as


getPath :: Tree -> Path
getPath (Leaf p) = p
getPath (Condition _ p _ _) = p
getPath (Node _ p _ _) = p


backOneStep :: Tree ->Tree -> Tree
backOneStep b a | (getPath a)==[] = a
                | otherwise = goByPath (dropLast (getPath a)) b


eqToSys :: Equation ->[Equation]
eqToSys a@(Eq (V x) y) = [a]
eqToSys a@(Eq (C x) y) = [a]
eqToSys a@(Eq x (V y)) = [(Eq (V y) x)]
eqToSys a@(Eq x (C y)) = [(Eq (C y) x)]
eqToSys (Eq (Not x) (Not y)) = [Eq x y]
eqToSys (Eq (Disj x1 x2) (Disj y1 y2)) = eqToSys (Eq x1 y1) ++ eqToSys (Eq x2 y2)
eqToSys (Eq (Conj x1 x2) (Conj y1 y2)) = eqToSys (Eq x1 y1) ++ eqToSys (Eq x2 y2)
eqToSys _ =[]



putInFormula :: Equation -> Formula -> Formula
putInFormula _ (C z) = (C z)
putInFormula (Eq (V x) y) (V z) | z==x = y
                           | otherwise = V z
putInFormula x (Not f)= Not (putInFormula x f)
putInFormula x (Disj f g)= Disj (putInFormula x f) (putInFormula x g)
putInFormula x (Conj f g)= Conj (putInFormula x f) (putInFormula x g)


putInEq :: Equation -> Equation -> Equation
putInEq x (Eq y z) = Eq (putInFormula x y) (putInFormula x z)                         
