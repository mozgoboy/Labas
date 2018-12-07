import Data.Foldable
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

-- Алгоритм 1 сведение к системе простейших

eqToSys :: Equation ->[Equation]
eqToSys a@(Eq (V x) y) = [a]
eqToSys a@(Eq x (V y)) = [(Eq (V y) x)]
eqToSys a@(Eq (C x) y) = [a]
eqToSys a@(Eq x (C y)) = [(Eq (C y) x)]
eqToSys (Eq (Not x) (Not y)) = [Eq x y]
eqToSys (Eq (Disj x1 x2) (Disj y1 y2)) | (eqToSys (Eq x1 y1)==[]) || (eqToSys (Eq x2 y2)==[]) = []
                                       | otherwise = eqToSys (Eq x1 y1) ++ eqToSys (Eq x2 y2)
eqToSys (Eq (Conj x1 x2) (Conj y1 y2)) | (eqToSys (Eq x1 y1)==[]) || (eqToSys (Eq x2 y2)==[]) = []
                                       | otherwise = eqToSys (Eq x1 y1) ++ eqToSys (Eq x2 y2)
eqToSys _ =[]

--Алгоритм 2 Подстановка выражения переменной или константы в формулу

putInFormula :: Equation -> Formula -> Formula
putInFormula _ (C z) = (C z)
putInFormula (Eq (V x) y) (V z) | z==x = y
                           | otherwise = V z
putInFormula (Eq (C x) y) (V z) | z==x = y
                                | otherwise = V z
putInFormula x (Not f)= Not (putInFormula x f)
putInFormula x (Disj f g)= Disj (putInFormula x f) (putInFormula x g)
putInFormula x (Conj f g)= Conj (putInFormula x f) (putInFormula x g)

--Подстановка выражения переменной или константы в уравнение

putInEq :: Equation -> Equation -> Equation
putInEq x (Eq y z) = Eq (putInFormula x y) (putInFormula x z)

--Реализация функции Vars

vars :: Formula -> [Formula]
vars f = delpov (vars0 f)
   where
   vars0 (V x) =[(V x)]
   vars0 (C x) =[]
   vars0 (Not f) = vars f
   vars0 (Disj f g)= (vars f) ++ (vars g)
   vars0 (Conj f g)= (vars f) ++ (vars g)
   delpov [] =[]
   delpov [f] = [f]
   delpov (f:as) | elem f as = (delpov as)
                 | otherwise = [f] ++ (delpov as)


-- Алгоритм 3 Выражение переменных и констант из уравнения


solveEq :: Equation -> Maybe [Equation]
solveEq e =
solve1 e | eqToSys e == [] = Nothing


--- testststs
