import Data.Foldable

data Formula = V Int
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
              | TRUE -- Не может ли формула быть такого вида даже вне контекста задачи функционального равенства?
              | FALSE
      deriving (Show,Eq,Read)


data Equation = Eq {
                      le :: Formula ,
                      re :: Formula
                   }
      deriving (Show,Eq,Read)

data Value = T | F | N  deriving (Show, Eq, Read)

data Predicate = Synt Formula Formula
               | Incl Formula Formula
               | Func Formula Formula
      deriving (Show, Eq, Read)

data Condition = P Predicate
               | Bar   Condition
               | Wedge Condition Condition
               | Vee   Condition Condition
      deriving (Show, Eq, Read)


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


--solveEq :: Equation -> Maybe [Equation]
--solveEq e =
--solve1 e | eqToSys e == [] = Nothing


-- Алгоритм 4 Проверка предиката синтаксического равенства (пока только для одного выражения)

syntPred :: Formula -> Formula -> [Equation] -> Value
syntPred = undefined
{-syntPred f g e:es | asd == [] = F
                | e:es == [] && asd' == [] = T
                | e:es == [] && asd' != [] = N
                | e:es
      where asd  = eqToSys (Equation f g)
            asd' = purge asd
            purge :: [Equation] -> [Equation]
            purge [] = []
            purge a:as = if (l a == r a) then (purge as) else (a : purge as)
-}

-- Алгоритм 5 Проверка подформульного предиката (пока только для одного выражения)

inclPred :: Formula -> Formula -> [Equation] -> Value
inclPred f (V    x)     es = syntPred f (V x) es
inclPred f (C    c)     es = syntPred f (C c) es
inclPred f (Not  g)     es = inclPred f  g    es
inclPred f (Conj g1 g2) es | q1 == T || q2 == T = T
                           | q1 == N || q2 == N = N
                           | otherwise          = F
      where q1 = inclPred f g1 es
            q2 = inclPred f g2 es


-- Упрощение формулы с использованием эквивалентностей (не стал на всякий случай много чего писать)

simplify :: Formula -> (Formula, Int)
-- Не написаны упрощения для формул типа Conj TRUE FALSE и т.д.
simplify (Not (Not f))  = (f, 1)
simplify (Conj f TRUE)  = (f, 1)
simplify (Conj f FALSE) = (FALSE, 1)
simplify (Disj f TRUE)  = (TRUE, 1)
simplify (Disj f FALSE) = (f, 1)
simplify (Conj f g) | f == g     = (f, 1)
                    | f == Not g = (FALSE, 1)
                    | otherwise  = (Conj f g, 0)
simplify (Disj f g) | f == g     = (f, 1)
                    | f == Not g = (TRUE, 1)
                    | otherwise  = (Disj f g, 0)
simplify (Conj f (Disj g h)) | g == f || h == f = (f, 1)
                             | otherwise        = (Conj f (Disj g h), 0)
simplify (Disj f (Conj g h)) | g == f || h == f = (f, 1)
                             | otherwise        = (Disj f (Conj g h), 0)
-- Для всех определений функции выше, кроме первой надо ещё писать определение для зеркальной ситуации типа Conj TRUE f
simplify f = (f, 0)
