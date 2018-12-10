import Data.Foldable

-- Formula definition
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
              | TRUE
              | FALSE
      deriving (Read)

instance Show Formula where
    show TRUE       = "TRUE"
    show FALSE      = "FALSE"
    show (V x)      = "V " ++ show x
    show (C c)      = "C " ++ show c
    show (Not  f)   = "~(" ++ show f ++ ")"
    show (Conj f g) = "("  ++ show f ++ ") & ("  ++ show g ++ ")"
    show (Disj f g) = "("  ++ show f ++ ") || (" ++ show g ++ ")"

instance Eq Formula where
    TRUE  == TRUE   =  True
    FALSE == FALSE  =  True
    TRUE  == FALSE  =  False
    FALSE == TRUE   =  False
    (V _) == (C _)  =  False
    (V x) == (V y)  =  (x == y)
    (C a) == (C b)  =  (a == b)
    (Not  f)     == (Not  g)      =  (f == g)
    (Not  _)     ==       _       =  False
    _            == (Not  g)      =  False
    (Conj _  _ ) == (Disj _  _ )  =  False
    (Disj _  _ ) == (Conj _  _ )  =  False
    (Conj f1 g1) == (Conj f2 g2)  =  ((f1 == f2) && (g1 == g2)) || ((f1 == g2) && (g1 == f2))
    (Disj f1 g1) == (Disj f2 g2)  =  ((f1 == f2) && (g1 == g2)) || ((f1 == g2) && (g1 == f2))


-- Equation definition
data Equation = Eq {
                      le :: Formula ,
                      re :: Formula
                   }
      deriving (Read)

instance Show Equation where
    show (Eq f g) = show f ++ " = " ++ show g

instance Eq Equation where
    (Eq f1 g1) == (Eq f2 g2)  =  ((f1 == f2) && (g1 == g2)) || ((f1 == g2) && (g1 == f2))


-- Predicate definition
data Predicate = Synt Formula Formula
               | Incl Formula Formula
               | Func Formula Formula
      deriving (Read)

instance Show Predicate where
    show (Synt f g) = show f ++ " == "  ++ show g
    show (Incl f g) = show f ++ " -> "  ++ show g
    show (Func f g) = show f ++ " <=> " ++ show g

instance Eq Predicate where
    (Synt f1 g1) == (Synt f2 g2)  =  ((f1 == f2) && (g1 == g2)) || ((f1 == g2) && (g1 == f2))
    (Incl f1 g1) == (Incl f2 g2)  =  ((f1 == f2) && (g1 == g2)) || ((f1 == g2) && (g1 == f2))
    (Func f1 g1) == (Func f2 g2)  =  ((f1 == f2) && (g1 == g2)) || ((f1 == g2) && (g1 == f2))


-- Condition definition
data Condition = P     Predicate
               | Bar   Condition
               | Wedge Condition Condition
               | Vee   Condition Condition
      deriving (Read)

instance Show Condition where
    show (P p)         = "P " ++ show p
    show (Bar      c ) = "~[" ++ show c  ++ "]"
    show (Wedge c1 c2) = "["  ++ show c1 ++ "] /\\ ["  ++ show c2 ++ "]"
    show (Vee   c1 c2) = "["  ++ show c1 ++ "] \\/ ["  ++ show c2 ++ "]"

instance Eq Condition where
    (P p1)        == (P p2)         =  (p1 == p2)
    (Bar p1)      == (Bar p2)       =  (p1 == p2)
    (Bar p )      ==      _         =  False
    _             == (Bar p )       =  False
    (Wedge _  _ ) == (Vee   _  _ )  =  False
    (Vee   _  _ ) == (Wedge _  _ )  =  False
    (Wedge a1 b1) == (Wedge a2 b2)  =  ((a1 == a2) && (b1 == b2)) || ((a1 == b2) && (b1 == a2))
    (Vee   a1 b1) == (Vee   a2 b2)  =  ((a1 == a2) && (b1 == b2)) || ((a1 == b2) && (b1 == a2))


-- Values definition
data Value = T | F | N  deriving (Show, Eq, Read)

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


-- Algorithm 4. Syntactic predicate checking

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

-- Algorithm 5. Subformula predicate checking

inclPred :: Formula -> Formula -> [Equation] -> Value
inclPred f (V    x)     es = syntPred f (V x) es
inclPred f (C    c)     es = syntPred f (C c) es
inclPred f (Not  g)     es = inclPred f  g    es
inclPred f (Conj g1 g2) es | q1 == T || q2 == T = T
                           | q1 == N || q2 == N = N
                           | otherwise          = F
      where q1 = inclPred f g1 es
            q2 = inclPred f g2 es


-- Reducing the formulas using simple equivalencies

simplify :: Formula -> (Formula, Int)
simplify (V x) = (V x, 0)
simplify (C c) = (C c, 0)
simplify TRUE  = (TRUE , 0)
simplify FALSE = (FALSE, 0)

simplify (Not  FALSE) = (TRUE , 1)
simplify (Not  TRUE ) = (FALSE, 1)

simplify (Conj FALSE FALSE) = (FALSE, 1)
simplify (Conj FALSE TRUE ) = (FALSE, 1)
simplify (Conj TRUE  FALSE) = (FALSE, 1)
simplify (Conj TRUE  TRUE ) = (TRUE , 1)

simplify (Disj FALSE FALSE) = (FALSE, 1)
simplify (Disj FALSE TRUE ) = (TRUE , 1)
simplify (Disj TRUE  FALSE) = (TRUE , 1)
simplify (Disj TRUE  TRUE ) = (TRUE , 1)

simplify (Not (Not f))  = (f, 1)
simplify (Not      f )  = (Not (fst res), snd res)
      where res = simplify f

simplify (Conj f  TRUE) = (f, 1)
simplify (Conj TRUE  f) = (f, 1)
simplify (Conj f FALSE) = (FALSE, 1)
simplify (Conj FALSE f) = (FALSE, 1)

simplify (Disj f  TRUE) = (TRUE, 1)
simplify (Disj TRUE  f) = (TRUE, 1)
simplify (Disj f FALSE) = (f, 1)
simplify (Disj FALSE f) = (f, 1)

simplify (Conj f (Not g))       | f == g = (FALSE, 1)
simplify (Conj (Not g) f)       = simplify (Conj f (Not g))
simplify (Conj f (Disj g h))    | g == f || h == f = (f, 1)
simplify (Conj (Disj g h) f)    = simplify (Conj f (Disj g h))
simplify (Conj f g) | f == g    = (f, 1)
                    | otherwise = (Conj (fst resf) (fst resg), max (snd resf) (snd resg))
      where resf = simplify f
            resg = simplify g

simplify (Disj f (Not g))       | f == g = (TRUE, 1)
simplify (Disj (Not g) f)       = simplify (Disj f (Not g))
simplify (Disj f (Conj g h))    | g == f || h == f = (f, 1)
simplify (Disj (Conj g h) f)    = simplify (Disj f (Conj g h))
simplify (Disj f g) | f == g    = (f, 1)
                    | otherwise = (Disj (fst resf) (fst resg), max (snd resf) (snd resg))
      where resf = simplify f
            resg = simplify g


fullSimplify :: Formula -> Formula
fullSimplify f | snd res == 1 = fullSimplify (fst res)
               | otherwise    = f
      where res = simplify f


-- Algorithm 6. Functional predicate checking

funcPred :: Formula -> Formula -> [Equation] -> Value
funcPred f g es | f' == g'       = T
                | f' == (Not g') = F
                | otherwise      = N
    where
        putForAll f (e:es) = putForAll (putInFormula e f) es
        f' = fullSimplify (putForAll f es)
        g' = fullSimplify (putForAll g es)


-- Reducing the formulas using simple equivalencies

-- MAYBE NOT NEEDED

-- Alogrithm 7. Checking the whole condition for the remaining equations

condCheck :: Condition -> [Equation] -> Value
condCheck c es = undefined
