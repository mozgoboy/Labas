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
    show TRUE           = "TRUE"
    show FALSE          = "FALSE"
    show (V x)          = "V " ++ show x
    show (C c)          = "C " ++ show c
    show (Not TRUE)     = "~TRUE"
    show (Not FALSE)    = "~FALSE"
    show (Not  f)       = "~(" ++ show f ++ ")"
    show (Conj f TRUE ) =  "(" ++ show f ++ ") & TRUE"
    show (Conj f FALSE) =  "(" ++ show f ++ ") & FALSE"
    show (Conj TRUE  g) =  "TRUE & ("  ++ show g ++ ")"
    show (Conj FALSE g) =  "FALSE & (" ++ show g ++ ")"
    show (Conj f g)     =  "(" ++ show f ++ ") & ("  ++ show g ++ ")"
    show (Disj f TRUE ) =  "(" ++ show f ++ ") || TRUE"
    show (Disj f FALSE) =  "(" ++ show f ++ ") || FALSE"
    show (Disj TRUE  g) =  "TRUE || ("  ++ show g ++ ")"
    show (Disj FALSE g) =  "FALSE || (" ++ show g ++ ")"
    show (Disj f g)     =  "(" ++ show f ++ ") || (" ++ show g ++ ")"

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


    -- Values definition
data Value = T | F | N  deriving (Show, Eq, Read)

neg :: Value -> Value
neg T = F
neg F = T
neg N = N

add :: Value -> Value -> Value -- Value || Value
add N _ = N
add _ N = N
add F F = F
add F T = T
add T F = T
add T T = T

mult :: Value -> Value -> Value -- Value && Value
mult N _ = N
mult _ N = N
mult F F = F
mult F T = F
mult T F = F
mult T T = T


-- Condition definition
data Condition = Val   Value
               | P     Predicate
               | Bar   Condition
               | Wedge Condition Condition  -- f /\ g
               | Vee   Condition Condition  -- f \/ g
      deriving (Read)

instance Show Condition where
    show (Val v)            = show v
    show (P p)              = show p
    show (Bar (Val v))      = "~"  ++ show v
    show (Bar   (P p))      = "~"  ++ show p
    show (Bar      c )      = "~[" ++ show c  ++ "]"
    show (Wedge (Val v) c2) =  show v ++ " /\\ ["  ++ show c2 ++ "]"
    show (Wedge   (P p) c2) =  show p ++ " /\\ ["  ++ show c2 ++ "]"
    show (Wedge c1 (Val v)) =  "[" ++ show c1 ++ "] /\\ "  ++ show v
    show (Wedge c1   (P p)) =  "[" ++ show c1 ++ "] /\\ "  ++ show p
    show (Wedge c1      c2) =  "[" ++ show c1 ++ "] /\\ [" ++ show c2 ++ "]"
    show (Vee   (Val v) c2) =  show v  ++ " \\/  ["  ++ show c2 ++ "]"
    show (Vee   (P p)   c2) =  show p  ++ " \\/  ["  ++ show c2 ++ "]"
    show (Vee   c1 (Val v)) =  "[" ++ show c1 ++ "] \\/  "  ++ show v
    show (Vee   c1   (P p)) =  "[" ++ show c1 ++ "] \\/  "  ++ show p
    show (Vee   c1      c2) =  "[" ++ show c1 ++ "] \\/ [" ++ show c2 ++ "]"

instance Eq Condition where
    (P p1)        == (P p2)         =  (p1 == p2)
    (Val v1)      == (Val v2)       =  (v1 == v2)
    (Bar p1)      == (Bar p2)       =  (p1 == p2)
    (Bar p )      ==      _         =  False
    _             == (Bar p )       =  False
    (Wedge _  _ ) == (Vee   _  _ )  =  False
    (Vee   _  _ ) == (Wedge _  _ )  =  False
    (Wedge a1 b1) == (Wedge a2 b2)  =  ((a1 == a2) && (b1 == b2)) || ((a1 == b2) && (b1 == a2))
    (Vee   a1 b1) == (Vee   a2 b2)  =  ((a1 == a2) && (b1 == b2)) || ((a1 == b2) && (b1 == a2))


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

solve1 :: Equation -> Maybe [Equation]
solve1 e | eqToSys e ==[] = Nothing
         | otherwise = Just (eqToSys e)

solve2 :: Maybe [Equation] -> Maybe [Equation]
solve2 Nothing = Nothing
solve2 (Just e) | check1 e = Just (del (e)) 
                | otherwise = Nothing
                  where check1 [(Eq (C a) (Not _))] =False
                        check1 [(Eq (C a) (Disj _ _ ))] = False
                        check1 [(Eq (C a) (Conj _ _ ))] = False
                        check1 [(Eq _ _)] = True
                        check1 (a:as) = (check1 [a] && check1 as)
                        del [(Eq (C a) (C b))] | a==b = []
                                               | otherwise = [(Eq (C a) (C b))]
                        del [(Eq (V a) (V b))] | a==b = []
                                               | otherwise = [(Eq (V a) (V b))]                       
                        del [x] = [x]                       
                        del (a:as) = (del [a]) ++ (del as)   

solve3 Nothing = Nothing
solve3 (Just e) | check2 e = Nothing
                | otherwise =Just e
                   where check2 [(Eq (V a) f)] = elem (V a) (vars f) 
                         check2 (a:as) = (check2 [a]) && (check2 as)    
solve4 Nothing = Nothing
solve4 (Just e) = (Just (del1 e))
           where del1 [x]  = [x]
                 del1 (a:as) | elem a as = del1 as
                             | otherwise = [a] ++ (del1 as)

--сас


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
inclPred f (Conj g1 g2) es | syntPred f (Conj g1 g2) es == T = T
                           | otherwise                       = add q1 q2
      where q1 = inclPred f g1 es
            q2 = inclPred f g2 es
inclPred f (Disj g1 g2) es | syntPred f (Disj g1 g2) es == T = T
                           | otherwise                       = add q1 q2
      where q1 = inclPred f g1 es
            q2 = inclPred f g2 es


-- Reducing the formulas using simple equivalencies

rdcForm :: Formula -> (Formula, Int)
rdcForm (V x) = (V x, 0)
rdcForm (C c) = (C c, 0)
rdcForm TRUE  = (TRUE , 0)
rdcForm FALSE = (FALSE, 0)

rdcForm (Not  FALSE) = (TRUE , 1)
rdcForm (Not  TRUE ) = (FALSE, 1)

rdcForm (Conj FALSE FALSE) = (FALSE, 1)
rdcForm (Conj FALSE TRUE ) = (FALSE, 1)
rdcForm (Conj TRUE  FALSE) = (FALSE, 1)
rdcForm (Conj TRUE  TRUE ) = (TRUE , 1)

rdcForm (Disj FALSE FALSE) = (FALSE, 1)
rdcForm (Disj FALSE TRUE ) = (TRUE , 1)
rdcForm (Disj TRUE  FALSE) = (TRUE , 1)
rdcForm (Disj TRUE  TRUE ) = (TRUE , 1)

rdcForm (Not (Not f))  = (f, 1)
rdcForm (Not      f )  = (Not (fst res), snd res)
      where res = rdcForm f

rdcForm (Conj f  TRUE) = (f, 1)
rdcForm (Conj TRUE  f) = (f, 1)
rdcForm (Conj f FALSE) = (FALSE, 1)
rdcForm (Conj FALSE f) = (FALSE, 1)

rdcForm (Disj f  TRUE) = (TRUE, 1)
rdcForm (Disj TRUE  f) = (TRUE, 1)
rdcForm (Disj f FALSE) = (f, 1)
rdcForm (Disj FALSE f) = (f, 1)

rdcForm (Conj f (Not g))       | f == g  =  (FALSE, 1)
rdcForm (Conj (Not g) f)       = rdcForm (Conj f (Not g))
rdcForm (Conj f (Disj g h))    | g == f || h == f  =  (f, 1)
rdcForm (Conj (Disj g h) f)    = rdcForm (Conj f (Disj g h))
rdcForm (Conj f g) | f == g    = (f, 1)
                   | otherwise = (Conj (fst resf) (fst resg), max (snd resf) (snd resg))
      where resf = rdcForm f
            resg = rdcForm g

rdcForm (Disj f (Not g))       | f == g  =  (TRUE, 1)
rdcForm (Disj (Not g) f)       = rdcForm (Disj f (Not g))
rdcForm (Disj f (Conj g h))    | g == f || h == f  =  (f, 1)
rdcForm (Disj (Conj g h) f)    = rdcForm (Disj f (Conj g h))
rdcForm (Disj f g) | f == g    = (f, 1)
                   | otherwise = (Disj (fst resf) (fst resg), max (snd resf) (snd resg))
      where resf = rdcForm f
            resg = rdcForm g

fullRdcForm :: Formula -> Formula
fullRdcForm f | snd res == 1 = fullRdcForm (fst res)
              | otherwise    = f
      where res = rdcForm f


-- Algorithm 6. Functional predicate checking

funcPred :: Formula -> Formula -> [Equation] -> Value
funcPred f g es | f' == g'       = T
                | f' == (Not g') = F
                | otherwise      = N
    where
        putForAll f (e:es) = putForAll (putInFormula e f) es
        f' = fullRdcForm (putForAll f es)
        g' = fullRdcForm (putForAll g es)


-- Alogrithm 7. Checking the whole condition for the remaining equations

condCheck :: Condition -> [Equation] -> Value
condCheck (Val v)        es = v
condCheck (P (Synt f g)) es = syntPred f g es
condCheck (P (Incl f g)) es = inclPred f g es
condCheck (P (Func f g)) es = funcPred f g es
condCheck (Bar c)        es = neg  (condCheck c  es)
condCheck (Wedge c1 c2)  es = add  (condCheck c1 es) (condCheck c2 es)
condCheck (Vee   c1 c2)  es = mult (condCheck c1 es) (condCheck c2 es)
