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
