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


eqToSys :: Equation ->[Equation]
eqToSys a@(Eq (V x) y) = [a]
eqToSys a@(Eq (C x) y) = [a]
eqToSys a@(Eq x (V y)) = [a]
eqToSys a@(Eq x (C y)) = [a]
eqToSys (Eq (Not x) (Not y)) = [Eq x y]
eqToSys (Eq (Disj x1 x2) (Disj y1 y2)) = eqToSys (Eq x1 y1) ++ eqToSys (Eq x2 y2)
eqToSys (Eq (Conj x1 x2) (Conj y1 y2)) = eqToSys (Eq x1 y1) ++ eqToSys (Eq x2 y2)
eqToSys _ =[]

