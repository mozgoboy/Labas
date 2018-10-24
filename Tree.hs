
data Const = C Int
    deriving (Show,Eq,Read)
data Formula =Variable
			         | Const
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
data Variable = V Int
	deriving (Show,Eq,Read)
data Equation = Eq {
                      l :: Formula ,
                      r :: Formula
                   }
	deriving (Show,Eq,Reads)
newtype System = [Equation]
	deriving (Show,Eq,Read)
