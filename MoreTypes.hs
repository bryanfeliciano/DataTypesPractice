-- data SixSidedDie = S1|S2|S3|S4|S5|S6 

-- instance Show SixSidedDie where
--     show S1 = "One"
--     show S2 = "Two"
--     show S3 = "Three"
--     show S4 = "Four"
--     show S5 = "Five"
--     show S6 = "Six"

-- instance Eq SixSidedDie where
--     (==) S6 S6 = True
--     (==) S5 S5 = True
--     (==) S4 S4 = True
--     (==) S3 S3 = True
--     (==) S2 S2 = True
--     (==) S1 S1 = True
--     (==) _  _ = False

-- instance Ord SixSidedDie where
--     compare S6 S6 = EQ
--     compare S6 _  = GT
--     compare _ S6 = LT
--     compare S5 S5 = EQ
--     compare S5 _ = GT
--     compare _ S5 = LT
--     compare S4 S4 = EQ
--     compare _  S4 = LT
--     compare S4 _  = GT

-- deriving show,eq and ord saves you from writing a lot of unnecessary code --

-- data SixSidedDie = S1|S2|S3|S4|S5|S6 deriving (Show,Eq,Ord,Enum)

-- enum is the exact same --

-- Type clases for more complex types

data Name = Name (String,String) deriving(Show,Eq)

instance Ord Name where
    compare (Name (f1,l1)) (Name(f2,l2)) = compare (l1,f1) (l2,f2)

-- However theres a more efficient way, use newtypes --
-- (anything that can be defined with data newtype can also pull off) --
-- newtype Name = Name (String,String) deriving (Show,Eq)

-- Create a five sided die ,
-- then define a type class named die and atleast one method thats usefull for a die to have

data FiveSidedDie = S1|S2|S3|S4|S5 deriving (Show,Eq,Ord,Enum)

class (Eq a, Enum a) => Die a where
    roll::Int -> a

instance Die FiveSidedDie where
    roll n = toEnum (n `mod` 5)
