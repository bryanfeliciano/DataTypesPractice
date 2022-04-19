data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType
data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType } deriving Show

type FirstName = String
type LastName = String
type Age = Int
type Height = Int
type PatientName = (String,String)
type MiddleName = String

-- display patient function --

firstName :: PatientName -> String
firstName patient = fst patient
lastName :: PatientName -> String
lastName patient = snd patient

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data Name = Name FirstName LastName
 | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l 

-- examples of name --

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"

-- now you're able to create bt data --

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

-- Show functions --

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

-- donate functions --

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _)_ = True
canDonateTo _(BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

-- Q12.1 Write a function similar to canDonateTo that takes two patients as arguments
-- rather than two BloodTypes.--

donorFor :: Patient -> Patient -> Bool
donorFor p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

-- Q12.2 Implement a patientSummary function that uses your final Patient type. patientSummary should output a string that looks like this:

showSex Male = "Male"
showSex Female = "Female"

patientSumary :: Patient -> String
patientSumary patient = "************\n" ++
                         "sex" ++ showSex (sex Patient) ++ "\n"
                        --  in the future ill format this to take in all patient information --