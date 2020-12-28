data A = AString String | ABool Bool deriving (Show)

type B = Int

-- does not work
newtype AORB = A | B
data AORB = A | B
type AORB = A | B

-- this compiles but adds constructors...
data AORB = An A | Ora B


-- only this one works
data AORB = AString String | ABool Bool | AInt Int deriving (Show)



