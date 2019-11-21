import Lib

main :: IO ()
main = putStrLn "Test suite not yet implemented"

sun = WorkDay "Ron" "Daniel" "Lital" "Omer"
mon = WorkDay "Daniel" "Ron" "Moshe" "Lital"
tues = WorkDay "Omer" "Daniel" "Lior" "Moshe"
wed = WorkDay "Lital" "Lior" "Omer" "Ron"

testWeek = WorkWeek sun mon tues wed