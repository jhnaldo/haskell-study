module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st name n = \str -> case () of
                        _ | str == name -> n
                          | otherwise -> st str

empty :: State
empty = \str -> 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st expr = case expr of
                    Var str -> st str
                    Val n -> n
                    Op left bop right -> (getOp bop) (evalE st left) (evalE st right)
  where
    getOp Plus = (+)
    getOp Minus = (-)
    getOp Times = (*)
    getOp Divide = quot
    getOp Gt = toNumOp (>)
    getOp Ge = toNumOp (>=)
    getOp Lt = toNumOp (<)
    getOp Le = toNumOp (<=)
    getOp Eql = toNumOp (==)
    toNumOp boolOp = \x y -> case boolOp x y of
                                True -> 1
                                False -> 0

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign str expr) = DAssign str expr
desugar (Incr str) = DAssign str (Op (Var str) Plus (Val 1))
desugar (If expr thenStmt elseStmt) =
  DIf expr (desugar thenStmt) (desugar elseStmt)
desugar (While expr stmt) = DWhile expr $ desugar stmt
desugar (For initStmt expr updateStmt bodyStmt) =
  DSequence dInitStmt (DWhile expr (DSequence dBodyStmt dUpdateStmt))
  where
    dInitStmt = desugar initStmt
    dBodyStmt = desugar bodyStmt
    dUpdateStmt = desugar updateStmt
desugar (Sequence stmt restStmt) = DSequence (desugar stmt) (desugar restStmt)
desugar Skip = DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign str expr) = extend st str (evalE st expr)
evalSimple st (DIf expr thenStmt elseStmt) = case evalE st expr of
                                              0 -> evalSimple st elseStmt
                                              otherwise -> evalSimple st thenStmt
evalSimple st while @ (DWhile expr stmt) =
  case evalE st expr of
    0 -> st
    otherwise -> evalSimple (evalSimple st stmt) while
evalSimple st (DSequence first remain) = evalSimple (evalSimple st first) remain
evalSimple st DSkip = st

run :: State -> Statement -> State
run st stmt = evalSimple st $ desugar stmt

runHelper :: Statement -> Int -> Int
runHelper stmt input = run empty (helper stmt input) "Out"
  where
    helper stmt n = (Sequence (Assign "In" (Val n)) stmt)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "Out" (Val 0)
                   , While (Op (Var "In") Ge (Op (Var "Out") Times (Var "Out")))
                       (Incr "Out")
                   , Assign "Out" (Op (Var "Out") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
