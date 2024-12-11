module MyMonadTransformers.Lang where 

data BinOp
  = Plus               
  | Minus              
  | Mult               
  | Div                 
  deriving (Eq, Show, Ord)

data UnOp 
  = Sin 
  | Asin 
  | Cos 
  | Acos 
  | Tan 
  | Atan 
  deriving (Eq, Show, Ord)

data Step 
    = Bin BinOp
    | Un UnOp
    | Store String 
    | Recall String
    | Seq Step Step
    | Put Float 
  deriving (Eq, Show, Ord) 
