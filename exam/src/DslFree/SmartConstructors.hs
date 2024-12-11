module DslFree.SmartConstructors where 

import DslFree.Lang
import Control.Monad.Free
import MyMonadTransformers.Lang (BinOp (..), UnOp (..))

recall_ :: String -> StepItem
recall_ s = liftF $ Recall s 0

put_ :: Float -> StepItem
put_ x = liftF $ Put x 0

store_ :: String -> StepItem
store_ x = liftF $ Store x 0

msin_ :: StepItem 
msin_ = liftF $ Un Sin 0

mcos_ :: StepItem 
mcos_ = liftF $ Un Cos 0

masin_ :: StepItem 
masin_ = liftF $ Un Asin 0

macos_ :: StepItem 
macos_ = liftF $ Un Acos 0 

mtan_ :: StepItem 
mtan_ = liftF $ Un Tan 0 

matan_ :: StepItem 
matan_ = liftF $ Un Atan 0

mplus_ :: StepItem 
mplus_ = liftF $ Bin Plus 0 

mminus_ :: StepItem 
mminus_ = liftF $ Bin Minus 0

mmult_ :: StepItem 
mmult_ = liftF $ Bin Mult 0

mdiv_ :: StepItem 
mdiv_ = liftF $ Bin Div 0