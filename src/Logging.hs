{-# LANGUAGE TemplateHaskell #-}
module Logging where 

import LoggingExcercise

add :: Int -> Int -> Int
add x y = x + y

multiply :: Int -> Int -> Int
multiply x y = x * y

$(generateLoggingFunctions ['add, 'multiply])

-- $(generateLoggingFunctionUntyped 'add)

-- $(generateLoggingFunction 'multiply)
