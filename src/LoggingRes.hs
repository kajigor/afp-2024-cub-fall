{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
module LoggingRes where 

import LoggingFunctions

-- $(makeLogging "test1")

-- $(makeLogging "test2")

-- $(makeListLogging ["test3"])

$(makeListLogging ["test1", "test2", "test3"])

test1Logs = fst $ test1Logging False
test2Logs = fst $ test2Logging True 2
test3Logs = fst $ test3Logging "one"
