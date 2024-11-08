{-# LANGUAGE TemplateHaskell #-}
module LoggingRes where 

import LoggingFunctions

-- $(makeLogging "test1")

-- $(makeLogging "test2")

$(makeListLogging ["test1", "test2"])

test1Logs = fst $ test1Logging False
test2Logs = fst $ test2Logging True 2
