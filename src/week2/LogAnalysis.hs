{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage msg =
  case words msg of
    ("I":ts:m) -> LogMessage Info (read ts) (unwords m)
    ("W":ts:m) -> LogMessage Warning (read ts) (unwords m)
    ("E":s:ts:m) -> LogMessage (Error (read s)) (read ts) (unwords m)
    _ -> Unknown msg

parse :: String -> [LogMessage]
parse logFile = map parseMessage (lines logFile)

insert :: LogMessage -> MessageTree -> MessageTree
insert logMsg tree =
  case logMsg of
    Unknown _ -> tree
    LogMessage _ ts _ ->
      case tree of
        Leaf -> Node Leaf logMsg Leaf
        Node l currentLog@(LogMessage _ nts _) r
          | ts < nts -> Node (insert logMsg l) currentLog r
          | ts > nts -> Node l currentLog (insert logMsg r)
        _ -> tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l currentLog r) = inOrder l ++ [currentLog] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMessages = map getLogMessageContent (filter isSeverityGreaterThan50 (inOrder (build logMessages)))

isSeverityGreaterThan50 :: LogMessage -> Bool
isSeverityGreaterThan50 (LogMessage (Error severity) _ _)
  | severity > 50 = True
isSeverityGreaterThan50 _ = False

getLogMessageContent :: LogMessage -> String
getLogMessageContent (LogMessage _ _ msg) = msg
getLogMessageContent (Unknown msg) = msg