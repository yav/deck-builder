{-# Language RecordWildCards #-}
module Messages where


data Event = Damage Integer
           | Block Integer
           | Attack Integer

data Message = Message { msgSender, msgReceiver :: CharId
                       , msgPayload :: Event
                       }

type CharId = Int


data MessageResponse = MessageResponse
  { rspNewProc       :: Maybe MsgProc
  , rspPassItOn      :: Maybe Message
  , rspNewMessages   :: [Message]
  }

data MsgProc = MsgProc { handleMessage :: Message -> MessageResponse
                       , getState      :: Integer
                       }

stop :: Maybe MsgProc -> MessageResponse
stop n = MessageResponse { rspNewProc = n
                         , rspPassItOn = Nothing
                         , rspNewMessages = []
                         }

passItOn :: Maybe MsgProc -> Message -> MessageResponse
passItOn n msg = (stop n) { rspPassItOn = Just msg }


strength :: CharId -> Integer -> MsgProc
strength self n = proc
  where
  proc = MsgProc { getState = n, .. }
  handleMessage msg
    | Attack x <- msgPayload msg
    , msgSender msg == self =
      passItOn (Just proc) msg { msgPayload = Attack (x + n) }
    | otherwise = passItOn (Just proc) msg


buffer :: CharId -> Integer -> MsgProc
buffer self n = proc
  where
  proc = MsgProc { getState = n, .. }
  handleMessage msg
    | Damage x <- msgPayload msg
    , x > 0
    , msgReceiver msg == self =
      stop (if n == 1 then Nothing else Just (buffer self (n-1)))
    | otherwise = passItOn (Just proc) msg

thorns :: CharId -> Integer -> MsgProc
thorns self n = proc
  where
  proc = MsgProc { getState = n, .. }
  handleMessage msg
    | Attack _ <- msgPayload msg
    , msgReceiver msg == self =
      MessageResponse { rspNewProc = Just proc
                      , rspPassItOn = Just msg
                      , rspNewMessages =
                        [ Message { msgReceiver = msgSender msg
                                  , msgSender   = msgReceiver msg
                                  , msgPayload  = Damage n
                                  }
                        ]
                      }

    | otherwise = passItOn (Just proc) msg




