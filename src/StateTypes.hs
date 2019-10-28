module StateTypes where

import GameTypes
import RNG


data Attribute = Attribute
  { attrName  :: !AttributeName
  , attrValue :: !Integer
  , attrOwner :: !CharId
  }

data Message = Message
  { msgSender, msgReceiver :: !CharId
  , msgPayload             :: !Event
  }

data SinkResponse =
    SinkAttrs [Attribute]
    -- ^ Set the given attributes

  | SinkRNG (Gen [Message])



data MessageResponse = MessageResponse
  { rspNewVal        :: !(Maybe Integer) -- ^ Nothing: delete attribute.
  , rspPassItOn      :: !(Maybe Message) -- ^ Nothing: message consumed.
  , rspNewMessages   :: ![Message]
    -- ^ New messages for pipeline.
    -- In order.
  }

data ResponsePart =
    SetAttr Integer
  | NewMessage Message
  | Continue Message

response :: [ResponsePart] -> MessageResponse
response = foldr step done
  where
  done = MessageResponse { rspNewVal = Nothing
                         , rspPassItOn = Nothing
                         , rspNewMessages = []
                         }
  step s r = case s of
               SetAttr a -> r { rspNewVal = Just a }
               Continue m -> r { rspPassItOn = Just m }
               NewMessage m -> r { rspNewMessages = m : rspNewMessages r }

notForMe :: Integer -> Message -> MessageResponse
notForMe a m = response [ SetAttr a, Continue m ]



