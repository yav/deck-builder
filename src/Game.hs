{-# Language RecordWildCards, BlockArguments #-}
module Game where


import StateTypes
import GameTypes


handleMessage :: Attribute -> Message -> MessageResponse
handleMessage Attribute { .. } msg@Message { .. } =
  case attrName of

    Strength
      | source, Attack x <- msgPayload ->
        response [ SetAttr attrValue
                 , Continue msg { msgPayload = Attack (x + attrValue) }
                 ]

    Buffer
      | target, Damage x <- msgPayload, x > 0 ->
        response $ if attrValue == 1 then []
                                     else [ SetAttr (attrValue - 1) ]

    Thorns
      | target, Attack _ <- msgPayload ->
        response [ SetAttr attrValue
                 , Continue msg
                 , NewMessage Message { msgReceiver = msgSender
                                      , msgSender   = msgReceiver
                                      , msgPayload  = Damage attrValue
                                      }
                 ]

    HP
      | target, Damage x <- msgPayload ->
        if x >= attrValue
          then response [ NewMessage
                          Message { msgReceiver = msgReceiver
                                  , msgSender   = msgReceiver
                                  , msgPayload  = Died
                                  } ]
          else response [ SetAttr (attrValue - x) ]

    Card Strike cl
      | target, ActivateCard tgt <- msgPayload ->
        response [ NewMessage
                   Message { msgReceiver = tgt
                           , msgSender = attrOwner
                           , msgPayload = Attack attrValue
                           }
                 , NewMessage
                   Message { msgReceiver = msgReceiver
                           , msgSender = msgSender
                           , msgPayload = GainAttribute
                                (Card Strike InDiscard) attrValue
                           }
                ]

    _ | target
      , GainAttribute a n <- msgPayload
      , attrName == a -> response [ SetAttr (attrValue + n) ]

    _ | target, Died <- msgPayload ->
        response [ Continue msg ]

    _ -> notForMe attrValue msg

  where
  target = msgReceiver == attrOwner
  source = msgSender   == attrOwner

sink :: Message -> [SinkResponse]
sink msg@Message { .. } =
  case msgPayload of
    Attack n -> [ AddMessage msg { msgPayload = Damage n } ]
    GainAttribute attrName attrValue ->
                [ NewAttribute Attribute { attrOwner = msgReceiver, .. } ]
    _ -> []



