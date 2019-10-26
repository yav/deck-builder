{-# Language RecordWildCards, BlockArguments #-}
module Game where


import StateTypes
import GameTypes


handleMessage :: Attribute -> Message -> MessageResponse
handleMessage Attribute { .. } msg@Message { .. } =
  case attrName of

    Strength
      | source, Attack x <- msgPayload ->
        respondRemain [ continue (Attack (x + attrValue)) ]

    Buffer
      | target, Damage x <- msgPayload, x > 0 ->
        response $ if attrValue == 1 then []
                                     else [ SetAttr (attrValue - 1) ]

    Thorns
      | target, Attack _ <- msgPayload ->
        respondRemain [ Continue msg, sendBack (Damage attrValue) ]

    HP
      | target
      , let isDmg = case msgPayload of
                      Damage a -> Just a
                      GainAttribute HP n | n < 0 -> Just (negate n)
                      _ -> Nothing
      , Just x <- isDmg ->
        if x >= attrValue
          then response [ NewMessage
                          Message { msgReceiver = msgReceiver
                                  , msgSender   = msgReceiver
                                  , msgPayload  = Died
                                  } ]
          else response [ SetAttr (attrValue - x) ]

    Block
      | target, Damage x <- msgPayload ->
        case compare x attrValue of
          LT -> response [ SetAttr (attrValue - x) ]
          EQ -> response []
          GT -> response [ Continue
                           Message { msgPayload = Damage (x - attrValue), .. } ]

    Card nm InDiscard
      | ShuffleDiscard <- msgPayload ->
        response [ Continue msg
                 , NewMessage
                   Message { msgReceiver = attrOwner
                           , msgPayload = GainAttribute
                                            (Card nm InDraw) attrValue
                           , ..
                           }
                 ]

    Card Strike cl
      | target, ActivateCard tgt <- msgPayload ->
        response [ NewMessage
                   Message { msgReceiver = tgt
                           , msgSender = msgSender
                           , msgPayload = Attack attrValue
                           }
                 , sendAlso (Discard Strike cl attrValue)
                 ]

    Approved
      | Attack n <- msgPayload ->
        approved [ sendAlso (Damage n) ]

      | Discard nm loc val <- msgPayload ->
        approved
          [ sendBack (GainAttribute (Count loc) (-1))
          , sendAlso (GainAttribute (Card nm InDiscard) val)
          ]

      | GainAttribute (Card _ loc) _ <- msgPayload ->
        approved
          [ Continue msg, sendBack (GainAttribute (Count loc) 1) ]


    _ | target
      , GainAttribute a n <- msgPayload
      , attrName == a -> response [ SetAttr (attrValue + n) ]

    _ | target, Died <- msgPayload ->
        response [ Continue msg ]


    _ -> notForMe attrValue msg

  where
  target = msgReceiver == attrOwner
  source = msgSender   == attrOwner
  respondRemain xs = response (SetAttr attrValue : xs)
  sendAlso p = NewMessage
               Message { msgPayload = p, .. }

  sendBack p = NewMessage
               Message { msgReceiver = msgSender
                       , msgSender   = msgReceiver
                       , msgPayload  = p
                       }
  continue p = Continue Message { msgPayload = p, .. }

  approved xs = respondRemain (Continue msg : xs)



sink :: Message -> [Attribute]
sink Message { .. } =
  case msgPayload of

    ShuffleDiscard ->
      [ Attribute { attrOwner = msgSender
                  , attrName = Count InDiscard
                  , attrValue = 0
                  }
      ]

    GainAttribute attrName attrValue ->
      [ Attribute { attrOwner = msgReceiver, .. } ]


    _ -> []

