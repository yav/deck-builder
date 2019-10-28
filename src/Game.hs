{-# Language RecordWildCards, BlockArguments #-}
module Game where


import StateTypes
import GameTypes
import RNG(shuffle)


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
                      HP := n | n < 0 -> Just (negate n)
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

-- Decks -----------------------------------------------------------------------


    -- Check for reshuffle
    Count InDraw
      | source
      , DrawTop <- msgPayload
      , attrValue == 0
      -> respondRemain [ sendAlso (ShuffleDiscard ShuffleBegin)
                       , sendAlso DrawTopAgain
                       ]

    Count InDraw
      | source
      , DrawTopAgain <- msgPayload ->
       if attrValue > 0
         then respondRemain [ sendAlso DrawTop ]
         else respondRemain []

    Count InDiscard
      | source
      , ShuffleDiscard ShuffleBegin <- msgPayload
       -> respondRemain
          if attrValue < 1
             then []
             else [ continue (ShuffleDiscard (ChooseOrder attrValue)) ]


    -- Increment deck counter whenever a deck gets a card
    Count loc
      | source
      , x := i <- msgPayload
      , Card CardInfo { .. } <- x
      , loc == cardLocation ->
        response [ SetAttr (attrValue + 1)
                 , Continue
                   Message
                     { msgPayload = x := case cardLocation of
                                           InDraw -> i
                                           _      -> attrValue
                     , ..
                     }
                 ]

    -- Move a card from one location to another (draw/discard)
    Card CardInfo { .. }
      | target
      , MoveTo newLoc <- msgPayload ->
        if cardLocation == newLoc
          then respondRemain []
          else response [ sendBack $ Count cardLocation := (-1)
                        , sendAlso $
                            Card CardInfo { cardLocation = newLoc, ..} := 0
                        ]

    -- Drawing a card from the draw pile
    Card CardInfo { cardLocation = InDraw, .. }
      | DrawTop <- msgPayload ->
        if attrValue == 0
          then respondRemain [ sendThis (MoveTo InHand)
                             , Continue msg ]
          else response [ SetAttr (attrValue - 1), Continue msg ]

    -- Move directly from discard to draw pile
    Card CardInfo { cardLocation = InDiscard, .. }
      | ShuffleDiscard (ShuffleCards (i:is)) <- msgPayload ->
        response [ continue (ShuffleDiscard (ShuffleCards is))
                 , sendBack $ Count InDiscard := (-1)
                 , sendThis $ Card CardInfo { cardLocation = InDraw, ..  } := i
                 ]


-- Playing cards ---------------------------------------------------------------


    Card CardInfo { cardName = Strike, .. }
      | target
      , ActivateCardOn tgt <- msgPayload ->
        respondRemain
          [ NewMessage
                   Message { msgReceiver = tgt
                           , msgSender   = msgSender
                           , msgPayload  = Attack 6
                           }
          , sendAlso (MoveTo InDiscard)
          ]


--------------------------------------------------------------------------------

    Approved
      | Attack n <- msgPayload ->
        approved [ sendAlso (Damage n) ]

    _ | target
      , a := n <- msgPayload
      , attrName == a -> response [ SetAttr (attrValue + n) ]


--------------------------------------------------------------------------------

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

  sendThis p = NewMessage
               Message { msgReceiver = attrOwner, msgPayload = p, .. }

  continue p = Continue Message { msgPayload = p, .. }

  approved xs = respondRemain (Continue msg : xs)



sink :: Message -> SinkResponse
sink Message { .. } =
  case msgPayload of

    attrName := attrValue ->
      SinkAttrs [ Attribute { attrOwner = msgReceiver, .. } ]

    ShuffleDiscard (ChooseOrder n) ->
      SinkRNG $ do xs <- shuffle [ 0 .. n - 1 ]
                   pure [ Message { msgPayload = ShuffleDiscard
                                               $ ShuffleCards xs
                                  , .. } ]

    _ -> SinkAttrs []

