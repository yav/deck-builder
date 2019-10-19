{-# Language RecordWildCards, BlockArguments #-}
module Messages where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe(fromJust)


data Attribute = Attribute
  { attrName  :: !AttributeName
  , attrValue :: !Integer
  , attrOwner :: !CharId
  }

showShortAttr :: Attribute -> String
showShortAttr Attribute { .. } =
  show attrOwner ++ ":" ++ show attrName ++ ":" ++ show attrValue


--------------------------------------------------------------------------------

data Message = Message
  { msgSender, msgReceiver :: !CharId
  , msgPayload             :: !Event
  }

type CharId = Int



--------------------------------------------------------------------------------
data MessageResponse = MessageResponse
  { rspNewVal        :: !(Maybe Integer) -- ^ Nothing: delete attribute.
  , rspPassItOn      :: !(Maybe Message) -- ^ Nothing: message consumed.
  , rspNewMessages   :: ![Message]
    -- ^ New messages for pipeline.
    -- In order.
  }

showShortMessage :: Message -> String
showShortMessage Message { .. } =
  show msgSender ++ "->" ++ show msgReceiver ++ ":" ++ show msgPayload




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




--------------------------------------------------------------------------------


data Event = Damage Integer
           | GainBlock Integer
           | Attack Integer
           | Died
             deriving Show





--------------------------------------------------------------------------------

data AttributeName = Strength | Buffer | Thorns | HP
  deriving (Eq,Ord, Show)  -- determines the order in which 


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


    _ | target, Died <- msgPayload ->
        response [ Continue msg ]

    _ -> notForMe attrValue msg

  where
  target = msgReceiver == attrOwner
  source = msgSender   == attrOwner

sink :: Message -> [Message]
sink msg@Message { .. } =
  case msgPayload of
    Attack n -> [ msg { msgPayload = Damage n } ]
    _        -> []

combineAttr :: AttributeName -> Integer {-^new-} -> Integer {-^old-} -> Integer
combineAttr _ = (+)


--------------------------------------------------------------------------------

data State = State { stateAttributes :: Attributes
                   , stateStatus     :: Status
                   , stateMessages   :: Q Message
                   }


instance Show State where
  show = showState


sendMessage :: CharId -> CharId -> Event -> State -> State
sendMessage msgSender msgReceiver msgPayload = sendMessages [ Message { .. } ]

sendMessages :: [Message] -> State -> State
sendMessages msgs State { .. } =
  State { stateMessages = enQs msgs stateMessages, .. }

stepState :: State -> Maybe State
stepState State { .. } =
  case stateStatus of
    Idle ->
      do (workMessage,newQ) <- deQ stateMessages
         pure State { stateStatus =
                        MessageWorking
                        WorkState { workTodo = attributesToList stateAttributes
                                  , ..
                                  }
                    , stateMessages = newQ
                    , ..
                    }

    MessageDone m ->
      pure State { stateStatus   = Idle
                 , stateMessages = enQs (sink m) stateMessages
                 , ..
                 }

    MessageWorking WorkState { .. } -> pure
      case workTodo of
        [] -> State { stateStatus = MessageDone workMessage, .. }
        a@Attribute { .. } : todo ->
          State
            { stateAttributes =
                case rspNewVal of
                  Nothing -> deleteAttribute attrOwner attrName stateAttributes
                  Just v  -> setAttr attrOwner attrName v stateAttributes

            , stateMessages = enQs rspNewMessages stateMessages

            , stateStatus =
                case rspPassItOn of
                  Nothing -> Idle
                  Just m1 -> MessageWorking
                               WorkState { workMessage = m1
                                         , workTodo    = todo
                                         }
            }
          where MessageResponse { .. } = handleMessage a workMessage


runState :: State -> State
runState s =
  case stepState s of
    Nothing -> s
    Just s1 -> runState s1

showState :: State -> String
showState State { .. } =
  unlines (
    [ showAttributes stateAttributes
    , "~~~"
    , showStaus stateStatus
    , "~~~"
    ] ++ map showShortMessage (qToList stateMessages)
  )

--------------------------------------------------------------------------------
data Status = Idle
            | MessageDone Message
            | MessageWorking WorkState

data WorkState = WorkState
  { workMessage :: Message
  , workTodo    :: [Attribute]
  }

showStaus :: Status -> String
showStaus s =
  unlines
  case s of
    Idle          -> [ "[Idle]" ]
    MessageDone m -> [ unwords [ "[Finished]", showShortMessage m ] ]
    MessageWorking WorkState { .. } ->
        unwords [ "[Working]", showShortMessage workMessage ]
      : [ "  " ++ showShortAttr a | a <- workTodo ]



--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

newtype Attributes = Attributes (Map AttributeName (Map CharId Integer))

attributesFromList :: [Attribute] -> Attributes
attributesFromList = foldr add noAttributes
  where add Attribute { .. } = addAttribute attrOwner attrName attrValue

attributesToList :: Attributes -> [Attribute]
attributesToList (Attributes as) =
  [ Attribute { .. }
  | (attrName,owns) <- Map.toList as, (attrOwner,attrValue) <- Map.toList owns
  ]

noAttributes :: Attributes
noAttributes = Attributes Map.empty

addAttribute :: CharId -> AttributeName -> Integer -> Attributes -> Attributes
addAttribute cid nm val (Attributes as) =
  Attributes (Map.insertWith jnMaps nm (Map.singleton cid val) as)
  where
  jnMaps = Map.unionWith (combineAttr nm)

deleteAttribute :: CharId -> AttributeName -> Attributes -> Attributes
deleteAttribute cid nm (Attributes as) = Attributes (Map.update upd nm as)
  where upd owns = let new = Map.delete cid owns
                   in if Map.null new then Nothing else Just new

setAttr :: CharId -> AttributeName -> Integer -> Attributes -> Attributes
setAttr cid nm v (Attributes as) =
  Attributes (Map.insertWith Map.union nm (Map.singleton cid v) as)


groupAttributes :: [Attribute] -> Map CharId (Map AttributeName Integer)
groupAttributes = Map.fromListWith Map.union . map entry
  where
  entry a = (attrOwner a, Map.singleton (attrName a) (attrValue a))

showCharAttrs :: CharId -> Map AttributeName Integer -> String
showCharAttrs x as =
  unlines $ ("Character " ++ show x ++ ":")
          : [ "  " ++ show a ++ " = " ++ show b | (a,b) <- Map.toList as ]

showAttributes :: Attributes -> String
showAttributes = unlines
               . map (uncurry showCharAttrs)
               . Map.toList
               . groupAttributes
               . attributesToList




--------------------------------------------------------------------------------

data Q a = Q [a] [a]

emptyQ :: Q a
emptyQ = Q [] []

enQ :: a -> Q a -> Q a
enQ a (Q xs ys) = Q xs (a:ys)

-- | Front of list gets in queue
enQs :: [a] -> Q a -> Q a
enQs as (Q xs ys) = Q xs (reverse as ++ ys)

deQ :: Q a -> Maybe (a, Q a)
deQ (Q xs ys) =
  case xs of
    x : more -> Just (x, Q more ys)
    [] -> case reverse ys of
            [] -> Nothing
            x : more -> Just (x, Q more [])

qFromList :: [a] -> Q a
qFromList xs = Q xs []

qToList :: Q a -> [a]
qToList (Q xs ys) = xs ++ reverse ys


--------------------------------------------------------------------------------

initState :: State
initState = State { stateAttributes = addAttribute 0 HP 7
                                    $ addAttribute 1 HP 20
                                    $ addAttribute 1 Buffer 1
                                    $ noAttributes
                  , stateMessages = emptyQ
                  , stateStatus = Idle
                  }


see :: State -> IO ()
see s = putStrLn (showState s)

--------------------------------------------------------------------------------
next s = fromJust $ stepState s


