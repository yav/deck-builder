{-# Language RecordWildCards #-}
module Messages where

import Data.Map(Map)
import qualified Data.Map as Map


data Attribute = Attribute
  { attrName  :: !AttributeName
  , attrValue :: !Integer
  , attrOwner :: !CharId
  }

attrDecrease :: Attribute -> Attribute
attrDecrease a = a { attrValue = attrValue a - 1 }


data Message = Message
  { msgSender, msgReceiver :: !CharId
  , msgPayload             :: !Event
  }

type CharId = Int



--------------------------------------------------------------------------------
data MessageResponse = MessageResponse
  { rspNewAttr       :: !(Maybe Attribute) -- ^ Nothing: delete attribute.
  , rspPassItOn      :: !(Maybe Message)   -- ^ Nothing: message consumed.
  , rspNewMessages   :: ![Message]
    -- ^ New messages for pipeline.
    -- In order.
  }


data ResponsePart =
    SetAttr Attribute
  | NewMessage Message
  | Continue Message

response :: [ResponsePart] -> MessageResponse
response = foldr step done
  where
  done = MessageResponse { rspNewAttr = Nothing
                         , rspPassItOn = Nothing
                         , rspNewMessages = []
                         }
  step s r = case s of
               SetAttr a -> r { rspNewAttr = Just a }
               Continue m -> r { rspPassItOn = Just m }
               NewMessage m -> r { rspNewMessages = m : rspNewMessages r }

notForMe :: Attribute -> Message -> MessageResponse
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
handleMessage attr@Attribute { .. } msg@Message { .. } =
  case attrName of

    Strength
      | Attack x <- msgPayload
      , msgSender == attrOwner ->
        response [ SetAttr attr
                 , Continue msg { msgPayload = Attack (x + attrValue) }
                 ]

    Buffer
      | Damage x <- msgPayload
      , x > 0
      , msgReceiver == attrOwner ->
        response $ if attrValue == 1 then []
                                     else [ SetAttr (attrDecrease attr) ]

    Thorns
      | Attack _ <- msgPayload
      , msgReceiver == attrOwner ->
        response [ SetAttr attr
                 , Continue msg
                 , NewMessage Message { msgReceiver = msgSender
                                      , msgSender   = msgReceiver
                                      , msgPayload  = Damage attrValue
                                      }
                 ]

    HP
      | Damage x <- msgPayload
      , msgReceiver == attrOwner ->
        if x >= attrValue
          then response [ NewMessage
                          Message { msgReceiver = msgReceiver
                                  , msgSender   = msgReceiver
                                  , msgPayload  = Died
                                  } ]
          else response [ SetAttr attr { attrValue = attrValue - x } ]


    _ | Died <- msgPayload
      , msgReceiver == attrOwner ->
        response [ Continue msg ]

    _ -> notForMe attr msg


sink :: Message -> [Message]
sink msg@Message { .. } =
  case msgPayload of
    Attack n -> [ msg { msgPayload = Damage n } ]
    _        -> []


--------------------------------------------------------------------------------

data State = Idle [Attribute]
           | Busy [Attribute] Message [Attribute] (Q Message)

sendMessage :: Message -> State -> State
sendMessage msg state =
  case state of
    Idle todo -> Busy [] msg todo emptyQ
    Busy done m todo msgs -> Busy done m todo (enQ m msgs)

stepState :: State -> Maybe State
stepState state =
  case state of
    Idle _ -> Nothing
    Busy done m todo msgs ->
      Just $
      case todo of
        [] -> nextMessage (reverse done) (enQs (sink m) msgs)
        a : as ->
          let MessageResponse { .. } = handleMessage a m
          in case rspPassItOn of
               Nothing ->
                  let as' = case rspNewAttr of
                              Nothing -> reverse done ++ as
                              Just a' -> reverse done ++ a' : done
                  in nextMessage as' (enQs rspNewMessages msgs)
               Just m1 ->
                  let done' = case rspNewAttr of
                                Nothing -> done
                                Just a' -> a' : done
                  in Busy done' m1 as (enQs rspNewMessages msgs)


  where
  nextMessage as ms =
    case deQ ms of
      Nothing        -> Idle as
      Just (m, more) -> Busy [] m as more




--------------------------------------------------------------------------------

groupAttributes :: [Attribute] -> Map CharId (Map AttributeName Integer)
groupAttributes = Map.fromListWith Map.union . map entry
  where
  entry a = (attrOwner a, Map.singleton (attrName a) (attrValue a))

showCharAttrs :: CharId -> Map AttributeName Integer -> String
showCharAttrs x as =
  unlines $ ("Character " ++ show x ++ ":")
          : [ "  " ++ show a ++ " = " ++ show b | (a,b) <- Map.toList as ]

showAttributes :: [Attribute] -> String
showAttributes = unlines
               . map (uncurry showCharAttrs)
               . Map.toList
               . groupAttributes

showShortAttr :: Attribute -> String
showShortAttr Attribute { .. } =
  show attrOwner ++ ":" ++ show attrName ++ ":" ++ show attrValue

showShortMessage :: Message -> String
showShortMessage Message { .. } =
  show msgSender ++ "->" ++ show msgReceiver ++ ":" ++ show msgPayload

showState :: State -> String
showState st =
  case st of
    Idle as -> unlines [ "Idle"
                       , "---------------------------"
                       , showAttributes as
                       , ""
                       ]
    Busy done m todo q ->
      unlines $ [ "Busy"
                , "---------------------"
                ] ++
                map showShortAttr done ++
                [ "~~~" ] ++
                [ showShortMessage m ] ++
                [ "~~~" ] ++
                map showShortAttr todo ++
                [ "~~~" ] ++
                map showShortMessage (qToList q) ++
                [ "" ]


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
initState = Idle [ set 0 HP 60, set 1 HP 60 ]

set o x v = Attribute { attrOwner = o, attrName = x, attrValue = v }
msg x y p = Message { msgSender = x, msgReceiver = y, msgPayload = p }

see :: State -> IO ()
see s = putStrLn (showState s)

runState :: State -> State
runState s =
  case stepState s of
    Nothing -> s
    Just s1 -> runState s1
