{-# Language RecordWildCards, BlockArguments #-}
module State where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe(fromJust)

import GameTypes
import StateTypes
import Game



showShortAttr :: Attribute -> String
showShortAttr Attribute { .. } =
  show attrOwner ++ ":" ++ show attrName ++ ":" ++ show attrValue

showShortMessage :: Message -> String
showShortMessage Message { .. } =
  show msgSender ++ "->" ++ show msgReceiver ++ ":" ++ show msgPayload




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
      let rs = sink m
      in pure State { stateStatus   = Idle
                    , stateMessages = enQs [ msg | AddMessage msg <- rs ]
                                           stateMessages
                    , stateAttributes =
                        foldr setAttrA stateAttributes
                                        [ a | NewAttribute a <- rs ]
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

attributesToList :: Attributes -> [Attribute]
attributesToList (Attributes as) =
  [ Attribute { .. }
  | (attrName,owns) <- Map.toList as, (attrOwner,attrValue) <- Map.toList owns
  ]

noAttributes :: Attributes
noAttributes = Attributes Map.empty

deleteAttribute :: CharId -> AttributeName -> Attributes -> Attributes
deleteAttribute cid nm (Attributes as) = Attributes (Map.update upd nm as)
  where upd owns = let new = Map.delete cid owns
                   in if Map.null new then Nothing else Just new

setAttrA :: Attribute ->  Attributes -> Attributes
setAttrA Attribute { .. } = setAttr attrOwner attrName attrValue

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
initState = State { stateAttributes = noAttributes
                  , stateMessages = emptyQ
                  , stateStatus = Idle
                  }


see :: State -> IO ()
see s = putStrLn (showState s)

--------------------------------------------------------------------------------
next s = fromJust $ stepState s


