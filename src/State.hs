{-# Language RecordWildCards, BlockArguments #-}
module State where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(unfoldr)

import GameTypes
import StateTypes
import Game


--------------------------------------------------------------------------------

showShortAttr :: Attribute -> String
showShortAttr Attribute { .. } =
  show attrOwner ++ ":" ++ show attrName ++ ":" ++ show attrValue

showShortMessage :: Message -> String
showShortMessage Message { .. } =
  show msgSender ++ "->" ++ show msgReceiver ++ ":" ++ show msgPayload

showState :: State -> String
showState State { .. } =
  unlines (
    [ replicate 80 '='
    , showAttributes stateAttributes
    , "~~~ Status:"
    , showStaus stateStatus
    , "~~~ Messages:"
    ] ++ map showShortMessage (qToList stateMessages)
    ++
    [ replicate 80 '=' ]
  )

showFrame :: Frame -> String
showFrame f =
  case f of
    WorkDone m -> unwords [ "[Finished]", showShortMessage m ]
    WorkTodo m -> unwords [ "[Todo]", showShortMessage m ]
    WorkProgress WorkState { .. } ->
      unlines $
        unwords [ "[Working]", showShortMessage workMessage ]
      : [ "  " ++ showShortAttr a | a <- workTodo ] ++
        (case qToList workMore of
           [] -> []
           ms -> [ "  *** Msgs" ] ++
                 [ "  " ++ showShortMessage m | m <- ms ])


showStaus :: [Frame] -> String
showStaus = unlines . map showFrame




--------------------------------------------------------------------------------

data State = State { stateAttributes :: Attributes
                   , stateStatus     :: [Frame]
                   , stateMessages   :: Q Message
                   }


instance Show State where
  show = showState

initState :: State
initState = State { stateAttributes = noAttributes
                  , stateMessages = emptyQ
                  , stateStatus = []
                  }

sendMessage :: CharId -> CharId -> Event -> State -> State
sendMessage msgSender msgReceiver msgPayload = sendMessages [ Message { .. } ]

sendMessages :: [Message] -> State -> State
sendMessages msgs State { .. } =
  State { stateMessages = enQs msgs stateMessages, .. }

stepState :: State -> Maybe State
stepState State { .. } =
  case stateStatus of
    [] ->
      do (newMsg,newQ) <- deQ stateMessages
         pure State { stateStatus = [WorkTodo newMsg]
                    , stateMessages = newQ
                    , ..
                    }

    f : moreFrames -> pure

      case f of
        WorkDone m ->
          State { stateStatus     = moreFrames
                , stateAttributes = foldr setAttrA stateAttributes (sink m)
                , ..
                }

        WorkTodo m ->
          State { stateStatus =
                    WorkProgress
                    WorkState { workMessage = m
                              , workTodo = attributesToList stateAttributes
                              , workMore = emptyQ
                              } : moreFrames
                , ..
                }

        WorkProgress WorkState { .. } ->
          case workTodo of

            [] -> State { stateStatus   = WorkDone workMessage
                                        : map WorkTodo (qToList workMore)
                                       ++ moreFrames
                        , ..
                        }

            a@Attribute { .. } : moreTodo ->
              case handleMessage a workMessage of
                MessageResponse { .. } ->
                  State
                    { stateAttributes =
                        case rspNewVal of
                          Nothing -> delAttr attrOwner attrName   stateAttributes
                          Just v  -> setAttr attrOwner attrName v stateAttributes

                    , stateStatus =
                        let newMs = enQs rspNewMessages workMore
                        in
                        case rspPassItOn of
                          Nothing -> map WorkTodo (qToList newMs) ++ moreFrames
                          Just newMsg ->
                            WorkProgress
                            WorkState
                              { workMore = newMs
                              , workTodo = moreTodo
                              , workMessage = newMsg
                              } : moreFrames

                   , ..
                   }




traceState :: State -> [State]
traceState s0 = s0 : unfoldr (fmap dup . stepState) s0
  where dup x = (x,x)

runState :: State -> State
runState s =
  case stepState s of
    Nothing -> s
    Just s1 -> runState s1

--------------------------------------------------------------------------------

data WorkState = WorkState
  { workMessage :: !Message
  , workTodo    :: ![Attribute]
  , workMore    :: !(Q Message)
  }

data Frame = WorkProgress WorkState
           | WorkTodo Message
           | WorkDone Message


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

delAttr :: CharId -> AttributeName -> Attributes -> Attributes
delAttr cid nm (Attributes as) = Attributes (Map.update upd nm as)
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
  unlines $ show x
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

catQs :: Q a -> Q a -> Q a
catQs (Q xs1 ys1) (Q xs2 ys2) =
  case (ys1, xs2, ys2) of
    ([], _,  _) -> Q (xs1 ++ xs2) ys2
    (_, [],  _) -> Q xs1 (ys1 ++ ys2)
    _           -> Q (xs1 ++ reverse ys1 ++ xs2) ys2

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
qToList (Q xs ys) =
  case ys of
    [] -> xs
    _  -> xs ++ reverse ys


--------------------------------------------------------------------------------



