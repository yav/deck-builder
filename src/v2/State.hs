{-# Language RecordWildCards, BlockArguments #-}
{-# Language TypeFamilies, FlexibleContexts #-}
module State
  ( Game(..)

  , State
  , initState
  , sendMessage, sendMessages
  , stepState, traceState, runState

  , Attribute(..)

  , Message(..)

  , MessageResponse
  , ResponsePart(..), response, notForMe

  , SinkResponse(..)

  ) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(unfoldr)
import RNG


import Q

class ( Ord (AttributeName g), Ord (CharId g)
      , Show (CharId g), Show (Event g), Show (AttributeName g)
      ) => Game g where
  data AttributeName g
  data CharId g
  data Event g

  handleMessage :: Attribute g -> Message g -> MessageResponse g
  sink          :: Message g -> SinkResponse g


data Message g = Message
  { msgSender, msgReceiver :: !(CharId g)
  , msgPayload             :: !(Event g)
  }

data SinkResponse g =
    SinkAttrs [Attribute g]
    -- ^ Set the given attributes

  | SinkRNG (Gen [Message g])



data MessageResponse g = MessageResponse
  { rspNewVal        :: !(Maybe Integer)     -- ^ Nothing: delete attribute.
  , rspPassItOn      :: !(Maybe (Message g)) -- ^ Nothing: message consumed.
  , rspNewMessages   :: ![Message g]
    -- ^ New messages for pipeline.
    -- In order.
  }

data ResponsePart g =
    SetAttr Integer
  | NewMessage (Message g)
  | Continue (Message g)

response :: [ResponsePart g] -> MessageResponse g
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

notForMe :: Integer -> Message g -> MessageResponse g
notForMe a m = response [ SetAttr a, Continue m ]





--------------------------------------------------------------------------------

showShortAttr :: Game g => Attribute g -> String
showShortAttr Attribute { .. } =
  show attrOwner ++ ":" ++ show attrName ++ ":" ++ show attrValue

showShortMessage :: Game g => Message g -> String
showShortMessage Message { .. } =
  show msgSender ++ "->" ++ show msgReceiver ++ ":" ++ show msgPayload

showState :: Game g => State g -> String
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

showFrame :: Game g => Frame g -> String
showFrame f =
  case f of
    WorkTodo m -> unwords [ "[Todo]", showShortMessage m ]
    WorkProgress WorkState { .. } ->
      unlines $
        unwords [ "[Working]", showShortMessage workMessage ]
      : [ "  " ++ showShortAttr a | a <- workTodo ] ++
        (case qToList workMore of
           [] -> []
           ms -> [ "  *** Msgs" ] ++
                 [ "  " ++ showShortMessage m | m <- ms ])


showStaus :: Game g => [Frame g] -> String
showStaus = unlines . map showFrame


instance Game g => Show (State g) where
  show = showState

showCharAttrs :: Game g => CharId g -> Map (AttributeName g) Integer -> String
showCharAttrs x as =
  unlines $ show x
          : [ "  " ++ show a ++ " = " ++ show b | (a,b) <- Map.toList as ]

showAttributes :: Game g => Attributes g -> String
showAttributes = unlines
               . map (uncurry showCharAttrs)
               . Map.toList
               . groupAttributes
               . attributesToList





--------------------------------------------------------------------------------

data State g = State { stateAttributes :: !(Attributes g)
                     , stateStatus     :: ![Frame g]
                     , stateMessages   :: !(Q (Message g))
                     , stateRNG        :: !RNG
                     }

data Frame g = WorkProgress (WorkState g)
             | WorkTodo (Message g)

data WorkState g = WorkState
  { workMessage :: !(Message g)
  , workTodo    :: ![Attribute g]
  , workMore    :: !(Q (Message g))
  }



--------------------------------------------------------------------------------



initState :: Int -> State g
initState seed = State { stateAttributes = noAttributes
                       , stateMessages = emptyQ
                       , stateStatus = []
                       , stateRNG = seededRNG seed
                       }

sendMessage :: CharId g -> CharId g -> Event g -> State g -> State g
sendMessage msgSender msgReceiver msgPayload = sendMessages [ Message { .. } ]

sendMessages :: [Message g] -> State g -> State g
sendMessages msgs State { .. } =
  State { stateMessages = enQs msgs stateMessages, .. }

stepState :: Game g => State g -> Maybe (State g)
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

            [] ->
              case sink workMessage of
                SinkAttrs as ->
                  State { stateStatus = map WorkTodo (qToList workMore)
                                       ++ moreFrames
                        , stateAttributes = foldr setAttrA stateAttributes as
                        , ..
                        }

                SinkRNG m ->
                  genRandFun stateRNG
                    do xs <- m
                       let q1 = enQs xs workMore
                       pure \newR -> State
                         { stateStatus = map WorkTodo (qToList q1) ++ moreFrames
                         , stateRNG = newR
                         , ..
                         }

            a@Attribute { .. } : moreTodo ->
              case handleMessage a workMessage of
                MessageResponse { .. } ->
                  State
                    { stateAttributes =
                        case rspNewVal of
                          Nothing -> delAttr attrOwner attrName stateAttributes
                          Just v  -> setAttr attrOwner attrName v
                                                                stateAttributes

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




traceState :: Game g => State g -> [State g]
traceState s0 = s0 : unfoldr (fmap dup . stepState) s0
  where dup x = (x,x)

runState :: Game g => State g -> State g
runState s =
  case stepState s of
    Nothing -> s
    Just s1 -> runState s1


--------------------------------------------------------------------------------

data Attribute g = Attribute
  { attrName  :: !(AttributeName g)
  , attrValue :: !Integer
  , attrOwner :: !(CharId g)
  }



newtype Attributes g = Attributes (Map (AttributeName g)
                                  (Map (CharId g) Integer))

attributesToList :: Game g => Attributes g -> [Attribute g]
attributesToList (Attributes as) =
  [ Attribute { .. }
  | (attrName,owns) <- Map.toList as, (attrOwner,attrValue) <- Map.toList owns
  ]

noAttributes :: Attributes g
noAttributes = Attributes Map.empty

delAttr :: Game g => CharId g -> AttributeName g -> Attributes g -> Attributes g
delAttr cid nm (Attributes as) = Attributes (Map.update upd nm as)
  where upd owns = let new = Map.delete cid owns
                   in if Map.null new then Nothing else Just new

setAttrA :: Game g => Attribute g -> Attributes g -> Attributes g
setAttrA Attribute { .. } = setAttr attrOwner attrName attrValue

setAttr :: Game g => CharId g -> AttributeName g -> Integer ->
                                          Attributes g -> Attributes g
setAttr cid nm v (Attributes as) =
  Attributes (Map.insertWith Map.union nm (Map.singleton cid v) as)


groupAttributes :: Game g => [Attribute g] ->
                   Map (CharId g) (Map (AttributeName g) Integer)
groupAttributes = Map.fromListWith Map.union . map entry
  where
  entry a = (attrOwner a, Map.singleton (attrName a) (attrValue a))







