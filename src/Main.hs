-----------------------------------------------------------------------------
{-# LANGUAGE CPP                #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           GHC.Generics
-----------------------------------------------------------------------------
import           Miso
import           Miso.Html
import           Miso.JSON
import           Miso.String
import           Miso.Lens
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
data Action
  = AddOne
  | SubtractOne
  | Subscribe
  | Unsubscribe
  | Welcomed
  | Oops
  | Failure MisoString
  | GetComponentId Int
  | Notification Message
  | Init
-----------------------------------------------------------------------------
data Message
  = Increment
  | Decrement
  deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
instance ToJSON Message where
  toJSON = \case
    Increment -> "inc"
    Decrement -> "dec"
-----------------------------------------------------------------------------
instance FromJSON Message where
  parseJSON = withText "Message" $ \case
    "inc" -> pure Increment
    "dec" -> pure Decrement
    x -> typeMismatch "Message" (String x)
-----------------------------------------------------------------------------
main :: IO ()
main = startApp defaultEvents server { mount = Just Init }
-----------------------------------------------------------------------------
arithmetic :: Topic Message
arithmetic = topic "arithmetic"
-----------------------------------------------------------------------------
type ParentModel = ()
-----------------------------------------------------------------------------
-- | Demonstrates a simple server / client, pub / sub setup for 'Component'
-- In this contrived example, the server component holds the
-- incrementing / decrementing actions, and relays them to the clients
-- via the pub / sub mechanism.
--
-- Notice the server has no 'model' (e.g. `()`)
--
server :: App ParentModel Action
server = component () update_ $ \() ->
  div_
  []
  [ "Server component"
  , button_ [ onClick AddOne ] [ "+" ]
  , button_ [ onClick SubtractOne ] [ "-" ]
  , mount_ (client_ "client 1")
  , mount_ (client_ "client 2")
  ] where
      update_ :: Action -> Effect ROOT ParentModel Action
      update_ = \case
        Init -> do
          io_ $ consoleLog ("parent subscribing")
          subscribe arithmetic Notification Failure
        Notification Increment ->
          io_ (consoleLog "parent got increment")
        Notification Decrement ->
          io_ (consoleLog "parent got decrement")
        Failure msg ->
          io_ $ consoleError ("Decode failure: " <> ms msg)
        AddOne ->
          io_ (publish arithmetic Increment)
        SubtractOne ->
          io_ (publish arithmetic Decrement)
        _ -> pure ()
-----------------------------------------------------------------------------
client_ :: MisoString -> Component ParentModel Int Action
client_ name = (clientComponent name)
  { mount = Just Subscribe
  , mailbox = receiveMail
  }
-----------------------------------------------------------------------------
receiveMail :: Value -> Maybe Action
receiveMail (String "welcome") = Just Welcomed
receiveMail _ = Just Oops
-----------------------------------------------------------------------------
clientComponent :: MisoString -> Component () Int Action
clientComponent name = component 0 update_ $ \m ->
  div_
  []
  [ br_ []
  , text (name <> " : " <> ms (m ^. _id))
  , button_ [ onClick Unsubscribe ] [ "unsubscribe" ]
  , button_ [ onClick Subscribe ] [ "subscribe" ]
  ] where
      update_ :: Action -> Effect () Int Action
      update_ = \case
        AddOne -> do
          _id += 1
        SubtractOne ->
          _id -= 1
        Unsubscribe ->
          unsubscribe arithmetic
        Subscribe ->
          subscribe arithmetic Notification Failure
        Notification Increment ->
          update_ AddOne
        Notification Decrement ->
          update_ SubtractOne
        Failure msg ->
          io_ $ consoleError ("Decode failure: " <> ms msg)
        Welcomed ->
          io_ (consoleLog "I was just welcomed by my parent")
        Oops ->
          io_ (consoleLog "oops, bad mail decoding")
        _ ->
          pure ()
-----------------------------------------------------------------------------
