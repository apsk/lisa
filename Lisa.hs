{-# LANGUAGE RecordWildCards, TemplateHaskell, QuasiQuotes #-}

module Lisa where

import Text.Printf
import Data.List
import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Network.XMPP hiding (stream)
import Network.XMPP.XEP.MUC
import Network.XMPP.Concurrent
import Text.XML.HaXml.Xtract.Parse (xtract)

cjr = "conference.jabber.ru"

(+@+) lhs rhs = lhs ++ "@" ++ rhs
(+/+) lhs rhs = lhs ++ "/" ++ rhs

groupMessagesWith cond = printf "message[%s & @type='groupchat' & ~(x/@xmlns='jabber:x:delay')" cond
toExceptFrom jid room user = printf "@to='%s' & @from!='%s/%s'" jid room user

data Room = Room { roomName, roomDomain :: String}

data Лиса = Лиса
  { _stream      :: Stream
  , _rooms       :: [String]
  , _currentRoom :: Room }

makeLenses ''Лиса

type ЛисаТ = StateT Лиса IO

xmpp action = do
  s <- use stream
  liftIO $ void $
    withStream s (void action)

adminREPL = do
  cmd : args <- liftIO $ words <$> getLine
  execAdminCommand cmd args
  adminREPL

execAdminCommand cmd args = case (cmd, args) of
  ("join", [room, nick]) -> join room cjr nick
  ("join", [room])       -> join room cjr "лиса"
  ("say",  _ : _) -> do
    Room name domain <- use currentRoom
    xmpp $ msg (intercalate " " args) (JID (Just name) domain Nothing)
  where
    join room domain nick = do
      xmpp $ mucJoin $ read (room +@+ domain +/+ nick)
      currentRoom .= Room room domain
      return ()

msg text target = do
  msgId <- getNextId
  outStanza $ Message
    { mFrom = Nothing
    , mTo = target
    , mId = show msgId
    , mType = GroupChat
    , mSubject = ""
    , mBody = text
    , mThread = ""
    , mExt = []
    }

main = do
  let server = "jabber.ru"
  putStr "password: "
  password <- getLine
  (_, stream) <- withNewStream $ do
    handle <- liftIO $ connectViaTcp server 5222
    initiateStream handle server "tzaphkiel" password "xmpp-haskell"
  evalStateT adminREPL $ Лиса stream [] (Room "" "")
