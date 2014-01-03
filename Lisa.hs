{-# LANGUAGE RecordWildCards, TemplateHaskell, QuasiQuotes #-}

module Lisa where

import Control.Concurrent
import Text.Printf
import Text.Read
import Text.Read.Lex
import Data.List as L
import Data.Map as M
import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Network.XMPP hiding (stream)
import Network.XMPP.XEP.MUC
import Network.XMPP.Concurrent
import Text.XML.HaXml.Xtract.Parse (xtract)

(+@+) lhs rhs = lhs ++ "@" ++ rhs
(+/+) lhs rhs = lhs ++ "/" ++ rhs

cjr = "conference.jabber.ru"

tryParseDomain = do
  Punc "@" <- lexP
  Ident domain <- lexP
  return domain

data Room = Room
  { roomName   :: String
  , roomDomain :: String
  } deriving (Eq, Ord)

instance Read Room where
  readPrec = do
    Ident name <- lexP
    Room name <$> (tryParseDomain <|> pure cjr)
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Show Room where
  show (Room name domain) = name +@+ domain

data Лиса = Лиса
  { _stream        :: Stream
  , _rooms         :: [String]
  , _currentRoom   :: Room
  , _roomListeners :: Map Room ThreadId }

makeLenses ''Лиса

printListener (room, tid) = printf ": %s - %s" (show room) (show tid)

xmpp action = do
  s <- use stream
  liftIO $ withStream s action

forkXMPP action = do
  s <- use stream
  liftIO $ forkIO $
    void $ withStream s action

listen = do
  m <- nextM
  liftIO $ print m
  listen

adminREPL = do
  cmd : args <- liftIO $ words <$> getLine
  execAdminCommand cmd args
  adminREPL

execAdminCommand cmd args = case (cmd, args) of
  ("join", [room, nick]) -> join room cjr nick
  ("join", [room])       -> join room cjr "лиса"
  ("listen", []) -> use currentRoom >>= addListener
  ("unlisten", [room]) -> do
    listeners <- use roomListeners
    case M.lookup (read room) listeners of
      Just tid -> liftIO $ killThread tid
      Nothing  -> return ()
  ("listeners", []) -> use roomListeners >>=
    liftIO . mapM_ printListener . M.toList
  ("say",  _ : _) -> void $ do
    Room name domain <- use currentRoom
    xmpp $ msg (intercalate " " args) (JID (Just name) domain Nothing)
  where
    join room domain nick = void $ do
      xmpp $ mucJoin $ read (room +@+ domain +/+ nick)
      currentRoom .= Room room domain
    addListener room = void $ do
      listeners <- use roomListeners
      when (not (M.member room listeners)) $ do
        tid <- forkXMPP listen
        roomListeners %= M.insert room tid

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
  evalStateT adminREPL $ Лиса stream [] (Room "" "") M.empty
