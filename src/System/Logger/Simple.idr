module System.Logger.Simple

import Control.App
import System.File

import Data.Fin
import Data.Time.Clock.System

%default total


-- ---------------------------------------------------------------------------

public export
data Priority
  = DEBUG | INFO | NOTICE | WARNING | ERROR | CRITICAL | ALERT | EMERGENCY

public export
Show Priority where
  show DEBUG = "DEBUG"
  show INFO = "INFO"
  show NOTICE = "NOTICE"
  show WARNING = "WARNING"
  show ERROR = "ERROR"
  show CRITICAL = "CRITICAL"
  show ALERT = "ALERT"
  show EMERGENCY = "EMERGENCY"


public export
Cast Priority (Fin 8) where
  cast DEBUG = 0
  cast INFO = 1
  cast NOTICE = 2
  cast WARNING = 3
  cast ERROR = 4
  cast CRITICAL = 5
  cast ALERT = 6
  cast EMERGENCY = 7

public export
Cast (Fin 8) Priority where
  cast 0 = DEBUG
  cast 1 = INFO
  cast 2 = NOTICE
  cast 3 = WARNING
  cast 4 = ERROR
  cast 5 = CRITICAL
  cast 6 = ALERT
  cast 7 = EMERGENCY


public export
Eq Priority where
  x == y = (cast {to=Fin 8} x) == cast y

public export
Ord Priority where
  compare x y = compare (cast {to=Fin 8} x) (cast y)


-- ---------------------------------------------------------------------------

public export
LogSource : Type
LogSource = String

-- ---------------------------------------------------------------------------

public export
record LogRecord where
  constructor MkLogRecord
  priority : Priority
  src : LogSource
  msg : Lazy String


-- ---------------------------------------------------------------------------

public export
LogFormatter : Type -> Type
LogFormatter a = a -> LogRecord -> IO String


export nullFormatter : LogFormatter a
nullFormatter _ lr = pure lr.msg




public export
data LogFormatElm
  = LFTxt String | LFMsg
  | LFPrio | LFLoggerName
  | LFUTCTime | LFLocalTime
  | LFThreadId | LFProcessId

public export
LogFormat : Type
LogFormat = List LogFormatElm

export simpleLogFormatter : LogFormat -> LogFormatter a
simpleLogFormatter format h lr = do
  ut <- getSystemTime
  lt <- getSystemLocalTime
  pure $ concat $
    map (\x => case x of
        LFTxt x => x
        LFMsg => lr.msg
        LFPrio => show lr.priority
        LFLoggerName => lr.src
        LFUTCTime => show ut     -- FIXME: format
        LFLocalTime => show lt   -- FIXME: format
        LFThreadId => "--"       -- FIXME:
        LFProcessId => "--"      -- FIXME:
        ) format




-- ---------------------------------------------------------------------------

public export
record LogHandler where
  constructor MkLogHandler
  priority : Priority
  prvtyp : Type
  privdata : prvtyp
  emit : prvtyp -> LogRecord -> IO ()
  close : prvtyp -> IO ()

public export
interface HasRootLogHandler m where
  getRootLogHandler : m LogHandler


export emitLogRecord : LogHandler -> LogRecord -> IO ()
emitLogRecord h lr = when (h.priority <= lr.priority) $ h.emit h.privdata lr


export log : (HasIO m, Monad m, HasRootLogHandler m) => Priority -> LogSource -> String -> m ()
log prio ls msg = do
  h <- getRootLogHandler
  liftIO $ emitLogRecord h $ MkLogRecord prio ls msg




-- ---------------------------------------------------------------------------

consoleLogHandler : LogHandler
consoleLogHandler = MkLogHandler
  DEBUG () () emitter (const (pure ()))
  where
    emitter : () -> LogRecord -> IO ()
    emitter _ x = let
      fmt = [ LFLocalTime, LFTxt " ", LFLoggerName, LFTxt " ", LFMsg ]
      in simpleLogFormatter fmt () x >>= putStrLn



fileLogHandler : HasIO io => String -> io (Either FileError LogHandler)
fileLogHandler filename = do
  Right h <- openFile filename Append
    | Left e => pure $ Left e
  pure $ Right $ MkLogHandler
    DEBUG File h emitter closeFile
  where
    emitter : File -> LogRecord -> IO ()
    emitter f x = let
      fmt = [ LFLocalTime, LFTxt " ", LFLoggerName, LFTxt " ", LFMsg ]
      in simpleLogFormatter fmt () x >>= fPutStrLn f >> pure ()




-- ---------------------------------------------------------------------------

public export
0 Logging : List Type -> Type
Logging = Has [State LogHandler LogHandler, PrimIO]

Has [State LogHandler LogHandler] e => HasRootLogHandler (App e) where
  getRootLogHandler = get LogHandler




-- ---------------------------------------------------------------------------




-- vim: tw=80 sw=2 expandtab :
