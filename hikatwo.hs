module Main (main) where 

import Network
import System.IO
import Text.Printf

import Data.List
import System.Exit
 
server     = "irc.freenode.org"
port       = 6667
chan       = "#vidyadev"
nick       = "hikatwo"
owners     = ["Fuuzetsu", "zalzane"]
t          = "?" -- token
listenOnly = False

ignores :: [Nickname]
ignores = [nick, "bro-bot", "bro-bot-indev", "pikatwo", "StreamBot[dev]"]

type Channel = String
type Nickname = String
data Trigger = Trigger { combin   :: (String -> [String] -> Bool)
                       , keywords :: [String]
                       , text     :: String
                       }

data Message = Message { nickname :: Nickname
                       , msgType  :: String
                       , channel  :: Channel
                       , message  :: String
                       } deriving (Show, Eq)

main = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    write h "NICK" nick
    write h "USER" (nick ++ " 0 * :Haskell pikatwo")
    write h "JOIN" chan
    listen h
 
write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t
 
listen :: Handle -> IO ()
listen h = forever $ do
    t <- hGetLine h
    let s = init t
    if ping s then pong s else eval h (mpartition s)
    putStrLn s
  where
    forever a = a >> forever a
    ping x         = "PING :" `isPrefixOf` x
    pong x         = write h "PONG" (':' : drop 6 x)

mpartition :: String -> Message
mpartition m = Message (name m) (msgT m) (ch m) (msg m)
    where name = takeWhile (/= '!') . drop 1
          msgT = (!! 1) . words
          ch   = (!! 2) . words
          msg  = drop 1 . dropWhile (/= ':') . drop 1

eval :: Handle -> Message -> IO ()
eval h (Message na mt ch msg)
    | mt /= "PRIVMSG"                    = return ()
    | na `elem` ignores                  = return ()
    | msg == (t ++ "quit")               = write h "QUIT" ":Exiting" >> exitWith ExitSuccess
    | listenOnly                         = return ()
    | (t ++ "id ") `isPrefixOf` msg      = chanmsg h ch (drop 4 msg) Nothing
    | nick `isInfixOf` msg               = chanmsg h ch ">being a faggot" $ Just na
    | otherwise                          = case getTriggerMessage msg triggers of
                                             Just t -> chanmsg h ch t Nothing
                                             Nothing -> return ()

chanmsg :: Handle -> Channel -> String -> Maybe Nickname -> IO ()
chanmsg h c s Nothing  = write h "PRIVMSG" (c ++ " :" ++ s)
chanmsg h c s (Just n) = write h "PRIVMSG" (c ++ " :" ++ n ++ ": " ++ s)


getTriggerMessage :: String -> [Trigger] -> Maybe String
getTriggerMessage m [] = Nothing
getTriggerMessage m ((Trigger c k e):ts) = if f (Trigger c k e) then Just e else getTriggerMessage m ts
    where f (Trigger x y _) = x m y


triggers :: [Trigger]
triggers = let s m ks = elem True $ map (\x -> x `isInfixOf` m) ks -- or
               a m ks = elem False $ map (\x -> x `isInfixOf` m) ks -- and
           in
             map (\(c, k, m) -> Trigger c k m) 
                     [ (s, ["g/f", "my gf"], "!!GUYS WANT TO HEAR ABOUT MY GIRLFRIEND SHE MADE ME A ::NODEV:: SHAPED CAKE!!")
                     , (s, ["stallman", "foss"], "!!FOSSFAG DETECTED!! HIDE YOUR WINDOWS BOXES HIDE YOUR SOFTWARE" ++ 
                             "LICENCES THE GPLOSERS ARE COMING FOR YOU")
                     , (a, ["ufeff", "morgawr"], "morgawr is a fag")
                     ]
