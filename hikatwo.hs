module Main (main) where 

import Network
import System.IO
import Text.Printf
import Control.Monad (forever)

import System.Random
import System.Directory

import Data.List
import Data.String.Utils (replace)
import System.Exit
 
server     = "irc.freenode.org"
port       = 6667
chan       = "#vidyadev"
nick       = "hikatwo"
owners     = ["Fuuzetsu", "zalzane"]
t          = ".:" -- token
listenOnly = False
sayShite   = False

rFile :: FilePath -> IO [String]
rFile f = readFile f >>= \x -> return $ lines x -- bulletproof
                 

ignores :: [Nickname]
ignores = [nick, "bro-bot", "bro-bot-indev", "pikatwo", "StreamBot[dev]", "privilegebot"]

type Channel = String
type Nickname = String

data Trigger = Trigger { combin   :: String -> [String] -> Bool
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
    | msg == (t ++ "quit")               = ownerEval h (Message na mt ch msg)
    | "normalfagalert" `isInfixOf` msg   = ownerEval h (Message na mt ch msg)
    | "languagewaralert" `isInfixOf` msg = ownerEval h (Message na mt ch msg)
    | na `elem` ignores                  = return ()
    | listenOnly                         = return ()
    | (t ++ "id ") `isPrefixOf` msg      = chanmsg h ch (drop 4 msg) Nothing
    | (t ++ "checkem") `isInfixOf` msg   = checkem >>= \x -> chanmsg h ch x $ Just na
    | nick `isInfixOf` msg               = sentenceRoll na >>= \x -> chanmsg h ch x $ Nothing
    | otherwise                          = case getTriggerMessage msg triggers of
                                             Just t -> chanmsg h ch t Nothing
                                             Nothing -> return ()

ownerEval :: Handle -> Message -> IO ()
ownerEval h (Message na mt ch msg)
    | msg == (t ++ "quit")                = write h "QUIT" ":Exiting" >> exitSuccess
    | "normalfagalert" `isInfixOf` msg    = chanmsg h ch nfa Nothing
    | "languagewaralert" `isInfixOf` msg  = chanmsg h ch lwa Nothing
    | otherwise                           = case getTriggerMessage msg triggers of
                                              Just t -> chanmsg h ch t Nothing
                                              Nothing -> return ()       
    where nfa = "☢ !!NORMALFAG DETECTED NORMALFAG DETECTED TREAD WITH CAUTION!! ☢"
          lwa = "☢ !!LANGUAGE WAR DETECTED LANGUAGE WAR DETECTED RUN FOR THE HILLS!! ☢,1>implying your language isnt shit"

sentenceRoll :: Nickname -> IO String
sentenceRoll n =  do ss <- rFile "sentences"
                     rs <- randomRIO (0, length ss - 1)
                     return $ replace "$%$" n $ ss !! rs
               
chanmsg :: Handle -> Channel -> String -> Maybe Nickname -> IO ()
chanmsg h c s Nothing  = write h "PRIVMSG" (c ++ " :" ++ s)
chanmsg h c s (Just n) = write h "PRIVMSG" (c ++ " :" ++ n ++ ": " ++ s)

checkem :: IO String
checkem = do x <- getStdRandom $ randomR (0, 99) :: IO Int
             case x `mod` 11  == 0 of
               False -> return $ "~" ++ show x ++ "~"
               True  -> if x == 0 
                          then return $ cm "00" 
                          else return . cm $ show x
                              where cm x = "~" ++ x ++ "~ HOLY SHIT YOU GOT ☢ DOUBLES☢ (ノﾟοﾟ)ノﾐ★゜・。。 ゜゜・。。・゜☆゜・。。・゜゜・。。・゜゜・。。・゜☆゜・。。・゜゜・。。・゜"
                            

getTriggerMessage :: String -> [Trigger] -> Maybe String
getTriggerMessage m [] = Nothing
getTriggerMessage m (Trigger c k e:ts) = if c m k && sayShite then Just e else getTriggerMessage m ts


triggers :: [Trigger]
triggers = let s m ks = elem True $ map (`isInfixOf` m) ks -- or
               a m ks = notElem False $ map (`isInfixOf` m) ks -- and
           in
             map (\(c, k, m) -> Trigger c k m) 
                     [ (s, ["g/f", "my gf"], "!!GUYS WANT TO HEAR ABOUT MY GIRLFRIEND SHE MADE ME A ::NODEV:: SHAPED CAKE!!")
                     , (s, ["stallman", "foss"], "!!FOSSFAG DETECTED!! HIDE YOUR WINDOWS BOXES HIDE YOUR SOFTWARE" ++ 
                             "LICENCES THE GPLOSERS ARE COMING FOR YOU")
                     , (a, ["ufeff", "morgawr"], "morgawr is a fag")
                     , (a, [t ++ "linkinpark"], "WHY DONT YOU PEOPLE UNDERSTAND MY PAIN (ノ °益°)ノ︵ (\\_.o.)\\")
                     ]

