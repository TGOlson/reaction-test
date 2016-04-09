module ReactionTest
    ( main
    ) where

import Control.Event.Handler
import Reactive.Banana
import Reactive.Banana.Frameworks

import Control.Concurrent
import Control.Monad
import Data.Time.Clock
import System.IO
import System.Random

main :: IO ()
main = do
    -- Setup the fancy UI
    hSetEcho stdin False
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering

    putStr "Starting new game. Press any key to begin."
    forever runGame

data GameState
    = NotStarted
    | WaitingForDelay
    | DelayOver UTCTime
    | Finished String
  deriving (Show)

runGame :: IO ()
runGame = do
    (keyPressHandler, pushKeyPressEvent) <- newAddHandler
    (delayHandler, pushDelayEvent)       <- newAddHandler

    network <- compile $ makeNetwork keyPressHandler delayHandler
    actuate network

    -- Strange event handler structure
    -- Subsequent event handling depending on previous event handling
    -- Pretty sure the below pattern wouldn't scale much more...
    -- TODO: what is a better way to structure this?

    -- Wait for initial keypress to start the game
    void getChar
    getCurrentTime >>= pushKeyPressEvent

    -- Fork a thread to push an event when the delay is over
    delayThread <- forkIO $ do
        secDelay <- randomRIO (1, 7)
        threadDelay (secDelay * 1000000)
        getCurrentTime >>= pushDelayEvent

    -- Fork a thread to listen for the second key press.
    -- Kill the delayThread once we have the second key press.
    void $ forkIO $ do
        void getChar
        getCurrentTime >>= pushKeyPressEvent
        killThread delayThread

makeNetwork :: AddHandler UTCTime -> AddHandler UTCTime -> MomentIO ()
makeNetwork keyPressHandler delayHandler = do
    eKeyPress <- fromAddHandler keyPressHandler
    eDelay    <- fromAddHandler delayHandler

    let events = unions [ handleKeyPress <$> eKeyPress
                        , handleDelay <$> eDelay
                        ]

    bGame <- accumB NotStarted events
    eGameChange <- changes bGame
    reactimate' $ fmap renderGameState <$> eGameChange
  where
    handleKeyPress t =
        \case NotStarted      -> WaitingForDelay
              WaitingForDelay -> Finished "Game over: You clicked too soon!"
              DelayOver t'    -> Finished ("Reaction time: " ++ show (diffUTCTime t t'))
              Finished _      -> NotStarted
    handleDelay t =
        \case WaitingForDelay -> DelayOver t
              Finished x      -> Finished x
              x               -> Finished ("ERROR: delay invoked in illegal state: " ++ show x)
    renderGameState =
        \case NotStarted      -> return ()
              WaitingForDelay -> putStrLn "Starting..."
              DelayOver _     -> putStrLn "----- CLICK!!! -----"
              Finished x      -> putStrLn (x ++ " (press any key to play again)")
