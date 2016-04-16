module ReactionTest
    ( main
    ) where


import Control.Concurrent
import Control.Event.Handler
import Control.Exception
import Control.Monad
import Data.Time.Clock
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Environment
import System.GPIO
import System.IO
import System.Random

import ReactionTest.IOInterface


data Env = Dev | RaspberryPi deriving (Show)


getInputInterface :: Env -> IO InputInterface
getInputInterface Dev         = return keyboardInputInterface
getInputInterface RaspberryPi = gpioInputInterface <$> initReaderPin P23

getOutputInterface :: Env -> IO OutputInterface
getOutputInterface Dev         = return keyboardOutputInterface
getOutputInterface RaspberryPi = gpioOutputInterface <$> initWriterPin P18

getEnvType :: IO Env
getEnvType = getArgs >>= \case ["--pi"] -> return RaspberryPi
                               _        -> return Dev


main :: IO ()
main = do
    env <- getEnvType

    putStrLn $ "Running ReactionTest in " ++ show env ++ " env."

    inputInterface  <- getInputInterface env
    outputInterface <- getOutputInterface env

    -- Setup the fancy UI
    hSetEcho stdin False
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering

    putStrLn "Starting new game. Press any key to begin."

    handle
        (shutdownCleanly inputInterface outputInterface)
        (forever $ runGame inputInterface outputInterface)
  where
    shutdownCleanly :: InputInterface -> OutputInterface -> SomeException -> IO ()
    shutdownCleanly inputInterface outputInterface _ = do
        putStrLn "Shutting down cleanly..."
        iiClose inputInterface
        oiClose outputInterface

data GameState
    = NotStarted
    | WaitingForDelay
    | DelayOver UTCTime
    | Finished String
  deriving (Show)

runGame :: InputInterface -> OutputInterface -> IO ()
runGame inputInterface outputInterface = do
    (keyPressHandler, pushKeyPressEvent) <- newAddHandler
    (delayHandler, pushDelayEvent)       <- newAddHandler

    network <- compile $ makeNetwork keyPressHandler delayHandler outputInterface
    actuate network

    -- Strange event handler structure
    -- Subsequent event handling depending on previous event handling
    -- Pretty sure the below pattern wouldn't scale much more...
    -- TODO: what is a better way to structure this?

    -- Wait for initial keypress to start the game

    -- Wait 300ms before asking for the next input event.
    -- TODO: should these be baked into the IOInterface?
    let getInput = do
          delayMillis 300
          iiGetInput inputInterface

    getInput >> getCurrentTime >>= pushKeyPressEvent

    -- Fork a thread to push an event when the delay is over
    delayThread <- forkIO $ do
        millis <- randomRIO (1000, 7000)
        delayMillis millis
        getCurrentTime >>= pushDelayEvent

    getInput >> getCurrentTime >>= pushKeyPressEvent

    -- Kill the delayThread once we have the second key press.
    killThread delayThread

delayMillis :: Int -> IO ()
delayMillis s = threadDelay (s * 1000)

makeNetwork :: AddHandler UTCTime -> AddHandler UTCTime -> OutputInterface -> MomentIO ()
makeNetwork keyPressHandler delayHandler outputInterface = do
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
        -- Note: rendering logic is kind of hacky
        -- We have an OK separation of rendering interfaces
        -- But here we have to pretend like they have the same API
        -- ... passing string to be ignored, 'un-rendering' when it's a no-op
        -- Also, sometimes we explicitly call putStrLn
        -- that should probably be abstracted through the interface
        -- TODO: decide on a more robust interface,
        -- or create a general type, and then a ReactionTest specific
        -- type w/ knowledge of game flow and game states on top of that...
        \case NotStarted      -> return ()
              WaitingForDelay -> putStrLn "Starting..."
              DelayOver _     -> oiRender outputInterface "----- CLICK!!! -----"
              Finished x      -> do
                  oiUnRender outputInterface
                  putStrLn (x ++ " (press any key to play again)")
