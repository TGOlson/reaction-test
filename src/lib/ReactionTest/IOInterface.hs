module ReactionTest.IOInterface
    ( InputInterface(..)
    , OutputInterface(..)
    , keyboardInputInterface
    , keyboardOutputInterface
    , gpioInputInterface
    , gpioOutputInterface
    ) where


import Control.Concurrent
import Control.Monad
import System.GPIO


data InputInterface  = InputInterface
    { iiGetInput :: IO ()
    , iiClose    :: IO ()
    }

-- A little naive and tailored to what we need in this app... but works
data OutputInterface = OutputInterface
    { oiRender   :: String -> IO ()
    , oiUnRender :: IO ()
    , oiClose    :: IO ()
    }


keyboardInputInterface :: InputInterface
keyboardInputInterface = InputInterface (void getChar) (return ())


keyboardOutputInterface :: OutputInterface
keyboardOutputInterface = OutputInterface putStrLn (return ()) (return ())

-- Note: consumers of this inputInterface have trouble debouncing input events.
-- It's important to know right when the button is clicked, but also buffer a bit
-- around the release of the key...
-- TODO: should there be a debounce or some sort of result stream?
gpioInputInterface :: ActivePin 'In -> InputInterface
gpioInputInterface p = InputInterface getGpioInput (closePin p)
  where
    getGpioInput = readPin p >>= \case
                     LO -> threadDelay 16667 >> getGpioInput
                     HI -> return ()


gpioOutputInterface :: ActivePin 'Out -> OutputInterface
gpioOutputInterface p = OutputInterface (const $ writePin p HI) (writePin p LO) (closePin p)
