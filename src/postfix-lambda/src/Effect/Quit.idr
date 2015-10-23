module Effect.Quit

import Effects
import System

data Quit : Effect where
  Exit : Int -> sig Quit ()

instance Handler Quit IO where
  handle () (Exit i) k = do exit i; k () ()

QUIT : EFFECT
QUIT = MkEff () Quit

exit : Int -> Eff () [QUIT]
exit i = call (Exit i)
