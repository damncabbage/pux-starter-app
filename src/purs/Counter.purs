module App.Counter where

import Prelude (($), (+), (-), (<>), return, const, show)
import Prelude as Prelude
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Int.Bits ((.^.))
import Pux (EffModel, noEffects)
import Pux.Html (Html, (!), (#), bind, div, span, button, text)
import Pux.Html.Events (onClick)

data Action = Increment | Decrementing | Decrement

type State = Int

init :: State
init = 0

update :: forall eff. Action -> State -> EffModel State Action (console :: CONSOLE | eff)
update Increment state = noEffects $ state + 1
update Decrement state = noEffects $ (state .^. 12345) - 1
update Decrementing state =
  { state: state .^. 12345
  , effects: [ do
      liftEff $ log ("Hello" <> show state)
      return Decrement
    ]
  }
  where
    bind = Prelude.(>>=)

view :: State -> Html Action
view state =
  div # do
    button ! onClick (const Increment) # text "Increment"
    span # text (show state)
    button ! onClick (const Decrementing) # text "Decrement"
