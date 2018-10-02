module Messages exposing (..)

import Model

type Msg
    = NewGame Model.GameState
    | Tick Float
    | ShipCommand Model.ShipCommand 
