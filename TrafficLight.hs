module TrafficLight where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import HDL.Hydra.Circuits.Register

{-
If reset = 1 the machine goes to initial state, which is red.
Otherwise the state transitions are: red -> blue, blue -> green, green -> red
-}

controller1 reset = (green, amber, red)
  where
    green1 = dff (or2 reset amber2)
    green2 = dff (and2 reset' green1)
    green3 = dff (and2 reset' green2)
    amber1 = dff (and2 reset' green3)
    red1 = dff (and2 reset' amber1)
    red2 = dff (and2 reset' red1)
    red3 = dff (and2 reset' red2)
    red4 = dff (and2 reset' red3)
    amber2 = dff (and2 reset' red4)

    reset' = inv reset

    green = or3 green1 green2 green3
    amber = or2 amber1 amber2
    red = or4 red1 red2 red3 red4

controller2 reset walk_request = ((green, amber, red), (wait, walk), walk_count)
  where
    green1 = dff (or3 reset amber2 (and2 green' walk_request'))
    amber1 = dff (and3 reset' walk_request green1)
    red1 = dff (and2 reset' amber1)
    red2 = dff (and2 reset' red1)
    red3 = dff (and2 reset' red2)
    amber2 = dff (and2 reset' red3)

    wait = or3 green1 amber1 amber2
    walk = or3 red1 red2 red3

    reset' = inv reset
    green' = and2 (inv red) (inv amber)
    walk_request' = inv walk_request

    word_size = 16
    zero_word = fanout word_size zero
    one_word = boolword word_size one
    walk_count = reg word_size (or2 reset walk_request) (mux1w reset s zero_word)
    (carry, s) = rippleAdd zero (bitslice2 walk_count one_word)

    green = green1
    amber = or2 amber1 amber2
    red = or3 red1 red2 red3
