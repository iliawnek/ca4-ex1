module TrafficLight where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import HDL.Hydra.Circuits.Register

{-
While reset = 1, only the green light will be on.
When reset = 0, the lights will infinitely cycle in the following pattern:
  green (x3) → amber → red (x4) → amber → repeat...
Reset must be asserted for one cycle at the beginning for the circuit to operate normally.
-}

controller1 reset = (green, amber, red)
  where
    -- states
    green1 = dff (or2 reset amber2) -- green always on whenever reset = 1
    green2 = dff (and2 reset' green1)
    green3 = dff (and2 reset' green2)
    amber1 = dff (and2 reset' green3)
    red1 = dff (and2 reset' amber1)
    red2 = dff (and2 reset' red1)
    red3 = dff (and2 reset' red2)
    red4 = dff (and2 reset' red3)
    amber2 = dff (and2 reset' red4)

    -- internal signals
    reset' = inv reset

    -- lights
    green = or3 green1 green2 green3
    amber = or2 amber1 amber2
    red = or4 red1 red2 red3 red4

{-
If reset = 1, the lights will reset to green/wait and the counter reset to 0.
When reset = 0, the lights will be at green/wait until walk_request = 1.
When walk_request = 1, the lights will transition through the following sequence:
  green/wait → amber/wait → red/walk (x3) → amber/wait → green/wait
During the sequence, assertions of walk_request will be ignored.
  The pedestrian must wait until the lights return to green/wait.
Reset must be asserted for one cycle at the beginning for the circuit to operate normally.
-}

controller2 reset walk_request = ((green, amber, red), (wait, walk), walk_count)
  where
    -- states
    green1 = dff (or3 reset amber2 (and2 green' walk_request'))
    amber1 = dff (and3 reset' walk_request green1)
    red1 = dff (and2 reset' amber1)
    red2 = dff (and2 reset' red1)
    red3 = dff (and2 reset' red2)
    amber2 = dff (and2 reset' red3)

    -- internal signals
    reset' = inv reset
    green' = and2 (inv red) (inv amber)
    walk_request' = inv walk_request

    -- counter
    word_size = 16
    zero_word = fanout word_size zero
    one_word = boolword word_size one
    walk_count = reg word_size (or2 reset walk_request) (mux1w reset sum zero_word)
    (_, sum) = rippleAdd zero (bitslice2 walk_count one_word)

    -- traffic lights
    green = green1
    amber = or2 amber1 amber2
    red = or3 red1 red2 red3

    -- pedestrian lights
    wait = or2 green amber
    walk = red
