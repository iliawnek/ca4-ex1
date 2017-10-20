module Main where
import HDL.Hydra.Core.Lib
import TrafficLight

main :: IO ()
main = do
  run_test_1 test_data_1
  run_test_2 test_data_2

run_test_1 :: [[Int]] -> IO ()
run_test_1 input = runAllInput input output
  where
    -- Extract input signals from readable input
    reset = getbit input 0
    -- Connect the circuit to its inputs and outputs
    (green, amber, red) = controller1 reset
    -- Format the signals for output
    output =
      [string "reset = ", bit reset,
       string "   (green, amber, red) = ", bit green, bit amber, bit red]

run_test_2 :: [[Int]] -> IO ()
run_test_2 input = runAllInput input output
 where
   -- Extract input signals from readable input
   reset = getbit input 0
   walk_request = getbit input 1

   -- Connect the circuit to its inputs and outputs
   (lights, peds, walk_count) = controller2 reset walk_request
   (green, amber, red) = lights
   (wait, walk) = peds
   -- Format the signals for output
   output =
     [string "reset = ", bit reset,
      string "   walk_request = ", bit walk_request,
      string "   (green, amber, red) = ", bit green, bit amber, bit red,
      string "   (wait, walk) = ", bit wait, bit walk,
      string "   walk_count = ", hex walk_count]


test_data_1, test_data_2 :: [[Int]]
test_data_1 =
  [[1],
   [0],
   [0],
   [0],
   [0],
   [0],
   [1],
   [0],
   [0],
   [0],
   [0],
   [0],
   [0],
   [0],
   [0],
   [0],
   [0],
   [0],
   [0],
   [0],
   [0],
   [0],
   [0],
   [0],
   [0],
   [0],
   [0]]

test_data_2 =
  [[1, 0],
   [0, 0],
   [0, 0],
   [0, 0],
   [0, 1],
   [0, 0],
   [0, 0],
   [0, 1],
   [0, 0],
   [0, 0],
   [0, 0],
   [0, 0],
   [0, 1],
   [0, 1],
   [0, 1],
   [0, 1],
   [0, 1],
   [0, 1],
   [0, 0],
   [1, 1],
   [0, 0],
   [0, 0],
   [0, 0]]
