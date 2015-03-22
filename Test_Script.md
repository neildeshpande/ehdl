If you have correctly setup Modelsim on your machine:

make
./test ../regsuite/

&lt;filename&gt;

.ehdl

this will generate "main.vhd" and will run the vhdl compiler (NOT Synthesizer). Thus you can check whether you generated vhdl has syntax errors.

To cover all possible errors (especially the ones related to function call), you must open Modelsim and try to run a simulation. Therefore, the test script is meant only for a quick syntax check!