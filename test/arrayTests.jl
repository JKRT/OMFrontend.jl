#=
This file defines the test cases for the array tests defined in the Arrays subfolder.
Tests here are typicaly small tests that are easy to debug, but tests semi-advanced features of the compiler.
To add a test add a reference string, a model to the ./Equation folder and the name of the model.

See the comment at the bottom of this file for more details.
=#

LoopUnrolling0 = "class TestMisc_LoopUnrolling0
  Real ARRV[1];
  Real ARRV[2];
  Real ARRV[3];
equation
  ARRV[1] = 1.0;
  ARRV[2] = 4.0;
end TestMisc_LoopUnrolling0;
"

LoopUnrolling1 = "class TestMisc_LoopUnrolling1
  Real ARRV2[1, 1];
  Real ARRV2[1, 2];
  Real ARRV2[2, 1];
  Real ARRV2[2, 2];
equation
  ARRV2[1, 1] = 2.0;
  ARRV2[1, 2] = 3.0;
  ARRV2[2, 1] = 3.0;
  ARRV2[2, 2] = 4.0;
end TestMisc_LoopUnrolling1;
"

LoopUnrolling2 = "class TestMisc_LoopUnrolling2
  Real ARRV2[1, 1, 1];
  Real ARRV2[1, 1, 2];
  Real ARRV2[1, 2, 1];
  Real ARRV2[1, 2, 2];
  Real ARRV2[2, 1, 1];
  Real ARRV2[2, 1, 2];
  Real ARRV2[2, 2, 1];
  Real ARRV2[2, 2, 2];
equation
  ARRV2[1, 1, 1] = 3.0;
  ARRV2[1, 1, 2] = 4.0;
  ARRV2[1, 2, 1] = 4.0;
  ARRV2[1, 2, 2] = 5.0;
  ARRV2[2, 1, 1] = 4.0;
  ARRV2[2, 1, 2] = 5.0;
  ARRV2[2, 2, 1] = 5.0;
  ARRV2[2, 2, 2] = 6.0;
end TestMisc_LoopUnrolling2;
"

LoopUnrolling3 = "class TestMisc_LoopUnrolling3
  Real ARRV2[1, 1, 1, 1];
  Real ARRV2[1, 1, 1, 2];
  Real ARRV2[1, 1, 2, 1];
  Real ARRV2[1, 1, 2, 2];
  Real ARRV2[1, 2, 1, 1];
  Real ARRV2[1, 2, 1, 2];
  Real ARRV2[1, 2, 2, 1];
  Real ARRV2[1, 2, 2, 2];
  Real ARRV2[2, 1, 1, 1];
  Real ARRV2[2, 1, 1, 2];
  Real ARRV2[2, 1, 2, 1];
  Real ARRV2[2, 1, 2, 2];
  Real ARRV2[2, 2, 1, 1];
  Real ARRV2[2, 2, 1, 2];
  Real ARRV2[2, 2, 2, 1];
  Real ARRV2[2, 2, 2, 2];
equation
  ARRV2[1, 1, 1, 1] = 4.0;
  ARRV2[1, 1, 1, 2] = 5.0;
  ARRV2[1, 1, 2, 1] = 5.0;
  ARRV2[1, 1, 2, 2] = 6.0;
  ARRV2[1, 2, 1, 1] = 5.0;
  ARRV2[1, 2, 1, 2] = 6.0;
  ARRV2[1, 2, 2, 1] = 6.0;
  ARRV2[1, 2, 2, 2] = 7.0;
  ARRV2[2, 1, 1, 1] = 5.0;
  ARRV2[2, 1, 1, 2] = 6.0;
  ARRV2[2, 1, 2, 1] = 6.0;
  ARRV2[2, 1, 2, 2] = 7.0;
  ARRV2[2, 2, 1, 1] = 6.0;
  ARRV2[2, 2, 1, 2] = 7.0;
  ARRV2[2, 2, 2, 1] = 7.0;
  ARRV2[2, 2, 2, 2] = 8.0;
end TestMisc_LoopUnrolling3;
"

LoopUnrolling4 = "class TestMisc_LoopUnrolling4
  parameter Integer N = 4;
  Real ARR[1];
  Real ARR[2];
  Real ARR[3];
  Real ARR[4];
equation
  ARR[1] = 1.0;
  ARR[2] = 1.0;
  ARR[3] = 0.0;
  ARR[4] = 1.0;
end TestMisc_LoopUnrolling4;
"

LoopUnrolling5 = "class TestMisc_LoopUnrolling5
  parameter Integer N = 4;
  Real ARR[1, 1];
  Real ARR[1, 2];
  Real ARR[1, 3];
  Real ARR[1, 4];
  Real ARR[2, 1];
  Real ARR[2, 2];
  Real ARR[2, 3];
  Real ARR[2, 4];
  Real ARR[3, 1];
  Real ARR[3, 2];
  Real ARR[3, 3];
  Real ARR[3, 4];
  Real ARR[4, 1];
  Real ARR[4, 2];
  Real ARR[4, 3];
  Real ARR[4, 4];
equation
  ARR[1, 1] = 0.0;
  ARR[1, 2] = 1.0;
  ARR[1, 3] = 1.0;
  ARR[1, 4] = 1.0;
  ARR[2, 1] = 1.0;
  ARR[2, 2] = 0.0;
  ARR[2, 3] = 1.0;
  ARR[2, 4] = 1.0;
  ARR[3, 1] = 1.0;
  ARR[3, 2] = 1.0;
  ARR[3, 3] = 0.0;
  ARR[3, 4] = 1.0;
  ARR[4, 1] = 1.0;
  ARR[4, 2] = 1.0;
  ARR[4, 3] = 1.0;
  ARR[4, 4] = 0.0;
end TestMisc_LoopUnrolling5;
"

#= The reference file for TestArrayMisc =#
const arrTstFile0 = "./Arrays/TestArrayMisc.mo"
#= Define new test as tuples here.
The first item in the tuple is the reference string.
The second item is the model we want to instantiate, the last is the reference file.
=#
LoopUnrolling0TST = (LoopUnrolling0, "TestMisc.LoopUnrolling0", arrTstFile0)
LoopUnrolling1TST = (LoopUnrolling1, "TestMisc.LoopUnrolling1", arrTstFile0)
LoopUnrolling2TST = (LoopUnrolling2, "TestMisc.LoopUnrolling2", arrTstFile0)
LoopUnrolling3TST = (LoopUnrolling3, "TestMisc.LoopUnrolling3", arrTstFile0)
LoopUnrolling4TST = (LoopUnrolling4, "TestMisc.LoopUnrolling4", arrTstFile0)
LoopUnrolling5TST = (LoopUnrolling5, "TestMisc.LoopUnrolling5", arrTstFile0)

arrayTests0 = Tuple[LoopUnrolling0TST,
                    LoopUnrolling1TST,
                    LoopUnrolling2TST,
                    LoopUnrolling3TST,
                    LoopUnrolling4TST,
                    LoopUnrolling5TST]
