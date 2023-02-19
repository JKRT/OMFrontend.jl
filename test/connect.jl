#= Different tests for connect =#
module ConnectTests

const Connect1="class Connect1\n  flow Real c1_f;\n  Real c1_e;\n  flow Real c2_f;\n  Real c2_e;\nequation\n  c1_e = c2_e;\n  (-c1_f) - c2_f = 0.0;\n  c1_f = 0.0;\n  c2_f = 0.0;\n  c1_e = 1.0;\n  c2_f = time;\nend Connect1;\n"

const Connect2="class Connect2
  flow Real c1_f;
  Real c1_e;
  flow Real c2_f;
  Real c2_e;
  flow Real c3_f;
  Real c3_e;
equation
  c1_e = c2_e;
  c1_e = c3_e;
  (-c1_f) - c3_f - c2_f = 0.0;
  c1_f = 0.0;
  c2_f = 0.0;
  c3_f = 0.0;
  c1_e = 1.0;
  c2_f = time;
  c3_f = 1.0;
end Connect2;
"

const Connect3 = ""
const Connect4 = "class Connect4
  Integer c1_i;
  flow Real c1_f;
  Integer c2_i;
  flow Real c2_f;
equation
  c1_i = c2_i;
  (-c1_f) - c2_f = 0.0;
  c1_f = 0.0;
  c2_f = 0.0;
  c1_i = integer(time * 10.0);
end Connect4;
"

const Connect5="class Connect5
  Boolean c1_b;
  flow Real c1_f;
  Boolean c2_b;
  flow Real c2_f;
equation
  c1_b = c2_b;
  (-c1_f) - c2_f = 0.0;
  c1_f = 0.0;
  c2_f = 0.0;
  c1_b = time < 0_5;
end Connect5;
"

const Connect6="class Connect6
  String c1_s;
  flow Real c1_f;
  String c2_s;
  flow Real c2_f;
  Boolean b;
equation
  c1_s = c2_s;
  (-c1_f) - c2_f = 0.0;
  c1_f = 0.0;
  c2_f = 0.0;
  c1_s = \"h\";
  b = c2_s == \"h\";
end Connect6;
"
const Connect7 ="class Connect7
  parameter Integer N = 1;
  Real c[1]_r;
  flow Real c[1]_x;
  Real c[2]_r;
  flow Real c[2]_x;
  Real cx_r;
  flow Real cx_x = 2.0;
  Real cy_r;
  flow Real cy_x = time;
equation
  cx_r = c[1]_r;
  (-c[1]_x) - cx_x = 0.0;
  cy_r = c[2]_r;
  (-cy_x) - c[2]_x = 0.0;
  c[1]_x = 0.0;
  c[2]_x = 0.0;
  cx_x = 0.0;
  cy_x = 0.0;
end Connect7;
"

const Connect8 = "class Connect8
  parameter Integer N = 2;
  Real c[1]_r;
  flow Real c[1]_x;
  Real c[2]_r;
  flow Real c[2]_x;
  Real cx_r;
  flow Real cx_x = 1.0;
  Real cy_r;
  flow Real cy_x = time;
equation
  c[2]_r = cx_r;
  c[2]_r = cy_r;
  (-c[2]_x) - cx_x - cy_x = 0.0;
  c[1]_x = 0.0;
  c[2]_x = 0.0;
  cx_x = 0.0;
  cy_x = 0.0;
  c[1]_x = time;
end Connect8;
"
const Connect9 = "class Connect9\n  input Real c1_x;\n  output Real c2_x;\nequation\n  c2_x = c1_x;\nend Connect9;\n"
const Connect10 = "class Connect10\n  input Real c1_x;\n  input Real c2_x;\nequation\n  c2_x = c1_x;\nend Connect10;\n"
const Connect11 = "class Connect11\n  output Real c1_x;\n  output Real c2_x;\nequation\n  c2_x = c1_x;\nend Connect11;\n"
const Connect12 = ""
const Connect13 = ""
const Connect14 = ""
const Connect15 = "class Connect15
  flow Real t1_p[1]_i;
  Real t1_p[1]_v;
  flow Real t1_p[2]_i;
  Real t1_p[2]_v;
  flow Real t2_p[1]_i;
  Real t2_p[1]_v;
  flow Real t2_p[2]_i;
  Real t2_p[2]_v;
equation
  t2_p[2]_v = t1_p[1]_v;
  t1_p[1]_i + t2_p[2]_i = 0.0;
  t1_p[2]_i = 0.0;
  t2_p[1]_i = 0.0;
end Connect15;
"
const Connect16 = ""
const Connect17 = "class Connect17
  parameter Integer p = 3;
  input Real x[1];
  input Real x[2];
  input Real x[3];
  output Real y[1];
  output Real y[2];
  output Real y[3];
  output Real y[4];
equation
  x[1] = y[2];
  y[3] = x[2];
  y[4] = x[3];
end Connect17;
"
const Connect18 = "class Connect18
  constant Integer n = 5;
  Real a[1]_e;
  flow Real a[1]_f;
  Real a[2]_e;
  flow Real a[2]_f;
  Real a[3]_e;
  flow Real a[3]_f;
  Real a[4]_e;
  flow Real a[4]_f;
  Real a[5]_e;
  flow Real a[5]_f;
equation
  a[3]_e = a[5]_e;
  a[3]_e = a[2]_e;
  a[3]_e = a[1]_e;
  a[3]_e = a[4]_e;
  (-a[3]_f) - a[1]_f - a[5]_f - a[4]_f - a[2]_f = 0.0;
  a[1]_f = 0.0;
  a[2]_f = 0.0;
  a[3]_f = 0.0;
  a[4]_f = 0.0;
  a[5]_f = 0.0;
  a[1]_e = 1.0;
end Connect18;
"

const Tank = "class Tank\n  parameter Real area = 1.0;\n  Real inlet_pressure;\n  flow Real inlet_volumeFlowRate;\n  Real outlet_pressure;\n  flow Real outlet_volumeFlowRate;\n  Real level(start = 2.0);\nequation\n  inlet_volumeFlowRate = 0.0;\n  outlet_volumeFlowRate = 0.0;\n  inlet_volumeFlowRate = 1.0;\n  inlet_pressure = 1.0;\n  area * der(level) = inlet_volumeFlowRate + outlet_volumeFlowRate;\n  outlet_pressure = inlet_pressure;\n  outlet_volumeFlowRate = 2.0;\nend Tank;\n"

const HeatTank = "class HeatTank\n  parameter Real area = 1.0;\n  Real inlet_pressure;\n  flow Real inlet_volumeFlowRate;\n  Real inlet_temp;\n  Real outlet_pressure;\n  flow Real outlet_volumeFlowRate;\n  Real outlet_temp;\n  Real level(start = 2.0);\n  Real temp;\nequation\n  inlet_volumeFlowRate = 0.0;\n  outlet_volumeFlowRate = 0.0;\n  inlet_temp = 25.0;\n  area * level * der(temp) = inlet_volumeFlowRate * inlet_temp + outlet_volumeFlowRate * outlet_temp;\n  outlet_temp = temp;\n  inlet_volumeFlowRate = 1.0;\n  inlet_pressure = 1.0;\n  area * der(level) = inlet_volumeFlowRate + outlet_volumeFlowRate;\n  outlet_pressure = inlet_pressure;\n  outlet_volumeFlowRate = 2.0;\nend HeatTank;\n"

const HeatTankExpanded="class HeatTankExpanded\n  parameter Real Area = 1.0;\n  Real inlet_pressure;\n  flow Real inlet_volumeFlowRate;\n  Real inlet_temp;\n  Real outlet_pressure;\n  flow Real outlet_volumeFlowRate;\n  Real outlet_temp;\n  Real level(start = 2.0);\n  Real temp;\nequation\n  inlet_volumeFlowRate = 0.0;\n  outlet_volumeFlowRate = 0.0;\n  inlet_volumeFlowRate = 1.0;\n  inlet_pressure = 1.0;\n  inlet_temp = 25.0;\n  Area * der(level) = inlet_volumeFlowRate + outlet_volumeFlowRate;\n  outlet_pressure = inlet_pressure;\n  Area * level * der(temp) = inlet_volumeFlowRate * inlet_temp + outlet_volumeFlowRate * outlet_temp;\n  outlet_temp = temp;\n  outlet_volumeFlowRate = 2.0;\nend HeatTankExpanded;\n"

#Tests that multiple inheritance is handled correctly with regards to connect_
const MultipleInheritanceConnect="class MultipleInheritanceConnect\n  Real e_port_p;\n  flow Real e_port_f;\n  Real e_d_port_p;\n  flow Real e_d_port_f;\nequation\n  e_port_p = e_d_port_p;\n  e_port_f = 0.0;\n  e_d_port_f - e_port_f = 0.0;\n  e_d_port_f = e_d_port_p;\nend MultipleInheritanceConnect;\n"

const ResistorCircuit0 = "class ElectricalComponentTest_ResistorCircuit0\n  Real R1_v;\n  Real R1_i;\n  Real R1_p_v;\n  flow Real R1_p_i;\n  Real R1_n_v;\n  flow Real R1_n_i;\n  parameter Real R1_R = 100.0;\n  Real R2_v;\n  Real R2_i;\n  Real R2_p_v;\n  flow Real R2_p_i;\n  Real R2_n_v;\n  flow Real R2_n_i;\n  parameter Real R2_R = 200.0;\nequation\n  R2_p_v = R1_p_v;\n  R2_p_i + R1_p_i = 0.0;\n  R1_n_i = 0.0;\n  R2_n_i = 0.0;\n  R1_R * R1_i = R1_v;\n  R1_v = R1_p_v - R1_n_v;\n  0.0 = R1_p_i + R1_n_i;\n  R1_i = R1_p_i;\n  R2_R * R2_i = R2_v;\n  R2_v = R2_p_v - R2_n_v;\n  0.0 = R2_p_i + R2_n_i;\n  R2_i = R2_p_i;\nend ElectricalComponentTest_ResistorCircuit0;\n"

const ResistorCircuit1 = "class ElectricalComponentTest_ResistorCircuit1\n  Real R1_v;\n  Real R1_i;\n  Real R1_p_v;\n  flow Real R1_p_i;\n  Real R1_n_v;\n  flow Real R1_n_i;\n  parameter Real R1_R = 100.0;\n  Real R2_v;\n  Real R2_i;\n  Real R2_p_v;\n  flow Real R2_p_i;\n  Real R2_n_v;\n  flow Real R2_n_i;\n  parameter Real R2_R = 200.0;\n  Real R3_v;\n  Real R3_i;\n  Real R3_p_v;\n  flow Real R3_p_i;\n  Real R3_n_v;\n  flow Real R3_n_i;\n  parameter Real R3_R = 300.0;\nequation\n  R2_p_v = R3_p_v;\n  R2_p_v = R1_p_v;\n  R1_n_i = 0.0;\n  R2_p_i + R3_p_i + R1_p_i = 0.0;\n  R2_n_i = 0.0;\n  R3_n_i = 0.0;\n  R1_R * R1_i = R1_v;\n  R1_v = R1_p_v - R1_n_v;\n  0.0 = R1_p_i + R1_n_i;\n  R1_i = R1_p_i;\n  R2_R * R2_i = R2_v;\n  R2_v = R2_p_v - R2_n_v;\n  0.0 = R2_p_i + R2_n_i;\n  R2_i = R2_p_i;\n  R3_R * R3_i = R3_v;\n  R3_v = R3_p_v - R3_n_v;\n  0.0 = R3_p_i + R3_n_i;\n  R3_i = R3_p_i;\nend ElectricalComponentTest_ResistorCircuit1;\n"

const SimpleCircuit = "class ElectricalComponentTest_SimpleCircuit\n  Real R1_v;\n  Real R1_i;\n  Real R1_p_v;\n  flow Real R1_p_i;\n  Real R1_n_v;\n  flow Real R1_n_i;\n  parameter Real R1_R = 10.0;\n  Real C_v;\n  Real C_i;\n  Real C_p_v;\n  flow Real C_p_i;\n  Real C_n_v;\n  flow Real C_n_i;\n  parameter Real C_C = 0.01;\n  Real R2_v;\n  Real R2_i;\n  Real R2_p_v;\n  flow Real R2_p_i;\n  Real R2_n_v;\n  flow Real R2_n_i;\n  parameter Real R2_R = 100.0;\n  Real L_v;\n  Real L_i;\n  Real L_p_v;\n  flow Real L_p_i;\n  Real L_n_v;\n  flow Real L_n_i;\n  parameter Real L_L = 0_1;\n  Real AC_v;\n  Real AC_i;\n  Real AC_p_v;\n  flow Real AC_p_i;\n  Real AC_n_v;\n  flow Real AC_n_i;\n  parameter Real AC_A = 1.0;\n  parameter Real AC_w = 1.0;\n  Real G_p_v;\n  flow Real G_p_i;\nequation\n  R2_p_v = AC_p_v;\n  R2_p_v = R1_p_v;\n  C_p_v = R1_n_v;\n  R2_n_v = L_p_v;\n  AC_n_v = C_n_v;\n  AC_n_v = L_n_v;\n  AC_n_v = G_p_v;\n  R1_n_i + C_p_i = 0.0;\n  L_p_i + R2_n_i = 0.0;\n  G_p_i + C_n_i + L_n_i + AC_n_i = 0.0;\n  R1_p_i + R2_p_i + AC_p_i = 0.0;\n  R1_R * R1_i = R1_v;\n  R1_v = R1_p_v - R1_n_v;\n  0.0 = R1_p_i + R1_n_i;\n  R1_i = R1_p_i;\n  C_i = C_C * der(C_v);\n  C_v = C_p_v - C_n_v;\n  0.0 = C_p_i + C_n_i;\n  C_i = C_p_i;\n  R2_R * R2_i = R2_v;\n  R2_v = R2_p_v - R2_n_v;\n  0.0 = R2_p_i + R2_n_i;\n  R2_i = R2_p_i;\n  L_L * der(L_i) = L_v;\n  L_v = L_p_v - L_n_v;\n  0.0 = L_p_i + L_n_i;\n  L_i = L_p_i;\n  AC_v = AC_A * sin(AC_w * time);\n  AC_v = AC_p_v - AC_n_v;\n  0.0 = AC_p_i + AC_n_i;\n  AC_i = AC_p_i;\n  G_p_v = 0.0;\nend ElectricalComponentTest_SimpleCircuit;\n"

end
