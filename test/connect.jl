#= Different tests for connect =#
module ConnectTests

const Connect1="class Connect1
  flow Real c1_f;
  Real c1_e;
  flow Real c2_f;
  Real c2_e;
equation
  c1_e = c2_e;
  (-c2_f) - c1_f = 0.0;
  c1_f = 0.0;
  c2_f = 0.0;
  c1_e = 1.0;
  c2_f = time;
end Connect1;
"
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
  (-c1_f) - c2_f - c3_f = 0.0;
  c1_f = 0.0;
  c2_f = 0.0;
  c3_f = 0.0;
  c1_e = 1.0;
  c2_f = time;
  c3_f = 1.0;
end Connect2;
"

const Connect3 = ""
const Connect4 ="class Connect4
  Integer c1_i;
  flow Real c1_f;
  Integer c2_i;
  flow Real c2_f;
equation
  c1_i = c2_i;
  (-c2_f) - c1_f = 0.0;
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
  c2_b = c1_b;
  (-c2_f) - c1_f = 0.0;
  c1_f = 0.0;
  c2_f = 0.0;
  c1_b = time < 0.5;
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
  (-c2_f) - c1_f = 0.0;
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
  c[1]_r = cx_r;
  (-c[1]_x) - cx_x = 0.0;
  c[2]_r = cy_r;
  (-c[2]_x) - cy_x = 0.0;
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
  c[2]_r = cy_r;
  c[2]_r = cx_r;
  (-cx_x) - cy_x - c[2]_x = 0.0;
  c[1]_x = 0.0;
  c[2]_x = 0.0;
  cx_x = 0.0;
  cy_x = 0.0;
  c[1]_x = time;
end Connect8;
"
const Connect9 = "class Connect9
  input Real c1_x;
  output Real c2_x;
equation
  c1_x = c2_x;
end Connect9;
"
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
  t1_p[1]_v = t2_p[2]_v;
  t2_p[2]_i + t1_p[1]_i = 0.0;
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
  a[5]_e = a[2]_e;
  a[5]_e = a[1]_e;
  a[5]_e = a[3]_e;
  a[5]_e = a[4]_e;
  (-a[4]_f) - a[5]_f - a[1]_f - a[2]_f - a[3]_f = 0.0;
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
const MultipleInheritanceConnect="class MultipleInheritanceConnect
  Real e_port_p;
  flow Real e_port_f;
  Real e_d_port_p;
  flow Real e_d_port_f;
equation
  e_port_p = e_d_port_p;
  e_port_f = 0.0;
  (-e_port_f) + e_d_port_f = 0.0;
  e_d_port_f = e_d_port_p;
end MultipleInheritanceConnect;
"

const ResistorCircuit0 = "class ElectricalComponentTest_ResistorCircuit0
  Real R1_v;
  Real R1_i;
  Real R1_p_v;
  flow Real R1_p_i;
  Real R1_n_v;
  flow Real R1_n_i;
  parameter Real R1_R = 100.0;
  Real R2_v;
  Real R2_i;
  Real R2_p_v;
  flow Real R2_p_i;
  Real R2_n_v;
  flow Real R2_n_i;
  parameter Real R2_R = 200.0;
equation
  R1_p_v = R2_p_v;
  R1_p_i + R2_p_i = 0.0;
  R1_n_i = 0.0;
  R2_n_i = 0.0;
  R1_R * R1_i = R1_v;
  R1_v = R1_p_v - R1_n_v;
  0.0 = R1_p_i + R1_n_i;
  R1_i = R1_p_i;
  R2_R * R2_i = R2_v;
  R2_v = R2_p_v - R2_n_v;
  0.0 = R2_p_i + R2_n_i;
  R2_i = R2_p_i;
end ElectricalComponentTest_ResistorCircuit0;
"

const ResistorCircuit1 = "class ElectricalComponentTest_ResistorCircuit1
  Real R1_v;
  Real R1_i;
  Real R1_p_v;
  flow Real R1_p_i;
  Real R1_n_v;
  flow Real R1_n_i;
  parameter Real R1_R = 100.0;
  Real R2_v;
  Real R2_i;
  Real R2_p_v;
  flow Real R2_p_i;
  Real R2_n_v;
  flow Real R2_n_i;
  parameter Real R2_R = 200.0;
  Real R3_v;
  Real R3_i;
  Real R3_p_v;
  flow Real R3_p_i;
  Real R3_n_v;
  flow Real R3_n_i;
  parameter Real R3_R = 300.0;
equation
  R3_p_v = R1_p_v;
  R3_p_v = R2_p_v;
  R1_n_i = 0.0;
  R1_p_i + R2_p_i + R3_p_i = 0.0;
  R2_n_i = 0.0;
  R3_n_i = 0.0;
  R1_R * R1_i = R1_v;
  R1_v = R1_p_v - R1_n_v;
  0.0 = R1_p_i + R1_n_i;
  R1_i = R1_p_i;
  R2_R * R2_i = R2_v;
  R2_v = R2_p_v - R2_n_v;
  0.0 = R2_p_i + R2_n_i;
  R2_i = R2_p_i;
  R3_R * R3_i = R3_v;
  R3_v = R3_p_v - R3_n_v;
  0.0 = R3_p_i + R3_n_i;
  R3_i = R3_p_i;
end ElectricalComponentTest_ResistorCircuit1;
"

const SimpleCircuit = "class ElectricalComponentTest_SimpleCircuit
  Real R1_v;
  Real R1_i;
  Real R1_p_v;
  flow Real R1_p_i;
  Real R1_n_v;
  flow Real R1_n_i;
  parameter Real R1_R = 10.0;
  Real C_v;
  Real C_i;
  Real C_p_v;
  flow Real C_p_i;
  Real C_n_v;
  flow Real C_n_i;
  parameter Real C_C = 0.01;
  Real R2_v;
  Real R2_i;
  Real R2_p_v;
  flow Real R2_p_i;
  Real R2_n_v;
  flow Real R2_n_i;
  parameter Real R2_R = 100.0;
  Real L_v;
  Real L_i;
  Real L_p_v;
  flow Real L_p_i;
  Real L_n_v;
  flow Real L_n_i;
  parameter Real L_L = 0.1;
  Real AC_v;
  Real AC_i;
  Real AC_p_v;
  flow Real AC_p_i;
  Real AC_n_v;
  flow Real AC_n_i;
  parameter Real AC_A = 1.0;
  parameter Real AC_w = 1.0;
  Real G_p_v;
  flow Real G_p_i;
equation
  R2_p_v = AC_p_v;
  R2_p_v = R1_p_v;
  R1_n_v = C_p_v;
  R2_n_v = L_p_v;
  AC_n_v = C_n_v;
  AC_n_v = G_p_v;
  AC_n_v = L_n_v;
  R1_n_i + C_p_i = 0.0;
  L_p_i + R2_n_i = 0.0;
  AC_n_i + G_p_i + C_n_i + L_n_i = 0.0;
  R1_p_i + AC_p_i + R2_p_i = 0.0;
  R1_R * R1_i = R1_v;
  R1_v = R1_p_v - R1_n_v;
  0.0 = R1_p_i + R1_n_i;
  R1_i = R1_p_i;
  C_i = C_C * der(C_v);
  C_v = C_p_v - C_n_v;
  0.0 = C_p_i + C_n_i;
  C_i = C_p_i;
  R2_R * R2_i = R2_v;
  R2_v = R2_p_v - R2_n_v;
  0.0 = R2_p_i + R2_n_i;
  R2_i = R2_p_i;
  L_L * der(L_i) = L_v;
  L_v = L_p_v - L_n_v;
  0.0 = L_p_i + L_n_i;
  L_i = L_p_i;
  AC_v = AC_A * sin(AC_w * time);
  AC_v = AC_p_v - AC_n_v;
  0.0 = AC_p_i + AC_n_i;
  AC_i = AC_p_i;
  G_p_v = 0.0;
end ElectricalComponentTest_SimpleCircuit;
"

end
