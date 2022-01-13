#= Different tests for connect =#
module ConnectTests

const Connect1="class Connect1\n  flow Real c1.f;\n  Real c1.e;\n  flow Real c2.f;\n  Real c2.e;\nequation\n  c1.e = c2.e;\n  (-c1.f) - c2.f = 0.0;\n  c1.f = 0.0;\n  c2.f = 0.0;\n  c1.e = 1.0;\n  c2.f = time;\nend Connect1;\n"

const Connect2="class Connect2
  flow Real c1.f;
  Real c1.e;
  flow Real c2.f;
  Real c2.e;
  flow Real c3.f;
  Real c3.e;
equation
  c1.e = c2.e;
  c1.e = c3.e;
  (-c1.f) - c3.f - c2.f = 0.0;
  c1.f = 0.0;
  c2.f = 0.0;
  c3.f = 0.0;
  c1.e = 1.0;
  c2.f = time;
  c3.f = 1.0;
end Connect2;
"

const Connect3 = ""
const Connect4 = "class Connect4
  Integer c1.i;
  flow Real c1.f;
  Integer c2.i;
  flow Real c2.f;
equation
  c1.i = c2.i;
  (-c1.f) - c2.f = 0.0;
  c1.f = 0.0;
  c2.f = 0.0;
  c1.i = integer(time * 10.0);
end Connect4;
"

const Connect5="class Connect5
  Boolean c1.b;
  flow Real c1.f;
  Boolean c2.b;
  flow Real c2.f;
equation
  c1.b = c2.b;
  (-c1.f) - c2.f = 0.0;
  c1.f = 0.0;
  c2.f = 0.0;
  c1.b = time < 0.5;
end Connect5;
"

const Connect6="class Connect6
  String c1.s;
  flow Real c1.f;
  String c2.s;
  flow Real c2.f;
  Boolean b;
equation
  c1.s = c2.s;
  (-c1.f) - c2.f = 0.0;
  c1.f = 0.0;
  c2.f = 0.0;
  c1.s = \"h\";
  b = c2.s == \"h\";
end Connect6;
"
const Connect7 ="class Connect7
  parameter Integer N = 1;
  Real c[1].r;
  flow Real c[1].x;
  Real c[2].r;
  flow Real c[2].x;
  Real cx.r;
  flow Real cx.x = 2.0;
  Real cy.r;
  flow Real cy.x = time;
equation
  cx.r = c[1].r;
  (-c[1].x) - cx.x = 0.0;
  cy.r = c[2].r;
  (-cy.x) - c[2].x = 0.0;
  c[1].x = 0.0;
  c[2].x = 0.0;
  cx.x = 0.0;
  cy.x = 0.0;
end Connect7;
"

const Connect8 = "class Connect8
  parameter Integer N = 2;
  Real c[1].r;
  flow Real c[1].x;
  Real c[2].r;
  flow Real c[2].x;
  Real cx.r;
  flow Real cx.x = 1.0;
  Real cy.r;
  flow Real cy.x = time;
equation
  c[2].r = cx.r;
  c[2].r = cy.r;
  (-c[2].x) - cx.x - cy.x = 0.0;
  c[1].x = 0.0;
  c[2].x = 0.0;
  cx.x = 0.0;
  cy.x = 0.0;
  c[1].x = time;
end Connect8;
"
const Connect9 = "class Connect9\n  input Real c1.x;\n  output Real c2.x;\nequation\n  c2.x = c1.x;\nend Connect9;\n"
const Connect10 = "class Connect10\n  input Real c1.x;\n  input Real c2.x;\nequation\n  c2.x = c1.x;\nend Connect10;\n"
const Connect11 = "class Connect11\n  output Real c1.x;\n  output Real c2.x;\nequation\n  c2.x = c1.x;\nend Connect11;\n"
const Connect12 = ""
const Connect13 = ""
const Connect14 = ""
const Connect15 = "class Connect15
  flow Real t1.p[1].i;
  Real t1.p[1].v;
  flow Real t1.p[2].i;
  Real t1.p[2].v;
  flow Real t2.p[1].i;
  Real t2.p[1].v;
  flow Real t2.p[2].i;
  Real t2.p[2].v;
equation
  t2.p[2].v = t1.p[1].v;
  t1.p[1].i + t2.p[2].i = 0.0;
  t1.p[2].i = 0.0;
  t2.p[1].i = 0.0;
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
  Real a[1].e;
  flow Real a[1].f;
  Real a[2].e;
  flow Real a[2].f;
  Real a[3].e;
  flow Real a[3].f;
  Real a[4].e;
  flow Real a[4].f;
  Real a[5].e;
  flow Real a[5].f;
equation
  a[3].e = a[5].e;
  a[3].e = a[2].e;
  a[3].e = a[1].e;
  a[3].e = a[4].e;
  (-a[3].f) - a[1].f - a[5].f - a[4].f - a[2].f = 0.0;
  a[1].f = 0.0;
  a[2].f = 0.0;
  a[3].f = 0.0;
  a[4].f = 0.0;
  a[5].f = 0.0;
  a[1].e = 1.0;
end Connect18;
"

const Tank = "class Tank\n  parameter Real area = 1.0;\n  Real inlet.pressure;\n  flow Real inlet.volumeFlowRate;\n  Real outlet.pressure;\n  flow Real outlet.volumeFlowRate;\n  Real level(start = 2.0);\nequation\n  inlet.volumeFlowRate = 0.0;\n  outlet.volumeFlowRate = 0.0;\n  inlet.volumeFlowRate = 1.0;\n  inlet.pressure = 1.0;\n  area * der(level) = inlet.volumeFlowRate + outlet.volumeFlowRate;\n  outlet.pressure = inlet.pressure;\n  outlet.volumeFlowRate = 2.0;\nend Tank;\n"

const HeatTank = "class HeatTank\n  parameter Real area = 1.0;\n  Real inlet.pressure;\n  flow Real inlet.volumeFlowRate;\n  Real inlet.temp;\n  Real outlet.pressure;\n  flow Real outlet.volumeFlowRate;\n  Real outlet.temp;\n  Real level(start = 2.0);\n  Real temp;\nequation\n  inlet.volumeFlowRate = 0.0;\n  outlet.volumeFlowRate = 0.0;\n  inlet.temp = 25.0;\n  area * level * der(temp) = inlet.volumeFlowRate * inlet.temp + outlet.volumeFlowRate * outlet.temp;\n  outlet.temp = temp;\n  inlet.volumeFlowRate = 1.0;\n  inlet.pressure = 1.0;\n  area * der(level) = inlet.volumeFlowRate + outlet.volumeFlowRate;\n  outlet.pressure = inlet.pressure;\n  outlet.volumeFlowRate = 2.0;\nend HeatTank;\n"

const HeatTankExpanded="class HeatTankExpanded\n  parameter Real Area = 1.0;\n  Real inlet.pressure;\n  flow Real inlet.volumeFlowRate;\n  Real inlet.temp;\n  Real outlet.pressure;\n  flow Real outlet.volumeFlowRate;\n  Real outlet.temp;\n  Real level(start = 2.0);\n  Real temp;\nequation\n  inlet.volumeFlowRate = 0.0;\n  outlet.volumeFlowRate = 0.0;\n  inlet.volumeFlowRate = 1.0;\n  inlet.pressure = 1.0;\n  inlet.temp = 25.0;\n  Area * der(level) = inlet.volumeFlowRate + outlet.volumeFlowRate;\n  outlet.pressure = inlet.pressure;\n  Area * level * der(temp) = inlet.volumeFlowRate * inlet.temp + outlet.volumeFlowRate * outlet.temp;\n  outlet.temp = temp;\n  outlet.volumeFlowRate = 2.0;\nend HeatTankExpanded;\n"

#Tests that multiple inheritance is handled correctly with regards to connect.
const MultipleInheritanceConnect="class MultipleInheritanceConnect\n  Real e.port.p;\n  flow Real e.port.f;\n  Real e.d.port.p;\n  flow Real e.d.port.f;\nequation\n  e.port.p = e.d.port.p;\n  e.port.f = 0.0;\n  e.d.port.f - e.port.f = 0.0;\n  e.d.port.f = e.d.port.p;\nend MultipleInheritanceConnect;\n"

const ResistorCircuit0 = "class ElectricalComponentTest.ResistorCircuit0\n  Real R1.v;\n  Real R1.i;\n  Real R1.p.v;\n  flow Real R1.p.i;\n  Real R1.n.v;\n  flow Real R1.n.i;\n  parameter Real R1.R = 100.0;\n  Real R2.v;\n  Real R2.i;\n  Real R2.p.v;\n  flow Real R2.p.i;\n  Real R2.n.v;\n  flow Real R2.n.i;\n  parameter Real R2.R = 200.0;\nequation\n  R2.p.v = R1.p.v;\n  R2.p.i + R1.p.i = 0.0;\n  R1.n.i = 0.0;\n  R2.n.i = 0.0;\n  R1.R * R1.i = R1.v;\n  R1.v = R1.p.v - R1.n.v;\n  0.0 = R1.p.i + R1.n.i;\n  R1.i = R1.p.i;\n  R2.R * R2.i = R2.v;\n  R2.v = R2.p.v - R2.n.v;\n  0.0 = R2.p.i + R2.n.i;\n  R2.i = R2.p.i;\nend ElectricalComponentTest.ResistorCircuit0;\n"

const ResistorCircuit1 = "class ElectricalComponentTest.ResistorCircuit1\n  Real R1.v;\n  Real R1.i;\n  Real R1.p.v;\n  flow Real R1.p.i;\n  Real R1.n.v;\n  flow Real R1.n.i;\n  parameter Real R1.R = 100.0;\n  Real R2.v;\n  Real R2.i;\n  Real R2.p.v;\n  flow Real R2.p.i;\n  Real R2.n.v;\n  flow Real R2.n.i;\n  parameter Real R2.R = 200.0;\n  Real R3.v;\n  Real R3.i;\n  Real R3.p.v;\n  flow Real R3.p.i;\n  Real R3.n.v;\n  flow Real R3.n.i;\n  parameter Real R3.R = 300.0;\nequation\n  R2.p.v = R3.p.v;\n  R2.p.v = R1.p.v;\n  R1.n.i = 0.0;\n  R2.p.i + R3.p.i + R1.p.i = 0.0;\n  R2.n.i = 0.0;\n  R3.n.i = 0.0;\n  R1.R * R1.i = R1.v;\n  R1.v = R1.p.v - R1.n.v;\n  0.0 = R1.p.i + R1.n.i;\n  R1.i = R1.p.i;\n  R2.R * R2.i = R2.v;\n  R2.v = R2.p.v - R2.n.v;\n  0.0 = R2.p.i + R2.n.i;\n  R2.i = R2.p.i;\n  R3.R * R3.i = R3.v;\n  R3.v = R3.p.v - R3.n.v;\n  0.0 = R3.p.i + R3.n.i;\n  R3.i = R3.p.i;\nend ElectricalComponentTest.ResistorCircuit1;\n"

const SimpleCircuit = "class ElectricalComponentTest.SimpleCircuit\n  Real R1.v;\n  Real R1.i;\n  Real R1.p.v;\n  flow Real R1.p.i;\n  Real R1.n.v;\n  flow Real R1.n.i;\n  parameter Real R1.R = 10.0;\n  Real C.v;\n  Real C.i;\n  Real C.p.v;\n  flow Real C.p.i;\n  Real C.n.v;\n  flow Real C.n.i;\n  parameter Real C.C = 0.01;\n  Real R2.v;\n  Real R2.i;\n  Real R2.p.v;\n  flow Real R2.p.i;\n  Real R2.n.v;\n  flow Real R2.n.i;\n  parameter Real R2.R = 100.0;\n  Real L.v;\n  Real L.i;\n  Real L.p.v;\n  flow Real L.p.i;\n  Real L.n.v;\n  flow Real L.n.i;\n  parameter Real L.L = 0.1;\n  Real AC.v;\n  Real AC.i;\n  Real AC.p.v;\n  flow Real AC.p.i;\n  Real AC.n.v;\n  flow Real AC.n.i;\n  parameter Real AC.A = 1.0;\n  parameter Real AC.w = 1.0;\n  Real G.p.v;\n  flow Real G.p.i;\nequation\n  R2.p.v = AC.p.v;\n  R2.p.v = R1.p.v;\n  C.p.v = R1.n.v;\n  R2.n.v = L.p.v;\n  AC.n.v = C.n.v;\n  AC.n.v = L.n.v;\n  AC.n.v = G.p.v;\n  R1.n.i + C.p.i = 0.0;\n  L.p.i + R2.n.i = 0.0;\n  G.p.i + C.n.i + L.n.i + AC.n.i = 0.0;\n  R1.p.i + R2.p.i + AC.p.i = 0.0;\n  R1.R * R1.i = R1.v;\n  R1.v = R1.p.v - R1.n.v;\n  0.0 = R1.p.i + R1.n.i;\n  R1.i = R1.p.i;\n  C.i = C.C * der(C.v);\n  C.v = C.p.v - C.n.v;\n  0.0 = C.p.i + C.n.i;\n  C.i = C.p.i;\n  R2.R * R2.i = R2.v;\n  R2.v = R2.p.v - R2.n.v;\n  0.0 = R2.p.i + R2.n.i;\n  R2.i = R2.p.i;\n  L.L * der(L.i) = L.v;\n  L.v = L.p.v - L.n.v;\n  0.0 = L.p.i + L.n.i;\n  L.i = L.p.i;\n  AC.v = AC.A * sin(AC.w * time);\n  AC.v = AC.p.v - AC.n.v;\n  0.0 = AC.p.i + AC.n.i;\n  AC.i = AC.p.i;\n  G.p.v = 0.0;\nend ElectricalComponentTest.SimpleCircuit;\n"

end

