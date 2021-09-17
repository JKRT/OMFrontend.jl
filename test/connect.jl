#= Different tests for connect =#
module ConnectTests

const Connect1="class Connect1\n  flow Real c1.f;\n  Real c1.e;\n  flow Real c2.f;\n  Real c2.e;\nequation\n  c1.e = c2.e;\n  (-c1.f) - c2.f = 0.0;\n  c1.f = 0.0;\n  c2.f = 0.0;\n  c1.e = 1.0;\n  c2.f = time;\nend Connect1;\n"

const Connect5="class Connect5
  Real a.c1.e(start = 1.0);
  flow Real a.c1.f;
  Real c2.e;
  flow Real c2.f;
equation
  c2.e = a.c1.e;
  a.c1.f - c2.f = 0.0;
  c2.f = 0.0;
end Connect5;
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

