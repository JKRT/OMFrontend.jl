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

const HeatTank = "class HeatTank\n  parameter Real area = 1.0;\n  Real inlet.pressure;\n  flow Real inlet.volumeFlowRate;\n  Real inlet.temp;\n  Real outlet.pressure;\n  flow Real outlet.volumeFlowRate;\n  Real outlet.temp;\n  Real level(start = 2.0);\n  Real temp;\nequation\n  inlet.volumeFlowRate = 0.0;\n  outlet.volumeFlowRate = 0.0;\n  inlet.temp = 25.0;\n  area * level * der(temp) = inlet.volumeFlowRate * inlet.temp + outlet.volumeFlowRate * outlet.temp;\n  outlet.temp = temp;\n  inlet.volumeFlowRate = 1.0;\n  inlet.pressure = 1.0;\n  area * der(level) = inlet.volumeFlowRate + outlet.volumeFlowRate;\n  outlet.pressure = inlet.pressure;\n  outlet.volumeFlowRate = 2.0;\nend HeatTank;\n"

const HeatTankExpanded="class HeatTankExpanded\n  parameter Real Area = 1.0;\n  Real inlet.pressure;\n  flow Real inlet.volumeFlowRate;\n  Real inlet.temp;\n  Real outlet.pressure;\n  flow Real outlet.volumeFlowRate;\n  Real outlet.temp;\n  Real level(start = 2.0);\n  Real temp;\nequation\n  inlet.volumeFlowRate = 0.0;\n  outlet.volumeFlowRate = 0.0;\n  inlet.volumeFlowRate = 1.0;\n  inlet.pressure = 1.0;\n  inlet.temp = 25.0;\n  Area * der(level) = inlet.volumeFlowRate + outlet.volumeFlowRate;\n  outlet.pressure = inlet.pressure;\n  Area * level * der(temp) = inlet.volumeFlowRate * inlet.temp + outlet.volumeFlowRate * outlet.temp;\n  outlet.temp = temp;\n  outlet.volumeFlowRate = 2.0;\nend HeatTankExpanded;\n"
end

