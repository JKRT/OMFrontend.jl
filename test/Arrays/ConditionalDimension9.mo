// Test: use_portsData=false with nPorts > 0.
// This is the actual RoomCO2 pattern. When use_portsData=false:
//   - portsData has dimension [if false then nPorts else 0] = [0]
//   - portsData_diameter_internal has dimension [nPorts] = [2]
//   - The binding portsData_diameter_internal = portsData.diameter is guarded
//     by "if use_portsData and nPorts > 0" which is false
// The type checker should NOT check dimension compatibility of an inactive
// conditional binding, but it does, causing "expected [2], got [0]".

record VesselPortsData
  Real diameter = 0.01;
  Real height = 0.0;
  Real zeta_in = 0.0;
  Real zeta_out = 0.0;
end VesselPortsData;

partial model PartialLumpedVessel
  parameter Integer nPorts = 0 "Number of ports"
    annotation(Evaluate = true);
  parameter Boolean use_portsData = true
    annotation(Evaluate = true);
  parameter VesselPortsData[if use_portsData then nPorts else 0] portsData;
protected
  Real[nPorts] portsData_diameter_internal = portsData.diameter
    if use_portsData and nPorts > 0;
  Real[nPorts] portsData_diameter;
equation
  if use_portsData then
    portsData_diameter = portsData_diameter_internal;
  else
    portsData_diameter = fill(0.04, nPorts);
  end if;
end PartialLumpedVessel;

model ClosedVolume
  extends PartialLumpedVessel;
  Real p = 1e5;
  Real T = 300.0;
end ClosedVolume;

model ConditionalDimension9
  ClosedVolume volume(nPorts = 2, use_portsData = false);
end ConditionalDimension9;
