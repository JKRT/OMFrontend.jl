// Test: mirrors the exact MSL pattern more closely.
// PartialLumpedVolume -> PartialLumpedVessel -> ClosedVolume
// Key: nPorts defaults to 0 in the base, connectorSizing sets it via modification.
// The Evaluate=true annotation on use_portsData and nPorts matters.

record VesselPortsData
  Real diameter = 0.01;
  Real height = 0.0;
  Real zeta_in = 0.0;
  Real zeta_out = 0.0;
end VesselPortsData;

partial model PartialLumpedVolume
  Real p "Pressure";
  Real T "Temperature";
end PartialLumpedVolume;

partial model PartialLumpedVessel
  extends PartialLumpedVolume;
  parameter Integer nPorts = 0 "Number of ports"
    annotation(Evaluate = true);
  parameter Boolean use_portsData = true
    "= false to neglect pressure loss and kinetic energy"
    annotation(Evaluate = true);
  parameter VesselPortsData[if use_portsData then nPorts else 0] portsData
    "Data of inlet/outlet ports";
protected
  Real[nPorts] portsData_diameter_internal = portsData.diameter
    if use_portsData and nPorts > 0;
  Real[nPorts] portsData_height_internal = portsData.height
    if use_portsData and nPorts > 0;
  Real[nPorts] portsData_diameter;
  Real[nPorts] portsData_height;
equation
  if use_portsData then
    portsData_diameter = portsData_diameter_internal;
    portsData_height = portsData_height_internal;
  else
    portsData_diameter = fill(0.04, nPorts);
    portsData_height = fill(0.0, nPorts);
  end if;
end PartialLumpedVessel;

model ClosedVolume
  extends PartialLumpedVessel;
equation
  p = 1e5;
  T = 300.0;
end ClosedVolume;

model ConditionalDimension6
  ClosedVolume volume(nPorts = 2);
end ConditionalDimension6;
