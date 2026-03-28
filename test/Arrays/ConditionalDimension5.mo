// Test: conditional dimension through component instantiation (mirrors RoomCO2 pattern)
// A top-level model instantiates a vessel component with nPorts=2.
// The binding portsData_diameter_internal = portsData.diameter must resolve
// through the component hierarchy: volume.portsData has conditional dimension
// [if use_portsData then nPorts else 0].

record VesselPortsData
  Real diameter = 0.01;
  Real height = 0.0;
  Real zeta_in = 0.0;
  Real zeta_out = 0.0;
end VesselPortsData;

partial model PartialVessel
  parameter Integer nPorts = 0 "Number of ports";
  parameter Boolean use_portsData = true;
  parameter VesselPortsData[if use_portsData then nPorts else 0] portsData;
protected
  Real[nPorts] portsData_diameter_internal = portsData.diameter if use_portsData and nPorts > 0;
  Real[nPorts] portsData_diameter;
equation
  if use_portsData then
    portsData_diameter = portsData_diameter_internal;
  else
    portsData_diameter = fill(0.04, nPorts);
  end if;
end PartialVessel;

model ClosedVolume
  extends PartialVessel;
  Real p = 1e5 "Pressure";
end ClosedVolume;

model ConditionalDimension5
  ClosedVolume volume(nPorts = 2);
end ConditionalDimension5;
