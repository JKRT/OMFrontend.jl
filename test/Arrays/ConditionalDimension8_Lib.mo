// Library file: loaded separately to create a shared class tree (like the MSL).
// This package defines the Vessels hierarchy with conditional array dimensions.
// When loaded as a library, the class tree for PartialLumpedVessel may cache
// the conditional dimension [if use_portsData then nPorts else 0] with the
// default nPorts=0 before any instance modification is applied.

package Vessels

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

end Vessels;
