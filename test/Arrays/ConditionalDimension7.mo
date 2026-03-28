// Test: shared class tree with multiple instantiations at different nPorts.
// This mirrors the MSL pattern where PartialLumpedVessel is a library class
// that gets instantiated by multiple models with different nPorts values.
// If the conditional dimension is cached from the first (default nPorts=0)
// instantiation, the second instantiation (nPorts=2) will see stale [0].

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

// First model: uses default nPorts=0 (no ports).
// This may cause the shared class tree to cache dimension as [0].
model EmptyVessel
  Vessels.ClosedVolume vol0;
end EmptyVessel;

// Second model: uses nPorts=2.
// If shared class tree cached [0] from EmptyVessel, this will fail with
// "expected [2], got [0]" on portsData.diameter binding.
model ConditionalDimension7
  Vessels.ClosedVolume volume(nPorts = 2);
end ConditionalDimension7;
