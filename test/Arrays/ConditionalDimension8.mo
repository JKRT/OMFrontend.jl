// Test model: loaded separately from the library.
// This model uses Vessels.ClosedVolume with nPorts=2.
// When the Vessels library is pre-loaded into a shared class tree,
// the conditional dimension on portsData may already be cached as [0]
// (from default nPorts=0), causing a dimension mismatch.

model ConditionalDimension8
  Vessels.ClosedVolume volume(nPorts = 2);
end ConditionalDimension8;
