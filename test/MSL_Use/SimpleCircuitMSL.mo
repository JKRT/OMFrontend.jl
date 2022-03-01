package EletricalTest

  import Modelica.Electrical.Analog.Basic;
  import Modelica.Electrical.Sources;

model SimpleCircuit
  Basic.Resistor R1;
  Basic.Capacitor C;
  Basic.Resistor R2;
  Basic.Inductor L;
  Basic.Ground G;
  //(Sources.SineVoltage AC( = 1., w = 1.);
equation
  //connect(AC.p, R1.p); // 1, Capacitor circuit
  // connect(R1.n, C.p); // Wire 2
  // connect(C.n, AC.n); // Wire 3
  // connect(R1.p, R2.p); // 2, Inductor circuit
  // connect(R2.n, L.p); // Wire 5
  // connect(L.n, C.n); // Wire 6
  //connect(AC.n, G.p); // 7, Ground
end SimpleCircuit;

end EletricalTest;
