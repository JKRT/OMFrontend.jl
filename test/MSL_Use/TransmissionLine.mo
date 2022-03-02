/*
  Transmission line model from the Scaleable testsuite by Francesco Casella Politecnico Milano ©
*/
model TransmissionLine "Modular model of an electrical transmission line"
  import Modelica.SIunits;
  import Modelica.Electrical.Analog;
  SIunits.Voltage vpg "voltage of pin p of the transmission line";
  SIunits.Voltage vng "voltage of pin n of the transmission line";
  SIunits.Current ipin_p
    "current flows through pin p of the transmission line";
  SIunits.Current ipin_n
    "current flows through pin n of the transmission line";
  Analog.Interfaces.Pin pin_p annotation(Placement(transformation(origin = {-100, 0}, extent = {{-10, -10}, {10, 10}}), iconTransformation(origin = {-100, 0}, extent = {{-10, -10}, {10, 10}})));
  Analog.Interfaces.Pin pin_n annotation(Placement(transformation(origin = {100, 0}, extent = {{-10, -10}, {10, 10}}), iconTransformation(origin = {100, 0}, extent = {{-10, -10}, {10, 10}})));
  Analog.Interfaces.Pin pin_ground "pin of the ground";
  Analog.Basic.Ground ground "ground of the transmission line";
  parameter Integer N = 1 "number of segments";
  parameter Real r "resistance per meter";
  parameter Real l "inductance per meter";
  parameter Real c "capacitance per meter";
  parameter Real length "length of tranmission line";
  Analog.Basic.Inductor L[N](L = fill(l * length / N, N)) "N inductors";
  Analog.Basic.Capacitor C[N](C = fill(c * length / N, N)) "N capacitors";
  Analog.Basic.Resistor R[N](R = fill(r * length / N, N)) "N resistors";
initial equation
  for i in 1:N loop
    C[i].v = 0;
    L[i].i = 0;
  end for;
equation
  vpg = pin_p.v - pin_ground.v;
  vng = pin_n.v - pin_ground.v;
  ipin_p = pin_p.i;
  ipin_n = pin_n.i;
  connect(pin_p, R[1].p);
  for i in 1:N loop
    connect(R[i].n, L[i].p);
    connect(C[i].p, L[i].n);
    connect(C[i].n, pin_ground);
  end for;
  for i in 1:N - 1 loop
    connect(L[i].n, R[i + 1].p);
  end for;
  connect(L[N].n, pin_n);
  connect(pin_ground, ground.p);
end TransmissionLine;
