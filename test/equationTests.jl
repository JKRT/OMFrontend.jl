circleReference = "class Circle
  Real x_out;
  Real y_out;
  Real x(start = 0.1);
  Real y(start = 0.1);
equation
  der(x) = -y;
  der(y) = x;
  x_out = x;
  y_out = y;
end Circle;
"

arrayfancyReference="class ArrayFancy
  parameter Integer N = 10;
  Real x[1](start = 1.0);
  Real x[2](start = 2.0);
  Real x[3](start = 3.0);
  Real x[4](start = 4.0);
  Real x[5](start = 5.0);
  Real x[6](start = 6.0);
  Real x[7](start = 7.0);
  Real x[8](start = 8.0);
  Real x[9](start = 9.0);
  Real x[10](start = 10.0);
equation
  x = {der(x[i]) for i in 1:10};
end ArrayFancy;
"

ifEquationDerReference = "class IfEquationDer
  parameter Real u = 4.0;
  parameter Real uMax = 10.0;
  parameter Real uMin = 2.0;
  Real y;
equation
  if uMax < time then
    der(y) = uMax;
  elseif uMin < time then
    der(y) = uMin;
  elseif true then
    der(y) = u;
  end if;
end IfEquationDer;
"
arrayfancy = (arrayfancyReference, "ArrayFancy", "./Equations/ArrayFancy.mo")
circle = (circleReference, "Circle", "./Equations/Circle.mo")
ifEquationDer = (ifEquationDerReference, "IfEquationDer", "./Equations/IfEquationDer.mo")
