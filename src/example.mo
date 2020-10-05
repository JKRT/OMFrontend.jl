model HelloWorld
  Real x;
  parameter Real a = 1;
initial equation
  x = 1.0;
equation
  der(x) = - a * x;
end HelloWorld;
