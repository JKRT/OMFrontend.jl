model HelloWorld
  Real x;
  parameter Real a = 1;
equation
  x = - a * x;
end HelloWorld;
