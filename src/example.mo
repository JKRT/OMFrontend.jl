model HelloWorld
  Real x;
  parameter Real a = 1;
equation
  der(x) = - a * x;
end HelloWorld;
