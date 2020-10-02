model HelloWorld
  Real x;
  parameter Real a = 1;
equation
  x = - a * der(x);
end HelloWorld;
