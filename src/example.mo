
model HelloWorld
  Real x(start = 1,fixed=true);
  parameter Real a = 1;
equation
  x = - a * der(x);
end HelloWorld;
