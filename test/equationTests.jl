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
  Real[10] x(start = {i for i in 1:10});
equation
  x = {der(x[i]) for i in 1:10};
end ArrayFancy;
"

circle = (circleReference, "Circle", "./Equations/Circle.mo")
arrayfancy = (arrayfancyReference, "ArrayFancy", "./Equations/ArrayFancy.mo")
