// Test: simplest case - array dimension is an if-expression
// This isolates whether the frontend can evaluate "if true then N else 0"
// as a dimension during typing.
model ConditionalDimension3
  parameter Integer N = 4;
  parameter Boolean flag = true;
  Real[if flag then N else 0] x;
equation
  for i in 1:N loop
    x[i] = i * 1.0;
  end for;
end ConditionalDimension3;
