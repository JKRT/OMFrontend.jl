class EquationFor5
  Real a[4];
equation
  for i in 2:2:4 loop
    a[i] = a[i-1] + 1.0;
  end for;
end EquationFor5;