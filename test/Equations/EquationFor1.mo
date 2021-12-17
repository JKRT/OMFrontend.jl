class EquationFor1
  Real a[5];
equation
  a[1] = 1.0;
  for i in {2,3,4,5} loop
    a[i] = a[i-1] + 1.0;
  end for;
end EquationFor1;
