class EquationFor2
  constant Integer N = 4;
  Real a[N];
equation
  a[1] = 1.0;
  for i in 1:N-1 loop
    a[i+1] = a[i] + 1.0;
  end for;
end EquationFor2;