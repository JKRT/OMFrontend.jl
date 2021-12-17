class EquationFor7
  Real a[2,3];
equation
  for i in 1:2, j in 1:3 loop
    a[i,j] =  i+j;
  end for;
end EquationFor7;