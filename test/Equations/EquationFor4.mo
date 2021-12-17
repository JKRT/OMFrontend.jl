connector Pin
  flow Real i;
  Real v;
end Pin;

class EquationFor4
  parameter Integer N = 4;
  Pin p[N];
equation
  for i in 1:N-1 loop
    connect(p[i],p[i+1]);
  end for;
end EquationFor4;