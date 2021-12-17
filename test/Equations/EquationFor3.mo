class EquationComponent3
  record R
    Real x,y;
  end R;
  R a,b,c;
equation
  (if true then a else b) = c;
end EquationComponent3;