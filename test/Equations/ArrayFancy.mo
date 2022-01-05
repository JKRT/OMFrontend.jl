model ArrayFancy
   parameter Integer N = 10;
   Real x[N](start = {i for i in 1:N});
equation
  x = {der(x[i]) for i in 1:N};
end ArrayFancy;