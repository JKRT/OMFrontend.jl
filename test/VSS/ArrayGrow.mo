/*
  This is an example of a model with structural variability
  We initially start with 10 equations, however during the simulation
  the ammount of equations are increased by 10. 
*/
model ArrayGrow
   parameter Integer N = 10;
   Real x[N](start = {i for i in 1:N});
equation
  x = {der(x[i]) for i in 1:N};
  when time > 5 then
    /*
      Recompilation with change of parameters.
      the name of this function is the subject of change.
      What is changed depends on the argument passed to this function.
    */
    recompilation(N, 20);
  end when;
end ArrayGrow;