/*
  A basic example of structural mode.
  In this model we have one model with one mode.
  The model SimpleSingleMode simple wraps its inner model for the entire
  duration of the simulation.
*/
model SimpleSingleMode
  model Single
    Real x (start = 0.0);
  equation
    x = der(time);
  end Single;
structuralmode Single single;
equation
  initialStructuralState(single);
end SimpleSingleMode;