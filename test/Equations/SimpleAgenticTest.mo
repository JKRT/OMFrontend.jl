model SimpleAgenticTest
  parameter Real y = 1.0;
  Real x(start = 0.0);
equation
  der(x) = y;
  reconfigure
    Real y;
    when x > 5.0 => x < 10.0;
    prompt("State x exceeded threshold 5.0. Choose new rate: positive to keep growing, negative to shrink.");
  end reconfigure;
end SimpleAgenticTest;
