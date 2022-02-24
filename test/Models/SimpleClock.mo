/* Model of a very simple clock that trigger one event every 0.1 seconds */
model SimpleClock
  Real x;
equation
  when sample(0,0.1) then
    x = time;
  end when;
end SimpleClock;