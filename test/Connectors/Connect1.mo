connector C
  flow Real f;
  Real e;
end C;
model Connect1
  C c1,c2;
equation
  connect(c1,c2);
  c1.e = 1;
  c2.f = time;
end Connect1;