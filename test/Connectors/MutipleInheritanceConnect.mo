connector Conn
  Real p "potential Variable";
  flow Real f "flow Variable";
end Conn;

partial model A
  Conn port;
end A;

partial model B
  extends A;
end B;

partial model C
  extends A;
end C;

model D
  extends B;
  extends C;
equation
  port.f = port.p;
end D;

model E
  Conn port;
  D d;
equation
  connect(d.port, port);
end E;

model MultipleInheritanceConnect
  E e;
end MultipleInheritanceConnect;