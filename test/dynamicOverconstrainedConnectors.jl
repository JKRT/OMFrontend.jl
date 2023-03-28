module OCC_ReferenceModels

ACPort = "class DynamicOverconstrainedConnectors_ACPort
  Real v_re(unit = \"1\");
  Real v_im(unit = \"1\");
  flow Real i_re(unit = \"1\");
  flow Real i_im(unit = \"1\");
  Real omegaRef;
equation
  i_re = 0.0;
  i_im = 0.0;
end DynamicOverconstrainedConnectors_ACPort;
"

Load = "class DynamicOverconstrainedConnectors_Load
  Real port_v_re(unit = \"1\");
  Real port_v_im(unit = \"1\");
  flow Real port_i_re(unit = \"1\");
  flow Real port_i_im(unit = \"1\");
  Real port_omegaRef;
  Real P(unit = \"1\") = 0.0;
  Real Q = 0.0;
equation
  port_i_re = 0.0;
  port_i_im = 0.0;
  Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re) = Complex.'constructor'.fromReal(P, Q);
end DynamicOverconstrainedConnectors_Load;
"

Generator = "class DynamicOverconstrainedConnectors_Generator
  parameter Real V(unit = \"1\") = 1.0;
  parameter Real Ta(unit = \"s\", quantity = \"Time\") = 10.0;
  parameter Real droop(unit = \"1\") = 0.05;
  Real port_v_re(unit = \"1\");
  Real port_v_im(unit = \"1\");
  flow Real port_i_re(unit = \"1\");
  flow Real port_i_im(unit = \"1\");
  Real port_omegaRef;
  Real Ps(unit = \"1\") = 1.0;
  Real Pc(unit = \"1\");
  Real Pe(unit = \"1\");
  Real theta(fixed = true, start = 0.0, displayUnit = \"deg\", unit = \"rad\", quantity = \"Angle\");
  Real omega(fixed = true, start = 1.0, unit = \"1\");
equation
  port_i_re = 0.0;
  port_i_im = 0.0;
  der(theta) = (omega - port_omegaRef) * 314.1592653589793;
  Ta * omega * der(omega) = Ps + Pc - Pe;
  port_v = Complex.'constructor'.fromReal(V * cos(theta), V * sin(theta));
  Pe = -Modelica.ComplexMath.real(Modelica.SIunits.ComplexPerUnit.'*'.multiply(port_v, Modelica.ComplexMath.conj(port_i)));
  Pc = -(omega - 1.0) / droop;
  port_omegaRef = omega;
end DynamicOverconstrainedConnectors_Generator;
"

TransmissionLine = "class DynamicOverconstrainedConnectors_TransmissionLine
  parameter Real B(unit = \"1\") = -5.0;
  discrete Real B_act(unit = \"1\");
  Boolean closed;
  Boolean open = false;
  Boolean close = false;
  Real port_a_v_re(unit = \"1\");
  Real port_a_v_im(unit = \"1\");
  flow Real port_a_i_re(unit = \"1\");
  flow Real port_a_i_im(unit = \"1\");
  Real port_a_omegaRef;
  Real port_b_v_re(unit = \"1\");
  Real port_b_v_im(unit = \"1\");
  flow Real port_b_i_re(unit = \"1\");
  flow Real port_b_i_im(unit = \"1\");
  Real port_b_omegaRef;
initial equation
  closed = true;
  B_act = B;
equation
  port_a_i_re = 0.0;
  port_a_i_im = 0.0;
  port_b_i_re = 0.0;
  port_b_i_im = 0.0;
  Complex.'constructor'.fromReal(c1_re + c2_re, c1_im + c2_im) = Complex.'constructor'.fromReal(0.0, 0.0);
  port_a_i = Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re);

  when open then
    closed = false;
    B_act = 0.0;
  elsewhen close then
    closed = true;
    B_act = B;
  end when;

  port_a_omegaRef = port_b_omegaRef;
end DynamicOverconstrainedConnectors_TransmissionLine;
"
System1 = "class DynamicOverconstrainedConnectors_System1
  parameter Real G1_V(unit = \"1\") = 1.0;
  parameter Real G1_Ta(unit = \"s\", quantity = \"Time\") = 10.0;
  parameter Real G1_droop(unit = \"1\") = 0.05;
  Real G1_port_v_re(unit = \"1\");
  Real G1_port_v_im(unit = \"1\");
  flow Real G1_port_i_re(unit = \"1\");
  flow Real G1_port_i_im(unit = \"1\");
  Real G1_port_omegaRef;
  Real G1_Ps(unit = \"1\") = 1.0;
  Real G1_Pc(unit = \"1\");
  Real G1_Pe(unit = \"1\");
  Real G1_theta(fixed = true, start = 0.0, displayUnit = \"deg\", unit = \"rad\", quantity = \"Angle\");
  Real G1_omega(fixed = true, start = 1.0, unit = \"1\");
  parameter Real G2_V(unit = \"1\") = 1.0;
  parameter Real G2_Ta(unit = \"s\", quantity = \"Time\") = 10.0;
  parameter Real G2_droop(unit = \"1\") = 0.05;
  Real G2_port_v_re(unit = \"1\");
  Real G2_port_v_im(unit = \"1\");
  flow Real G2_port_i_re(unit = \"1\");
  flow Real G2_port_i_im(unit = \"1\");
  Real G2_port_omegaRef;
  Real G2_Ps(unit = \"1\") = 1.0;
  Real G2_Pc(unit = \"1\");
  Real G2_Pe(unit = \"1\");
  Real G2_theta(fixed = true, start = 0.0, displayUnit = \"deg\", unit = \"rad\", quantity = \"Angle\");
  Real G2_omega(fixed = true, start = 1.0, unit = \"1\");
  Real L1_port_v_re(unit = \"1\");
  Real L1_port_v_im(unit = \"1\");
  flow Real L1_port_i_re(unit = \"1\");
  flow Real L1_port_i_im(unit = \"1\");
  Real L1_port_omegaRef;
  Real L1_P(unit = \"1\") = 1.0;
  Real L1_Q = 0.0;
  Real L2_port_v_re(unit = \"1\");
  Real L2_port_v_im(unit = \"1\");
  flow Real L2_port_i_re(unit = \"1\");
  flow Real L2_port_i_im(unit = \"1\");
  Real L2_port_omegaRef;
  Real L2_P(unit = \"1\") = if time < 1.0 then 1.0 else 0.8;
  Real L2_Q = 0.0;
  parameter Real T_B(unit = \"1\") = -5.0;
  discrete Real T_B_act(unit = \"1\");
  Boolean T_closed;
  Boolean T_open = false;
  Boolean T_close = false;
  Real T_port_a_v_re(unit = \"1\");
  Real T_port_a_v_im(unit = \"1\");
  flow Real T_port_a_i_re(unit = \"1\");
  flow Real T_port_a_i_im(unit = \"1\");
  Real T_port_a_omegaRef;
  Real T_port_b_v_re(unit = \"1\");
  Real T_port_b_v_im(unit = \"1\");
  flow Real T_port_b_i_re(unit = \"1\");
  flow Real T_port_b_i_im(unit = \"1\");
  Real T_port_b_omegaRef;
initial equation
  T_closed = true;
  T_B_act = T_B;
equation
  T_port_a_omegaRef = L1_port_omegaRef;
  T_port_a_omegaRef = G1_port_omegaRef;
  L1_port_v_re = G1_port_v_re;
  L1_port_v_re = T_port_a_v_re;
  T_port_a_v_im = G1_port_v_im;
  T_port_a_v_im = L1_port_v_im;
  T_port_b_omegaRef = G2_port_omegaRef;
  T_port_b_omegaRef = L2_port_omegaRef;
  L2_port_v_re = T_port_b_v_re;
  L2_port_v_re = G2_port_v_re;
  T_port_b_v_im = G2_port_v_im;
  T_port_b_v_im = L2_port_v_im;
  T_port_a_i_re + L1_port_i_re + G1_port_i_re = 0.0;
  G1_port_i_im + L1_port_i_im + T_port_a_i_im = 0.0;
  L2_port_i_re + G2_port_i_re + T_port_b_i_re = 0.0;
  T_port_b_i_im + G2_port_i_im + L2_port_i_im = 0.0;
  der(G1_theta) = (G1_omega - G1_port_omegaRef) * 314.1592653589793;
  G1_Ta * G1_omega * der(G1_omega) = G1_Ps + G1_Pc - G1_Pe;
  G1_port_v = Complex.'constructor'.fromReal(G1_V * cos(G1_theta), G1_V * sin(G1_theta));
  G1_Pe = -Modelica.ComplexMath.real(Modelica.SIunits.ComplexPerUnit.'*'.multiply(G1_port_v, Modelica.ComplexMath.conj(G1_port_i)));
  G1_Pc = -(G1_omega - 1.0) / G1_droop;
  G1_port_omegaRef = G1_omega;
  der(G2_theta) = (G2_omega - G2_port_omegaRef) * 314.1592653589793;
  G2_Ta * G2_omega * der(G2_omega) = G2_Ps + G2_Pc - G2_Pe;
  G2_port_v = Complex.'constructor'.fromReal(G2_V * cos(G2_theta), G2_V * sin(G2_theta));
  G2_Pe = -Modelica.ComplexMath.real(Modelica.SIunits.ComplexPerUnit.'*'.multiply(G2_port_v, Modelica.ComplexMath.conj(G2_port_i)));
  G2_Pc = -(G2_omega - 1.0) / G2_droop;
  Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re) = Complex.'constructor'.fromReal(L1_P, L1_Q);
  Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re) = Complex.'constructor'.fromReal(L2_P, L2_Q);
  Complex.'constructor'.fromReal(c1_re + c2_re, c1_im + c2_im) = Complex.'constructor'.fromReal(0.0, 0.0);
  T_port_a_i = Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re);

  when T_open then
    T_closed = false;
    T_B_act = 0.0;
  elsewhen T_close then
    T_closed = true;
    T_B_act = T_B;
  end when;

  T_port_a_omegaRef = T_port_b_omegaRef;
end DynamicOverconstrainedConnectors_System1;
"

System2 = "class DynamicOverconstrainedConnectors_System2
  parameter Real G1_V(unit = \"1\") = 1.0;
  parameter Real G1_Ta(unit = \"s\", quantity = \"Time\") = 10.0;
  parameter Real G1_droop(unit = \"1\") = 0.05;
  Real G1_port_v_re(unit = \"1\");
  Real G1_port_v_im(unit = \"1\");
  flow Real G1_port_i_re(unit = \"1\");
  flow Real G1_port_i_im(unit = \"1\");
  Real G1_port_omegaRef;
  Real G1_Ps(unit = \"1\") = 1.0;
  Real G1_Pc(unit = \"1\");
  Real G1_Pe(unit = \"1\");
  Real G1_theta(fixed = true, start = 0.0, displayUnit = \"deg\", unit = \"rad\", quantity = \"Angle\");
  Real G1_omega(fixed = true, start = 1.0, unit = \"1\");
  parameter Real G2_V(unit = \"1\") = 1.0;
  parameter Real G2_Ta(unit = \"s\", quantity = \"Time\") = 10.0;
  parameter Real G2_droop(unit = \"1\") = 0.05;
  Real G2_port_v_re(unit = \"1\");
  Real G2_port_v_im(unit = \"1\");
  flow Real G2_port_i_re(unit = \"1\");
  flow Real G2_port_i_im(unit = \"1\");
  Real G2_port_omegaRef;
  Real G2_Ps(unit = \"1\") = 1.0;
  Real G2_Pc(unit = \"1\");
  Real G2_Pe(unit = \"1\");
  Real G2_theta(fixed = true, start = 0.0, displayUnit = \"deg\", unit = \"rad\", quantity = \"Angle\");
  Real G2_omega(fixed = true, start = 1.0, unit = \"1\");
  Real L1_port_v_re(unit = \"1\");
  Real L1_port_v_im(unit = \"1\");
  flow Real L1_port_i_re(unit = \"1\");
  flow Real L1_port_i_im(unit = \"1\");
  Real L1_port_omegaRef;
  Real L1_P(unit = \"1\") = 1.0;
  Real L1_Q = 0.0;
  Real L2_port_v_re(unit = \"1\");
  Real L2_port_v_im(unit = \"1\");
  flow Real L2_port_i_re(unit = \"1\");
  flow Real L2_port_i_im(unit = \"1\");
  Real L2_port_omegaRef;
  Real L2_P(unit = \"1\") = if time < 1.0 then 1.0 else 0.8;
  Real L2_Q = 0.0;
  parameter Real T1a_B(unit = \"1\") = -5.0;
  discrete Real T1a_B_act(unit = \"1\");
  Boolean T1a_closed;
  Boolean T1a_open = false;
  Boolean T1a_close = false;
  Real T1a_port_a_v_re(unit = \"1\");
  Real T1a_port_a_v_im(unit = \"1\");
  flow Real T1a_port_a_i_re(unit = \"1\");
  flow Real T1a_port_a_i_im(unit = \"1\");
  Real T1a_port_a_omegaRef;
  Real T1a_port_b_v_re(unit = \"1\");
  Real T1a_port_b_v_im(unit = \"1\");
  flow Real T1a_port_b_i_re(unit = \"1\");
  flow Real T1a_port_b_i_im(unit = \"1\");
  Real T1a_port_b_omegaRef;
  parameter Real T1b_B(unit = \"1\") = -5.0;
  discrete Real T1b_B_act(unit = \"1\");
  Boolean T1b_closed;
  Boolean T1b_open = false;
  Boolean T1b_close = false;
  Real T1b_port_a_v_re(unit = \"1\");
  Real T1b_port_a_v_im(unit = \"1\");
  flow Real T1b_port_a_i_re(unit = \"1\");
  flow Real T1b_port_a_i_im(unit = \"1\");
  Real T1b_port_a_omegaRef;
  Real T1b_port_b_v_re(unit = \"1\");
  Real T1b_port_b_v_im(unit = \"1\");
  flow Real T1b_port_b_i_re(unit = \"1\");
  flow Real T1b_port_b_i_im(unit = \"1\");
  Real T1b_port_b_omegaRef;
  parameter Real T2_B(unit = \"1\") = -10.0;
  discrete Real T2_B_act(unit = \"1\");
  Boolean T2_closed;
  Boolean T2_open = false;
  Boolean T2_close = false;
  Real T2_port_a_v_re(unit = \"1\");
  Real T2_port_a_v_im(unit = \"1\");
  flow Real T2_port_a_i_re(unit = \"1\");
  flow Real T2_port_a_i_im(unit = \"1\");
  Real T2_port_a_omegaRef;
  Real T2_port_b_v_re(unit = \"1\");
  Real T2_port_b_v_im(unit = \"1\");
  flow Real T2_port_b_i_re(unit = \"1\");
  flow Real T2_port_b_i_im(unit = \"1\");
  Real T2_port_b_omegaRef;
initial equation
  T1a_closed = true;
  T1a_B_act = T1a_B;
  T1b_closed = true;
  T1b_B_act = T1b_B;
  T2_closed = true;
  T2_B_act = T2_B;
equation
  T1b_port_a_omegaRef = L1_port_omegaRef;
  T1b_port_a_omegaRef = G1_port_omegaRef;
  T2_port_b_omegaRef = L2_port_omegaRef;
  T2_port_b_omegaRef = G2_port_omegaRef;
  L2_port_v_re = T2_port_b_v_re;
  L2_port_v_re = G2_port_v_re;
  T2_port_b_v_im = G2_port_v_im;
  T2_port_b_v_im = L2_port_v_im;
  L1_port_v_re = G1_port_v_re;
  L1_port_v_re = T1a_port_a_v_re;
  L1_port_v_re = T1b_port_a_v_re;
  T1a_port_a_v_im = T1b_port_a_v_im;
  T1a_port_a_v_im = G1_port_v_im;
  T1a_port_a_v_im = L1_port_v_im;
  T2_port_a_omegaRef = T1a_port_b_omegaRef;
  T2_port_a_omegaRef = T1b_port_b_omegaRef;
  T1b_port_b_v_re = T1a_port_b_v_re;
  T1b_port_b_v_re = T2_port_a_v_re;
  T1a_port_b_v_im = T2_port_a_v_im;
  T1a_port_b_v_im = T1b_port_b_v_im;
  L2_port_i_re + G2_port_i_re + T2_port_b_i_re = 0.0;
  T2_port_b_i_im + G2_port_i_im + L2_port_i_im = 0.0;
  G1_port_i_re + T1a_port_a_i_re + T1b_port_a_i_re + L1_port_i_re = 0.0;
  G1_port_i_im + T1b_port_a_i_im + L1_port_i_im + T1a_port_a_i_im = 0.0;
  T1a_port_b_i_re + T2_port_a_i_re + T1b_port_b_i_re = 0.0;
  T1a_port_b_i_im + T2_port_a_i_im + T1b_port_b_i_im = 0.0;
  der(G1_theta) = (G1_omega - G1_port_omegaRef) * 314.1592653589793;
  G1_Ta * G1_omega * der(G1_omega) = G1_Ps + G1_Pc - G1_Pe;
  G1_port_v = Complex.'constructor'.fromReal(G1_V * cos(G1_theta), G1_V * sin(G1_theta));
  G1_Pe = -Modelica.ComplexMath.real(Modelica.SIunits.ComplexPerUnit.'*'.multiply(G1_port_v, Modelica.ComplexMath.conj(G1_port_i)));
  G1_Pc = -(G1_omega - 1.0) / G1_droop;
  G1_port_omegaRef = G1_omega;
  der(G2_theta) = (G2_omega - G2_port_omegaRef) * 314.1592653589793;
  G2_Ta * G2_omega * der(G2_omega) = G2_Ps + G2_Pc - G2_Pe;
  G2_port_v = Complex.'constructor'.fromReal(G2_V * cos(G2_theta), G2_V * sin(G2_theta));
  G2_Pe = -Modelica.ComplexMath.real(Modelica.SIunits.ComplexPerUnit.'*'.multiply(G2_port_v, Modelica.ComplexMath.conj(G2_port_i)));
  G2_Pc = -(G2_omega - 1.0) / G2_droop;
  Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re) = Complex.'constructor'.fromReal(L1_P, L1_Q);
  Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re) = Complex.'constructor'.fromReal(L2_P, L2_Q);
  Complex.'constructor'.fromReal(c1_re + c2_re, c1_im + c2_im) = Complex.'constructor'.fromReal(0.0, 0.0);
  T1a_port_a_i = Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re);

  when T1a_open then
    T1a_closed = false;
    T1a_B_act = 0.0;
  elsewhen T1a_close then
    T1a_closed = true;
    T1a_B_act = T1a_B;
  end when;

  T1a_port_a_omegaRef = T1a_port_b_omegaRef;
  Complex.'constructor'.fromReal(c1_re + c2_re, c1_im + c2_im) = Complex.'constructor'.fromReal(0.0, 0.0);
  T1b_port_a_i = Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re);

  when T1b_open then
    T1b_closed = false;
    T1b_B_act = 0.0;
  elsewhen T1b_close then
    T1b_closed = true;
    T1b_B_act = T1b_B;
  end when;

  T1b_port_a_omegaRef = T1b_port_b_omegaRef;
  Complex.'constructor'.fromReal(c1_re + c2_re, c1_im + c2_im) = Complex.'constructor'.fromReal(0.0, 0.0);
  T2_port_a_i = Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re);

  when T2_open then
    T2_closed = false;
    T2_B_act = 0.0;
  elsewhen T2_close then
    T2_closed = true;
    T2_B_act = T2_B;
  end when;

  T2_port_a_omegaRef = T2_port_b_omegaRef;
end DynamicOverconstrainedConnectors_System2;
"

System3 = "class DynamicOverconstrainedConnectors_System3
  parameter Real G1_V(unit = \"1\") = 1.0;
  parameter Real G1_Ta(unit = \"s\", quantity = \"Time\") = 10.0;
  parameter Real G1_droop(unit = \"1\") = 0.05;
  Real G1_port_v_re(unit = \"1\");
  Real G1_port_v_im(unit = \"1\");
  flow Real G1_port_i_re(unit = \"1\");
  flow Real G1_port_i_im(unit = \"1\");
  Real G1_port_omegaRef;
  Real G1_Ps(unit = \"1\") = 1.0;
  Real G1_Pc(unit = \"1\");
  Real G1_Pe(unit = \"1\");
  Real G1_theta(fixed = true, start = 0.0, displayUnit = \"deg\", unit = \"rad\", quantity = \"Angle\");
  Real G1_omega(fixed = true, start = 1.0, unit = \"1\");
  parameter Real G2_V(unit = \"1\") = 1.0;
  parameter Real G2_Ta(unit = \"s\", quantity = \"Time\") = 10.0;
  parameter Real G2_droop(unit = \"1\") = 0.05;
  Real G2_port_v_re(unit = \"1\");
  Real G2_port_v_im(unit = \"1\");
  flow Real G2_port_i_re(unit = \"1\");
  flow Real G2_port_i_im(unit = \"1\");
  Real G2_port_omegaRef;
  Real G2_Ps(unit = \"1\") = 1.0;
  Real G2_Pc(unit = \"1\");
  Real G2_Pe(unit = \"1\");
  Real G2_theta(fixed = true, start = 0.0, displayUnit = \"deg\", unit = \"rad\", quantity = \"Angle\");
  Real G2_omega(fixed = true, start = 1.0, unit = \"1\");
  Real L1_port_v_re(unit = \"1\");
  Real L1_port_v_im(unit = \"1\");
  flow Real L1_port_i_re(unit = \"1\");
  flow Real L1_port_i_im(unit = \"1\");
  Real L1_port_omegaRef;
  Real L1_P(unit = \"1\") = 1.0;
  Real L1_Q = 0.0;
  Real L2_port_v_re(unit = \"1\");
  Real L2_port_v_im(unit = \"1\");
  flow Real L2_port_i_re(unit = \"1\");
  flow Real L2_port_i_im(unit = \"1\");
  Real L2_port_omegaRef;
  Real L2_P(unit = \"1\") = if time < 1.0 then 1.0 else 0.8;
  Real L2_Q = 0.0;
  parameter Real T1a_B(unit = \"1\") = -5.0;
  discrete Real T1a_B_act(unit = \"1\");
  Boolean T1a_closed;
  Boolean T1a_open = false;
  Boolean T1a_close = false;
  Real T1a_port_a_v_re(unit = \"1\");
  Real T1a_port_a_v_im(unit = \"1\");
  flow Real T1a_port_a_i_re(unit = \"1\");
  flow Real T1a_port_a_i_im(unit = \"1\");
  Real T1a_port_a_omegaRef;
  Real T1a_port_b_v_re(unit = \"1\");
  Real T1a_port_b_v_im(unit = \"1\");
  flow Real T1a_port_b_i_re(unit = \"1\");
  flow Real T1a_port_b_i_im(unit = \"1\");
  Real T1a_port_b_omegaRef;
  parameter Real T1b_B(unit = \"1\") = -5.0;
  discrete Real T1b_B_act(unit = \"1\");
  Boolean T1b_closed;
  Boolean T1b_open = false;
  Boolean T1b_close = false;
  Real T1b_port_a_v_re(unit = \"1\");
  Real T1b_port_a_v_im(unit = \"1\");
  flow Real T1b_port_a_i_re(unit = \"1\");
  flow Real T1b_port_a_i_im(unit = \"1\");
  Real T1b_port_a_omegaRef;
  Real T1b_port_b_v_re(unit = \"1\");
  Real T1b_port_b_v_im(unit = \"1\");
  flow Real T1b_port_b_i_re(unit = \"1\");
  flow Real T1b_port_b_i_im(unit = \"1\");
  Real T1b_port_b_omegaRef;
  parameter Real T2_B(unit = \"1\") = -10.0;
  discrete Real T2_B_act(unit = \"1\");
  Boolean T2_closed;
  Boolean T2_open = if time < 10.0 then false else true;
  Boolean T2_close = false;
  Real T2_port_a_v_re(unit = \"1\");
  Real T2_port_a_v_im(unit = \"1\");
  flow Real T2_port_a_i_re(unit = \"1\");
  flow Real T2_port_a_i_im(unit = \"1\");
  Real T2_port_a_omegaRef;
  Real T2_port_b_v_re(unit = \"1\");
  Real T2_port_b_v_im(unit = \"1\");
  flow Real T2_port_b_i_re(unit = \"1\");
  flow Real T2_port_b_i_im(unit = \"1\");
  Real T2_port_b_omegaRef;
initial equation
  T1a_closed = true;
  T1a_B_act = T1a_B;
  T1b_closed = true;
  T1b_B_act = T1b_B;
  T2_closed = true;
  T2_B_act = T2_B;
equation
  T1b_port_a_omegaRef = L1_port_omegaRef;
  T1b_port_a_omegaRef = G1_port_omegaRef;
  T2_port_b_omegaRef = L2_port_omegaRef;
  T2_port_b_omegaRef = G2_port_omegaRef;
  L2_port_v_re = T2_port_b_v_re;
  L2_port_v_re = G2_port_v_re;
  T2_port_b_v_im = G2_port_v_im;
  T2_port_b_v_im = L2_port_v_im;
  L1_port_v_re = G1_port_v_re;
  L1_port_v_re = T1a_port_a_v_re;
  L1_port_v_re = T1b_port_a_v_re;
  T1a_port_a_v_im = T1b_port_a_v_im;
  T1a_port_a_v_im = G1_port_v_im;
  T1a_port_a_v_im = L1_port_v_im;
  T2_port_a_omegaRef = T1a_port_b_omegaRef;
  T2_port_a_omegaRef = T1b_port_b_omegaRef;
  T1b_port_b_v_re = T1a_port_b_v_re;
  T1b_port_b_v_re = T2_port_a_v_re;
  T1a_port_b_v_im = T2_port_a_v_im;
  T1a_port_b_v_im = T1b_port_b_v_im;
  L2_port_i_re + G2_port_i_re + T2_port_b_i_re = 0.0;
  T2_port_b_i_im + G2_port_i_im + L2_port_i_im = 0.0;
  G1_port_i_re + T1a_port_a_i_re + T1b_port_a_i_re + L1_port_i_re = 0.0;
  G1_port_i_im + T1b_port_a_i_im + L1_port_i_im + T1a_port_a_i_im = 0.0;
  T1a_port_b_i_re + T2_port_a_i_re + T1b_port_b_i_re = 0.0;
  T1a_port_b_i_im + T2_port_a_i_im + T1b_port_b_i_im = 0.0;
  der(G1_theta) = (G1_omega - G1_port_omegaRef) * 314.1592653589793;
  G1_Ta * G1_omega * der(G1_omega) = G1_Ps + G1_Pc - G1_Pe;
  G1_port_v = Complex.'constructor'.fromReal(G1_V * cos(G1_theta), G1_V * sin(G1_theta));
  G1_Pe = -Modelica.ComplexMath.real(Modelica.SIunits.ComplexPerUnit.'*'.multiply(G1_port_v, Modelica.ComplexMath.conj(G1_port_i)));
  G1_Pc = -(G1_omega - 1.0) / G1_droop;
  G1_port_omegaRef = G1_omega;
  der(G2_theta) = (G2_omega - G2_port_omegaRef) * 314.1592653589793;
  G2_Ta * G2_omega * der(G2_omega) = G2_Ps + G2_Pc - G2_Pe;
  G2_port_v = Complex.'constructor'.fromReal(G2_V * cos(G2_theta), G2_V * sin(G2_theta));
  G2_Pe = -Modelica.ComplexMath.real(Modelica.SIunits.ComplexPerUnit.'*'.multiply(G2_port_v, Modelica.ComplexMath.conj(G2_port_i)));
  G2_Pc = -(G2_omega - 1.0) / G2_droop;
  Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re) = Complex.'constructor'.fromReal(L1_P, L1_Q);
  Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re) = Complex.'constructor'.fromReal(L2_P, L2_Q);
  Complex.'constructor'.fromReal(c1_re + c2_re, c1_im + c2_im) = Complex.'constructor'.fromReal(0.0, 0.0);
  T1a_port_a_i = Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re);

  when T1a_open then
    T1a_closed = false;
    T1a_B_act = 0.0;
  elsewhen T1a_close then
    T1a_closed = true;
    T1a_B_act = T1a_B;
  end when;

  T1a_port_a_omegaRef = T1a_port_b_omegaRef;
  Complex.'constructor'.fromReal(c1_re + c2_re, c1_im + c2_im) = Complex.'constructor'.fromReal(0.0, 0.0);
  T1b_port_a_i = Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re);

  when T1b_open then
    T1b_closed = false;
    T1b_B_act = 0.0;
  elsewhen T1b_close then
    T1b_closed = true;
    T1b_B_act = T1b_B;
  end when;

  T1b_port_a_omegaRef = T1b_port_b_omegaRef;
  Complex.'constructor'.fromReal(c1_re + c2_re, c1_im + c2_im) = Complex.'constructor'.fromReal(0.0, 0.0);
  T2_port_a_i = Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re);

  when T2_open then
    T2_closed = false;
    T2_B_act = 0.0;
  elsewhen T2_close then
    T2_closed = true;
    T2_B_act = T2_B;
  end when;

  T2_port_a_omegaRef = T2_port_b_omegaRef;
end DynamicOverconstrainedConnectors_System3;
"
System4 = "class DynamicOverconstrainedConnectors_System4
  parameter Real G1_V(unit = \"1\") = 1.0;
  parameter Real G1_Ta(unit = \"s\", quantity = \"Time\") = 10.0;
  parameter Real G1_droop(unit = \"1\") = 0.05;
  Real G1_port_v_re(unit = \"1\");
  Real G1_port_v_im(unit = \"1\");
  flow Real G1_port_i_re(unit = \"1\");
  flow Real G1_port_i_im(unit = \"1\");
  Real G1_port_omegaRef;
  Real G1_Ps(unit = \"1\") = 1.0;
  Real G1_Pc(unit = \"1\");
  Real G1_Pe(unit = \"1\");
  Real G1_theta(fixed = true, start = 0.0, displayUnit = \"deg\", unit = \"rad\", quantity = \"Angle\");
  Real G1_omega(fixed = true, start = 1.0, unit = \"1\");
  parameter Real G2_V(unit = \"1\") = 1.0;
  parameter Real G2_Ta(unit = \"s\", quantity = \"Time\") = 10.0;
  parameter Real G2_droop(unit = \"1\") = 0.05;
  Real G2_port_v_re(unit = \"1\");
  Real G2_port_v_im(unit = \"1\");
  flow Real G2_port_i_re(unit = \"1\");
  flow Real G2_port_i_im(unit = \"1\");
  Real G2_port_omegaRef;
  Real G2_Ps(unit = \"1\") = 1.0;
  Real G2_Pc(unit = \"1\");
  Real G2_Pe(unit = \"1\");
  Real G2_theta(fixed = true, start = 0.0, displayUnit = \"deg\", unit = \"rad\", quantity = \"Angle\");
  Real G2_omega(fixed = true, start = 1.0, unit = \"1\");
  Real L1_port_v_re(unit = \"1\");
  Real L1_port_v_im(unit = \"1\");
  flow Real L1_port_i_re(unit = \"1\");
  flow Real L1_port_i_im(unit = \"1\");
  Real L1_port_omegaRef;
  Real L1_P(unit = \"1\") = 1.0;
  Real L1_Q = 0.0;
  Real L2_port_v_re(unit = \"1\");
  Real L2_port_v_im(unit = \"1\");
  flow Real L2_port_i_re(unit = \"1\");
  flow Real L2_port_i_im(unit = \"1\");
  Real L2_port_omegaRef;
  Real L2_P(unit = \"1\") = if time < 1.0 then 1.0 else 0.8;
  Real L2_Q = 0.0;
  parameter Real T1a_B(unit = \"1\") = -5.0;
  discrete Real T1a_B_act(unit = \"1\");
  Boolean T1a_closed;
  Boolean T1a_open = false;
  Boolean T1a_close = false;
  Real T1a_port_a_v_re(unit = \"1\");
  Real T1a_port_a_v_im(unit = \"1\");
  flow Real T1a_port_a_i_re(unit = \"1\");
  flow Real T1a_port_a_i_im(unit = \"1\");
  Real T1a_port_a_omegaRef;
  Real T1a_port_b_v_re(unit = \"1\");
  Real T1a_port_b_v_im(unit = \"1\");
  flow Real T1a_port_b_i_re(unit = \"1\");
  flow Real T1a_port_b_i_im(unit = \"1\");
  Real T1a_port_b_omegaRef;
  parameter Real T1b_B(unit = \"1\") = -5.0;
  discrete Real T1b_B_act(unit = \"1\");
  Boolean T1b_closed;
  Boolean T1b_open = false;
  Boolean T1b_close = false;
  Real T1b_port_a_v_re(unit = \"1\");
  Real T1b_port_a_v_im(unit = \"1\");
  flow Real T1b_port_a_i_re(unit = \"1\");
  flow Real T1b_port_a_i_im(unit = \"1\");
  Real T1b_port_a_omegaRef;
  Real T1b_port_b_v_re(unit = \"1\");
  Real T1b_port_b_v_im(unit = \"1\");
  flow Real T1b_port_b_i_re(unit = \"1\");
  flow Real T1b_port_b_i_im(unit = \"1\");
  Real T1b_port_b_omegaRef;
  parameter Real T2_B(unit = \"1\") = -10.0;
  discrete Real T2_B_act(unit = \"1\");
  Boolean T2_closed;
  Boolean T2_open = if time < 10.0 then false else true;
  Boolean T2_close = false;
  Real T2_port_a_v_re(unit = \"1\");
  Real T2_port_a_v_im(unit = \"1\");
  flow Real T2_port_a_i_re(unit = \"1\");
  flow Real T2_port_a_i_im(unit = \"1\");
  Real T2_port_a_omegaRef;
  Real T2_port_b_v_re(unit = \"1\");
  Real T2_port_b_v_im(unit = \"1\");
  flow Real T2_port_b_i_re(unit = \"1\");
  flow Real T2_port_b_i_im(unit = \"1\");
  Real T2_port_b_omegaRef;
  protected Real T2_port_b_int_v_re(unit = \"1\");
  protected Real T2_port_b_int_v_im(unit = \"1\");
  protected flow Real T2_port_b_int_i_re(unit = \"1\");
  protected flow Real T2_port_b_int_i_im(unit = \"1\");
  protected Real T2_port_b_int_omegaRef;
initial equation
  T1a_closed = true;
  T1a_B_act = T1a_B;
  T1b_closed = true;
  T1b_B_act = T1b_B;
  T2_closed = true;
  T2_B_act = T2_B;
equation
  T1b_port_a_omegaRef = L1_port_omegaRef;
  T1b_port_a_omegaRef = G1_port_omegaRef;
  T2_port_b_omegaRef = L2_port_omegaRef;
  T2_port_b_omegaRef = G2_port_omegaRef;
  L2_port_v_re = T2_port_b_v_re;
  L2_port_v_re = G2_port_v_re;
  T2_port_b_v_im = G2_port_v_im;
  T2_port_b_v_im = L2_port_v_im;
  L1_port_v_re = G1_port_v_re;
  L1_port_v_re = T1a_port_a_v_re;
  L1_port_v_re = T1b_port_a_v_re;
  T1a_port_a_v_im = T1b_port_a_v_im;
  T1a_port_a_v_im = G1_port_v_im;
  T1a_port_a_v_im = L1_port_v_im;
  T2_port_a_omegaRef = T1a_port_b_omegaRef;
  T2_port_a_omegaRef = T1b_port_b_omegaRef;
  T1b_port_b_v_re = T1a_port_b_v_re;
  T1b_port_b_v_re = T2_port_a_v_re;
  T1a_port_b_v_im = T2_port_a_v_im;
  T1a_port_b_v_im = T1b_port_b_v_im;
  L2_port_i_re + G2_port_i_re + T2_port_b_i_re = 0.0;
  T2_port_b_i_im + G2_port_i_im + L2_port_i_im = 0.0;
  G1_port_i_re + T1a_port_a_i_re + T1b_port_a_i_re + L1_port_i_re = 0.0;
  G1_port_i_im + T1b_port_a_i_im + L1_port_i_im + T1a_port_a_i_im = 0.0;
  T1a_port_b_i_re + T2_port_a_i_re + T1b_port_b_i_re = 0.0;
  T1a_port_b_i_im + T2_port_a_i_im + T1b_port_b_i_im = 0.0;
  T2_port_b_int_i_re = 0.0;
  T2_port_b_int_i_im = 0.0;
  der(G1_theta) = (G1_omega - G1_port_omegaRef) * 314.1592653589793;
  G1_Ta * G1_omega * der(G1_omega) = G1_Ps + G1_Pc - G1_Pe;
  G1_port_v = Complex.'constructor'.fromReal(G1_V * cos(G1_theta), G1_V * sin(G1_theta));
  G1_Pe = -Modelica.ComplexMath.real(Modelica.SIunits.ComplexPerUnit.'*'.multiply(G1_port_v, Modelica.ComplexMath.conj(G1_port_i)));
  G1_Pc = -(G1_omega - 1.0) / G1_droop;
  G1_port_omegaRef = G1_omega;
  der(G2_theta) = (G2_omega - G2_port_omegaRef) * 314.1592653589793;
  G2_Ta * G2_omega * der(G2_omega) = G2_Ps + G2_Pc - G2_Pe;
  G2_port_v = Complex.'constructor'.fromReal(G2_V * cos(G2_theta), G2_V * sin(G2_theta));
  G2_Pe = -Modelica.ComplexMath.real(Modelica.SIunits.ComplexPerUnit.'*'.multiply(G2_port_v, Modelica.ComplexMath.conj(G2_port_i)));
  G2_Pc = -(G2_omega - 1.0) / G2_droop;
  G2_port_omegaRef = G2_omega;
  Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re) = Complex.'constructor'.fromReal(L1_P, L1_Q);
  Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re) = Complex.'constructor'.fromReal(L2_P, L2_Q);
  Complex.'constructor'.fromReal(c1_re + c2_re, c1_im + c2_im) = Complex.'constructor'.fromReal(0.0, 0.0);
  T1a_port_a_i = Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re);

  when T1a_open then
    T1a_closed = false;
    T1a_B_act = 0.0;
  elsewhen T1a_close then
    T1a_closed = true;
    T1a_B_act = T1a_B;
  end when;

  T1a_port_a_omegaRef = T1a_port_b_omegaRef;
  Complex.'constructor'.fromReal(c1_re + c2_re, c1_im + c2_im) = Complex.'constructor'.fromReal(0.0, 0.0);
  T1b_port_a_i = Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re);

  when T1b_open then
    T1b_closed = false;
    T1b_B_act = 0.0;
  elsewhen T1b_close then
    T1b_closed = true;
    T1b_B_act = T1b_B;
  end when;

  T1b_port_a_omegaRef = T1b_port_b_omegaRef;
  Complex.'constructor'.fromReal(c1_re + c2_re, c1_im + c2_im) = Complex.'constructor'.fromReal(0.0, 0.0);
  T2_port_a_i = Complex.'constructor'.fromReal(c1_re * c2_re - c1_im * c2_im, c1_re * c2_im + c1_im * c2_re);

  when T2_open then
    T2_closed = false;
    T2_B_act = 0.0;
  elsewhen T2_close then
    T2_closed = true;
    T2_B_act = T2_B;
  end when;

  T2_port_a_omegaRef = T2_port_b_int_omegaRef;
end DynamicOverconstrainedConnectors_System4;
"


end
