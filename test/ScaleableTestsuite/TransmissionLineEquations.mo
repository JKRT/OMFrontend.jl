model TransmissionLineEquations
  "Transmission line circuit - Direct implementation by equations"
  import Modelica.SIunits;
  parameter Integer N = 1 "number of segments";
  parameter SIunits.Length L "length of the transmission line";
  final parameter SIunits.Length l = L / N "length of the each segment";
  parameter SIunits.Resistance res "resistance per meter";
  parameter SIunits.Capacitance cap "capacitance per meter";
  parameter SIunits.Inductance ind "inductance per meter";
  final parameter SIunits.Resistance RL = (ind / cap) ^ (1 / 2)
    "load resistance";
  parameter SIunits.AngularFrequency w;
  final parameter SIunits.Time TD = L / v
    "time delay of the transmission line";
  final parameter SIunits.Velocity v = 1 / (ind * cap) ^ (1 / 2)
    "velocity of the signal";
  SIunits.Voltage Vstep = if time > 0 then 1 else 0 "input step voltage";
  SIunits.Current cur[N](each start = 0)
    "current values at the nodes of the transmission line";
  SIunits.Voltage vol[N](each start = 0)
    "voltage values at the nodes of the transmission line";
  Real vvol "derivative of input voltage";
equation
  vvol = der(vol[1]);
  //Vstep = vol[1] + 2 * Rf * Cf * der(vol[1]) + Rf ^ 2 * Cf ^ 2 * der(vvol);
  Vstep = vol[1] + 2 * (1 / w) * der(vol[1]) + 1 / w ^ 2 * der(vvol);
  vol[N] = cur[N] * RL;
  for i in 1:N - 1 loop
    cap * der(vol[i + 1]) = (cur[i] - cur[i + 1]) / l;
    ind * der(cur[i]) = (-res * cur[i]) - (vol[i + 1] - vol[i]) / l;
  end for;
initial equation
  vol = zeros(N);
  cur[1:N-1] = zeros(N-1);
  vvol = 0;
  annotation(Documentation(info = "<html><p>In this model, a transmission line circuit is implemented by equations. Transmission line circuit is represented as in the figure below. The application is the same as the TransmissionLineModelica model.</p><p><img src=\"modelica://ScalableTestSuite/Resources/Images/TransmissionLine/TransmissionLineModelica.png\"/></p><p>Considering the nodes of the discrete transmission line(implemented in Electrical.Models.TransmissionLine), circuit equations are described. In the transmission line, there are N segments, therefore, there will be N+1 nodes and N+1 voltage and current variables.</p><p><img src=\"modelica://ScalableTestSuite/Resources/Images/TransmissionLine/tlmequation.png\"/></p><p>where j= 2,..,N and Rx is the resistance per meter, Cx is the capacitance per meter and Lx is the inductance per meter.</p><p>output voltage is described as:</p><p><img src=\"modelica://ScalableTestSuite/Resources/Images/TransmissionLine/tlmequation1.png\"/></p><p>Moreover, considering the form of the second order low pass filter, equation of the filter to a step input can be defined in the following way:</p><p><img src=\"modelica://ScalableTestSuite/Resources/Images/TransmissionLine/tlmequation2.png\"/></p><p>where Vstep  is the step voltage and v1is the output voltage of the filter. The parameters of the TransmissionLineEquations are:</p><table border=\"1\" cellspacing=\"0\" cellpadding=\"2\">
<tr>
  <th>Parameters</th>
  <th>Comment</th>
</tr>
            <tr>
  <td valign=\"top\">N</td>
  <td valign=\"top\">number of segments</td>
</tr>
<tr>
  <td valign=\"top\">L</td>
  <td valign=\"top\">length of transmission line</td>
</tr>
<tr>
  <td valign=\"top\">l</td>
  <td valign=\"top\">length of each segment</td>
</tr>
<tr>
  <td valign=\"top\">res</td>
  <td valign=\"top\">resistor per meter</td>
</tr>
            <tr>
  <td valign=\"top\">cap</td>
  <td valign=\"top\">capacitance per meter</td>
</tr>
            <tr>
  <td valign=\"top\">ind</td>
  <td valign=\"top\">inductance per meter</td>
</tr>
            <tr>
  <td valign=\"top\">RL</td>
  <td valign=\"top\">load resistance</td>
</tr>
            <tr>
  <td valign=\"top\">w</td>
  <td valign=\"top\">angular frequency</td>
</tr>
            <tr>
  <td valign=\"top\">TD</td>
  <td valign=\"top\">time delay of transmission line</td>
</tr>
            <tr>
  <td valign=\"top\">v</td>
  <td valign=\"top\">velocity of signal</td>
</tr>
</table>
</html>"));
end TransmissionLineEquations;