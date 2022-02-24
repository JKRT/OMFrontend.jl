model TransmissionLineModelica
  "Transmission line circuit - Implementation using the Modelica Standard Library"
  import Modelica.SIunits;
  parameter Integer N = 1 "number of segments of the transmission line";
  parameter SIunits.Resistance r "resistance per meter";
  parameter SIunits.Inductance l "inductance per meter";
  parameter SIunits.Capacitance c "capacitance per meter";
  parameter SIunits.Length length "length of the transmission line";
  parameter SIunits.AngularFrequency w "cut-off frequency";
  final parameter SIunits.Resistance RL = (l / c) ^ (1 / 2)
    "load resistance";
  final parameter SIunits.Time TD = length / v
    "time delay of the transmission line";
  final parameter SIunits.Velocity v = 1 / (l * c) ^ (1 / 2)
    "velocity of the signal";
  Modelica.Electrical.Analog.Sources.SignalVoltage signalvoltage annotation(Placement(transformation(origin = {-34, 54}, extent = {{-10, -10}, {10, 10}}, rotation = 180)));
  Modelica.Electrical.Analog.Basic.Ground ground1 annotation(Placement(transformation(origin = {-56, 40}, extent = {{-10, -10}, {10, 10}})));
  Modelica.Blocks.Continuous.SecondOrder lowpassfilter(w = w, D = 1,
    initType=Modelica.Blocks.Types.Init.InitialState)                annotation(Placement(transformation(origin = {-50, 14}, extent = {{-10, -10}, {10, 10}})));
  Modelica.Blocks.Sources.Step step annotation(Placement(transformation(origin = {-84, 14}, extent = {{-10, -10}, {10, 10}})));
  Models.TransmissionLine transmissionline(N = N, r = r, l = l, c = c, length = length) annotation(Placement(transformation(origin = {-6, 54}, extent = {{-10, -10}, {10, 10}})));
  Modelica.Electrical.Analog.Basic.Resistor resistor(R = RL) annotation(Placement(transformation(origin = {26, 54}, extent = {{-10, -10}, {10, 10}})));
  Modelica.Electrical.Analog.Basic.Ground ground2 annotation(Placement(transformation(origin = {54, 40}, extent = {{-10, -10}, {10, 10}})));
equation
  connect(resistor.n, ground2.p) annotation(Line(points = {{36, 54}, {54, 54}, {54, 50}}, color = {0, 0, 255}));
  connect(transmissionline.pin_n, resistor.p) annotation(Line(points = {{4, 54}, {16, 54}}, color = {0, 0, 255}));
  connect(signalvoltage.p, transmissionline.pin_p) annotation(Line(points = {{-24, 54}, {-16, 54}}, color = {0, 0, 255}));
  connect(step.y, lowpassfilter.u) annotation(Line(points = {{-73, 14}, {-62, 14}}, color = {0, 0, 127}));
  connect(lowpassfilter.y, signalvoltage.v) annotation(Line(points = {{-39, 14}, {-34, 14}, {-34, 47}}, color = {0, 0, 127}));
  connect(ground1.p, signalvoltage.n) annotation(Line(points = {{-56, 50}, {-56, 54}, {-44, 54}}, color = {0, 0, 255}));
  annotation(Documentation(info = "<html><p>In this model, a transmission line circuit is implemented by the Modelica Standard Library components. Transmission line circuit is represented as in the figure below. The application is the same as TransmissionLineEquations.</p><p><img src=\"modelica://ScalableTestSuite/Resources/Images/TransmissionLine/TransmissionLineModelica.png\"/></p><p>The parameters for the TransmissionLineModelica are: </p><table border=\"1\" cellspacing=\"0\" cellpadding=\"2\">
<tr>
  <th>Parameters</th>
  <th>Comment</th>
</tr>
            <tr>
  <td valign=\"top\">N</td>
  <td valign=\"top\">number of segments</td>
</tr>
<tr>
  <td valign=\"top\">r</td>
  <td valign=\"top\">resistance per meter</td>
</tr>
<tr>
  <td valign=\"top\">l</td>
  <td valign=\"top\">inductance per meter</td>
</tr>
<tr>
  <td valign=\"top\">c</td>
  <td valign=\"top\">capacitance per meter</td>
</tr>
            <tr>
  <td valign=\"top\">length</td>
  <td valign=\"top\">length of the transmission line</td>
</tr>
            <tr>
  <td valign=\"top\">w</td>
  <td valign=\"top\">cut-off frequency</td>
</tr>
            <tr>
  <td valign=\"top\">RL</td>
  <td valign=\"top\">load resistance</td>
</tr>
            <tr>
  <td valign=\"top\">TD</td>
  <td valign=\"top\">time delay of the transmission line</td>
</tr>
            <tr>
  <td valign=\"top\">v</td>
  <td valign=\"top\">velocity of signal</td>
</tr>
</table>
</html>"));
end TransmissionLineModelica;
