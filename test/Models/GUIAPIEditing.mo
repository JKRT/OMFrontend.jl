package GUIAPIEditing
  connector Pin
    Real v;
    flow Real i;
  end Pin;

  model Source
    Pin p annotation(Placement(transformation(extent = {{-10, -10}, {10, 10}}, origin = {90, 0})));
  end Source;

  model Load
    Pin n annotation(Placement(transformation(extent = {{-10, -10}, {10, 10}}, origin = {-90, 0})));
  end Load;

  model Example
    Source src annotation(Placement(transformation(extent = {{-70, -10}, {-50, 10}})));
    Load load annotation(Placement(transformation(extent = {{50, -10}, {70, 10}})));
  equation
    connect(src.p, load.n) annotation(Line(points = {{-50, 0}, {50, 0}}, color = {0, 0, 255}));
    annotation(Diagram(coordinateSystem(extent = {{-100, -100}, {100, 100}}, preserveAspectRatio = false)));
  end Example;
end GUIAPIEditing;
