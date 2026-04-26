package MechanicsExamples
import Modelica.Mechanics.MultiBody.Examples.Loops.Engine1a;
import Modelica.Mechanics.MultiBody.Examples.Loops.Engine1b;
import Modelica.Mechanics.MultiBody.Examples.Loops.Engine1b_analytic;
import Modelica.Mechanics.MultiBody.Examples.Loops.EngineV6;
import Modelica.Mechanics.MultiBody.Examples.Loops.EngineV6_analytic;
import Modelica.Mechanics.MultiBody.Examples.Elementary.*;
import Modelica.Mechanics.MultiBody.Examples.Systems.RobotR3;
//TODO Add more models from the MSL Mechanics Library here.

model PendulumTest
Pendulum pendulum;
end PendulumTest;

model DoublePendulumTest
DoublePendulum doublePendulum;
end DoublePendulumTest;

model Engine1aTest
Engine1a engine;
end Engine1aTest;

model Engine1bTest
Engine1b engine;
end Engine1bTest;

model Engine1bAnalyticTest
Engine1b_analytic engine;
end Engine1bAnalyticTest;

model EngineV6Test
EngineV6 engineV6;
end EngineV6Test;

//Not currently tested.
model RobotTest
RobotR3.FullRobot robot;
end RobotTest;



end MechanicsExamples;