package ModelicaServices "ModelicaServices (OpenModelica implementation) - Models and functions used in the Modelica Standard Library requiring a tool specific implementation"
  extends Modelica.Icons.Package;

  package Machine "Machine dependent constants"
    extends Modelica.Icons.Package;
    final constant Real eps = 1e-15 "Biggest number such that 1.0 + eps = 1.0";
    final constant Real small = 1e-60 "Smallest number such that small and -small are representable on the machine";
    final constant Real inf = 1e60 "Biggest Real number such that inf and -inf are representable on the machine";
    final constant Integer Integer_inf = OpenModelica.Internal.Architecture.integerMax() "Biggest Integer number such that Integer_inf and -Integer_inf are representable on the machine";
  end Machine;
  annotation(version = "3.2.3", versionBuild = 4, versionDate = "2019-01-23", dateModified = "2020-06-04 11:00:00Z");
end ModelicaServices;

package Modelica "Modelica Standard Library - Version 3.2.3"
  extends Modelica.Icons.Package;

  package Blocks "Library of basic input/output control blocks (continuous, discrete, logical, table blocks)"
    import SI = Modelica.SIunits;
    extends Modelica.Icons.Package;

    package Examples "Library of examples to demonstrate the usage of package Blocks"
      extends Modelica.Icons.ExamplesPackage;

      model PID_Controller "Demonstrates the usage of a Continuous.LimPID controller"
        extends Modelica.Icons.Example;
        parameter Modelica.SIunits.Angle driveAngle = 1.570796326794897 "Reference distance to move";
        Modelica.Blocks.Continuous.LimPID PI(k = 100, Ti = 0.1, yMax = 12, Ni = 0.1, initType = Modelica.Blocks.Types.InitPID.SteadyState, limitsAtInit = false, controllerType = Modelica.Blocks.Types.SimpleController.PI, limiter(u(start = 0)), Td = 0.1);
        Modelica.Mechanics.Rotational.Components.Inertia inertia1(phi(fixed = true, start = 0), J = 1, a(fixed = true, start = 0));
        Modelica.Mechanics.Rotational.Sources.Torque torque;
        Modelica.Mechanics.Rotational.Components.SpringDamper spring(c = 1e4, d = 100, stateSelect = StateSelect.prefer, w_rel(fixed = true));
        Modelica.Mechanics.Rotational.Components.Inertia inertia2(J = 2);
        Modelica.Blocks.Sources.KinematicPTP kinematicPTP(startTime = 0.5, deltaq = {driveAngle}, qd_max = {1}, qdd_max = {1});
        Modelica.Blocks.Continuous.Integrator integrator(initType = Modelica.Blocks.Types.Init.InitialState);
        Modelica.Mechanics.Rotational.Sensors.SpeedSensor speedSensor;
        Modelica.Mechanics.Rotational.Sources.ConstantTorque loadTorque(tau_constant = 10, useSupport = false);
      initial equation
        der(spring.w_rel) = 0;
      equation
        connect(spring.flange_b, inertia2.flange_a);
        connect(inertia1.flange_b, spring.flange_a);
        connect(torque.flange, inertia1.flange_a);
        connect(kinematicPTP.y[1], integrator.u);
        connect(speedSensor.flange, inertia1.flange_b);
        connect(loadTorque.flange, inertia2.flange_b);
        connect(PI.y, torque.tau);
        connect(speedSensor.w, PI.u_m);
        connect(integrator.y, PI.u_s);
        annotation(experiment(StopTime = 4));
      end PID_Controller;
    end Examples;

    package Continuous "Library of continuous control blocks with internal states"
      import Modelica.Blocks.Interfaces;
      import Modelica.SIunits;
      extends Modelica.Icons.Package;

      block Integrator "Output the integral of the input signal with optional reset"
        import Modelica.Blocks.Types.Init;
        parameter Real k(unit = "1") = 1 "Integrator gain";
        parameter Boolean use_reset = false "=true, if reset port enabled" annotation(Evaluate = true, HideResult = true);
        parameter Boolean use_set = false "=true, if set port enabled and used as reinitialization value when reset" annotation(Evaluate = true, HideResult = true);
        parameter Modelica.Blocks.Types.Init initType = Modelica.Blocks.Types.Init.InitialState "Type of initialization (1: no init, 2: steady state, 3,4: initial output)" annotation(Evaluate = true);
        parameter Real y_start = 0 "Initial or guess value of output (= state)";
        extends Interfaces.SISO(y(start = y_start));
        Modelica.Blocks.Interfaces.BooleanInput reset if use_reset "Optional connector of reset signal";
        Modelica.Blocks.Interfaces.RealInput set if use_reset and use_set "Optional connector of set signal";
      protected
        Modelica.Blocks.Interfaces.BooleanOutput local_reset annotation(HideResult = true);
        Modelica.Blocks.Interfaces.RealOutput local_set annotation(HideResult = true);
      initial equation
        if initType == Init.SteadyState then
          der(y) = 0;
        elseif initType == Init.InitialState or initType == Init.InitialOutput then
          y = y_start;
        end if;
      equation
        if use_reset then
          connect(reset, local_reset);
          if use_set then
            connect(set, local_set);
          else
            local_set = y_start;
          end if;
          when local_reset then
            reinit(y, local_set);
          end when;
        else
          local_reset = false;
          local_set = 0;
        end if;
        der(y) = k * u;
      end Integrator;

      block Derivative "Approximated derivative block"
        import Modelica.Blocks.Types.Init;
        parameter Real k(unit = "1") = 1 "Gains";
        parameter SIunits.Time T(min = Modelica.Constants.small) = 0.01 "Time constants (T>0 required; T=0 is ideal derivative block)";
        parameter Modelica.Blocks.Types.Init initType = Modelica.Blocks.Types.Init.NoInit "Type of initialization (1: no init, 2: steady state, 3: initial state, 4: initial output)" annotation(Evaluate = true);
        parameter Real x_start = 0 "Initial or guess value of state";
        parameter Real y_start = 0 "Initial value of output (= state)";
        extends Interfaces.SISO;
        output Real x(start = x_start) "State of block";
      protected
        parameter Boolean zeroGain = abs(k) < Modelica.Constants.eps;
      initial equation
        if initType == Init.SteadyState then
          der(x) = 0;
        elseif initType == Init.InitialState then
          x = x_start;
        elseif initType == Init.InitialOutput then
          if zeroGain then
            x = u;
          else
            y = y_start;
          end if;
        end if;
      equation
        der(x) = if zeroGain then 0 else (u - x) / T;
        y = if zeroGain then 0 else k / T * (u - x);
      end Derivative;

      block LimPID "P, PI, PD, and PID controller with limited output, anti-windup compensation, setpoint weighting and optional feed-forward"
        import Modelica.Blocks.Types.InitPID;
        import Modelica.Blocks.Types.Init;
        import Modelica.Blocks.Types.SimpleController;
        extends Modelica.Blocks.Interfaces.SVcontrol;
        output Real controlError = u_s - u_m "Control error (set point - measurement)";
        parameter .Modelica.Blocks.Types.SimpleController controllerType = .Modelica.Blocks.Types.SimpleController.PID "Type of controller";
        parameter Real k(min = 0, unit = "1") = 1 "Gain of controller";
        parameter Modelica.SIunits.Time Ti(min = Modelica.Constants.small) = 0.5 "Time constant of Integrator block";
        parameter Modelica.SIunits.Time Td(min = 0) = 0.1 "Time constant of Derivative block";
        parameter Real yMax(start = 1) "Upper limit of output";
        parameter Real yMin = -yMax "Lower limit of output";
        parameter Real wp(min = 0) = 1 "Set-point weight for Proportional block (0..1)";
        parameter Real wd(min = 0) = 0 "Set-point weight for Derivative block (0..1)";
        parameter Real Ni(min = 100 * Modelica.Constants.eps) = 0.9 "Ni*Ti is time constant of anti-windup compensation";
        parameter Real Nd(min = 100 * Modelica.Constants.eps) = 10 "The higher Nd, the more ideal the derivative block";
        parameter Boolean withFeedForward = false "Use feed-forward input?" annotation(Evaluate = true);
        parameter Real kFF = 1 "Gain of feed-forward input";
        parameter .Modelica.Blocks.Types.InitPID initType = .Modelica.Blocks.Types.InitPID.DoNotUse_InitialIntegratorState "Type of initialization (1: no init, 2: steady state, 3: initial state, 4: initial output)" annotation(Evaluate = true);
        parameter Real xi_start = 0 "Initial or guess value for integrator output (= integrator state)";
        parameter Real xd_start = 0 "Initial or guess value for state of derivative block";
        parameter Real y_start = 0 "Initial value of output";
        parameter Modelica.Blocks.Types.LimiterHomotopy homotopyType = Modelica.Blocks.Types.LimiterHomotopy.Linear "Simplified model for homotopy-based initialization" annotation(Evaluate = true);
        parameter Boolean strict = false "= true, if strict limits with noEvent(..)" annotation(Evaluate = true);
        parameter Boolean limitsAtInit = true "Has no longer an effect and is only kept for backwards compatibility (the implementation uses now the homotopy operator)" annotation(Evaluate = true);
        constant Modelica.SIunits.Time unitTime = 1 annotation(HideResult = true);
        Modelica.Blocks.Interfaces.RealInput u_ff if withFeedForward "Optional connector of feed-forward input signal";
        Modelica.Blocks.Math.Add addP(k1 = wp, k2 = -1);
        Modelica.Blocks.Math.Add addD(k1 = wd, k2 = -1) if with_D;
        Modelica.Blocks.Math.Gain P(k = 1);
        Modelica.Blocks.Continuous.Integrator I(k = unitTime / Ti, y_start = xi_start, initType = if initType == InitPID.SteadyState then Init.SteadyState else if initType == InitPID.InitialState or initType == InitPID.DoNotUse_InitialIntegratorState then Init.InitialState else Init.NoInit) if with_I;
        Modelica.Blocks.Continuous.Derivative D(k = Td / unitTime, T = max([Td / Nd, 1.e-14]), x_start = xd_start, initType = if initType == InitPID.SteadyState or initType == InitPID.InitialOutput then Init.SteadyState else if initType == InitPID.InitialState then Init.InitialState else Init.NoInit) if with_D;
        Modelica.Blocks.Math.Gain gainPID(k = k);
        Modelica.Blocks.Math.Add3 addPID;
        Modelica.Blocks.Math.Add3 addI(k2 = -1) if with_I;
        Modelica.Blocks.Math.Add addSat(k1 = +1, k2 = -1) if with_I;
        Modelica.Blocks.Math.Gain gainTrack(k = 1 / (k * Ni)) if with_I;
        Modelica.Blocks.Nonlinear.Limiter limiter(uMax = yMax, uMin = yMin, strict = strict, limitsAtInit = limitsAtInit, homotopyType = homotopyType);
      protected
        parameter Boolean with_I = controllerType == SimpleController.PI or controllerType == SimpleController.PID annotation(Evaluate = true, HideResult = true);
        parameter Boolean with_D = controllerType == SimpleController.PD or controllerType == SimpleController.PID annotation(Evaluate = true, HideResult = true);
      public
        Modelica.Blocks.Sources.Constant Dzero(k = 0) if not with_D;
        Modelica.Blocks.Sources.Constant Izero(k = 0) if not with_I;
        Modelica.Blocks.Sources.Constant FFzero(k = 0) if not withFeedForward;
        Modelica.Blocks.Math.Add addFF(k1 = 1, k2 = kFF);
      initial equation
        if initType == InitPID.InitialOutput then
          gainPID.y = y_start;
        end if;
      equation
        if initType == InitPID.InitialOutput and (y_start < yMin or y_start > yMax) then
          Modelica.Utilities.Streams.error("LimPID: Start value y_start (=" + String(y_start) + ") is outside of the limits of yMin (=" + String(yMin) + ") and yMax (=" + String(yMax) + ")");
        end if;
        connect(u_s, addP.u1);
        connect(u_s, addD.u1);
        connect(u_s, addI.u1);
        connect(addP.y, P.u);
        connect(addD.y, D.u);
        connect(addI.y, I.u);
        connect(P.y, addPID.u1);
        connect(D.y, addPID.u2);
        connect(I.y, addPID.u3);
        connect(limiter.y, addSat.u1);
        connect(limiter.y, y);
        connect(addSat.y, gainTrack.u);
        connect(gainTrack.y, addI.u3);
        connect(u_m, addP.u2);
        connect(u_m, addD.u2);
        connect(u_m, addI.u2);
        connect(Dzero.y, addPID.u2);
        connect(Izero.y, addPID.u3);
        connect(addPID.y, gainPID.u);
        connect(addFF.y, limiter.u);
        connect(gainPID.y, addFF.u1);
        connect(FFzero.y, addFF.u2);
        connect(addFF.u2, u_ff);
        connect(addFF.y, addSat.u2);
      end LimPID;
    end Continuous;

    package Interfaces "Library of connectors and partial models for input/output blocks"
      import Modelica.SIunits;
      extends Modelica.Icons.InterfacesPackage;
      connector RealInput = input Real "'input Real' as connector";
      connector RealOutput = output Real "'output Real' as connector";
      connector BooleanInput = input Boolean "'input Boolean' as connector";
      connector BooleanOutput = output Boolean "'output Boolean' as connector";

      partial block SO "Single Output continuous control block"
        extends Modelica.Blocks.Icons.Block;
        RealOutput y "Connector of Real output signal";
      end SO;

      partial block MO "Multiple Output continuous control block"
        extends Modelica.Blocks.Icons.Block;
        parameter Integer nout(min = 1) = 1 "Number of outputs";
        RealOutput[nout] y "Connector of Real output signals";
      end MO;

      partial block SISO "Single Input Single Output continuous control block"
        extends Modelica.Blocks.Icons.Block;
        RealInput u "Connector of Real input signal";
        RealOutput y "Connector of Real output signal";
      end SISO;

      partial block SI2SO "2 Single Input / 1 Single Output continuous control block"
        extends Modelica.Blocks.Icons.Block;
        RealInput u1 "Connector of Real input signal 1";
        RealInput u2 "Connector of Real input signal 2";
        RealOutput y "Connector of Real output signal";
      end SI2SO;

      partial block SVcontrol "Single-Variable continuous controller"
        extends Modelica.Blocks.Icons.Block;
        RealInput u_s "Connector of setpoint input signal";
        RealInput u_m "Connector of measurement input signal";
        RealOutput y "Connector of actuator output signal";
      end SVcontrol;
    end Interfaces;

    package Math "Library of Real mathematical functions as input/output blocks"
      import Modelica.SIunits;
      import Modelica.Blocks.Interfaces;
      extends Modelica.Icons.Package;

      block Gain "Output the product of a gain value with the input signal"
        parameter Real k(start = 1, unit = "1") "Gain value multiplied with input signal";
        Interfaces.RealInput u "Input signal connector";
        Interfaces.RealOutput y "Output signal connector";
      equation
        y = k * u;
      end Gain;

      block Add "Output the sum of the two inputs"
        extends Interfaces.SI2SO;
        parameter Real k1 = +1 "Gain of input signal 1";
        parameter Real k2 = +1 "Gain of input signal 2";
      equation
        y = k1 * u1 + k2 * u2;
      end Add;

      block Add3 "Output the sum of the three inputs"
        extends Modelica.Blocks.Icons.Block;
        parameter Real k1 = +1 "Gain of input signal 1";
        parameter Real k2 = +1 "Gain of input signal 2";
        parameter Real k3 = +1 "Gain of input signal 3";
        Interfaces.RealInput u1 "Connector of Real input signal 1";
        Interfaces.RealInput u2 "Connector of Real input signal 2";
        Interfaces.RealInput u3 "Connector of Real input signal 3";
        Interfaces.RealOutput y "Connector of Real output signal";
      equation
        y = k1 * u1 + k2 * u2 + k3 * u3;
      end Add3;
    end Math;

    package Nonlinear "Library of discontinuous or non-differentiable algebraic control blocks"
      import Modelica.Blocks.Interfaces;
      extends Modelica.Icons.Package;

      block Limiter "Limit the range of a signal"
        parameter Real uMax(start = 1) "Upper limits of input signals";
        parameter Real uMin = -uMax "Lower limits of input signals";
        parameter Boolean strict = false "= true, if strict limits with noEvent(..)" annotation(Evaluate = true);
        parameter Types.LimiterHomotopy homotopyType = Modelica.Blocks.Types.LimiterHomotopy.Linear "Simplified model for homotopy-based initialization" annotation(Evaluate = true);
        parameter Boolean limitsAtInit = true "Has no longer an effect and is only kept for backwards compatibility (the implementation uses now the homotopy operator)" annotation(Evaluate = true);
        extends Interfaces.SISO;
      protected
        Real simplifiedExpr "Simplified expression for homotopy-based initialization";
      equation
        assert(uMax >= uMin, "Limiter: Limits must be consistent. However, uMax (=" + String(uMax) + ") < uMin (=" + String(uMin) + ")");
        simplifiedExpr = if homotopyType == Types.LimiterHomotopy.Linear then u else if homotopyType == Types.LimiterHomotopy.UpperLimit then uMax else if homotopyType == Types.LimiterHomotopy.LowerLimit then uMin else 0;
        if strict then
          if homotopyType == Types.LimiterHomotopy.NoHomotopy then
            y = smooth(0, noEvent(if u > uMax then uMax else if u < uMin then uMin else u));
          else
            y = homotopy(actual = smooth(0, noEvent(if u > uMax then uMax else if u < uMin then uMin else u)), simplified = simplifiedExpr);
          end if;
        else
          if homotopyType == Types.LimiterHomotopy.NoHomotopy then
            y = smooth(0, if u > uMax then uMax else if u < uMin then uMin else u);
          else
            y = homotopy(actual = smooth(0, if u > uMax then uMax else if u < uMin then uMin else u), simplified = simplifiedExpr);
          end if;
        end if;
      end Limiter;
    end Nonlinear;

    package Sources "Library of signal source blocks generating Real, Integer and Boolean signals"
      import Modelica.Blocks.Interfaces;
      import Modelica.SIunits;
      extends Modelica.Icons.SourcesPackage;

      block Constant "Generate constant signal of type Real"
        parameter Real k(start = 1) "Constant output value";
        extends Interfaces.SO;
      equation
        y = k;
      end Constant;

      block KinematicPTP "Move as fast as possible along a distance within given kinematic constraints"
        parameter Real[:] deltaq = {1} "Distance to move";
        parameter Real[:] qd_max(each final min = Modelica.Constants.small) = {1} "Maximum velocities der(q)";
        parameter Real[:] qdd_max(each final min = Modelica.Constants.small) = {1} "Maximum accelerations der(qd)";
        parameter SIunits.Time startTime = 0 "Time instant at which movement starts";
        extends Interfaces.MO(final nout = max([size(deltaq, 1); size(qd_max, 1); size(qdd_max, 1)]));
      protected
        parameter Real[nout] p_deltaq = if size(deltaq, 1) == 1 then ones(nout) * deltaq[1] else deltaq;
        parameter Real[nout] p_qd_max = if size(qd_max, 1) == 1 then ones(nout) * qd_max[1] else qd_max;
        parameter Real[nout] p_qdd_max = if size(qdd_max, 1) == 1 then ones(nout) * qdd_max[1] else qdd_max;
        Real sd_max;
        Real sdd_max;
        Real sdd;
        Real[nout] aux1;
        Real[nout] aux2;
        SIunits.Time Ta1;
        SIunits.Time Ta2;
        SIunits.Time Tv;
        SIunits.Time Te;
        Boolean noWphase;
      equation
        for i in 1:nout loop
          aux1[i] = p_deltaq[i] / p_qd_max[i];
          aux2[i] = p_deltaq[i] / p_qdd_max[i];
        end for;
        sd_max = 1 / max(abs(aux1));
        sdd_max = 1 / max(abs(aux2));
        Ta1 = sqrt(1 / sdd_max);
        Ta2 = sd_max / sdd_max;
        noWphase = Ta2 >= Ta1;
        Tv = if noWphase then Ta1 else 1 / sd_max;
        Te = if noWphase then Ta1 + Ta1 else Tv + Ta2;
        sdd = if time < startTime then 0 else if noWphase then if time < Ta1 + startTime then sdd_max else if time < Te + startTime then -sdd_max else 0 else if time < Ta2 + startTime then sdd_max else if time < Tv + startTime then 0 else if time < Te + startTime then -sdd_max else 0;
        y = p_deltaq * sdd;
      end KinematicPTP;
    end Sources;

    package Types "Library of constants, external objects and types with choices, especially to build menus"
      extends Modelica.Icons.TypesPackage;
      type Init = enumeration(NoInit "No initialization (start values are used as guess values with fixed=false)", SteadyState "Steady state initialization (derivatives of states are zero)", InitialState "Initialization with initial states", InitialOutput "Initialization with initial outputs (and steady state of the states if possible)") "Enumeration defining initialization of a block" annotation(Evaluate = true);
      type InitPID = enumeration(NoInit "No initialization (start values are used as guess values with fixed=false)", SteadyState "Steady state initialization (derivatives of states are zero)", InitialState "Initialization with initial states", InitialOutput "Initialization with initial outputs (and steady state of the states if possible)", DoNotUse_InitialIntegratorState "Do not use, only for backward compatibility (initialize only integrator state)") "Enumeration defining initialization of PID and LimPID blocks" annotation(Evaluate = true);
      type SimpleController = enumeration(P "P controller", PI "PI controller", PD "PD controller", PID "PID controller") "Enumeration defining P, PI, PD, or PID simple controller type" annotation(Evaluate = true);
      type LimiterHomotopy = enumeration(NoHomotopy "Homotopy is not used", Linear "Simplified model without limits", UpperLimit "Simplified model fixed at upper limit", LowerLimit "Simplified model fixed at lower limit") "Enumeration defining use of homotopy in limiter components" annotation(Evaluate = true);
    end Types;

    package Icons "Icons for Blocks"
      extends Modelica.Icons.IconsPackage;

      partial block Block "Basic graphical layout of input/output block" end Block;
    end Icons;
  end Blocks;

  package Mechanics "Library of 1-dim. and 3-dim. mechanical components (multi-body, rotational, translational)"
    extends Modelica.Icons.Package;

    package Rotational "Library to model 1-dimensional, rotational mechanical systems"
      extends Modelica.Icons.Package;
      import SI = Modelica.SIunits;

      package Components "Components for 1D rotational mechanical drive trains"
        extends Modelica.Icons.Package;

        model Inertia "1D-rotational component with inertia"
          extends Rotational.Interfaces.PartialTwoFlanges;
          parameter SI.Inertia J(min = 0, start = 1) "Moment of inertia";
          parameter StateSelect stateSelect = StateSelect.default "Priority to use phi and w as states" annotation(HideResult = true);
          SI.Angle phi(stateSelect = stateSelect) "Absolute rotation angle of component";
          SI.AngularVelocity w(stateSelect = stateSelect) "Absolute angular velocity of component (= der(phi))";
          SI.AngularAcceleration a "Absolute angular acceleration of component (= der(w))";
        equation
          phi = flange_a.phi;
          phi = flange_b.phi;
          w = der(phi);
          a = der(w);
          J * a = flange_a.tau + flange_b.tau;
        end Inertia;

        model SpringDamper "Linear 1D rotational spring and damper in parallel"
          parameter SI.RotationalSpringConstant c(final min = 0, start = 1.0e5) "Spring constant";
          parameter SI.RotationalDampingConstant d(final min = 0, start = 0) "Damping constant";
          parameter SI.Angle phi_rel0 = 0 "Unstretched spring angle";
          extends Modelica.Mechanics.Rotational.Interfaces.PartialCompliantWithRelativeStates;
          extends Modelica.Thermal.HeatTransfer.Interfaces.PartialElementaryConditionalHeatPortWithoutT;
        protected
          Modelica.SIunits.Torque tau_c "Spring torque";
          Modelica.SIunits.Torque tau_d "Damping torque";
        equation
          tau_c = c * (phi_rel - phi_rel0);
          tau_d = d * w_rel;
          tau = tau_c + tau_d;
          lossPower = tau_d * w_rel;
        end SpringDamper;
      end Components;

      package Sensors "Sensors to measure variables in 1D rotational mechanical components"
        extends Modelica.Icons.SensorsPackage;

        model SpeedSensor "Ideal sensor to measure the absolute flange angular velocity"
          extends Rotational.Interfaces.PartialAbsoluteSensor;
          Modelica.Blocks.Interfaces.RealOutput w(unit = "rad/s") "Absolute angular velocity of flange as output signal";
        equation
          w = der(flange.phi);
        end SpeedSensor;
      end Sensors;

      package Sources "Sources to drive 1D rotational mechanical components"
        extends Modelica.Icons.SourcesPackage;

        model Torque "Input signal acting as external torque on a flange"
          extends Modelica.Mechanics.Rotational.Interfaces.PartialElementaryOneFlangeAndSupport2;
          Modelica.Blocks.Interfaces.RealInput tau(unit = "N.m") "Accelerating torque acting at flange (= -flange.tau)";
        equation
          flange.tau = -tau;
        end Torque;

        model ConstantTorque "Constant torque, not dependent on speed"
          extends Rotational.Interfaces.PartialTorque;
          parameter Modelica.SIunits.Torque tau_constant "Constant torque (if negative, torque is acting as load in positive direction of rotation)";
          Modelica.SIunits.AngularVelocity w "Angular velocity of flange with respect to support (= der(phi))";
          Modelica.SIunits.Torque tau "Accelerating torque acting at flange (= -flange.tau)";
        equation
          w = der(phi);
          tau = -flange.tau;
          tau = tau_constant;
        end ConstantTorque;
      end Sources;

      package Interfaces "Connectors and partial models for 1D rotational mechanical components"
        extends Modelica.Icons.InterfacesPackage;

        connector Flange "One-dimensional rotational flange"
          SI.Angle phi "Absolute rotation angle of flange";
          flow SI.Torque tau "Cut torque in the flange";
        end Flange;

        connector Flange_a "One-dimensional rotational flange of a shaft (filled circle icon)"
          extends Flange;
        end Flange_a;

        connector Flange_b "One-dimensional rotational flange of a shaft (non-filled circle icon)"
          extends Flange;
        end Flange_b;

        connector Support "Support/housing flange of a one-dimensional rotational shaft"
          extends Flange;
        end Support;

        partial model PartialTwoFlanges "Partial model for a component with two rotational 1-dim. shaft flanges"
          Flange_a flange_a "Flange of left shaft";
          Flange_b flange_b "Flange of right shaft";
        end PartialTwoFlanges;

        partial model PartialCompliantWithRelativeStates "Partial model for the compliant connection of two rotational 1-dim. shaft flanges where the relative angle and speed are used as preferred states"
          Modelica.SIunits.Angle phi_rel(start = 0, stateSelect = stateSelect, nominal = if phi_nominal >= Modelica.Constants.eps then phi_nominal else 1) "Relative rotation angle (= flange_b.phi - flange_a.phi)";
          Modelica.SIunits.AngularVelocity w_rel(start = 0, stateSelect = stateSelect) "Relative angular velocity (= der(phi_rel))";
          Modelica.SIunits.AngularAcceleration a_rel(start = 0) "Relative angular acceleration (= der(w_rel))";
          Modelica.SIunits.Torque tau "Torque between flanges (= flange_b.tau)";
          Flange_a flange_a "Left flange of compliant 1-dim. rotational component";
          Flange_b flange_b "Right flange of compliant 1-dim. rotational component";
          parameter SI.Angle phi_nominal(displayUnit = "rad", min = 0.0) = 1e-4 "Nominal value of phi_rel (used for scaling)";
          parameter StateSelect stateSelect = StateSelect.prefer "Priority to use phi_rel and w_rel as states" annotation(HideResult = true);
        equation
          phi_rel = flange_b.phi - flange_a.phi;
          w_rel = der(phi_rel);
          a_rel = der(w_rel);
          flange_b.tau = tau;
          flange_a.tau = -tau;
        end PartialCompliantWithRelativeStates;

        partial model PartialElementaryOneFlangeAndSupport2 "Partial model for a component with one rotational 1-dim. shaft flange and a support used for textual modeling, i.e., for elementary models"
          parameter Boolean useSupport = false "= true, if support flange enabled, otherwise implicitly grounded" annotation(Evaluate = true, HideResult = true);
          Flange_b flange "Flange of shaft";
          Support support(phi = phi_support, tau = -flange.tau) if useSupport "Support/housing of component";
        protected
          Modelica.SIunits.Angle phi_support "Absolute angle of support flange";
        equation
          if not useSupport then
            phi_support = 0;
          end if;
        end PartialElementaryOneFlangeAndSupport2;

        partial model PartialTorque "Partial model of a torque acting at the flange (accelerates the flange)"
          extends Modelica.Mechanics.Rotational.Interfaces.PartialElementaryOneFlangeAndSupport2;
          Modelica.SIunits.Angle phi "Angle of flange with respect to support (= flange.phi - support.phi)";
        equation
          phi = flange.phi - phi_support;
        end PartialTorque;

        partial model PartialAbsoluteSensor "Partial model to measure a single absolute flange variable"
          extends Modelica.Icons.RotationalSensor;
          Flange_a flange "Flange of shaft from which sensor information shall be measured";
        equation
          0 = flange.tau;
        end PartialAbsoluteSensor;
      end Interfaces;
    end Rotational;
  end Mechanics;

  package Thermal "Library of thermal system components to model heat transfer and simple thermo-fluid pipe flow"
    extends Modelica.Icons.Package;

    package HeatTransfer "Library of 1-dimensional heat transfer with lumped elements"
      extends Modelica.Icons.Package;

      package Interfaces "Connectors and partial models"
        extends Modelica.Icons.InterfacesPackage;

        partial connector HeatPort "Thermal port for 1-dim. heat transfer"
          Modelica.SIunits.Temperature T "Port temperature";
          flow Modelica.SIunits.HeatFlowRate Q_flow "Heat flow rate (positive if flowing from outside into the component)";
        end HeatPort;

        connector HeatPort_a "Thermal port for 1-dim. heat transfer (filled rectangular icon)"
          extends HeatPort;
        end HeatPort_a;

        partial model PartialElementaryConditionalHeatPortWithoutT "Partial model to include a conditional HeatPort in order to dissipate losses, used for textual modeling, i.e., for elementary models"
          parameter Boolean useHeatPort = false "=true, if heatPort is enabled" annotation(Evaluate = true, HideResult = true);
          Modelica.Thermal.HeatTransfer.Interfaces.HeatPort_a heatPort(final Q_flow = -lossPower) if useHeatPort "Optional port to which dissipated losses are transported in form of heat";
          Modelica.SIunits.Power lossPower "Loss power leaving component via heatPort (> 0, if heat is flowing out of component)";
        end PartialElementaryConditionalHeatPortWithoutT;
      end Interfaces;
    end HeatTransfer;
  end Thermal;

  package Math "Library of mathematical functions (e.g., sin, cos) and of functions operating on vectors and matrices"
    import SI = Modelica.SIunits;
    extends Modelica.Icons.Package;

    package Icons "Icons for Math"
      extends Modelica.Icons.IconsPackage;

      partial function AxisCenter "Basic icon for mathematical function with y-axis in the center" end AxisCenter;
    end Icons;

    function asin "Inverse sine (-1 <= u <= 1)"
      extends Modelica.Math.Icons.AxisCenter;
      input Real u;
      output SI.Angle y;
      external "builtin" y = asin(u);
    end asin;

    function exp "Exponential, base e"
      extends Modelica.Math.Icons.AxisCenter;
      input Real u;
      output Real y;
      external "builtin" y = exp(u);
    end exp;
  end Math;

  package Utilities "Library of utility functions dedicated to scripting (operating on files, streams, strings, system)"
    extends Modelica.Icons.UtilitiesPackage;

    package Streams "Read from files and write to files"
      extends Modelica.Icons.FunctionsPackage;

      function error "Print error message and cancel all actions - in case of an unrecoverable error"
        extends Modelica.Icons.Function;
        input String string "String to be printed to error message window";
        external "C" ModelicaError(string) annotation(Library = "ModelicaExternalC");
      end error;
    end Streams;
  end Utilities;

  package Constants "Library of mathematical constants and constants of nature (e.g., pi, eps, R, sigma)"
    import SI = Modelica.SIunits;
    import NonSI = Modelica.SIunits.Conversions.NonSIunits;
    extends Modelica.Icons.Package;
    final constant Real pi = 2 * Modelica.Math.asin(1.0);
    final constant Real eps = ModelicaServices.Machine.eps "Biggest number such that 1.0 + eps = 1.0";
    final constant Real small = ModelicaServices.Machine.small "Smallest number such that small and -small are representable on the machine";
    final constant SI.Velocity c = 299792458 "Speed of light in vacuum";
    final constant SI.FaradayConstant F = 9.648533289e4 "Faraday constant, C/mol (previous value: 9.64853399e4)";
    final constant Real N_A(final unit = "1/mol") = 6.022140857e23 "Avogadro constant (previous value: 6.0221415e23)";
    final constant Real mue_0(final unit = "N/A2") = 4 * pi * 1.e-7 "Magnetic constant";
  end Constants;

  package Icons "Library of icons"
    extends Icons.Package;

    partial package ExamplesPackage "Icon for packages containing runnable examples"
      extends Modelica.Icons.Package;
    end ExamplesPackage;

    partial model Example "Icon for runnable examples" end Example;

    partial package Package "Icon for standard packages" end Package;

    partial package InterfacesPackage "Icon for packages containing interfaces"
      extends Modelica.Icons.Package;
    end InterfacesPackage;

    partial package SourcesPackage "Icon for packages containing sources"
      extends Modelica.Icons.Package;
    end SourcesPackage;

    partial package SensorsPackage "Icon for packages containing sensors"
      extends Modelica.Icons.Package;
    end SensorsPackage;

    partial package UtilitiesPackage "Icon for utility packages"
      extends Modelica.Icons.Package;
    end UtilitiesPackage;

    partial package TypesPackage "Icon for packages containing type definitions"
      extends Modelica.Icons.Package;
    end TypesPackage;

    partial package FunctionsPackage "Icon for packages containing functions"
      extends Modelica.Icons.Package;
    end FunctionsPackage;

    partial package IconsPackage "Icon for packages containing icons"
      extends Modelica.Icons.Package;
    end IconsPackage;

    partial class RotationalSensor "Icon representing a round measurement device" end RotationalSensor;

    partial function Function "Icon for functions" end Function;
  end Icons;

  package SIunits "Library of type and unit definitions based on SI units according to ISO 31-1992"
    extends Modelica.Icons.Package;

    package Conversions "Conversion functions to/from non SI units and type definitions of non SI units"
      extends Modelica.Icons.Package;

      package NonSIunits "Type definitions of non SI units"
        extends Modelica.Icons.Package;
        type Temperature_degC = Real(final quantity = "ThermodynamicTemperature", final unit = "degC") "Absolute temperature in degree Celsius (for relative temperature use SIunits.TemperatureDifference)" annotation(absoluteValue = true);
      end NonSIunits;
    end Conversions;

    type Angle = Real(final quantity = "Angle", final unit = "rad", displayUnit = "deg");
    type Time = Real(final quantity = "Time", final unit = "s");
    type AngularVelocity = Real(final quantity = "AngularVelocity", final unit = "rad/s");
    type AngularAcceleration = Real(final quantity = "AngularAcceleration", final unit = "rad/s2");
    type Velocity = Real(final quantity = "Velocity", final unit = "m/s");
    type Acceleration = Real(final quantity = "Acceleration", final unit = "m/s2");
    type MomentOfInertia = Real(final quantity = "MomentOfInertia", final unit = "kg.m2");
    type Inertia = MomentOfInertia;
    type Torque = Real(final quantity = "Torque", final unit = "N.m");
    type RotationalSpringConstant = Real(final quantity = "RotationalSpringConstant", final unit = "N.m/rad");
    type RotationalDampingConstant = Real(final quantity = "RotationalDampingConstant", final unit = "N.m.s/rad");
    type Power = Real(final quantity = "Power", final unit = "W");
    type ThermodynamicTemperature = Real(final quantity = "ThermodynamicTemperature", final unit = "K", min = 0.0, start = 288.15, nominal = 300, displayUnit = "degC") "Absolute temperature (use type TemperatureDifference for relative temperatures)" annotation(absoluteValue = true);
    type Temperature = ThermodynamicTemperature;
    type HeatFlowRate = Real(final quantity = "Power", final unit = "W");
    type ElectricCharge = Real(final quantity = "ElectricCharge", final unit = "C");
    type FaradayConstant = Real(final quantity = "FaradayConstant", final unit = "C/mol");
  end SIunits;
  annotation(version = "3.2.3", versionBuild = 4, versionDate = "2019-01-23", dateModified = "2020-06-04 11:00:00Z");
end Modelica;

model PID_Controller_total  "Demonstrates the usage of a Continuous.LimPID controller"
  extends Modelica.Blocks.Examples.PID_Controller;
 annotation(experiment(StopTime = 4));
end PID_Controller_total;
