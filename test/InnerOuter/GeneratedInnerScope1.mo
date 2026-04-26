package StateGraphLib
  "Library package containing an outer component with a relative type path"

  package Interfaces
    model CompositeStepState
      Real x = 1.0;
    end CompositeStepState;
  end Interfaces;

  partial model PartialStep
    "The outer declaration uses a relative path: Interfaces.CompositeStepState
     This is relative to StateGraphLib, not to the top-level model."
    outer Interfaces.CompositeStepState stateGraphRoot;
  end PartialStep;

  model Step1
    extends PartialStep;
    Real y = stateGraphRoot.x + 1.0;
  end Step1;
end StateGraphLib;

model GeneratedInnerScope1
  "This model is OUTSIDE StateGraphLib.
   When no inner stateGraphRoot is declared, the compiler auto-generates one.
   The generated inner has type Interfaces.CompositeStepState (relative to
   StateGraphLib). After reparenting to GeneratedInnerScope1, the lookup
   for Interfaces.CompositeStepState fails because Interfaces is not visible
   from GeneratedInnerScope1's scope."
  StateGraphLib.Step1 step;
end GeneratedInnerScope1;
