// Minimal test for inner/outer with protected outer declaration.
// The outer is in a protected section. When the compiler auto-generates
// the inner, it must still be accessible through the inner/outer mechanism
// even though the declaration is protected.
// Also tests resolveInnerCref which must not shadow the node() function.
package InnerOuterLib
  model SharedState
    Real value = 42.0;
  end SharedState;

  partial model BaseStep
  protected
    outer SharedState sharedRoot;
  end BaseStep;

  model ConcreteStep
    extends BaseStep;
    Real y = sharedRoot.value + 1.0;
  end ConcreteStep;
end InnerOuterLib;

model ProtectedOuterTest1
  InnerOuterLib.ConcreteStep step;
end ProtectedOuterTest1;
