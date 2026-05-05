package TestConstantTableLookup
  "Constant array indexed by runtime CREFs must stay symbolic in flat output."

  model RealConstTableLookup
    constant Real Table[3, 3] = [10, 11, 12;
                                  20, 21, 22;
                                  30, 31, 32];
    Integer in1;
    Integer in2;
    Real auxiliary;
    Real t(start = 0.0);
  equation
    in1 = 1;
    in2 = 2;
    auxiliary = Table[in1, in2];
    der(t) = 1.0;
  end RealConstTableLookup;

  model EnumConstTableLookup
    "Mirrors Modelica.Electrical.Digital.Basic.And — enum table indexed by runtime enums."
    type Logic = enumeration('U', 'X', '0', '1');
    constant Logic AndTable[Logic, Logic] = [
      Logic.'U', Logic.'U', Logic.'0', Logic.'U';
      Logic.'U', Logic.'X', Logic.'0', Logic.'X';
      Logic.'0', Logic.'0', Logic.'0', Logic.'0';
      Logic.'U', Logic.'X', Logic.'0', Logic.'1'
    ];
    Logic in1;
    Logic in2;
    Logic auxiliary;
    Real t(start = 0.0);
  equation
    in1 = Logic.'1';
    in2 = Logic.'1';
    auxiliary = AndTable[in1, in2];
    der(t) = 1.0;
  end EnumConstTableLookup;
end TestConstantTableLookup;
