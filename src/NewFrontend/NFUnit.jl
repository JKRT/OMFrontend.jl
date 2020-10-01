  module NFUnit

    using MetaModelica
    using ExportAll
    #= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

    @UniontypeDecl Unit
    @UniontypeDecl Token

         #= /*
         * This file is part of OpenModelica.
         *
         * Copyright (c) 1998-2014, Open Source Modelica Consortium (OSMC),
         * c/o Linköpings universitet, Department of Computer and Information Science,
         * SE-58183 Linköping, Sweden.
         *
         * All rights reserved.
         *
         * THIS PROGRAM IS PROVIDED UNDER THE TERMS OF GPL VERSION 3 LICENSE OR
         * THIS OSMC PUBLIC LICENSE (OSMC-PL) VERSION 1.2.
         * ANY USE, REPRODUCTION OR DISTRIBUTION OF THIS PROGRAM CONSTITUTES
         * RECIPIENT'S ACCEPTANCE OF THE OSMC PUBLIC LICENSE OR THE GPL VERSION 3,
         * ACCORDING TO RECIPIENTS CHOICE.
         *
         * The OpenModelica software and the Open Source Modelica
         * Consortium (OSMC) Public License (OSMC-PL) are obtained
         * from OSMC, either from the above address,
         * from the URLs: http:www.ida.liu.se/projects/OpenModelica or
         * http:www.openmodelica.org, and in the OpenModelica distribution.
         * GNU version 3 is obtained from: http:www.gnu.org/copyleft/gpl.html.
         *
         * This program is distributed WITHOUT ANY WARRANTY; without
         * even the implied warranty of  MERCHANTABILITY or FITNESS
         * FOR A PARTICULAR PURPOSE, EXCEPT AS EXPRESSLY SET FORTH
         * IN THE BY RECIPIENT SELECTED SUBSIDIARY LICENSE CONDITIONS OF OSMC-PL.
         *
         * See the full OSMC Public License conditions for more details.
         *
         */ =#

        import ..ComponentRef
        import ..Debug
        import ..Error
        import ..Flags
        import ..NFHashTableStringToUnit; HashTableStringToUnit=NFHashTableStringToUnit
        import ..NFHashTableUnitToString; HashTableUnitToString=NFHashTableUnitToString
        import ..Util
         @Uniontype Unit begin
              @Record UNIT begin
                      factor #= prefix =#::AbstractFloat
                      mol #= exponent =#::Integer
                      cd #= exponent =#::Integer
                      m #= exponent =#::Integer
                      s #= exponent =#::Integer
                      A #= exponent =#::Integer
                      K #= exponent =#::Integer
                      g #= exponent =#::Integer
                       #= Real K_shift;
                       =#
              end

              @Record MASTER begin
                      varList::List{ComponentRef}
              end

              @Record UNKNOWN begin
                      unit::String
              end
         end

         @Uniontype Token begin
              @Record T_NUMBER begin
                      number::Integer
              end

              @Record T_UNIT begin
                      unit::String
              end

              @Record T_MUL begin
              end

              @Record T_DIV begin
              end

              @Record T_LPAREN begin
              end

              @Record T_RPAREN begin
              end
         end

        const UPDATECREF = STRING("jhagemann", EMPTY())::ComponentRef

        const LU_COMPLEXUNITS = list(("mol", UNIT(1e0, 1, 0, 0, 0, 0, 0, 0)), ("cd", UNIT(1e0, 0, 1, 0, 0, 0, 0, 0)), ("m", UNIT(1e0, 0, 0, 1, 0, 0, 0, 0)), ("s", UNIT(1e0, 0, 0, 0, 1, 0, 0, 0)), ("A", UNIT(1e0, 0, 0, 0, 0, 1, 0, 0)), ("K", UNIT(1e0, 0, 0, 0, 0, 0, 1, 0)), ("g", UNIT(1e0, 0, 0, 0, 0, 0, 0, 1)), ("V", UNIT(1e3, 0, 0, 2, -3, -1, 0, 1)), ("W", UNIT(1e3, 0, 0, 2, -3, 0, 0, 1)), ("Hz", UNIT(1e0, 0, 0, 0, -1, 0, 0, 0)), ("Ohm", UNIT(1e3, 0, 0, 2, -3, -2, 0, 1)), ("F", UNIT(1e-3, 0, 0, -2, 4, 2, 0, -1)), ("H", UNIT(1e3, 0, 0, 2, -2, -2, 0, 1)), ("C", UNIT(1e0, 0, 0, 0, 1, 1, 0, 0)), ("T", UNIT(1e3, 0, 0, 0, -2, -1, 0, 1)), ("S", UNIT(1e-3, 0, 0, -2, 3, 2, 0, -1)), ("Wb", UNIT(1e3, 0, 0, 2, -2, -1, 0, 1)), ("N", UNIT(1e3, 0, 0, 1, -2, 0, 0, 1)), ("Pa", UNIT(1e3, 0, 0, -1, -2, 0, 0, 1)), ("bar", UNIT(1e8, 0, 0, -1, -2, 0, 0, 1)), ("J", UNIT(1e3, 0, 0, 2, -2, 0, 0, 1)), ("min", UNIT(6e1, 0, 0, 0, 1, 0, 0, 0)), ("h", UNIT(3.6e3, 0, 0, 0, 1, 0, 0, 0)), ("d", UNIT(8.64e4, 0, 0, 0, 1, 0, 0, 0)), ("l", UNIT(1e-3, 0, 0, 3, 0, 0, 0, 0)), ("kg", UNIT(1e3, 0, 0, 0, 0, 0, 0, 1)), ("kat", UNIT(1e0, 1, 0, 0, -1, 0, 0, 0)), ("1", UNIT(1e0, 0, 0, 0, 0, 0, 0, 0)), ("rad", UNIT(1e0, 0, 0, 0, 0, 0, 0, 0)), ("degC", UNIT(1e0, 0, 0, 0, 0, 0, 1, 0)), ("degF", UNIT(0.55555555555555555555555555555555555555, 0, 0, 0, 0, 0, 1, 0)))::List
         #= /*                   fac,mol,cd, m, s, A, K, g*/ =#
         #= Mol
         =#
         #= Candela
         =#
         #= Meter
         =#
         #= Sekunde
         =#
         #= Ampere
         =#
         #= Kelvin
         =#
         #= Gramm
         =#
         #= Volt
         =#
         #= Watt
         =#
         #= (\"VA\",         UNIT(1e3, 0, 0, 2,-3, 0, 0, 1)), Voltampere=Watt
         =#
         #= (\"var\",        UNIT(1e3, 0, 0, 2,-3, 0, 0, 1)), Var=Watt
         =#
         #= Hertz
         =#
         #= Ohm
         =#
         #= Farad
         =#
         #= Henry
         =#
         #= Coulomb
         =#
         #= Tesla
         =#
         #= Siemens
         =#
         #= Weber
         =#
         #= (\"lm\",         UNIT(1e0, 0, 1, 0, 0, 0, 0, 0)), Lumen=Candela
         =#
         #= (\"lx\",         UNIT(1e0, 0, 1,-2, 0, 0, 0, 0)), Lux=lm/m^2
         =#
         #= Newton
         =#
         #= Pascal; displayUnit =\"bar\"
         =#
         #= bar = 100kPa
         =#
         #= Joule=N*m
         =#
         #= Minute
         =#
         #= Stunde
         =#
         #= Tag
         =#
         #= Liter
         =#
         #= Kilogramm
         =#
         #= (\"Bq\",         UNIT(1e0, 0, 0, 0,-1, 0, 0, 0)), Becquerel = Hertz
         =#
         #= (\"Gy\",         UNIT(1e0, 0, 0, 2,-2, 0, 0, 1)), Gray
         =#
         #= (\"Sv\",         UNIT(1e0, 0, 0, 2,-2, 0, 0, 1)), Sievert=Gray
         =#
         #= (\"eV\",         UNIT(1.60218e-16, 0, 0, 2,-2, 0, 0, 1)), Elektronenvolt    1, 602...*10^-19 kg*m^2/s^2
         =#
         #= (\"R\",          UNIT(2.58e-7, 0, 0, 0, 1, 1, 0,-1)), Röntgen    2, 58*10^-4 C/kg
         =#
         #= Katal
         =#
         #= 1
         =#
         #= rad; displayUnit =\"deg\"
         =#
         #= (\"B\",          UNIT(1e-2, 0, 0, 0, 0, 0, 0, 0)), Bel (dezibel dB)
         =#
         #= (\"phon\",       UNIT(1e0, 0, 0, 0, 0, 0, 0, 0)), Phon
         =#
         #= (\"sone\",       UNIT(1e0, 0, 0, 0, 0, 0, 0, 0)), Sone
         =#
         #= (\"sr\",         UNIT(1e0, 0, 0, 0, 0, 0, 0, 0)), Steradiant=m^2/m^2
         =#
         #= °Celsius
         =#
         #= °Fahrenheit
         =#
         #= (\"degF\",       UNIT(5.0 / 9.0, 0, 0, 0, 0, 0, 1, 0, 459.67)), °Fahrenheit
         =#
         #= (\"degC\",       UNIT(1e0, 0, 0, 0, 0, 0, 1, 0, 273.15))};°Celsius
         =#
         #= /*                 fac, mol, cd, m, s, A, K, g*/ =#

        function getKnownUnits() ::HashTableStringToUnit.HashTable
              local outKnownUnits::HashTableStringToUnit.HashTable

              @assign outKnownUnits = HashTableStringToUnit.emptyHashTableSized(Util.nextPrime(4 * listLength(LU_COMPLEXUNITS)))
              for unit in LU_COMPLEXUNITS
                @assign outKnownUnits = BaseHashTable.add(unit, outKnownUnits)
              end
          outKnownUnits
        end

        function getKnownUnitsInverse() ::HashTableUnitToString.HashTable
              local outKnownUnitsInverse::HashTableUnitToString.HashTable

              local s::String
              local ut::Unit

              @assign outKnownUnitsInverse = HashTableUnitToString.emptyHashTableSized(Util.nextPrime(4 * listLength(LU_COMPLEXUNITS)))
              for unit in LU_COMPLEXUNITS
                @assign (s, ut) = unit
                if ! BaseHashTable.hasKey(ut, outKnownUnitsInverse)
                  @assign outKnownUnitsInverse = BaseHashTable.add((ut, s), outKnownUnitsInverse)
                end
              end
          outKnownUnitsInverse
        end

        function isUnit(inUnit::Unit) ::Bool
              local b::Bool

              @assign b = begin
                @match inUnit begin
                  UNIT(__)  => begin
                    true
                  end

                  _  => begin
                      false
                  end
                end
              end
          b
        end

        function isMaster(unit::Unit) ::Bool
              local res::Bool

              @assign res = begin
                @match unit begin
                  MASTER(__)  => begin
                    true
                  end

                  _  => begin
                      false
                  end
                end
              end
          res
        end

        function hashUnitMod(inKey::Unit, inMod::Integer) ::Integer
              local outHash::Integer

              local str::String

              @assign str = unit2string(inKey)
              @assign outHash = stringHashDjb2Mod(str, inMod)
          outHash
        end

        function unitEqual(inKey::Unit, inKey2::Unit) ::Bool
              local res::Bool

              @assign res = begin
                  local factor1::AbstractFloat
                  local factor2::AbstractFloat
                  local r::AbstractFloat
                  local i1::Integer
                  local i2::Integer
                  local i3::Integer
                  local i4::Integer
                  local i5::Integer
                  local i6::Integer
                  local i7::Integer
                  local j1::Integer
                  local j2::Integer
                  local j3::Integer
                  local j4::Integer
                  local j5::Integer
                  local j6::Integer
                  local j7::Integer
                  local s::String
                  local s2::String
                  local lcr::List{ComponentRef}
                  local lcr2::List{ComponentRef}
                @matchcontinue (inKey, inKey2) begin
                  (UNIT(factor1, i1, i2, i3, i4, i5, i6, i7), UNIT(factor2, j1, j2, j3, j4, j5, j6, j7))  => begin
                      @match true = realEq(factor1, factor2)
                      @match true = intEq(i1, j1)
                      @match true = intEq(i2, j2)
                      @match true = intEq(i3, j3)
                      @match true = intEq(i4, j4)
                      @match true = intEq(i5, j5)
                      @match true = intEq(i6, j6)
                      @match true = intEq(i7, j7)
                    true
                  end

                  (UNIT(factor1, i1, i2, i3, i4, i5, i6, i7), UNIT(factor2, j1, j2, j3, j4, j5, j6, j7))  => begin
                      @assign r = realMax(realAbs(factor1), realAbs(factor2))
                      @match true = realLe(realDiv(realAbs(realSub(factor1, factor2)), r), 1e-3)
                      @match true = intEq(i1, j1)
                      @match true = intEq(i2, j2)
                      @match true = intEq(i3, j3)
                      @match true = intEq(i4, j4)
                      @match true = intEq(i5, j5)
                      @match true = intEq(i6, j6)
                      @match true = intEq(i7, j7)
                    true
                  end

                  (MASTER(__), MASTER(__))  => begin
                    true
                  end

                  (UNKNOWN(s), UNKNOWN(s2))  => begin
                      @match true = stringEqual(s, s2)
                    true
                  end

                  _  => begin
                      false
                  end
                end
              end
               #= equation
               =#
               #=  lcr comparison????
               =#
          res
        end

        function unit2string(inUnit::Unit) ::String
              local outString::String

              @assign outString = begin
                  local s::String
                  local str::String
                  local b::Bool
                  local crefList::List{ComponentRef}
                  local factor1::AbstractFloat
                  local i1::Integer
                  local i2::Integer
                  local i3::Integer
                  local i4::Integer
                  local i5::Integer
                  local i6::Integer
                  local i7::Integer
                @match inUnit begin
                  UNIT(factor1, i1, i2, i3, i4, i5, i6, i7)  => begin
                      @assign str = realString(factor1) + " * "
                      @assign b = false
                      @assign s = "mol^(" + intString(i1) + ")"
                      @assign s = if intEq(i1, 0)
                            ""
                          else
                            s
                          end
                      @assign b = b || intNe(i1, 0)
                      @assign str = str + s
                      @assign s = if b && intNe(i2, 0)
                            " * "
                          else
                            ""
                          end
                      @assign str = str + s
                      @assign s = "cd^(" + intString(i2) + ")"
                      @assign s = if intEq(i2, 0)
                            ""
                          else
                            s
                          end
                      @assign b = b || intNe(i2, 0)
                      @assign str = str + s
                      @assign s = if b && intNe(i3, 0)
                            " * "
                          else
                            ""
                          end
                      @assign str = str + s
                      @assign s = "m^(" + intString(i3) + ")"
                      @assign s = if intEq(i3, 0)
                            ""
                          else
                            s
                          end
                      @assign b = b || intNe(i3, 0)
                      @assign str = str + s
                      @assign s = if b && intNe(i4, 0)
                            " * "
                          else
                            ""
                          end
                      @assign str = str + s
                      @assign s = "s^(" + intString(i4) + ")"
                      @assign s = if intEq(i4, 0)
                            ""
                          else
                            s
                          end
                      @assign b = b || intNe(i4, 0)
                      @assign str = str + s
                      @assign s = if b && intNe(i5, 0)
                            " * "
                          else
                            ""
                          end
                      @assign str = str + s
                      @assign s = "A^(" + intString(i5) + ")"
                      @assign s = if intEq(i5, 0)
                            ""
                          else
                            s
                          end
                      @assign b = b || intNe(i5, 0)
                      @assign str = str + s
                      @assign s = if b && intNe(i6, 0)
                            " * "
                          else
                            ""
                          end
                      @assign str = str + s
                      @assign s = "K^(" + intString(i6) + ")"
                      @assign s = if intEq(i6, 0)
                            ""
                          else
                            s
                          end
                      @assign b = b || intNe(i6, 0)
                      @assign str = str + s
                      @assign s = if b && intNe(i7, 0)
                            " * "
                          else
                            ""
                          end
                      @assign str = str + s
                      @assign s = "g^(" + intString(i7) + ")"
                      @assign s = if intEq(i7, 0)
                            ""
                          else
                            s
                          end
                      @assign b = b || intNe(i7, 0)
                      @assign str = str + s
                      @assign s = if b
                            ""
                          else
                            "1"
                          end
                      @assign str = str + s
                    str
                  end

                  MASTER(crefList)  => begin
                      @assign str = "MASTER("
                      @assign str = str + printListCr(crefList)
                      @assign str = str + ")"
                    str
                  end

                  UNKNOWN(s)  => begin
                      @assign str = "UNKOWN(" + s + ")"
                    str
                  end
                end
              end
               #= /* , shift1 */ =#
               #= s = \"(K-\" + realString(shift1) + \")^(\" + intString(i6) + \")\";
               =#
          outString
        end

        function printListCr(inlCr::List{<:ComponentRef}) ::String
              local outS::String

              @assign outS = begin
                  local lCr::List{ComponentRef}
                  local cr::ComponentRef
                  local s::String
                @match inlCr begin
                   nil()  => begin
                    ""
                  end

                  cr <|  nil()  => begin
                      @assign s = toString(cr)
                    s
                  end

                  cr <| lCr  => begin
                      @assign s = toString(cr)
                      @assign s = s + ", " + printListCr(lCr)
                    s
                  end
                end
              end
          outS
        end

        function unitMul(inUnit1::Unit, inUnit2::Unit) ::Unit
              local outUnit::Unit
              local factor1::AbstractFloat
              local factor2::AbstractFloat
              local i1::Integer
              local i2::Integer
              local i3::Integer
              local i4::Integer
              local i5::Integer
              local i6::Integer
              local i7::Integer
              local j1::Integer
              local j2::Integer
              local j3::Integer
              local j4::Integer
              local j5::Integer
              local j6::Integer
              local j7::Integer

              @match UNIT(factor1, i1, i2, i3, i4, i5, i6, i7) = inUnit1
              @match UNIT(factor2, j1, j2, j3, j4, j5, j6, j7) = inUnit2
              @assign factor1 = factor1 * factor2
              @assign i1 = i1 + j1
              @assign i2 = i2 + j2
              @assign i3 = i3 + j3
              @assign i4 = i4 + j4
              @assign i5 = i5 + j5
              @assign i6 = i6 + j6
              @assign i7 = i7 + j7
              @assign outUnit = UNIT(factor1, i1, i2, i3, i4, i5, i6, i7)
          outUnit
        end

        function unitDiv(inUnit1::Unit, inUnit2::Unit) ::Unit
              local outUnit::Unit

              local factor1::AbstractFloat
              local factor2::AbstractFloat
              local i1::Integer
              local i2::Integer
              local i3::Integer
              local i4::Integer
              local i5::Integer
              local i6::Integer
              local i7::Integer
              local j1::Integer
              local j2::Integer
              local j3::Integer
              local j4::Integer
              local j5::Integer
              local j6::Integer
              local j7::Integer

              @match UNIT(factor1, i1, i2, i3, i4, i5, i6, i7) = inUnit1
              @match UNIT(factor2, j1, j2, j3, j4, j5, j6, j7) = inUnit2
              @assign factor1 = factor1 / factor2
              @assign i1 = i1 - j1
              @assign i2 = i2 - j2
              @assign i3 = i3 - j3
              @assign i4 = i4 - j4
              @assign i5 = i5 - j5
              @assign i6 = i6 - j6
              @assign i7 = i7 - j7
              @assign outUnit = UNIT(factor1, i1, i2, i3, i4, i5, i6, i7)
          outUnit
        end

        function unitPow(inUnit::Unit, inExp::Integer #= exponent =#) ::Unit
              local outUnit::Unit

              local factor::AbstractFloat
              local i1::Integer
              local i2::Integer
              local i3::Integer
              local i4::Integer
              local i5::Integer
              local i6::Integer
              local i7::Integer

              @match UNIT(factor, i1, i2, i3, i4, i5, i6, i7) = inUnit
              @assign factor = realPow(factor, intReal(inExp))
              @assign i1 = i1 * inExp
              @assign i2 = i2 * inExp
              @assign i3 = i3 * inExp
              @assign i4 = i4 * inExp
              @assign i5 = i5 * inExp
              @assign i6 = i6 * inExp
              @assign i7 = i7 * inExp
              @assign outUnit = UNIT(factor, i1, i2, i3, i4, i5, i6, i7)
          outUnit
        end

        function unitMulReal(inUnit::Unit, inFactor::AbstractFloat) ::Unit
              local outUnit::Unit

              @assign outUnit = begin
                  local unit::Unit
                @match inUnit begin
                  unit && UNIT(__)  => begin
                      @assign unit.factor = unit.factor * inFactor
                    unit
                  end

                  _  => begin
                      fail()
                  end
                end
              end
          outUnit
        end

        function unitRoot(inUnit::Unit, inExponent::AbstractFloat) ::Unit
              local outUnit::Unit

              local r::AbstractFloat
              local factor::AbstractFloat
              local i::Integer
              local i1::Integer
              local i2::Integer
              local i3::Integer
              local i4::Integer
              local i5::Integer
              local i6::Integer
              local i7::Integer

              @assign i = realInt(inExponent)
              @assign r = realDiv(1.0, inExponent)
              @match UNIT(factor, i1, i2, i3, i4, i5, i6, i7) = inUnit
              @assign factor = realPow(factor, r)
              @assign r = realDiv(intReal(i1), inExponent)
              @assign i1 = intDiv(i1, i)
              @match true = realEq(r, intReal(i1))
              @assign r = realDiv(intReal(i2), inExponent)
              @assign i2 = intDiv(i2, i)
              @match true = realEq(r, intReal(i2))
              @assign r = realDiv(intReal(i3), inExponent)
              @assign i3 = intDiv(i3, i)
              @match true = realEq(r, intReal(i3))
              @assign r = realDiv(intReal(i4), inExponent)
              @assign i4 = intDiv(i4, i)
              @match true = realEq(r, intReal(i4))
              @assign r = realDiv(intReal(i5), inExponent)
              @assign i5 = intDiv(i5, i)
              @match true = realEq(r, intReal(i5))
              @assign r = realDiv(intReal(i6), inExponent)
              @assign i6 = intDiv(i6, i)
              @match true = realEq(r, intReal(i6))
              @assign r = realDiv(intReal(i7), inExponent)
              @assign i7 = intDiv(i7, i)
              @match true = realEq(r, intReal(i7))
              @assign outUnit = UNIT(factor, i1, i2, i3, i4, i5, i6, i7)
          outUnit
        end

        """ #= Unit to Modelica unit string =#"""
        function unitString(inUnit::Unit, inHtU2S::HashTableUnitToString.HashTable = getKnownUnitsInverse()) ::String
              local outString::String

              @assign outString = begin
                  local s::String
                  local s1::String
                  local s2::String
                  local s3::String
                  local s4::String
                  local s5::String
                  local s6::String
                  local s7::String
                  local sExponent::String
                  local b::Bool
                  local unit::Unit
                @match inUnit begin
                  _ where (BaseHashTable.hasKey(inUnit, inHtU2S))  => begin
                      @assign s = BaseHashTable.get(inUnit, inHtU2S)
                    s
                  end

                  unit && UNIT(__)  => begin
                      @assign s = prefix2String(unit.factor)
                      @assign s = if realEq(unit.factor, 1.0)
                            ""
                          else
                            s
                          end
                      @assign b = false
                      @assign sExponent = if intEq(unit.mol, 1)
                            ""
                          else
                            intString(unit.mol)
                          end
                      @assign s1 = "mol" + sExponent
                      @assign s1 = if intEq(unit.mol, 0)
                            ""
                          else
                            s1
                          end
                      @assign b = b || intNe(unit.mol, 0)
                      @assign s2 = if b && intNe(unit.cd, 0)
                            "."
                          else
                            ""
                          end
                      @assign sExponent = if intEq(unit.cd, 1)
                            ""
                          else
                            intString(unit.cd)
                          end
                      @assign s2 = s2 + "cd" + sExponent
                      @assign s2 = if intEq(unit.cd, 0)
                            ""
                          else
                            s2
                          end
                      @assign b = b || intNe(unit.cd, 0)
                      @assign s3 = if b && intNe(unit.m, 0)
                            "."
                          else
                            ""
                          end
                      @assign sExponent = if intEq(unit.m, 1)
                            ""
                          else
                            intString(unit.m)
                          end
                      @assign s3 = s3 + "m" + sExponent
                      @assign s3 = if intEq(unit.m, 0)
                            ""
                          else
                            s3
                          end
                      @assign b = b || intNe(unit.m, 0)
                      @assign s4 = if b && intNe(unit.s, 0)
                            "."
                          else
                            ""
                          end
                      @assign sExponent = if intEq(unit.s, 1)
                            ""
                          else
                            intString(unit.s)
                          end
                      @assign s4 = s4 + "s" + sExponent
                      @assign s4 = if intEq(unit.s, 0)
                            ""
                          else
                            s4
                          end
                      @assign b = b || intNe(unit.s, 0)
                      @assign s5 = if b && intNe(unit.A, 0)
                            "."
                          else
                            ""
                          end
                      @assign sExponent = if intEq(unit.A, 1)
                            ""
                          else
                            intString(unit.A)
                          end
                      @assign s5 = s5 + "A" + sExponent
                      @assign s5 = if intEq(unit.A, 0)
                            ""
                          else
                            s5
                          end
                      @assign b = b || intNe(unit.A, 0)
                      @assign s6 = if b && intNe(unit.K, 0)
                            "."
                          else
                            ""
                          end
                      @assign sExponent = if intEq(unit.K, 1)
                            ""
                          else
                            intString(unit.K)
                          end
                      @assign s6 = s6 + "K" + sExponent
                      @assign s6 = if intEq(unit.K, 0)
                            ""
                          else
                            s6
                          end
                      @assign b = b || intNe(unit.K, 0)
                      @assign s7 = if b && intNe(unit.g, 0)
                            "."
                          else
                            ""
                          end
                      @assign sExponent = if intEq(unit.g, 1)
                            ""
                          else
                            intString(unit.g)
                          end
                      @assign s7 = s7 + "g" + sExponent
                      @assign s7 = if intEq(unit.g, 0)
                            ""
                          else
                            s7
                          end
                      @assign b = b || intNe(unit.g, 0)
                      @assign s = if b
                            s + s1 + s2 + s3 + s4 + s5 + s6 + s7
                          else
                            "1"
                          end
                    s
                  end

                  _  => begin
                        Error.addCompilerWarning("function Unit.unitString failed for \\" + unit2string(inUnit) + "\\.")
                      fail()
                  end
                end
              end
          outString
        end

        function prefix2String(inReal::AbstractFloat) ::String
              local outPrefix::String

              @assign outPrefix = begin
                @match inReal begin
                  1e-24  => begin
                    "y"
                  end

                  1e-21  => begin
                    "z"
                  end

                  1e-18  => begin
                    "a"
                  end

                  1e-15  => begin
                    "f"
                  end

                  1e-12  => begin
                    "p"
                  end

                  1e-6  => begin
                    "u"
                  end

                  1e-3  => begin
                    "m"
                  end

                  1e-2  => begin
                    "c"
                  end

                  1e-1  => begin
                    "d"
                  end

                  1e1  => begin
                    "da"
                  end

                  1e2  => begin
                    "h"
                  end

                  1e3  => begin
                    "k"
                  end

                  1e6  => begin
                    "M"
                  end

                  1e9  => begin
                    "G"
                  end

                  1e12  => begin
                    "T"
                  end

                  1e15  => begin
                    "P"
                  end

                  1e18  => begin
                    "E"
                  end

                  1e21  => begin
                    "Z"
                  end

                  1e24  => begin
                    "Y"
                  end

                  _  => begin
                      realString(inReal)
                  end
                end
              end
          outPrefix
        end

        """ #= author: lochel
          The second argument is optional. =#"""
        function parseUnitString(inUnitString::String, inKnownUnits::HashTableStringToUnit.HashTable = getKnownUnits()) ::Unit
              local outUnit::Unit

              local charList::List{String}
              local tokenList::List{Token}

              @assign charList = stringListStringChar(inUnitString)
              if listEmpty(charList)
                fail()
              end
              @assign tokenList = lexer(charList)
              @assign outUnit = parser3(list(true, true), tokenList, UNIT(1e0, 0, 0, 0, 0, 0, 0, 0), inKnownUnits)
              if ! isUnit(outUnit)
                if Flags.isSet(Flags.FAILTRACE)
                  Debug.traceln(getInstanceName() + ": failed to parse unit string " + inUnitString)
                end
                fail()
              end
          outUnit
        end

        function parser3(inMul::List{<:Bool} #= true=Mul, false=Div, initial call with true =#, inTokenList::List{<:Token} #= Tokenliste =#, inUnit::Unit #= initial call with UNIT(1e0, 0, 0, 0, 0, 0, 0, 0) =#, inHtS2U::HashTableStringToUnit.HashTable) ::Unit
              local outUnit::Unit

              @assign outUnit = begin
                  local s::String
                  local s1::String
                  local s2::String
                  local unit::String
                  local tokens::List{Token}
                  local ut::Unit
                  local exponent::Integer
                  local bMul::Bool
                  local b::Bool
                  local bRest::List{Bool}
                   #=  \"\"
                   =#
                @matchcontinue (inMul, inTokenList, inUnit, inHtS2U) begin
                  (true <|  nil(),  nil(), _, _)  => begin
                    inUnit
                  end

                  (bMul <| bRest, T_NUMBER(number = 1) <| tokens, _, _)  => begin
                      @assign ut = UNIT(1e0, 0, 0, 0, 0, 0, 0, 0)
                      @assign ut = if bMul
                            unitMul(inUnit, ut)
                          else
                            unitDiv(inUnit, ut)
                          end
                      @assign ut = parser3(bRest, tokens, ut, inHtS2U)
                    ut
                  end

                  (bMul <| bRest, T_UNIT(unit = s) <| T_NUMBER(exponent) <| tokens, _, _)  => begin
                      @assign ut = unitToken2unit(s, inHtS2U)
                      @assign ut = unitPow(ut, exponent)
                      @assign ut = if bMul
                            unitMul(inUnit, ut)
                          else
                            unitDiv(inUnit, ut)
                          end
                      @assign ut = parser3(bRest, tokens, ut, inHtS2U)
                    ut
                  end

                  (bMul <| bRest, T_UNIT(unit = s) <| tokens, _, _)  => begin
                      @assign ut = unitToken2unit(s, inHtS2U)
                      @assign ut = if bMul
                            unitMul(inUnit, ut)
                          else
                            unitDiv(inUnit, ut)
                          end
                      @assign ut = parser3(bRest, tokens, ut, inHtS2U)
                    ut
                  end

                  (bMul <| _, T_MUL(__) <| T_LPAREN(__) <| tokens, _, _)  => begin
                      @assign ut = parser3(_cons(bMul, _cons(bMul, inMul)), tokens, inUnit, inHtS2U)
                    ut
                  end

                  (bMul <| _, T_DIV(__) <| T_LPAREN(__) <| tokens, _, _)  => begin
                      @assign b = ! bMul
                      @assign ut = parser3(_cons(b, _cons(b, inMul)), tokens, inUnit, inHtS2U)
                    ut
                  end

                  (_ <| bRest, T_RPAREN(__) <| tokens, _, _)  => begin
                      @assign ut = parser3(bRest, tokens, inUnit, inHtS2U)
                    ut
                  end

                  (bMul <| _, T_MUL(__) <| tokens, _, _)  => begin
                      @assign ut = parser3(_cons(bMul, inMul), tokens, inUnit, inHtS2U)
                    ut
                  end

                  (bMul <| _, T_DIV(__) <| tokens, _, _)  => begin
                      @assign b = ! bMul
                      @assign ut = parser3(_cons(b, inMul), tokens, inUnit, inHtS2U)
                    ut
                  end

                  _  => begin
                      UNKNOWN("")
                  end
                end
              end
               #=  \"1\"
               =#
               #= /* , 0e0 */ =#
               #=  \"unit^i\"
               =#
               #=  \"unit\"
               =#
               #=  \"*(\"
               =#
               #=  \"/(\"
               =#
               #=  \")\"
               =#
               #=  \"*\"
               =#
               #=  \"/\"
               =#
          outUnit
        end

        function unitToken2unit(inS::String, inHtS2U::HashTableStringToUnit.HashTable) ::Unit
              local outUnit::Unit

              @assign outUnit = begin
                  local s::String
                  local s2::String
                  local r::AbstractFloat
                  local ut::Unit
                @matchcontinue (inS, inHtS2U) begin
                  (_, _)  => begin
                      @assign ut = BaseHashTable.get(inS, inHtS2U)
                    ut
                  end

                  _  => begin
                        @assign s = stringGetStringChar(inS, 1)
                        @assign (r, s) = getPrefix(s, inS)
                        @assign ut = unitToken2unit(s, inHtS2U)
                        @assign ut = unitMulReal(ut, r)
                      ut
                  end
                end
              end
          outUnit
        end

        function getPrefix(inS::String, inS2::String) ::Tuple{AbstractFloat, String}
              local outUnit::String
              local outR::AbstractFloat

              @assign (outR, outUnit) = begin
                  local strRest::List{String}
                  local s::String
                @matchcontinue (inS, inS2) begin
                  ("y", _)  => begin
                      @match _cons(_, strRest) = stringListStringChar(inS2)
                      @assign s = stringCharListString(strRest)
                    (1e-24, s)
                  end

                  ("z", _)  => begin
                      @match _cons(_, strRest) = stringListStringChar(inS2)
                      @assign s = stringCharListString(strRest)
                    (1e-21, s)
                  end

                  ("a", _)  => begin
                      @match _cons(_, strRest) = stringListStringChar(inS2)
                      @assign s = stringCharListString(strRest)
                    (1e-18, s)
                  end

                  ("f", _)  => begin
                      @match _cons(_, strRest) = stringListStringChar(inS2)
                      @assign s = stringCharListString(strRest)
                    (1e-15, s)
                  end

                  ("p", _)  => begin
                      @match _cons(_, strRest) = stringListStringChar(inS2)
                      @assign s = stringCharListString(strRest)
                    (1e-12, s)
                  end

                  ("u", _)  => begin
                      @match _cons(_, strRest) = stringListStringChar(inS2)
                      @assign s = stringCharListString(strRest)
                    (1e-6, s)
                  end

                  ("m", _)  => begin
                      @match _cons(_, strRest) = stringListStringChar(inS2)
                      @assign s = stringCharListString(strRest)
                    (1e-3, s)
                  end

                  ("c", _)  => begin
                      @match _cons(_, strRest) = stringListStringChar(inS2)
                      @assign s = stringCharListString(strRest)
                    (1e-2, s)
                  end

                  ("d", _)  => begin
                      @assign strRest = stringListStringChar(inS2)
                      @match _cons("d", _cons("a", strRest)) = strRest
                      @assign s = stringCharListString(strRest)
                    (1e1, s)
                  end

                  ("d", _)  => begin
                      @match _cons(_, strRest) = stringListStringChar(inS2)
                      @assign s = stringCharListString(strRest)
                    (1e-1, s)
                  end

                  ("h", _)  => begin
                      @match _cons(_, strRest) = stringListStringChar(inS2)
                      @assign s = stringCharListString(strRest)
                    (1e2, s)
                  end

                  ("k", _)  => begin
                      @match _cons(_, strRest) = stringListStringChar(inS2)
                      @assign s = stringCharListString(strRest)
                    (1e3, s)
                  end

                  ("M", _)  => begin
                      @match _cons(_, strRest) = stringListStringChar(inS2)
                      @assign s = stringCharListString(strRest)
                    (1e6, s)
                  end

                  ("G", _)  => begin
                      @match _cons(_, strRest) = stringListStringChar(inS2)
                      @assign s = stringCharListString(strRest)
                    (1e9, s)
                  end

                  ("T", _)  => begin
                      @match _cons(_, strRest) = stringListStringChar(inS2)
                      @assign s = stringCharListString(strRest)
                    (1e12, s)
                  end

                  ("P", _)  => begin
                      @match _cons(_, strRest) = stringListStringChar(inS2)
                      @assign s = stringCharListString(strRest)
                    (1e15, s)
                  end

                  ("E", _)  => begin
                      @match _cons(_, strRest) = stringListStringChar(inS2)
                      @assign s = stringCharListString(strRest)
                    (1e18, s)
                  end

                  ("Z", _)  => begin
                      @match _cons(_, strRest) = stringListStringChar(inS2)
                      @assign s = stringCharListString(strRest)
                    (1e21, s)
                  end

                  ("Y", _)  => begin
                      @match _cons(_, strRest) = stringListStringChar(inS2)
                      @assign s = stringCharListString(strRest)
                    (1e24, s)
                  end

                  _  => begin
                      fail()
                  end
                end
              end
               #= -24
               =#
               #= -21
               =#
               #= -18
               =#
               #= -15
               =#
               #= -12
               =#
               #= -6
               =#
               #= -3
               =#
               #= -2
               =#
               #= +1
               =#
               #= -1
               =#
               #= +2
               =#
               #= +3
               =#
               #= +6
               =#
               #= +9
               =#
               #= +12
               =#
               #= +15
               =#
               #= +18
               =#
               #= +21
               =#
               #= +24
               =#
          (outR, outUnit)
        end

        """ #= author: lochel
          Tokenizer: charList to tokenList =#"""
        function lexer(inCharList::List{<:String}) ::List{Token}
              local outTokenList::List{Token}

              @assign outTokenList = begin
                  local charList::List{String}
                  local number::String
                  local unit::String
                  local tokenList::List{Token}
                  local i::Integer
                @matchcontinue inCharList begin
                   nil()  => begin
                    nil
                  end

                  "." <| charList  => begin
                      @assign tokenList = lexer(charList)
                    _cons(T_MUL(), tokenList)
                  end

                  "(" <| charList  => begin
                      @assign tokenList = lexer(charList)
                    _cons(T_LPAREN(), tokenList)
                  end

                  ")" <| charList  => begin
                      @assign tokenList = lexer(charList)
                    _cons(T_RPAREN(), tokenList)
                  end

                  "/" <| charList  => begin
                      @assign tokenList = lexer(charList)
                    _cons(T_DIV(), tokenList)
                  end

                  "+" <| charList  => begin
                      @assign (charList, number) = popNumber(charList)
                      @match false = number == ""
                      @assign tokenList = lexer(charList)
                      @assign i = stringInt(number)
                    _cons(T_NUMBER(i), tokenList)
                  end

                  "-" <| charList  => begin
                      @assign (charList, number) = popNumber(charList)
                      @match false = number == ""
                      @assign tokenList = lexer(charList)
                      @assign i = -stringInt(number)
                    _cons(T_NUMBER(i), tokenList)
                  end

                  charList  => begin
                      @assign (charList, number) = popNumber(charList)
                      @match false = number == ""
                      @assign tokenList = lexer(charList)
                      @assign i = stringInt(number)
                    _cons(T_NUMBER(i), tokenList)
                  end

                  charList  => begin
                      @assign (charList, unit) = popUnit(charList)
                      @match false = unit == ""
                      @assign tokenList = lexer(charList)
                    _cons(T_UNIT(unit), tokenList)
                  end

                  _  => begin
                        Error.addInternalError("function lexer failed", sourceInfo())
                      fail()
                  end
                end
              end
          outTokenList
        end

        function popUnit(inCharList::List{<:String}) ::Tuple{List{String}, String}
              local outUnit::String
              local outCharList::List{String}

              @assign (outCharList, outUnit) = begin
                  local s1::String
                  local s2::String
                  local strRest::List{String}
                @matchcontinue inCharList begin
                   nil()  => begin
                    (nil, "")
                  end

                  s1 <| strRest  => begin
                      @match true = stringCompare(s1, "a") >= 0 && stringCompare(s1, "z") <= 0
                      @assign (strRest, s2) = popUnit(strRest)
                    (strRest, s1 + s2)
                  end

                  s1 <| strRest  => begin
                      @match true = stringCompare(s1, "A") >= 0 && stringCompare(s1, "Z") <= 0
                      @assign (strRest, s2) = popUnit(strRest)
                    (strRest, s1 + s2)
                  end

                  _  => begin
                      (inCharList, "")
                  end
                end
              end
          (outCharList, outUnit)
        end

        function popNumber(inCharList::List{<:String}) ::Tuple{List{String}, String}
              local outNumber::String
              local outCharList::List{String}

              @assign (outCharList, outNumber) = begin
                  local s1::String
                  local s2::String
                  local strRest::List{String}
                  local i::Integer
                @matchcontinue inCharList begin
                   nil()  => begin
                    (nil, "")
                  end

                  s1 <| strRest  => begin
                      @assign i = stringInt(s1)
                      @match true = intString(i) == s1
                      @assign (strRest, s2) = popNumber(strRest)
                    (strRest, s1 + s2)
                  end

                  _  => begin
                      (inCharList, "")
                  end
                end
              end
          (outCharList, outNumber)
        end

    @exportAll()
  end
