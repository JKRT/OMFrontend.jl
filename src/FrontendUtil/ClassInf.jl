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
module ClassInf
    using MetaModelica
    #= ExportAll is not good practice but it makes it so that we do not have to write export after each function :( =#
    using ExportAll
    #= Necessary to write declarations for your uniontypes until Julia adds support for mutually recursive types =#

    @UniontypeDecl SMNode
    @UniontypeDecl Event

        import SCode

        import Absyn

          #= - Machine states, the string contains the classname. =#
         @Uniontype SMNode begin
              @Record UNKNOWN begin

                       path::Absyn.Path
              end

              @Record OPTIMIZATION begin

                       path::Absyn.Path
              end

              @Record MODEL begin

                       path::Absyn.Path
              end

              @Record RECORD begin

                       path::Absyn.Path
              end

              @Record BLOCK begin

                       path::Absyn.Path
              end

              @Record CONNECTOR begin

                       path::Absyn.Path
                       isExpandable::Bool
              end

              @Record TYPE begin

                       path::Absyn.Path
              end

              @Record PACKAGE begin

                       path::Absyn.Path
              end

              @Record FUNCTION begin

                       path::Absyn.Path
                       isImpure::Bool
              end

              @Record ENUMERATION begin

                       path::Absyn.Path
              end

              @Record HAS_RESTRICTIONS begin

                       path::Absyn.Path
                       hasEquations::Bool
                       hasAlgorithms::Bool
                       hasConstraints::Bool
              end

              @Record TYPE_INTEGER begin

                       path::Absyn.Path
              end

              @Record TYPE_REAL begin

                       path::Absyn.Path
              end

              @Record TYPE_STRING begin

                       path::Absyn.Path
              end

              @Record TYPE_BOOL begin

                       path::Absyn.Path
              end

               #=  BTH
               =#

              @Record TYPE_CLOCK begin

                       path::Absyn.Path
              end

              @Record TYPE_ENUM begin

                       path::Absyn.Path
              end

              @Record EXTERNAL_OBJ begin

                       path::Absyn.Path
              end

               #= /* MetaModelica extension */ =#

              @Record META_TUPLE begin

                       path::Absyn.Path
              end

              @Record META_LIST begin

                       path::Absyn.Path
              end

              @Record META_OPTION begin

                       path::Absyn.Path
              end

              @Record META_RECORD begin

                       path::Absyn.Path
              end

              @Record META_UNIONTYPE begin

                       path::Absyn.Path
                       typeVars::List{String}
              end

              @Record META_ARRAY begin

                       path::Absyn.Path
              end

              @Record META_POLYMORPHIC begin

                       path::Absyn.Path
              end

               #= /*---------------------*/ =#
         end

          #= - Events =#
         @Uniontype Event begin
              @Record FOUND_EQUATION begin

              end

              @Record FOUND_ALGORITHM begin

              end

              @Record FOUND_CONSTRAINT begin

              end

              @Record FOUND_EXT_DECL begin

              end

              @Record NEWDEF begin

              end

              @Record FOUND_COMPONENT begin

                       name #= name of the component =#::String
              end
         end
    @exportAll()
  end
