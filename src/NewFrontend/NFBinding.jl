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

const VariabilityType = Int

@UniontypeDecl Binding

@Uniontype Binding begin
  @Record INVALID_BINDING begin
    binding::Binding
    errors::List
  end

  @Record CEVAL_BINDING begin
    bindingExp::Expression
  end

  @Record FLAT_BINDING begin
    bindingExp::Expression
    variability::VariabilityType
  end

  @Record TYPED_BINDING begin
    bindingExp::Expression
    bindingType
    variability::VariabilityType
    eachType::Int
    evaluated::Bool
    isFlattened::Bool
    info::SourceInfo
  end

  @Record UNTYPED_BINDING begin
    bindingExp::Expression
    isProcessing::Bool
    scope::InstNode
    isEach::Bool
    info::SourceInfo
  end

  @Record RAW_BINDING begin
    bindingExp::Absyn.Exp
    scope::InstNode
    parents::List{InstNode}
    isEach::Bool
    info::SourceInfo
  end

  @Record UNBOUND begin
    parents::List{InstNode}
    isEach::Bool
    info::SourceInfo
  end
end

const EMPTY_BINDING = UNBOUND(nil, false, AbsynUtil.dummyInfo)::Binding

EachType = (() -> begin #= Enumeration =#
  NOT_EACH = 1
  EACH = 2
  REPEAT = 3
  () -> (NOT_EACH; EACH; REPEAT)
end)()
const EachTypeType = Int
