#=
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
=#

const VariabilityType = Int

@UniontypeDecl Binding

abstract type Binding end

mutable struct INVALID_BINDING <: Binding
  binding::Binding
  errors::List
end

mutable struct CEVAL_BINDING <: Binding
  bindingExp::Expression
end

mutable struct FLAT_BINDING <: Binding
  bindingExp::Expression
  variability::VariabilityType
end

mutable struct TYPED_BINDING <: Binding
  bindingExp::Expression
  bindingType
  variability::VariabilityType
  eachType::Int
  evaluated::Bool
  isFlattened::Bool
  info::SourceInfo
end

mutable struct UNTYPED_BINDING <: Binding
  bindingExp::Expression
  isProcessing::Bool
  scope::InstNode
  isEach::Bool
  info::SourceInfo
end

mutable struct RAW_BINDING <: Binding
  bindingExp::Absyn.Exp
  scope::InstNode
  parents::List{InstNode}
  isEach::Bool
  info::SourceInfo
end

mutable struct UNBOUND <: Binding
  parents::List{InstNode}
  isEach::Bool
  info::SourceInfo
end

#const EMPTY_BINDING() = UNBOUND(nil, false, AbsynUtil.dummyInfo)::Binding
function EMPTY_BINDING()
  return UNBOUND(nil, false, AbsynUtil.dummyInfo)
end

struct EachTypeStruct{T <: Int}
  NOT_EACH::T
  EACH::T
  REPEAT::T
end

const EachTypeType = Int
const EachType::EachTypeStruct = EachTypeStruct(1,2,3)
