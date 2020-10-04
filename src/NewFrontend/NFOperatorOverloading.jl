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
import Absyn
import ..AbsynUtil
import ..SCodeUtil
M_Type = NFType
ComponentRef = NFComponentRef
Expression = NFExpression

function instConstructor(path::Absyn.Path, recordNode::InstNode, info::SourceInfo)::InstNode
  local ctor_ref::ComponentRef
  local ctor_path::Absyn.Path
  local ctor_overloaded::Bool
  local ctor_node::InstNode
  #=  Check if the operator record has an overloaded constructor declared.
  =#
  try
    @assign ctor_ref = P_Function.lookupFunctionSimple("'constructor'", recordNode)
    @assign ctor_overloaded = true
  catch
    @assign ctor_overloaded = false
  end
  if ctor_overloaded
    @assign (_, ctor_node) = P_Function.instFunctionRef(ctor_ref, info)
    @assign ctor_path = scopePath(ctor_node, includeRoot = true)
    for f in P_Function.getCachedFuncs(ctor_node)
      checkOperatorConstructorOutput(f, lastBaseClass(recordNode), ctor_path, info)
      @assign recordNode = cacheAddFunc(recordNode, f, false)
    end
  end
  #=  If it has an overloaded constructor, instantiate it and add the
  =#
  #=  function(s) to the record node.
  =#
  #= ctor_node := ComponentRef.node(ctor_ref);
  =#
  #= ctor_node := Function.instFunction2(ctor_path, ctor_node, info);
  =#
  @assign recordNode = Record.instDefaultConstructor(path, recordNode, info)
  return recordNode
end

function instOperatorFunctions(node::InstNode, info::SourceInfo)::InstNode

  local tree::ClassTree
  local mclss::Array{InstNode}
  local path::Absyn.Path
  local allfuncs::List{M_Function} = nil
  local funcs::List{M_Function}

  checkOperatorRestrictions(node)
  @assign tree = classTree(getClass(node))
  @assign () = begin
    @match tree begin
      FLAT_TREE(classes = mclss) => begin
        for op in mclss
          P_Function.instFunctionNode(op)
          @assign funcs = P_Function.getCachedFuncs(op)
          @assign allfuncs = listAppend(funcs, allfuncs)
        end
        #= path := InstNode.scopePath(op, includeRoot = true);
        =#
        #= Function.instFunction2(path, op, info);
        =#
        for f in allfuncs
          @assign node = cacheAddFunc(node, f, false)
        end
        ()
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got non-instantiated function",
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return node
end

function checkOperatorRestrictions(operatorNode::InstNode)
  return if !SCodeUtil.isElementEncapsulated(definition(operatorNode))
    Error.addSourceMessage(
      Error.OPERATOR_NOT_ENCAPSULATED,
      list(AbsynUtil.pathString(scopePath(operatorNode, includeRoot = true))),
      info(operatorNode),
    )
    fail()
  end
end

function lookupOperatorFunctionsInType(operatorName::String, ty::M_Type)::List{M_Function}
  local functions::List{M_Function}

  local node::InstNode
  local fn_ref::ComponentRef
  local is_defined::Bool

  @assign functions = begin
    @match arrayElementType(ty) begin
      TYPE_COMPLEX(cls = node) => begin
        try
          @assign fn_ref = P_Function.lookupFunctionSimple(operatorName, node)
          @assign is_defined = true
        catch
          @assign is_defined = false
        end
        if is_defined
          @assign fn_ref = P_Function.instFunctionRef(fn_ref, info(node))
          @assign functions = P_Function.typeRefCache(fn_ref)
        else
          @assign functions = nil
        end
        functions
      end

      _ => begin
        nil
      end
    end
  end
  return functions
end

""" #= Patches operator record constructors to avoid recursive binding.

     They often have outputs declared as:
       output RecordType result = RecordType(args)

     The binding in such cases causes a recursive definition of the constructor,
     so to avoid that we rewrite any calls to the constructor in the binding as
     record expressions. =#"""
function patchOperatorRecordConstructorBinding(fn::M_Function)::M_Function

  local output_node::InstNode
  local output_comp::Component
  local output_binding::Binding

  #=  Due to how this function is used it might also be called on destructors,
  =#
  #=  which we just ignore.
  =#
  if listLength(fn.outputs) != 1
    return fn
  end
  @assign output_node = listHead(fn.outputs)
  @assign output_comp = component(output_node)
  @assign output_binding = getBinding(output_comp)
  if !isBound(output_binding)
    return fn
  end
  @assign output_binding = mapExp(
    output_binding,
    (fn) -> patchOperatorRecordConstructorBinding_traverser(constructorFn = fn),
  )
  @assign output_comp = P_Component.setBinding(output_binding, output_comp)
  @assign output_node = updateComponent(output_comp, output_node)
  return fn
end

function checkOperatorConstructorOutput(
  fn::M_Function,
  recordNode::InstNode,
  path::Absyn.Path,
  info::SourceInfo,
)
  local output_node::InstNode
  local output_ty::InstNode

  if listLength(fn.outputs) != 1
    Error.addSourceMessage(
      Error.OPERATOR_OVERLOADING_ONE_OUTPUT_ERROR,
      list(AbsynUtil.pathString(path)),
      info,
    )
    fail()
  end
  @assign output_node = listHead(fn.outputs)
  @assign output_ty = classScope(output_node)
  return if !isSame(output_ty, recordNode)
    Error.addSourceMessage(
      Error.OPERATOR_OVERLOADING_INVALID_OUTPUT_TYPE,
      list(
        name(output_node),
        AbsynUtil.pathString(path),
        name(recordNode),
        name(output_ty),
      ),
      info,
    )
    fail()
  end
end

function patchOperatorRecordConstructorBinding_traverser(
  exp::Expression,
  constructorFn::M_Function,
)::Expression
  local outExp::Expression

  local fn::M_Function
  local args::List{Expression}
  local ty::M_Type

  @assign outExp = begin
    @match exp begin
      CALL_EXPRESSION(
        call = TYPED_CALL(fn = fn, ty = ty, arguments = args),
      ) where {(referenceEq(constructorFn.node, fn.node))} => begin
        P_Expression.Expression.makeRecord(P_Function.name(constructorFn), ty, args)
      end

      _ => begin
        exp
      end
    end
  end
  return outExp
end
