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
function inlineCallExp(callExp::Expression)::Expression
  local result::Expression
  @assign result = begin
    local call::Call
    local shouldInline::Bool
    @match callExp begin
      CALL_EXPRESSION(call = call && TYPED_CALL(__)) => begin
        @assign shouldInline = begin
          @match inlineType(call) begin
            DAE.BUILTIN_EARLY_INLINE(__) => begin
              true
            end
            DAE.EARLY_INLINE(__) where {(Flags.isSet(Flags.INLINE_FUNCTIONS))} => begin
              true
            end
            _ => begin
              false
            end
          end
        end
        if shouldInline
          inlineCall(call)
        else
          callExp
        end
      end
      _ => begin
        callExp
      end
    end
  end
  return result
end


"""
  Inline function for nonbuiltin callexps
  @author johti17
"""
function inlineSimpleCall(callExp::Expression)::Expression
  local result::Expression
  local call::Call
  local shouldInline = @match callExp begin
    CALL_EXPRESSION(c && TYPED_CALL(fn, ty, var, arguments, attributes)) => begin
      call = c
      shouldInline = true && !(attributes.builtin || isExternal(c))
#      println("Inline = $(shouldInline) for: " * toString(c))
      #= We might want to inline more things, so check arguments anyway =#
      if !shouldInline
        local newArgs = list(map(arg, inlineSimpleCall) for arg in arguments)
        @assign call.arguments = newArgs
        return CALL_EXPRESSION(call)
      end
      shouldInline
    end
    _ => false
  end
  result = if shouldInline
    inlineCall(call)
  else
    callExp
  end
  return result
end

"""
  Function to inline calls
@author johti17
"""
function inlineCall(call::Call)::Expression
  local exp::Expression
  exp = begin
    local fn::M_Function3
    local arg::Expression
    local args::List{Expression}
    local inputs::List{InstNode}
    local outputs::List{InstNode}
    local locals::List{InstNode}
    local body::Vector{Statement}
    local stmt::Statement
    @match call begin
      TYPED_CALL(
        fn = fn && M_FUNCTION(inputs = inputs, outputs = outputs, locals = locals),
        arguments = args,
      ) => begin
        #= External functions can't be inlined =#
        if isExternal(call)
          exp = CALL_EXPRESSION(call)
          return exp
        end
        body = getBody(fn)
        #=  This function can so far only handle functions with at most one =#
        #=  statement and output and no local variables. =#
        if length(body) > 1 || listLength(outputs) != 1 || listLength(locals) > 0
          exp = CALL_EXPRESSION(call)
          return exp
        end
        Error.assertion(
          listLength(inputs) == listLength(args),
          getInstanceName() +
          " got wrong number of arguments for " +
          AbsynUtil.pathString(name(fn)),
          sourceInfo(),
        )
        #=
        If we have no body there is nothing to inline.
        This might occur for instance for complex operators that are registered as calls in the frontend
        -johti17 2023-03-26
        =#
        if isempty(body)
          exp = CALL_EXPRESSION(call)
          return exp
        end
        stmt = body[1]
        #=
        TODO: Instead of repeating this for each input we should probably
          just build a lookup tree or hash table and go through the
          statement once.
        =#
        for i in inputs
          @match _cons(arg, args) = args
          stmt = mapExp(stmt,
                        (exp) -> map(exp, (exp) -> replaceCrefNode(exp, i, arg)))
        end
        getOutputExp(stmt, listHead(outputs), call)
      end

      _ => begin
        CALL_EXPRESSION(call)
      end
    end
  end
  return exp
end

function replaceCrefNode(exp::Expression, node::InstNode, value::Expression)::Expression
  local cr_node::InstNode
  local rest_cr::ComponentRef
  local subs::List{Subscript}
  local ty::M_Type
  local repl_ty::M_Type
  @assign exp = begin
    @match exp begin
      CREF_EXPRESSION(
        cref = COMPONENT_REF_CREF(
          node = cr_node,
          subscripts = subs,
          restCref = rest_cr,
        ),
      ) where {(
        refEqual(node, cr_node) &&
        !isFromCref(rest_cr)
      )} => begin
        applySubscripts(subs, value)
      end

      _ => begin
        exp
      end
    end
  end
  #=  TODO: This only works for simple crefs, for complex crefs (i.e. records)
  =#
  #=        we need to somehow replace the rest of the cref with nodes from the
  =#
  #=        record.
  =#
  #=  Replace expressions in dimensions too.
  =#
  ty = typeOf(exp)
  repl_ty =
    mapDims(ty, (dimArg) -> replaceDimExp(dimArg, node, value))
  if !referenceEq(ty, repl_ty)
    exp = setType(repl_ty, exp)
  end
  return exp
end

function replaceDimExp(dim::Dimension, node::InstNode, value::Expression)::Dimension
  dim = begin
    local exp::Expression
    @match dim begin
      DIMENSION_EXP(__) => begin
        exp = map(
          dim.exp,
          (x) -> replaceCrefNode(x, node, value),
        )
        fromExp(exp, dim.var)
      end
      _ => begin
        dim
      end
    end
  end
  return dim
end

function getOutputExp(stmt::Statement, outputNode::InstNode, call::Call)::Expression
  local exp::Expression
  exp = begin
    local cr_node::InstNode
    local rest_cr::ComponentRef
    @match stmt begin
      ALG_ASSIGNMENT(
        lhs = CREF_EXPRESSION(
          cref = COMPONENT_REF_CREF(
            node = cr_node,
            subscripts = nil(),
            restCref = rest_cr,
          ),
        ),
      ) where {(
        refEqual(outputNode, cr_node) &&
        !isFromCref(rest_cr)
      )} => begin
        stmt.rhs
      end
      _ => begin
        CALL_EXPRESSION(call)
      end
    end
  end
  return exp
end
