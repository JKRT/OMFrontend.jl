module NFInline

using MetaModelica
using ExportAll

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
import ..P_NFComponentRef
P_ComponentRef = P_NFComponentRef
ComponentRef = P_NFComponentRef.NFComponentRef
import ..DAE.InlineType
import ..P_NFDimension
P_Dimension = P_NFDimension
Dimension = P_NFDimension.NFDimension
import ..P_NFExpression
P_Expression = P_NFExpression
Expression = P_NFExpression.NFExpression
import ..Flags
import ..NFCall.P_Call
import ..NFFunction.P_Function
import ..NFInstNode.P_InstNode
import ..P_NFStatement
P_Statement = P_NFStatement
Statement = P_NFStatement.NFStatement
import ..P_NFSubscript
P_Subscript = P_NFSubscript
Subscript = P_NFSubscript.NFSubscript
import ..P_NFType
P_M_Type = P_NFType
M_Type = NFType

function inlineCallExp(callExp::Expression)::Expression
  local result::Expression

  @assign result = begin
    local call::Call
    local shouldInline::Bool
    @match callExp begin
      CALL_EXPRESSION(call = call && P_Call.TYPED_CALL(__)) => begin
        @assign shouldInline = begin
          @match P_Call.inlineType(call) begin
            DAE.InlineType.BUILTIN_EARLY_INLINE(__) => begin
              true
            end

            DAE.InlineType.EARLY_INLINE(__) where {(Flags.isSet(Flags.INLINE_FUNCTIONS))} => begin
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

function inlineCall(call::Call)::Expression
  local exp::Expression

  @assign exp = begin
    local fn::M_Function
    local arg::Expression
    local args::List{Expression}
    local inputs::List{InstNode}
    local outputs::List{InstNode}
    local locals::List{InstNode}
    local body::List{Statement}
    local stmt::Statement
    @match call begin
      P_Call.TYPED_CALL(
        fn = fn && P_Function.FUNCTION(inputs = inputs, outputs = outputs, locals = locals),
        arguments = args,
      ) => begin
        @assign body = P_Function.getBody(fn)
        #=  This function can so far only handle functions with exactly one
        =#
        #=  statement and output and no local variables.
        =#
        if listLength(body) != 1 || listLength(outputs) != 1 || listLength(locals) > 0
          @assign exp = CALL_EXPRESSION(call)
          return
        end
        Error.assertion(
          listLength(inputs) == listLength(args),
          getInstanceName() +
          " got wrong number of arguments for " +
          AbsynUtil.pathString(P_Function.name(fn)),
          sourceInfo(),
        )
        @assign stmt = listHead(body)
        #=  TODO: Instead of repeating this for each input we should probably
        =#
        #=        just build a lookup tree or hash table and go through the
        =#
        #=        statement once.
        =#
        for i in inputs
          @match _cons(arg, args) = args
          @assign stmt = P_Statement.Statement.mapExp(
            stmt,
            () -> map(
              func = (i, arg) -> replaceCrefNode(node = i, value = arg),
            ),
          )#= AbsyntoJulia.dumpPattern: UNHANDLED Abyn.Exp  =#
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
        cref = CREF(
          node = cr_node,
          subscripts = subs,
          restCref = rest_cr,
        ),
      ) where {(
        refEqual(node, cr_node) &&
        !isFromCref(rest_cr)
      )} => begin
        P_Expression.Expression.applySubscripts(subs, value)
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
  @assign ty = typeOf(exp)
  @assign repl_ty =
    mapDims(ty, (node, value) -> replaceDimExp(node = node, value = value))
  if !referenceEq(ty, repl_ty)
    @assign exp = P_Expression.Expression.setType(repl_ty, exp)
  end
  return exp
end

function replaceDimExp(dim::Dimension, node::InstNode, value::Expression)::Dimension

  @assign dim = begin
    local exp::Expression
    @match dim begin
      P_Dimension.Dimension.EXP(__) => begin
        @assign exp = map(
          dim.exp,
          (node, value) -> replaceCrefNode(node = node, value = value),
        )
        P_Dimension.Dimension.fromExp(exp, dim.var)
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

  @assign exp = begin
    local cr_node::InstNode
    local rest_cr::ComponentRef
    @match stmt begin
      P_Statement.Statement.ASSIGNMENT(
        lhs = CREF_EXPRESSION(
          cref = CREF(
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

@exportAll()
end
