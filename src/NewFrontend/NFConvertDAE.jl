@UniontypeDecl VariableConversionSettings


function convert(
  flatModel::FlatModel,
  functions::FunctionTree,
  name::String,
  info::SourceInfo,
)::Tuple{DAE.DAE_LIST, DAE.FunctionTree}
  local daeFunctions::DAE.FunctionTree
  local dae::DAE.DAE_LIST
  local elems::List{DAE.Element}
  local class_elem::DAE.Element
  daeFunctions = convertFunctionTree(functions)
  elems = convertVariables(flatModel.variables, nil)
  elems = convertEquations(flatModel.equations, elems)
  elems = convertInitialEquations(flatModel.initialEquations, elems)
  elems = convertAlgorithms(flatModel.algorithms, elems)
  elems = convertInitialAlgorithms(flatModel.initialAlgorithms, elems)
  @assign class_elem =
    DAE.COMP(name, elems, DAE.emptyElementSource, flatModel.comment) # TODO DAE.ElementSource_createElementSource(info)
  @assign dae = DAE.DAE_LIST(list(class_elem))
  return (dae, daeFunctions)
end

@Uniontype VariableConversionSettings begin
  @Record VARIABLE_CONVERSION_SETTINGS begin
    useLocalDirection::Bool
    isFunctionParameter::Bool
    addTypeToSource::Bool
  end
end

const FUNCTION_VARIABLE_CONVERSION_SETTINGS =
  VARIABLE_CONVERSION_SETTINGS(false, true, false)::VariableConversionSettings

function convertVariables(
  variables::List{<:Variable},
  elements::List{<:DAE.Element},
)::List{DAE.Element}
#  local settings::VariableConversionSettings
  @assign settings = VARIABLE_CONVERSION_SETTINGS(
     true, #Flags.getConfigBool(Flags.USE_LOCAL_DIRECTION),
    false,
    true# addTypeToSource = Flags.isSet(Flags.INFO_XML_OPERATIONS) ||
    #                   Flags.isSet(Flags.VISUAL_XML),
  )
  for var in listReverse(variables)
    @assign elements = _cons(convertVariable(var, settings), elements)
  end
  return elements
end

function convertVariable(var::Variable, settings::VariableConversionSettings)::DAE.Element
  local daeVar::DAE.Element
  local var_attr::Option{DAE.VariableAttributes}
  local binding_exp::Option{DAE.Exp}
  @assign binding_exp = toDAEExp(var.binding)
  @assign var_attr = convertVarAttributes(var.typeAttributes, var.ty, var.attributes)
  @assign daeVar = makeDAEVar(
    var.name,
    var.ty,
    binding_exp,
    var.attributes,
    var.visibility,
    var_attr,
    var.comment,
    settings,
    var.info,
  )
  return daeVar
end

function makeDAEVar(
  cref::ComponentRef,
  ty::NFType,
  binding::Option{<:DAE.Exp},
  attr::Attributes,
  vis::VisibilityType,
  vattr::Option{<:DAE.VariableAttributes},
  comment::Option{<:SCode.Comment},
  settings::VariableConversionSettings,
  info::SourceInfo,
)::DAE.Element
  local var::DAE.Element

  local dcref::DAE.ComponentRef
  local dty::DAE.Type
  local source::DAE.ElementSource
  local dir::DirectionType

  @assign dcref = toDAE(cref)
  @assign dty = toDAE(if settings.isFunctionParameter
    arrayElementType(ty)
  else
    ty
  end)
  @assign source = DAE.emptyElementSource #ElementSource_createElementSource(info)
  if settings.addTypeToSource
    @assign source = addComponentTypeToSource(cref, source)
  end
  @assign var = begin
    @match attr begin
      ATTRIBUTES(__) => begin
        #=  Strip input/output from non top-level components unless
        =#
        #=  --useLocalDirection=true has been set.
        =#
        if attr.direction == Direction.NONE || settings.useLocalDirection
          @assign dir = attr.direction
        else
          @assign dir = getComponentDirection(attr.direction, cref)
        end
        DAE.VAR(
          dcref,
          variabilityToDAE(attr.variability),
          directionToDAE(dir),
          parallelismToDAE(attr.parallelism),
          visibilityToDAE(vis),
          dty,
          binding,
          nil,#crefDims(dcref) TODO
          toDAE(attr.connectorType),
          source,
          vattr,
          comment,
          Absyn.NOT_INNER_OUTER(),
        )
      end

      _ => begin
        DAE.VAR(
          dcref,
          DAE.VARIABLE(),
          DAE.VarDirection.BIDIR(),
          DAE.NON_PARALLEL(),
          P_Prefixes.visibilityToDAE(vis),
          dty,
          binding,
          nil,
          DAE.ConnectorType.NON_CONNECTOR(),
          source,
          vattr,
          comment,
          Absyn.NOT_INNER_OUTER(),
        )
      end
    end
  end
  return var
end

function addComponentTypeToSource(
  cref::ComponentRef,
  source::DAE.ElementSource,
)::DAE.ElementSource
  @assign source = begin
    @match cref begin
      COMPONENT_REF_CREF(__) => begin
        # TODO!  @assign source = DAE.ElementSource_createElementSource(source)
        # addComponentTypeToSource(cref.restCref, source)
        source
      end

      _ => begin
        source
      end
    end
  end
  return source
end

""" #= Returns the given direction if the cref refers to a top-level component or to
   a component in a top-level connector, otherwise returns Direction.NONE. =#"""
function getComponentDirection(dir::DirectionType, cref::ComponentRef)::DirectionType
  local rest_cref::ComponentRef = rest(cref)
  dir = begin
    @match rest_cref begin
      COMPONENT_REF_EMPTY(__) => begin
        dir
      end
      COMPONENT_REF_CREF(__) => begin
        if isConnector(rest_cref.node)
          getComponentDirection(dir, rest_cref)
        else
          Direction.NONE
        end
      end
    end
  end
  return dir
end

function convertVarAttributes(
  attrs::List{<:Tuple{<:String, Binding}},
  ty::NFType,
  compAttrs::Attributes,
)::Option{DAE.VariableAttributes}
  local attributes::Option{DAE.VariableAttributes}
  local is_final::Bool
  local is_final_opt::Option{Bool}
  local elTy::M_Type
  local is_array::Bool = false

  @assign is_final =
    compAttrs.isFinal || compAttrs.variability == Variability.STRUCTURAL_PARAMETER
  if listEmpty(attrs) && !is_final
    @assign attributes = NONE()
    return attributes
  end
  @assign is_final_opt = SOME(is_final)
  @assign attributes = begin
    @match arrayElementType(ty) begin
      TYPE_REAL(__) => begin
        convertRealVarAttributes(attrs, is_final_opt)
      end

      TYPE_INTEGER(__) => begin
        convertIntVarAttributes(attrs, is_final_opt)
      end

      TYPE_BOOLEAN(__) => begin
        convertBoolVarAttributes(attrs, is_final_opt)
      end

      TYPE_STRING(__) => begin
        convertStringVarAttributes(attrs, is_final_opt)
      end

      TYPE_ENUMERATION(__) => begin
        convertEnumVarAttributes(attrs, is_final_opt)
      end

      _ => begin
        NONE()
      end
    end
  end
  return attributes
end

function convertRealVarAttributes(
  attrs::List{<:Tuple{<:String, Binding}},
  isFinal::Option{<:Bool},
)::Option{DAE.VariableAttributes}
  local attributes::Option{DAE.VariableAttributes}

  local name::String
  local b::Binding
  local quantity::Option{DAE.Exp} = NONE()
  local unit::Option{DAE.Exp} = NONE()
  local displayUnit::Option{DAE.Exp} = NONE()
  local min::Option{DAE.Exp} = NONE()
  local max::Option{DAE.Exp} = NONE()
  local start::Option{DAE.Exp} = NONE()
  local fixed::Option{DAE.Exp} = NONE()
  local nominal::Option{DAE.Exp} = NONE()
  local state_select::Option{DAE.StateSelect} = NONE()

  for attr in attrs
    @assign (name, b) = attr
    @assign () = begin
      @match name begin
        "displayUnit" => begin
          @assign displayUnit = convertVarAttribute(b)
          ()
        end

        "fixed" => begin
          @assign fixed = convertVarAttribute(b)
          ()
        end

        "max" => begin
          @assign max = convertVarAttribute(b)
          ()
        end

        "min" => begin
          @assign min = convertVarAttribute(b)
          ()
        end

        "nominal" => begin
          @assign nominal = convertVarAttribute(b)
          ()
        end

        "quantity" => begin
          @assign quantity = convertVarAttribute(b)
          ()
        end

        "start" => begin
          @assign start = convertVarAttribute(b)
          ()
        end

        "stateSelect" => begin
          @assign state_select = convertStateSelectAttribute(b)
          ()
        end

        "unbounded" => begin
          ()
        end

        "unit" => begin
          #=  TODO: VAR_ATTR_REAL has no field for unbounded.
          =#
          @assign unit = convertVarAttribute(b)
          ()
        end

        _ => begin
          #=  The attributes should already be type checked, so we shouldn't get any
          =#
          #=  unknown attributes here.
          =#
          Error.assertion(
            false,
            getInstanceName() + " got unknown type attribute " + name,
            sourceInfo(),
          )
          fail()
        end
      end
    end
  end
  @assign attributes = SOME(DAE.VAR_ATTR_REAL(
    quantity,
    unit,
    displayUnit,
    min,
    max,
    start,
    fixed,
    nominal,
    state_select,
    NONE(),
    NONE(),
    NONE(),
    NONE(),
    isFinal,
    NONE(),
  ))
  return attributes
end

function convertIntVarAttributes(
  attrs::List{<:Tuple{<:String, Binding}},
  isFinal::Option{<:Bool},
)::Option{DAE.VariableAttributes}
  local attributes::Option{DAE.VariableAttributes}

  local name::String
  local b::Binding
  local quantity::Option{DAE.Exp} = NONE()
  local min::Option{DAE.Exp} = NONE()
  local max::Option{DAE.Exp} = NONE()
  local start::Option{DAE.Exp} = NONE()
  local fixed::Option{DAE.Exp} = NONE()

  for attr in attrs
    @assign (name, b) = attr
    @assign () = begin
      @match name begin
        "quantity" => begin
          @assign quantity = convertVarAttribute(b)
          ()
        end

        "min" => begin
          @assign min = convertVarAttribute(b)
          ()
        end

        "max" => begin
          @assign max = convertVarAttribute(b)
          ()
        end

        "start" => begin
          @assign start = convertVarAttribute(b)
          ()
        end

        "fixed" => begin
          @assign fixed = convertVarAttribute(b)
          ()
        end

        _ => begin
          #=  The attributes should already be type checked, so we shouldn't get any
          =#
          #=  unknown attributes here.
          =#
          Error.assertion(
            false,
            getInstanceName() + " got unknown type attribute " + name,
            sourceInfo(),
          )
          fail()
        end
      end
    end
  end
  @assign attributes = SOME(DAE.VAR_ATTR_INT(
    quantity,
    min,
    max,
    start,
    fixed,
    NONE(),
    NONE(),
    NONE(),
    NONE(),
    isFinal,
    NONE(),
  ))
  return attributes
end

function convertBoolVarAttributes(
  attrs::List{<:Tuple{<:String, Binding}},
  isFinal::Option{<:Bool},
)::Option{DAE.VariableAttributes}
  local attributes::Option{DAE.VariableAttributes}

  local name::String
  local b::Binding
  local quantity::Option{DAE.Exp} = NONE()
  local start::Option{DAE.Exp} = NONE()
  local fixed::Option{DAE.Exp} = NONE()

  for attr in attrs
    @assign (name, b) = attr
    @assign () = begin
      @match name begin
        "quantity" => begin
          @assign quantity = convertVarAttribute(b)
          ()
        end

        "start" => begin
          @assign start = convertVarAttribute(b)
          ()
        end

        "fixed" => begin
          @assign fixed = convertVarAttribute(b)
          ()
        end

        _ => begin
          #=  The attributes should already be type checked, so we shouldn't get any
          =#
          #=  unknown attributes here.
          =#
          Error.assertion(
            false,
            getInstanceName() + " got unknown type attribute " + name,
            sourceInfo(),
          )
          fail()
        end
      end
    end
  end
  @assign attributes = SOME(DAE.VAR_ATTR_BOOL(
    quantity,
    start,
    fixed,
    NONE(),
    NONE(),
    isFinal,
    NONE(),
  ))
  return attributes
end

function convertStringVarAttributes(
  attrs::List{<:Tuple{<:String, Binding}},
  isFinal::Option{<:Bool},
)::Option{DAE.VariableAttributes}
  local attributes::Option{DAE.VariableAttributes}

  local name::String
  local b::Binding
  local quantity::Option{DAE.Exp} = NONE()
  local start::Option{DAE.Exp} = NONE()
  local fixed::Option{DAE.Exp} = NONE()

  for attr in attrs
    @assign (name, b) = attr
    @assign () = begin
      @match name begin
        "quantity" => begin
          @assign quantity = convertVarAttribute(b)
          ()
        end

        "start" => begin
          @assign start = convertVarAttribute(b)
          ()
        end

        "fixed" => begin
          @assign fixed = convertVarAttribute(b)
          ()
        end

        _ => begin
          #=  The attributes should already be type checked, so we shouldn't get any
          =#
          #=  unknown attributes here.
          =#
          Error.assertion(
            false,
            getInstanceName() + " got unknown type attribute " + name,
            sourceInfo(),
          )
          fail()
        end
      end
    end
  end
  @assign attributes = SOME(DAE.VAR_ATTR_STRING(
    quantity,
    start,
    fixed,
    NONE(),
    NONE(),
    isFinal,
    NONE(),
  ))
  return attributes
end

function convertEnumVarAttributes(
  attrs::List{<:Tuple{<:String, Binding}},
  isFinal::Option{<:Bool},
)::Option{DAE.VariableAttributes}
  local attributes::Option{DAE.VariableAttributes}

  local name::String
  local b::Binding
  local quantity::Option{DAE.Exp} = NONE()
  local min::Option{DAE.Exp} = NONE()
  local max::Option{DAE.Exp} = NONE()
  local start::Option{DAE.Exp} = NONE()
  local fixed::Option{DAE.Exp} = NONE()

  for attr in attrs
    @assign (name, b) = attr
    @assign () = begin
      @match name begin
        "fixed" => begin
          @assign fixed = convertVarAttribute(b)
          ()
        end

        "max" => begin
          @assign max = convertVarAttribute(b)
          ()
        end

        "min" => begin
          @assign min = convertVarAttribute(b)
          ()
        end

        "quantity" => begin
          @assign quantity = convertVarAttribute(b)
          ()
        end

        "start" => begin
          @assign start = convertVarAttribute(b)
          ()
        end

        _ => begin
          #=  The attributes should already be type checked, so we shouldn't get any
          =#
          #=  unknown attributes here.
          =#
          Error.assertion(
            false,
            getInstanceName() + " got unknown type attribute " + name,
            sourceInfo(),
          )
          fail()
        end
      end
    end
  end
  @assign attributes = SOME(DAE.VAR_ATTR_ENUMERATION(
    quantity,
    min,
    max,
    start,
    fixed,
    NONE(),
    NONE(),
    isFinal,
    NONE(),
  ))
  return attributes
end

function convertVarAttribute(binding::Binding)::Option{DAE.Exp}
  local attribute::Option{DAE.Exp} =
    SOME(toDAE(getTypedExp(binding)))
  return attribute
end

function convertStateSelectAttribute(binding::Binding)::Option{DAE.StateSelect}
  local stateSelect::Option{DAE.StateSelect}

  local node::InstNode
  local name::String
  local exp::Expression =
    getBindingExp(getTypedExp(binding))

  @assign name = begin
    @match exp begin
      ENUM_LITERAL_EXPRESSION(__) => begin
        exp.name
      end

      CREF_EXPRESSION(cref = CREF(node = node)) => begin
        name(node)
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() +
          " got invalid StateSelect expression " +
          toString(exp),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  @assign stateSelect = SOME(lookupStateSelectMember(name))
  return stateSelect
end

function lookupStateSelectMember(name::String)::DAE.StateSelect
  local stateSelect::DAE.StateSelect

  @assign stateSelect = begin
    @match name begin
      "never" => begin
        DAE.StateSelect.NEVER()
      end

      "avoid" => begin
        DAE.StateSelect.AVOID()
      end

      "default" => begin
        DAE.StateSelect.DEFAULT()
      end

      "prefer" => begin
        DAE.StateSelect.PREFER()
      end

      "always" => begin
        DAE.StateSelect.ALWAYS()
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got unknown StateSelect literal " + name,
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return stateSelect
end

function convertEquations(
  equations::List{<:Equation},
  elements::List{<:DAE.Element} = nil,
)::List{DAE.Element}
  for eq in listReverse(equations)
    elements = convertEquation(eq, elements)
  end
  return elements
end

function convertEquation(eq::Equation, elements::List{<:DAE.Element})::List{DAE.Element}
  @assign elements = begin
    local e1::DAE.Exp
    local e2::DAE.Exp
    local e3::DAE.Exp
    local cr1::DAE.ComponentRef
    local cr2::DAE.ComponentRef
    local dims::List{DAE.Dimension}
    local body::List{DAE.Element}
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        @assign e1 = toDAE(eq.lhs)
        @assign e2 = toDAE(eq.rhs)
        _cons(
          if isComplex(eq.ty)
            DAE.Element.COMPLEX_EQUATION(e1, e2, eq.source)
          elseif (isArray(eq.ty))
            DAE.Element.ARRAY_EQUATION(
              list(toDAE(d) for d in arrayDims(eq.ty)),
              e1,
              e2,
              eq.source,
            )
          else
            DAE.EQUATION(e1, e2, eq.source)
          end,
          elements,
        )
      end

      EQUATION_CREF_EQUALITY(__) => begin
        @assign cr1 = toDAE(eq.lhs)
        @assign cr2 = toDAE(eq.rhs)
        #_cons(DAE.EQUEQUATION(cr1, cr2, eq.source), elements)
        #Adrians suggestion
        _cons(DAE.EQUATION(DAE.BINARY(DAE.CREF(cr1, cr1.identType),
                                      DAE.ADD(cr1.identType),
                                      DAE.UNARY(DAE.UMINUS(cr2.identType), DAE.CREF(cr2, cr2.identType))),
                           DAE.RCONST(0.), eq.source), elements)
      end

      EQUATION_ARRAY_EQUALITY(__) => begin
        @assign e1 = toDAE(eq.lhs)
        @assign e2 = toDAE(eq.rhs)
        @assign dims = list(toDAE(d) for d in arrayDims(eq.ty))
        _cons(DAE.ARRAY_EQUATION(dims, e1, e2, eq.source), elements)
      end

      EQUATION_FOR(__) => begin
        _cons(convertForEquation(eq), elements)
      end

      EQUATION_IF(__) => begin
        _cons(convertIfEquation(eq.branches, eq.source, isInitial = false), elements)
      end

      EQUATION_WHEN(__) => begin
        _cons(convertWhenEquation(eq.branches, eq.source), elements)
      end

      EQUATION_ASSERT(__) => begin
        @assign e1 = toDAE(eq.condition)
        @assign e2 = toDAE(eq.message)
        @assign e3 = toDAE(eq.level)
        _cons(DAE.ASSERT(e1, e2, e3, eq.source), elements)
      end

      EQUATION_TERMINATE(__) => begin
        _cons(
          DAE.Element.TERMINATE(toDAE(eq.message), eq.source),
          elements,
        )
      end

      EQUATION_REINIT(__) => begin
        @assign cr1 =
          toDAE(toCref(eq.cref))
        @assign e1 = toDAE(eq.reinitExp)
        _cons(DAE.REINIT(cr1, e1, eq.source), elements)
      end

      EQUATION_NORETCALL(__) => begin
        _cons(
          DAE.NORETCALL(toDAE(eq.exp), eq.source),
          elements,
        )
      end

      _ => begin
        elements
      end
    end
  end
  return elements
end

function convertForEquation(forEquation::Equation)::DAE.Element
  local forDAE::DAE.Element

  local iterator::InstNode
  local ty::M_Type
  local range::Expression
  local body::List{Equation}
  local dbody::List{DAE.Element}
  local source::DAE.ElementSource

  @match EQUATION_FOR(
    iterator = iterator,
    range = SOME(range),
    body = body,
    source = source,
  ) = forEquation
  @assign dbody = convertEquations(body)
  @match ITERATOR_COMPONENT(ty = ty) = component(iterator)
  @assign forDAE = DAE.Element.FOR_EQUATION(
    toDAE(ty),
    isArray(ty),
    name(iterator),
    0,
    toDAE(range),
    dbody,
    source,
  )
  return forDAE
end

function convertIfEquation(
  ifBranches::List{<:Equation_Branch},
  source::DAE.ElementSource;
  isInitial::Bool,
)::DAE.Element
  @info "Converting if equation"
  local ifEquation::DAE.Element
  local conds::List{Expression} = nil
  local branches::List{List{Equation}} = nil
  local dconds::List{DAE.Exp}
  local dbranches::List{List{DAE.Element}}
  local else_branch::List{DAE.Element}

  for branch in ifBranches
    @assign (conds, branches) = begin
      @match branch begin
        EQUATION_BRANCH(__) => begin
          (_cons(branch.condition, conds), _cons(branch.body, branches))
        end

        P_Equation.Equation.INVALID_BRANCH(__) => begin
          P_Equation.Equation.triggerErrors(branch)
          fail()
        end
      end
    end
  end
  @assign dbranches = if isInitial
    list(convertInitialEquations(b) for b in branches)
  else
    list(convertEquations(b) for b in branches)
  end
  #=  Transform the last branch to an else-branch if its condition is true.
  =#
  if isTrue(listHead(conds))
    @match _cons(else_branch, dbranches) = dbranches
    @assign conds = listRest(conds)
  else
    @assign else_branch = nil
  end
  @assign dconds = listReverse(list(toDAE(c) for c in conds))
  @assign dbranches = listReverseInPlace(dbranches)
  @assign ifEquation = if isInitial
    DAE.INITIAL_IF_EQUATION(dconds, dbranches, else_branch, source)
  else
    DAE.IF_EQUATION(dconds, dbranches, else_branch, source)
  end
  return ifEquation
end

function convertWhenEquation(
  whenBranches::List{<:Equation_Branch},
  source::DAE.ElementSource,
)::DAE.Element
  local whenEquation::DAE.Element

  local cond::DAE.Exp
  local els::List{DAE.Element}
  local when_eq::Option{DAE.Element} = NONE()

  for b in listReverse(whenBranches)
    @assign when_eq = begin
      @match b begin
        EQUATION_BRANCH(__) => begin
          @assign cond = toDAE(b.condition)
          @assign els = convertEquations(b.body)
          SOME(DAE.WHEN_EQUATION(cond, els, when_eq, source))
        end
      end
    end
  end
  @match SOME(whenEquation) = when_eq
  return whenEquation
end

function convertInitialEquations(
  equations::List{<:Equation},
  elements::List{<:DAE.Element} = nil,
)::List{DAE.Element}

  for eq in listReverse(equations)
    @assign elements = convertInitialEquation(eq, elements)
  end
  return elements
end

function convertInitialEquation(
  eq::Equation,
  elements::List{<:DAE.Element},
)::List{DAE.Element}

  @assign elements = begin
    local e1::DAE.Exp
    local e2::DAE.Exp
    local e3::DAE.Exp
    local cref::DAE.ComponentRef
    local dims::List{DAE.Dimension}
    local body::List{DAE.Element}
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        @assign e1 = toDAE(eq.lhs)
        @assign e2 = toDAE(eq.rhs)
        _cons(if isComplex(eq.ty)
          DAE.INITIAL_COMPLEX_EQUATION(e1, e2, eq.source)
        else
          DAE.INITIALEQUATION(e1, e2, eq.source)
        end, elements)
      end

      EQUATION_ARRAY_EQUALITY(__) => begin
        @assign e1 = toDAE(eq.lhs)
        @assign e2 = toDAE(eq.rhs)
        @assign dims = list(toDAE(d) for d in arrayDims(eq.ty))
        _cons(DAE.INITIAL_ARRAY_EQUATION(dims, e1, e2, eq.source), elements)
      end

      EQUATION_FOR(__) => begin
        _cons(convertForEquation(eq), elements)
      end

      EQUATION_IF(__) => begin
        _cons(convertIfEquation(eq.branches, eq.source, isInitial = true), elements)
      end

      EQUATION_ASSERT(__) => begin
        @assign e1 = toDAE(eq.condition)
        @assign e2 = toDAE(eq.message)
        @assign e3 = toDAE(eq.level)
        _cons(DAE.Element.INITIAL_ASSERT(e1, e2, e3, eq.source), elements)
      end

      EQUATION_TERMINATE(__) => begin
        _cons(
          DAE.Element.INITIAL_TERMINATE(
            toDAE(eq.message),
            eq.source,
          ),
          elements,
        )
      end

      EQUATION_NORETCALL(__) => begin
        _cons(
          DAE.Element.INITIAL_NORETCALL(toDAE(eq.exp), eq.source),
          elements,
        )
      end

      _ => begin
        elements
      end
    end
  end
  return elements
end

function convertAlgorithms(
  algorithms::List{<:Algorithm},
  elements::List{<:DAE.Element},
)::List{DAE.Element}

  for alg in listReverse(algorithms)
    @assign elements = convertAlgorithm(alg, elements)
  end
  return elements
end

function convertAlgorithm(alg::Algorithm, elements::List{<:DAE.Element})::List{DAE.Element}

  local stmts::List{DAE.P_Statement.Statement}
  local dalg::DAE.P_Algorithm.Algorithm
  local src::DAE.ElementSource

  @assign stmts = convertStatements(alg.statements)
  @assign dalg = DAE.ALGORITHM_STMTS(stmts)
  @assign elements = _cons(DAE.ALGORITHM(dalg, alg.source), elements)
  return elements
end

function convertStatements(statements::List{<:Statement})::List{DAE.Statement}
  local elements::List{DAE.Statement}
  elements = list(convertStatement(s) for s in statements)
  return elements
end

function convertStatement(stmt::Statement)::DAE.Statement
  local elem::DAE.Statement
  @assign elem = begin
    local e1::DAE.Exp
    local e2::DAE.Exp
    local e3::DAE.Exp
    local ty::DAE.Type
    local body::List{DAE.Statement}
    @match stmt begin
      ALG_ASSIGNMENT(__) => begin
        convertAssignment(stmt)
      end

      ALG_FUNCTION_ARRAY_INIT(__) => begin
        @assign ty = toDAE(stmt.ty)
        DAE.STMT_ARRAY_INIT(stmt.name, ty, stmt.source)
      end

      ALG_FOR(__) => begin
        convertForStatement(stmt)
      end

      ALG_IF(__) => begin
        convertIfStatement(stmt.branches, stmt.source)
      end

      ALG_WHEN(__) => begin
        convertWhenStatement(stmt.branches, stmt.source)
      end

      ALG_ASSERT(__) => begin
        e1 = toDAE(stmt.condition)
        e2 = toDAE(stmt.message)
        e3 = toDAE(stmt.level)
        DAE.STMT_ASSERT(e1, e2, e3, stmt.source)
      end

      ALG_TERMINATE(__) => begin
        DAE.STMT_TERMINATE(
          toDAE(stmt.message),
          stmt.source,
        )
      end

      ALG_NORETCALL(__) => begin
        DAE.Statement.STMT_NORETCALL(
          toDAE(stmt.exp),
          stmt.source,
        )
      end

      ALG_WHILE(__) => begin
        e1 = toDAE(stmt.condition)
        body = convertStatements(stmt.body)
        DAE.STMT_WHILE(e1, body, stmt.source)
      end

      ALG_RETURN(__) => begin
        DAE.STMT_RETURN(stmt.source)
      end

      ALG_BREAK(__) => begin
        DAE.STMT_BREAK(stmt.source)
      end

      ALG_FAILURE(__) => begin
        DAE.STMT_FAILURE(convertStatements(stmt.body), stmt.source)
      end
    end
  end
  return elem
end

function convertAssignment(stmt::Statement)::DAE.Statement
  local daeStmt::DAE.Statement
  local lhs::Expression
  local rhs::Expression
  local src::DAE.ElementSource
  local ty::M_Type
  local dty::DAE.Type
  local dlhs::DAE.Exp
  local drhs::DAE.Exp
  local expl::List{Expression}
  @match ALG_ASSIGNMENT(lhs, rhs, ty, src) = stmt
  if isTuple(ty)
    @match TUPLE_EXPRESSION(elements = expl) = lhs
    daeStmt = begin
      @match expl begin
        nil() => begin
          DAE.STMT_NORETCALL(toDAE(rhs), src)
        end

        lhs <| nil() => begin
          #=  () := call(...) => call(...)
          =#
          #=  (lhs) := call(...) => lhs := TSUB[call(...), 1]
          =#
          @assign dty = toDAE(ty)
          @assign dlhs = toDAE(lhs)
          @assign drhs = DAE.TSUB(toDAE(rhs), 1, dty)
          if isArray(ty)
            @assign daeStmt =
              DAE.STMT_ASSIGN_ARR(dty, dlhs, drhs, src)
          else
            @assign daeStmt = DAE.STMT_ASSIGN(dty, dlhs, drhs, src)
          end
          daeStmt
        end

        _ => begin
          @assign dty = toDAE(ty)
          @assign drhs = toDAE(rhs)
          DAE.STMT_TUPLE_ASSIGN(
            dty,
            list(toDAE(e) for e in expl),
            drhs,
            src,
          )
        end
      end
    end
  else
    dty = toDAE(ty)
    dlhs = toDAE(lhs)
    drhs = toDAE(rhs)
    if isArray(ty)
      daeStmt = DAE.STMT_ASSIGN_ARR(dty, dlhs, drhs, src)
    else
      daeStmt = DAE.STMT_ASSIGN(dty, dlhs, drhs, src)
    end
  end
  return daeStmt
end

function convertForStatement(forStmt::Statement)::DAE.P_Statement.Statement
  local forDAE::DAE.P_Statement.Statement

  local iterator::InstNode
  local ty::M_Type
  local range::Expression
  local body::List{Statement}
  local dbody::List{DAE.P_Statement.Statement}
  local source::DAE.ElementSource

  @match P_Statement.Statement.FOR(
    iterator = iterator,
    range = SOME(range),
    body = body,
    source = source,
  ) = forStmt
  @assign dbody = convertStatements(body)
  @match ITERATOR_COMPONENT(ty = ty) = component(iterator)
  @assign forDAE = DAE.P_Statement.Statement.STMT_FOR(
    toDAE(ty),
    isArray(ty),
    name(iterator),
    0,
    toDAE(range),
    dbody,
    source,
  )
  return forDAE
end

function convertIfStatement(
  ifBranches::List{<:Tuple{<:Expression, List{<:Statement}}},
  source::DAE.ElementSource,
)::DAE.Statement
  local ifStatement::DAE.Statement
  local cond::Expression
  local dcond::DAE.Exp
  local stmts::List{Statement}
  local dstmts::List{DAE.Statement}
  local first::Bool = true
  local else_stmt::DAE.Else = DAE.NOELSE()
  for b in listReverse(ifBranches)
    (cond, stmts) = b
    dcond = toDAE(cond)
    dstmts = convertStatements(stmts)
    if first && isTrue(cond)
      else_stmt = DAE.ELSE(dstmts)
    else
      else_stmt = DAE.ELSEIF(dcond, dstmts, else_stmt)
    end
    first = false
  end
  #=  This should always be an ELSEIF due to branch selection in earlier phases. =#
  @match DAE.ELSEIF(dcond, dstmts, else_stmt) = else_stmt
  ifStatement = DAE.STMT_IF(dcond, dstmts, else_stmt, source)
  return ifStatement
end

function convertWhenStatement(
  whenBranches::List{<:Tuple{<:Expression, List{<:Statement}}},
  source::DAE.ElementSource,
)::DAE.P_Statement.Statement
  local whenStatement::DAE.Statement

  local cond::DAE.Exp
  local stmts::List{DAE.Statement}
  local when_stmt::Option{DAE.Statement} = NONE()

  for b in listReverse(whenBranches)
    @assign cond = toDAE(Util.tuple21(b))
    @assign stmts = convertStatements(Util.tuple22(b))
    @assign when_stmt =
      SOME(DAE.STMT_WHEN(cond, nil, false, stmts, when_stmt, source))
  end
  @match SOME(whenStatement) = when_stmt
  return whenStatement
end

function convertInitialAlgorithms(
  algorithms::List{<:Algorithm},
  elements::List{<:DAE.Element},
)::List{DAE.Element}

  for alg in listReverse(algorithms)
    @assign elements = convertInitialAlgorithm(alg, elements)
  end
  return elements
end

function convertInitialAlgorithm(
  alg::Algorithm,
  elements::List{<:DAE.Element},
)::List{DAE.Element}

  local stmts::List{DAE.P_Statement.Statement}
  local dalg::DAE.P_Algorithm.Algorithm

  @assign stmts = convertStatements(alg.statements)
  @assign dalg = DAE.ALGORITHM_STMTS(stmts)
  @assign elements = _cons(DAE.INITIALALGORITHM(dalg, alg.source), elements)
  return elements
end

function convertFunctionTree(funcs::FunctionTree)::DAE.FunctionTree
  local dfuncs::DAE.FunctionTree
  @assign dfuncs = begin
    local left::DAE.FunctionTree
    local right::DAE.FunctionTree
    local fn::DAE.P_Function
    @match funcs begin
      NFFunctionTree.NODE(__) => begin
        @assign fn = convertFunction(funcs.value)
        @assign left = convertFunctionTree(funcs.left)
        @assign right = convertFunctionTree(funcs.right)
        DAE.FunctionTree.NODE(funcs.key, SOME(fn), funcs.height, left, right)
      end
      NFFunctionTree.LEAF(__) => begin
        @assign fn = convertFunction(funcs.value)
        DAE.FunctionTree.LEAF(funcs.key, SOME(fn))
      end
      NFFunctionTree.EMPTY(__) => begin
        Dict()
      end
    end
  end
  return dfuncs
end

function convertFunction(func::M_Function)::DAE.Function
  local dfunc::DAE.P_Function
  local cls::Class
  local elems::List{DAE.Element}
  local def::DAE.FunctionDefinition
  local sections::Sections
  @assign cls = getClass(P_Function.instance(func))
  @assign dfunc = begin
    @match cls begin
      INSTANCED_CLASS(
        sections = sections,
        restriction = RESTRICTION_FUNCTION(__),
      ) => begin
        @assign elems = convertFunctionParams(func.inputs, nil)
        @assign elems = convertFunctionParams(func.outputs, elems)
        @assign elems = convertFunctionParams(func.locals, elems)
        @assign def = begin
          @match sections begin
            SECTIONS(__) => begin
              #=  A function with an algorithm section. =#
              @assign elems = convertAlgorithms(sections.algorithms, elems)
              DAE.FunctionDefinition.FUNCTION_DEF(listReverse(elems))
            end
            SECTIONS_EXTERNAL(__) => begin
              convertExternalDecl(sections, listReverse(elems))
            end
            _ => begin
              DAE.FUNCTION_DEF(listReverse(elems))
            end
          end
        end
        toDAE(func, def)
      end
      INSTANCED_CLASS(
        restriction = RECORD_CONSTRUCTOR(__),
      ) => begin
        DAE.RECORD_CONSTRUCTOR(
          name(func),
          makeDAEType(func),
          DAE.emptyElementSource,
        )
      end
      _ => begin
        Error.assertion(false, getInstanceName() + " got unknown function", sourceInfo())
        fail()
      end
    end
  end
  return dfunc
end

function convertFunctionParams(
  params::List{<:InstNode},
  elements::List{<:DAE.Element},
)::List{DAE.Element}

  for p in params
    @assign elements = _cons(convertFunctionParam(p), elements)
  end
  return elements
end

function convertFunctionParam(node::InstNode)::DAE.Element
  local element::DAE.Element
  local comp::Component
  local cls::Class
  local info::SourceInfo
  local var_attr::Option{DAE.VariableAttributes}
  local cref::ComponentRef
  local attr::Attributes
  local ty::M_Type
  local bindingV::Option{DAE.Exp}
  local ty_attr::List{Tuple{String, Binding}}
  comp = component(node)
  element = begin
    @match comp begin
      TYPED_COMPONENT(ty = ty, info = info, attributes = attr) => begin
        cref = fromNode(node, ty)
        bindingV = toDAEExp(comp.binding)
        cls = getClass(comp.classInst)
        ty_attr = list((name(m), binding(m)) for m in getTypeAttributes(cls))
        var_attr = convertVarAttributes(ty_attr, ty, attr)
        makeDAEVar(
          cref,
          ty,
          bindingV,
          attr,
          visibility(node),
          var_attr,
          comp.comment,
          FUNCTION_VARIABLE_CONVERSION_SETTINGS,
          info,
        )
      end

      _ => begin
        Error.assertion(false, getInstanceName() + " got untyped component.", sourceInfo())
        fail()
      end
    end
  end
  return element
end

function convertExternalDecl(
  extDecl::Sections,
  parameters::List{<:DAE.Element},
)::DAE.FunctionDefinition
  local funcDef::DAE.FunctionDefinition
  local decl::DAE.ExternalDecl
  local args::List{DAE.ExtArg}
  local ret_arg::DAE.ExtArg
  @assign funcDef = begin
    @match extDecl begin
      SECTIONS_EXTERNAL(__) => begin
        @assign args = list(convertExternalDeclArg(e) for e in extDecl.args)
        @assign ret_arg = convertExternalDeclOutput(extDecl.outputRef)
        @assign decl = DAE.ExternalDecl.EXTERNALDECL(
          extDecl.name,
          args,
          ret_arg,
          extDecl.language,
          extDecl.ann,
        )
        DAE.FunctionDefinition.FUNCTION_EXT(parameters, decl)
      end
    end
  end
  return funcDef
end

function convertExternalDeclArg(exp::Expression)::DAE.ExtArg
  local arg::DAE.ExtArg
  @assign arg = begin
    local dir::Absyn.Direction
    local cref::ComponentRef
    local e::Expression
    @match exp begin
      CREF_EXPRESSION(cref = cref && COMPONENT_REF_CREF(__)) =>
        begin
          @assign dir =
            P_Prefixes.directionToAbsyn(P_Component.direction(component(cref.node)))
          DAE.ExtArg.EXTARG(
            toDAE(cref),
            dir,
            toDAE(exp.ty),
          )
        end

      SIZE_EXPRESSION(
        exp = CREF_EXPRESSION(
          cref = cref && COMPONENT_REF_CREF(__),
        ),
        dimIndex = SOME(e),
      ) => begin
        DAE.ExtArg.EXTARGSIZE(
          toDAE(cref),
          toDAE(cref.ty),
          toDAE(e),
        )
      end

      _ => begin
        DAE.ExtArg.EXTARGEXP(
          toDAE(exp),
          toDAE(typeOf(exp)),
        )
      end
    end
  end
  return arg
end

function convertExternalDeclOutput(cref::ComponentRef)::DAE.ExtArg
  local arg::DAE.ExtArg
  @assign arg = begin
    local dir::Absyn.Direction
    @match cref begin
      CREF(__) => begin
        @assign dir =
          P_Prefixes.directionToAbsyn(P_Component.direction(component(cref.node)))
        DAE.ExtArg.EXTARG(toDAE(cref), dir, toDAE(cref.ty))
      end

      _ => begin
        DAE.ExtArg.NOEXTARG()
      end
    end
  end
  return arg
end

function makeTypeVars(complexCls::InstNode)::List{DAE.Var}
  local typeVars::List{DAE.Var}
  local comp::Component
  local type_var::DAE.Var
  typeVars = begin
    cls = getClass(complexCls)
    @match cls begin
      INSTANCED_CLASS(restriction = RESTRICTION_RECORD(__)) => begin
        list(makeTypeRecordVar(c) for c in getComponents(cls.elements))
      end
      INSTANCED_CLASS(elements = FLAT_TREE(__)) => begin
        list(
          makeTypeVar(c)
          for
          c in getComponents(cls.elements) if !isOnlyOuter(c)
        )
      end
      _ => begin
        nil
      end
    end
  end
  return typeVars
end

function makeTypeVar(component::InstNode)::DAE.Var
  local typeVar::DAE.Var
  local comp::Component
  local attr::Attributes

  @assign comp = component(resolveOuter(component))
  @assign attr = P_Component.getAttributes(comp)
  @assign typeVar = DAE.TYPES_VAR(
    name(component),
    toDAE(attr, visibility(component)),
    toDAE(getType(comp)),
    toDAE(getBinding(comp)),
    false,
    NONE(),
  )
  return typeVar
end

function makeTypeRecordVar(componentArg::InstNode)::DAE.Var
  local typeVar::DAE.Var
  local comp::Component
  local attr::Attributes
  local vis::VisibilityType
  local binding::Binding
  local bind_from_outside::Bool
  local ty::M_Type
  comp = component(componentArg)
  attr = getAttributes(comp)
  if isConst(comp) && hasBinding(comp)
    vis = Visibility.PROTECTED
  else
    vis = visibility(componentArg)
  end
  binding = getBinding(comp)
  binding = mapExp(binding, stripScopePrefixExp)
  bind_from_outside = parentCount(binding) > 1
  ty = getType(comp)
  ty = mapDims(ty, stripScopePrefixFromDim)
  typeVar = DAE.TYPES_VAR(
    name(componentArg),
    toDAE(attr, vis),
    toDAE(ty),
    toDAE(binding),
    bind_from_outside,
    NONE(),
  )
  return typeVar
end

function stripScopePrefixFromDim(dim::Dimension)::Dimension
  @assign dim = mapExp(dim, stripScopePrefixCrefExp)
  return dim
end

function stripScopePrefixExp(exp::Expression)::Expression
  @assign exp = map(exp, stripScopePrefixCrefExp)
  return exp
end

function stripScopePrefixCrefExp(exp::Expression)::Expression
  @assign () = begin
    @match exp begin
      CREF_EXPRESSION(__) => begin
        @assign exp.cref = stripScopePrefixCref(exp.cref)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return exp
end

function stripScopePrefixCref(cref::ComponentRef)::ComponentRef
  if isSimple(cref)
    return cref
  end
  @assign () = begin
    @match cref begin
      CREF(__) => begin
        if isFromCref(cref.restCref)
          @assign cref.restCref = stripScopePrefixCref(cref.restCref)
        else
          @assign cref.restCref = EMPTY()
        end
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return cref
end
