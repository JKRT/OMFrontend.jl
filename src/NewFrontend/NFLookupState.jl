#= LookupState is used by the lookup to keep track of what state it's in so that the rules for composite name lookup can be enforced. =#
@Uniontype LookupState begin
  @Record LOOKUP_STATE_ERROR begin
    errorState::LookupState
  end
  @Record LOOKUP_STATE_IMPORT begin
  end
  @Record LOOKUP_STATE_PREDEF_CLASS begin
  end
  @Record LOOKUP_STATE_PREDEF_COMP begin
  end
  @Record LOOKUP_STATE_FUNC begin
  end
  @Record LOOKUP_STATE_CLASS begin
  end
  @Record LOOKUP_STATE_PACKAGE begin
  end
  @Record LOOKUP_STATE_COMP_FUNC begin
  end
  @Record LOOKUP_STATE_COMP_CLASS begin
  end
  @Record LOOKUP_STATE_COMP_COMP begin
  end
  @Record LOOKUP_STATE_COMP begin
  end
  @Record LOOKUP_STATE_BEGIN begin
  end
end

@UniontypeDecl LookupStateName
@Uniontype LookupStateName begin
  @Record LOOKUP_STATE_NAME_CREF begin
    cref::Absyn.ComponentRef
  end
  @Record LOOKUP_STATE_NAME_PATH begin
    path::Absyn.Path
  end
end

function secondIdent(name::LookupStateName)::String
  local id::String
  @assign id = begin
    @match name begin
      LOOKUP_STATE_NAME_PATH(__) => begin
        AbsynUtil.pathSecondIdent(name.path)
      end
      LOOKUP_STATE_NAME_CREF(__) => begin
        AbsynUtil.crefSecondIdent(name.cref)
      end
    end
  end
  return id
end

function firstIdent(name::LookupStateName)::String
  local id::String

  @assign id = begin
    @match name begin
      LOOKUP_STATE_NAME_PATH(__) => begin
        AbsynUtil.pathFirstIdent(name.path)
      end

      LOOKUP_STATE_NAME_CREF(__) => begin
        AbsynUtil.crefFirstIdent(name.cref)
      end
    end
  end
  return id
end

function toString(name::LookupStateName)::String
  local str::String

  @assign str = begin
    @match name begin
      LOOKUP_STATE_NAME_PATH(__) => begin
        AbsynUtil.pathString(name.path)
      end

      LOOKUP_STATE_NAME_CREF(__) => begin
        Dump.printComponentRefStr(name.cref)
      end
    end
  end
  return str
end

""" #= This function implements the state machine that checks which transitions are
    valid during composite name lookup, as defined in section 5.3.2 of the
    specification. elementState is expected to be one of COMP,
    LOOKUP_STATE_CLASS, FUNC or LOOKUP_STATE_PACKAGE, indicating what type the found
    element is. The state machine looks like this flow diagram (nodes in
    [brackets] are nodes with an edge to themselves):

       LOOKUP_STATE_BEGIN----------------+-----------------+-------------+
                            |(LOOKUP_STATE_COMP)           |(LOOKUP_STATE_PACKAGE)    |(LOOKUP_STATE_CLASS/FUNC)
                            v                 v             v
           +---------------LOOKUP_STATE_COMP------+----[LOOKUP_STATE_PACKAGE]<->[LOOKUP_STATE_CLASS/FUNC]
           |(LOOKUP_STATE_CLASS|LOOKUP_STATE_PACKAGE) |(LOOKUP_STATE_FUNC)  |(LOOKUP_STATE_COMP)                |(LOOKUP_STATE_COMP)
           |                |        |                      |only if
           v                |        v                      |package-like
      [LOOKUP_STATE_COMP_CLASS]          |   [LOOKUP_STATE_COMP_COMP]<----------------+
           ^(CLASS|LOOKUP_STATE_PACKAGE) |
           |                |
           v(LOOKUP_STATE_FUNC)          |
      [LOOKUP_STATE_COMP_FUNC]<----------+

    There's also LOOKUP_STATE_PREDEF_COMP and LOOKUP_STATE_PREDEF_CLASS for the predefined types
    and components, e.g. Real, time, etc., which are handled as special cases in
    lookupName and bypasses this state machine.
     =#"""
function next2(
  elementState::LookupState,
  currentState::LookupState,
  node::InstNode,
)::LookupState
  local nextState::LookupState

  @assign nextState = begin
    local str::String
    #=  Transitions from BEGIN.
    =#
    @match (elementState, currentState) begin
      (_, LOOKUP_STATE_BEGIN(__)) => begin
        elementState
      end

      (LOOKUP_STATE_COMP(__), LOOKUP_STATE_COMP(__)) => begin
        LOOKUP_STATE_COMP_COMP()
      end

      (LOOKUP_STATE_FUNC(__), LOOKUP_STATE_COMP(__)) => begin
        LOOKUP_STATE_COMP_FUNC()
      end

      (_, LOOKUP_STATE_COMP(__)) => begin
        LOOKUP_STATE_COMP_CLASS()
      end

      (LOOKUP_STATE_COMP(__), LOOKUP_STATE_COMP_COMP(__)) => begin
        LOOKUP_STATE_COMP_COMP()
      end

      (LOOKUP_STATE_COMP(__), LOOKUP_STATE_PACKAGE(__)) => begin
        LOOKUP_STATE_COMP_COMP()
      end

      (_, LOOKUP_STATE_PACKAGE(__)) => begin
        elementState
      end

      (LOOKUP_STATE_COMP(__), LOOKUP_STATE_CLASS(__)) => begin
        LOOKUP_STATE_COMP_COMP()
      end

      (_, LOOKUP_STATE_CLASS(__)) => begin
        elementState
      end

      (LOOKUP_STATE_COMP(__), LOOKUP_STATE_FUNC(__)) => begin
        LOOKUP_STATE_COMP_COMP()
      end

      (_, LOOKUP_STATE_FUNC(__)) => begin
        elementState
      end

      (LOOKUP_STATE_FUNC(__), LOOKUP_STATE_COMP_CLASS(__)) => begin
        LOOKUP_STATE_COMP_FUNC()
      end

      (LOOKUP_STATE_CLASS(__), LOOKUP_STATE_COMP_CLASS(__)) => begin
        LOOKUP_STATE_COMP_CLASS()
      end

      (LOOKUP_STATE_PACKAGE(__), LOOKUP_STATE_COMP_CLASS(__)) => begin
        LOOKUP_STATE_COMP_CLASS()
      end

      (LOOKUP_STATE_FUNC(__), LOOKUP_STATE_COMP_FUNC(__)) => begin
        LOOKUP_STATE_COMP_FUNC()
      end

      (LOOKUP_STATE_CLASS(__), LOOKUP_STATE_COMP_FUNC(__)) => begin
        LOOKUP_STATE_COMP_CLASS()
      end

      (LOOKUP_STATE_PACKAGE(__), LOOKUP_STATE_COMP_FUNC(__)) => begin
        LOOKUP_STATE_COMP_CLASS()
      end

      (LOOKUP_STATE_COMP(__), _) => begin
        LOOKUP_STATE_ERROR(LOOKUP_STATE_COMP_FUNC())
      end

      (_, LOOKUP_STATE_COMP_COMP(__)) => begin
        LOOKUP_STATE_ERROR(LOOKUP_STATE_COMP_COMP())
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() +
          " failed on unknown transition for element " +
          name(node),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return nextState
end

""" #= Returns the lookup state of a given element. =#"""
function elementState(element::SCode.Element)::LookupState
  local state::LookupState
  @assign state = begin
    @match element begin
      SCode.CLASS(restriction = SCode.R_PACKAGE(__)) => begin
        LOOKUP_STATE_PACKAGE()
      end

      SCode.CLASS(restriction = SCode.R_FUNCTION(__)) => begin
        LOOKUP_STATE_FUNC()
      end
      SCode.CLASS(__) => begin
        LOOKUP_STATE_CLASS()
      end
      _ => begin
        Error.assertion(false, getInstanceName() + " got unknown element.", sourceInfo())
        fail()
      end
    end
  end
  return state
end

function nodeState(node::InstNode)::LookupState
  local state::LookupState
  if isComponent(node) || isName(node)
    @assign state = LOOKUP_STATE_COMP()
  else
    @assign state = elementState(definition(node))
  end
  return state
end

""" #= Checks if a found element is protected during lookup, and prints an error if
     the element was not the first part of a name while being protected.
     I.e. P.a is allowed if P is protected, but not e.g. a.P or a.P.b. =#"""
function checkProtection(node::InstNode, currentState::LookupState)
  return @assign () = begin
    @match currentState begin
      LOOKUP_STATE_BEGIN(__) => begin
        ()
      end
      _ => begin
        if isProtected(node)
          Error.addSourceMessage(
            Error.PROTECTED_ACCESS,
            list(name(node)),
            info(node),
          )
          fail()
        end
        ()
      end
    end
  end
end

""" #= Checks that the found name is allowed to be looked up given the current state
     of the name lookup, and returns the new state if it is. Otherwise it will
     print a (hopefully relevant) error message and fail. =#"""
function next(
  node::InstNode,
  currentState::LookupState,
  checkAccessViolations::Bool = true,
)::LookupState
  local nextState::LookupState
  local entry_ty::LookupState
  local el::SCode.Element
  if checkAccessViolations
    checkProtection(node, currentState)
  end
  @assign entry_ty = nodeState(node)
  @assign nextState = next2(entry_ty, currentState, node)
  return nextState
end

""" #= Helper function to assertState, prints out an error when the wrong kind
     of element was found. =#"""
function printFoundWrongTypeError(
  foundState::LookupState,
  expectedState::LookupState,
  name::LookupStateName,
  info::SourceInfo,
)
  local name_str::String
  local found_str::String
  local expected_str::String

  @assign name_str = P_LookupStateName.toString(name)
  @assign found_str = lookupStateString(foundState)
  @assign expected_str = lookupStateString(expectedState)
  return Error.addSourceMessage(
    Error.LOOKUP_FOUND_WRONG_TYPE,
    list(name_str, expected_str, found_str),
    info,
  )
end

""" #= Returns the string representation of a LookupState, with translation. =#"""
function lookupStateString(state::LookupState)::String
  local str::String

  @assign str = begin
    @match state begin
      LOOKUP_STATE_BEGIN(__) => begin
        "<begin>"
      end

      LOOKUP_STATE_COMP(__) => begin
        System.gettext("component")
      end

      LOOKUP_STATE_COMP_COMP(__) => begin
        System.gettext("component")
      end

      LOOKUP_STATE_COMP_CLASS(__) => begin
        System.gettext("class")
      end

      LOOKUP_STATE_COMP_FUNC(__) => begin
        System.gettext("function")
      end

      LOOKUP_STATE_PACKAGE(__) => begin
        System.gettext("package")
      end

      LOOKUP_STATE_CLASS(__) => begin
        System.gettext("class")
      end

      LOOKUP_STATE_FUNC(__) => begin
        System.gettext("function")
      end

      LOOKUP_STATE_PREDEF_COMP(__) => begin
        System.gettext("component")
      end

      LOOKUP_STATE_PREDEF_CLASS(__) => begin
        System.gettext("class")
      end
    end
  end
  return str
end

function isError(state::LookupState)::Bool
  local isError::Bool

  @assign isError = begin
    @match state begin
      LOOKUP_STATE_ERROR(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isError
end

function assertState(
  endState::LookupState,
  expectedState::LookupState,
  node::InstNode,
  name::LookupStateName,
  info::SourceInfo,
)
  return @assign () = begin
    local name_str::String
    local info2::SourceInfo
    #=  Found the expected kind of element.
    =#
    @match (endState, expectedState) begin
      (LOOKUP_STATE_COMP(__), LOOKUP_STATE_COMP(__)) => begin
        ()
      end

      (LOOKUP_STATE_COMP_COMP(__), LOOKUP_STATE_COMP(__)) => begin
        ()
      end

      (LOOKUP_STATE_PREDEF_COMP(__), LOOKUP_STATE_COMP(__)) => begin
        ()
      end

      (LOOKUP_STATE_FUNC(__), LOOKUP_STATE_COMP(__)) => begin
        ()
      end

      (LOOKUP_STATE_COMP_FUNC(__), LOOKUP_STATE_COMP(__)) => begin
        ()
      end

      (LOOKUP_STATE_PACKAGE(__), LOOKUP_STATE_CLASS(__)) => begin
        ()
      end

      (LOOKUP_STATE_CLASS(__), LOOKUP_STATE_CLASS(__)) => begin
        ()
      end

      (LOOKUP_STATE_PREDEF_CLASS(__), LOOKUP_STATE_CLASS(__)) => begin
        ()
      end

      (LOOKUP_STATE_FUNC(__), LOOKUP_STATE_CLASS(__)) => begin
        ()
      end

      (LOOKUP_STATE_FUNC(__), LOOKUP_STATE_FUNC(__)) => begin
        ()
      end

      (LOOKUP_STATE_COMP_FUNC(__), FUNC(__)) => begin
        ()
      end

      (LOOKUP_STATE_CLASS(__), LOOKUP_STATE_FUNC(__)) where {(isCallableType(node))} => begin
        ()
      end

      (LOOKUP_STATE_COMP(__), LOOKUP_STATE_FUNC(__)) where {(isCallableComponent(node))} => begin
        ()
      end

      (LOOKUP_STATE_COMP_COMP(__), LOOKUP_STATE_FUNC(__)) where {(isCallableComponent(node))} => begin
        ()
      end

      (LOOKUP_STATE_COMP_CLASS(__), LOOKUP_STATE_FUNC(__)) => begin
        #=  Found a class via a component, but expected a function.
        =#
        printFoundWrongTypeError(endState, expectedState, name, info)
        fail()
      end

      (LOOKUP_STATE_COMP_FUNC(__), _) => begin
        #=  Found a function via a component, but didn't expect a function.
        =#
        @assign name_str = P_LookupStateName.toString(name)
        Error.addSourceMessage(Error.FOUND_FUNC_NAME_VIA_COMP_NONCALL, list(name_str), info)
        fail()
      end

      (LOOKUP_STATE_COMP_CLASS(__), _) => begin
        #=  Found a class via a component. Only components and functions are
        =#
        #=  allowed to be lookup up via a component.
        =#
        Error.addSourceMessage(
          Error.FOUND_CLASS_NAME_VIA_COMPONENT,
          list(P_LookupStateName.toString(name)),
          info,
        )
        fail()
      end

      (LOOKUP_STATE_ERROR(errorState = LOOKUP_STATE_COMP_FUNC(__)), LOOKUP_STATE_FUNC(__)) => begin
        #=  Invalid form when looking for a function via a component, only
        =#
        #=  c.C1...Cn.f is allowed.
        =#
        @assign name_str = name(node)
        @assign info2 = info(node)
        Error.addSourceMessage(Error.NON_CLASS_IN_COMP_FUNC_NAME, list(name_str), info2)
        fail()
      end

      (LOOKUP_STATE_ERROR(errorState = LOOKUP_STATE_COMP_FUNC(__)), LOOKUP_STATE_COMP(__)) => begin
        #=  Found class when looking up a composite component name.
        =#
        @assign name_str = name(node)
        Error.addSourceMessage(
          Error.CLASS_IN_COMPOSITE_COMP_NAME,
          list(name_str, P_LookupStateName.toString(name)),
          info,
        )
        fail()
      end

      (LOOKUP_STATE_ERROR(errorState = LOOKUP_STATE_COMP_FUNC(__)), _) => begin
        #=  Found class via composite component name when actually looking for a class.
        =#
        @assign name_str = name(node)
        Error.addSourceMessage(
          Error.LOOKUP_CLASS_VIA_COMP_COMP,
          list(name_str, P_LookupStateName.toString(name)),
          info,
        )
        fail()
      end

      (LOOKUP_STATE_ERROR(errorState = LOOKUP_STATE_COMP_COMP(__)), LOOKUP_STATE_COMP(__)) => begin
        #=  Found class when looking up a composite component name.
        =#
        @assign name_str = name(node)
        Error.addSourceMessage(
          Error.CLASS_IN_COMPOSITE_COMP_NAME,
          list(name_str, P_LookupStateName.toString(name)),
          info,
        )
        fail()
      end

      (LOOKUP_STATE_ERROR(errorState = LOOKUP_STATE_COMP_COMP(__)), _) => begin
        #=  Found class via composite component name when actually looking for a class.
        =#
        @assign name_str = name(node)
        Error.addSourceMessage(
          Error.LOOKUP_CLASS_VIA_COMP_COMP,
          list(name_str, P_LookupStateName.toString(name)),
          info,
        )
        fail()
      end

      (LOOKUP_STATE_ERROR(errorState = LOOKUP_STATE_IMPORT(__)), _) => begin
        #=  Found import as part of a composite name where it's not the first
        =#
        #=  identifier, e.g. A.B.C where B or C are imported names.
        =#
        @assign name_str = name(node)
        Error.addSourceMessage(
          Error.IMPORT_IN_COMPOSITE_NAME,
          list(name_str, P_LookupStateName.toString(name)),
          info,
        )
        fail()
      end

      (_, LOOKUP_STATE_IMPORT(__)) => begin
        ()
      end

      _ => begin
        printFoundWrongTypeError(endState, expectedState, name, info)
        fail()
      end
    end
  end
end

function isClass(state::LookupState)::Bool
  local isClass::Bool

  @assign isClass = begin
    @match state begin
      LOOKUP_STATE_COMP_CLASS(__) => begin
        true
      end

      LOOKUP_STATE_CLASS(__) => begin
        true
      end

      LOOKUP_STATE_PREDEF_CLASS(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isClass
end

function isFunction(state::LookupState, node::InstNode)::Bool
  local isFunction::Bool

  @assign isFunction = begin
    @match state begin
      LOOKUP_STATE_FUNC(__) => begin
        true
      end

      LOOKUP_STATE_COMP_FUNC(__) => begin
        true
      end

      LOOKUP_STATE_CLASS(__) => begin
        isCallableType(node)
      end

      LOOKUP_STATE_COMP(__) => begin
        isCallableComponent(node)
      end

      LOOKUP_STATE_COMP_COMP(__) => begin
        isCallableComponent(node)
      end

      _ => begin
        false
      end
    end
  end
  return isFunction
end

function isCallableComponent(node::InstNode)::Bool
  local callable::Bool
  @assign callable = isFunction(getClass(node))
  return callable
end

function isCallableType(node::InstNode)::Bool
  local callable::Bool
  local def::SCode.Element = definition(node)
  @assign callable = SCodeUtil.isRecord(def) || SCodeUtil.isOperator(def)
  return callable
end

function assertImport(
  endState::LookupState,
  node::InstNode,
  name::Absyn.Path,
  info::SourceInfo,
)
  return assertState(
    endState,
    LOOKUP_STATE_IMPORT(),
    node,
    LOOKUP_STATE_NAME_PATH(name),
    info,
  )
end

function assertComponent(
  endState::LookupState,
  node::InstNode,
  name::Absyn.ComponentRef,
  info::SourceInfo,
)
  return assertState(
    endState,
    LOOKUP_STATE_COMP(),
    node,
    LOOKUP_STATE_NAME_CREF(name),
    info,
  )
end

function assertFunction(
  endState::LookupState,
  node::InstNode,
  name::Absyn.Path,
  info::SourceInfo
)
  return assertState(
    endState,
    LOOKUP_STATE_FUNC(),
    node,
    LOOKUP_STATE_NAME_CREF(name),
    info
  )
end

function assertClass(
  endState::LookupState,
  node::InstNode,
  name::Absyn.Path,
  info::SourceInfo
)
  return assertState(
    endState,
    LOOKUP_STATE_CLASS(),
    node,
    LOOKUP_STATE_NAME_PATH(name),
    info
  )
end
