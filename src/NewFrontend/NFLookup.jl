MatchType = #= Enumeration =# (() -> begin
                               FOUND  = 1
                               NOT_FOUND  = 2
                               PARTIAL  = 3
                               ()->(FOUND ;NOT_FOUND ;PARTIAL )
                               end)()
const MatchTypeTy = Int

function lookupClassName(name::Absyn.Path, scope::InstNode, info::SourceInfo, checkAccessViolations::Bool = true) ::InstNode
  local node::InstNode
  local state::LookupState
  (node, state) = lookupNameWithError(name, scope, info, "error placeholderx", checkAccessViolations)
  assertClass(state, node, name, info)
  return node
end

function lookupBaseClassName(name::Absyn.Path, scope::InstNode, info::SourceInfo) ::List{InstNode}
  local nodes::List{InstNode}
  local state::LookupState
  try
    (nodes, state) = lookupNames(name, scope)
  catch e
    @error "Error looking up base class $e"
    #Error.addSourceMessage(Error.LOOKUP_BASECLASS_ERROR, list(AbsynUtil.pathString(name), scopeName(scope)), info)
    throw(e)
  end
  assertClass(state, listHead(nodes), name, info)
  nodes
end

function lookupComponent(cref::Absyn.ComponentRef, scope::InstNode #= The scope to look in. =#, info::SourceInfo) ::Tuple{ComponentRef, InstNode}
  @debug "Calling lookup component! cref: $cref"
  local foundScope::InstNode #= The scope the cref was found in. =#
  local foundCref::ComponentRef
  local state::LookupState
  local nodeVar::InstNode
  try
    (foundCref, foundScope, state) = lookupCref(cref, scope)
    nodeVar = node(foundCref)
    @match false = isName(nodeVar)
  catch e
    # Error.addSourceMessageAndFail(Error.LOOKUP_VARIABLE_ERROR, list(Dump.printComponentRefStr(cref), scopeName(scope)), info)
    #treee = lookupTree(scope.cls.x.elements)
#    @error "Lookupvariable error for cref:$cref in scope $(scope.name). Error: $e"
#    @error "Our tree was $(LookupTree.printTreeStr(treee))"
    #    @error "Repr3: $treee"
    @error "Variable lookup error: $(e)"
    fail()
  end
  state = fixTypenameState(nodeVar, state)
  assertComponent(state, nodeVar, cref, info)
  (foundCref, foundScope #= The scope the cref was found in. =#)
end

function lookupConnector(cref::Absyn.ComponentRef, scope::InstNode #= The scope to look in. =#, info::SourceInfo) ::Tuple{ComponentRef, InstNode}
  local foundScope::InstNode #= The scope the cref was found in. =#
  local foundCref::ComponentRef

  local state::LookupState
  local nodeVar::InstNode

  try
    (foundCref, foundScope, state) = lookupCref(cref, scope)
  catch e
    @error "Error looking up connector: $e"
    #Error.addSourceMessageAndFail(Error.LOOKUP_VARIABLE_ERROR, list(Dump.printComponentRefStr(cref), scopeName(scope)), info)
    throw("Lookup error") #My addition
  end
  nodeVar = node(foundCref)
  state = fixTypenameState(nodeVar, state)
  assertComponent(state, nodeVar, cref, info)
  (foundCref, foundScope #= The scope the cref was found in. =#)
end

function fixTypenameState(n::InstNode, state::LookupState) ::LookupState
  local ty::NFType
  if isClass(n)
    ty = getType(expand(n))
    state = begin
      @match ty begin
        TYPE_ENUMERATION(__)  => begin
          LOOKUP_STATE_COMP()
        end
        TYPE_BOOLEAN(__)  => begin
          LOOKUP_STATE_COMP()
        end
        _  => begin
          state
        end
      end
    end
  end
  state
end

""" #= Looks up a component in the local scope, without searching in any enclosing
                 scopes. The found scope is returned since it can be different from the given
                 scope in the case where the cref refers to an outer component. =#"""
                   function lookupLocalComponent(cref::Absyn.ComponentRef, scope::InstNode #= The scope to look in. =#, info::SourceInfo) ::Tuple{ComponentRef, InstNode}
                     local foundScope::InstNode #= The scope the cref was found in. =#
                     local foundCref::ComponentRef
                     local state::LookupState
                     local node::InstNode
                     (foundCref, foundScope, state) = lookupLocalCref(cref, scope, info)
                     assertComponent(state, node(foundCref), cref, info)
                     (foundCref, foundScope #= The scope the cref was found in. =#)
                   end

function lookupFunctionName(cref::Absyn.ComponentRef, scope::InstNode #= The scope to look in. =#, info::SourceInfo) ::Tuple{ComponentRef, InstNode}
  local foundScope::InstNode
  local foundCref::ComponentRef
  local state::LookupState
  local nodeVar::InstNode
  try
    (foundCref, foundScope, state) = lookupCref(cref, scope)
    nodeVar = node(foundCref)
    @match false = isName(nodeVar)
  catch e
    @error "Function lookup error for function $cref. With exception $e"
  end
  (foundCref, state) = fixExternalObjectCall(nodeVar, foundCref, state)
  assertFunction(state, nodeVar, cref, info)
  (foundCref, foundScope)
end

function lookupFunctionNameSilent(cref::Absyn.ComponentRef, scope::InstNode #= The scope to look in. =#) ::Tuple{ComponentRef, InstNode}
  local foundScope::InstNode
  local foundCref::ComponentRef

  local state::LookupState
  local node::InstNode

  (foundCref, foundScope, state) = lookupCref(cref, scope)
  node = node(foundCref)
  (foundCref, state) = fixExternalObjectCall(node, foundCref, state)
  @match true = isFunction(state, node)
  (foundCref, foundScope)
end

""" 
  Changes calls to external objects so that the constructor is called instead,
                 i.e. a call such as
                   'ExtObj eo = ExtObj(...)'
                 is changed to
                   'ExtObj eo = ExtObj.constructor(...)' 
"""
 function fixExternalObjectCall(node::InstNode, cref::ComponentRef, state::LookupState) ::Tuple{ComponentRef, LookupState}
   local cls::Class
   local constructor::InstNode
   #=  If it's not a class it can't be an external object.
   =#
   if ! isClass(state)
     return (cref, state)
   end
   Inst.expand(node)
   cls = getClass(node)
   () = begin
     @match cls begin
       PARTIAL_BUILTIN(ty = TYPE_COMPLEX(complexTy = COMPLEX_EXTERNAL_OBJECT(constructor = constructor)))  => begin
         cref = prefixCref(constructor, TYPE_UNKNOWN(), nil, cref)
         state = LOOKUP_STATE_FUNC()
         ()
       end
       _  => begin
         ()
       end
     end
   end
   (cref, state)
 end

function lookupImport(name::Absyn.Path, scope::InstNode, info::SourceInfo) ::InstNode
  local element::InstNode

  local state::LookupState

  (element, state) = lookupNameWithError(name, topScope(scope), info, Error.LOOKUP_IMPORT_ERROR)
  assertImport(state, element, name, info)
  element
end

function lookupCrefWithError(cref::Absyn.ComponentRef, scope::InstNode, info::SourceInfo, errMsg) ::Tuple{ComponentRef, InstNode, LookupState}
  local state::LookupState
  local foundScope::InstNode
  local foundCref::ComponentRef
  try
    (foundCref, foundScope, state) = lookupCref(cref, scope)
  catch
    #Error.addSourceMessage(errMsg, list(Dump.printComponentRefStr(cref), scopeName(scope)), info)
    @error "Failed to locate $cref"
    fail()
  end
  (foundCref, foundScope, state)
end

""" #= This function will look up an Absyn.ComponentRef in the given scope, and
                   construct a ComponentRef from the found nodes. The scope where the first part
                   of the cref was found will also be returned. =#"""
function lookupCref(cref::Absyn.ComponentRef, scope::InstNode #= The scope to look in. =#) ::Tuple{ComponentRef, InstNode, LookupState}
  local state::LookupState
  local foundScope::InstNode #= The scope where the first part of the cref was found. =#
  local foundCref::ComponentRef

  local node::InstNode

  (foundCref, foundScope, state) = begin
    @match cref begin
      Absyn.CREF_IDENT(__)  => begin
        (_, foundCref, foundScope, state) = lookupSimpleCref(cref.name, cref.subscripts, scope)
        (foundCref, foundScope, state)
      end

      Absyn.CREF_QUAL(__)  => begin
        (node, foundCref, foundScope, state) = lookupSimpleCref(cref.name, cref.subscripts, scope)
        (foundCref, foundScope, state) = lookupCrefInNode(cref.componentRef, node, foundCref, foundScope, state)
        (foundCref, foundScope, state)
      end

      Absyn.CREF_FULLYQUALIFIED(__)  => begin
        lookupCref(cref.componentRef, topScope(scope))
      end

      Absyn.WILD(__)  => begin
        (WILD(), scope, P_LookupState.LOOKUP_STATE_PREDEF_COMP())
      end

      Absyn.ALLWILD(__)  => begin
        (WILD(), scope, P_LookupState.LOOKUP_STATE_PREDEF_COMP())
      end
    end
  end
  (foundCref, foundScope #= The scope where the first part of the cref was found. =#, state)
end

""" #= Looks up a cref in the local scope without going into any enclosing scopes. =#"""
function lookupLocalCref(cref::Absyn.ComponentRef, scope::InstNode #= The scope to look in. =#, info::SourceInfo) ::Tuple{ComponentRef, InstNode, LookupState}
  local state::LookupState
  local foundScope::InstNode #= The scope where the first part of the cref was found. =#
  local foundCref::ComponentRef
  local match_ty::MatchTypeTy
  local node::InstNode
  (foundCref, foundScope, state) = begin
    local found_scope::InstNode
    @matchcontinue cref begin
      Absyn.CREF_IDENT(__)  => begin
        (node, foundScope) = lookupLocalSimpleCref(cref.name, scope)
        state = nodeState(node)
        (fromAbsyn(node, cref.subscripts), foundScope, state)
      end

      Absyn.CREF_QUAL(__)  => begin
        (node, foundScope) = lookupLocalSimpleCref(cref.name, scope)
        state = nodeState(node)
        foundCref = fromAbsyn(node, cref.subscripts)
        (foundCref, foundScope, state) = lookupCrefInNode(cref.componentRef, node, foundCref, foundScope, state)
        (foundCref, foundScope, state)
      end

      _  => begin
        #Error.addSourceMessage(Error.LOOKUP_VARIABLE_ERROR, list(Dump.printComponentRefStr(cref), scopeName(scope)), info)
        @error "Lookup error $cref"
        fail()
      end
    end
  end
  (foundCref, foundScope #= The scope where the first part of the cref was found. =#, state)
end

""" #= Looks up the corresponding inner node given an outer node. =#"""
function lookupInner(outerNode::InstNode, scope::InstNode) ::InstNode
  local innerNode::InstNode

  local name::String = name(outerNode)
  local cur_scope::InstNode = scope
  local prev_scope::InstNode = scope

  while ! isEmpty(cur_scope)
    try
      innerNode = resolveOuter(lookupElement(name, getClass(cur_scope)))
      @match true = isInner(innerNode)
      return innerNode
    catch e
#      @error "DBG error $e"
      prev_scope = cur_scope
      cur_scope = derivedParent(cur_scope)
    end
  end
  innerNode = generateInner(outerNode, prev_scope)
  innerNode
end

""" #= Looks up a name in the given scope, without continuing the search in any
                 enclosing scopes if the name isn't found. =#"""
function lookupLocalSimpleName(n::String, scope::InstNode) ::Tuple{InstNode, Bool}
  local isImport::Bool = false
  local node::InstNode
  @debug "Looking up simple name $n"
  (node, isImport) = lookupElement(n, getClass(scope))
  @debug "We lookup an element"
  node = resolveInner(node)
  return (node, isImport)
end

function lookupSimpleName(nameStr::String, scope::InstNode) ::InstNode
  local node::InstNode
  local cur_scope::InstNode = scope
  for i in 1:Global.recursionDepthLimit
    try
      (node, _) = lookupLocalSimpleName(nameStr, cur_scope)
      @debug "The node $nameStr is resolved in some scope"
      return node
    catch e
#      @error "DBG error with $(e)"
      if nameStr == name(cur_scope) && isClass(cur_scope)
        node = cur_scope
        return node
      end
      cur_scope = parentScope(cur_scope)
    end
  end
  @error "Failed to lookup simple name for $nameStr in scope:$scope"
  fail()
end

function lookupNameWithError(name::Absyn.Path, scope::InstNode, info::SourceInfo, errorType, checkAccessViolations::Bool = true) ::Tuple{InstNode, LookupState}
  local state::LookupState
  local node::InstNode
  try
    (node, state) = lookupName(name, scope, checkAccessViolations)
  catch e
    #   Error.addSourceMessage(errorType, list(AbsynUtil.pathString(name), scopeName(scope)), info)
    @error "Lookup error for path: $(AbsynUtil.pathString(name)) in the scope $(scopeName(scope))
       with the following error: $(e)"
    throw(e)
  end
  (node, state)
end

function lookupName(name::Absyn.Path, scope::InstNode, checkAccessViolations::Bool) ::Tuple{InstNode, LookupState}
  local state::LookupState
  local node::InstNode
  (node, state) = begin
    @match name begin
      Absyn.IDENT(__)  => begin
        lookupFirstIdent(name.name, scope)
      end
      Absyn.QUALIFIED(__)  => begin
        (node, state) = lookupFirstIdent(name.name, scope)
        lookupLocalName(name.path, node, state, checkAccessViolations, refEqual(node, scope))
      end
      Absyn.FULLYQUALIFIED(__)  => begin
        lookupName(name.path, topScope(scope), checkAccessViolations)
      end
    end
  end
  @debug "Returning in lookup name"
  (node, state)
end

function lookupNames(name::Absyn.Path, scope::InstNode) ::Tuple{List{InstNode}, LookupState}
  @debug "Calling lookupNames with path: $name"
  local state::LookupState
  local nodes::List{InstNode}
  (nodes, state) = begin
    local node::InstNode
    #=  Simple name, look it up in the given scope. =#
    @match name begin
      Absyn.IDENT(__)  => begin
        (node, state) = lookupFirstIdent(name.name, scope)
        (list(node), state)
      end
      Absyn.QUALIFIED(__)  => begin
        (node, state) = lookupFirstIdent(name.name, scope)

        lookupLocalNames(name.path, node, list(node), state, refEqual(node, scope))
      end

      Absyn.FULLYQUALIFIED(__)  => begin
        lookupNames(name.path, topScope(scope))
      end
    end
  end
  #=  Fully qualified path, start from top scope.
  =#
  @debug "Done looking up names"
  (nodes, state)
end

""" #= Looks up the first part of a name. =#"""
function lookupFirstIdent(name::String, scope::InstNode) ::Tuple{InstNode, LookupState}
  local state::LookupState
  local node::InstNode
  try
    node = lookupSimpleBuiltinName(name)
    state = LOOKUP_STATE_PREDEF_CLASS()
  catch e
    node = lookupSimpleName(name, scope)
    state = nodeState(node)
  end
  (node, state)
end

""" #= Looks up a path in the given scope, without continuing the search in any
                 enclosing scopes if the path isn't found. =#"""
function lookupLocalName(name::Absyn.Path, node::InstNode, state::LookupState, checkAccessViolations::Bool = true, selfReference::Bool = false) ::Tuple{InstNode, LookupState}
  local is_import::Bool
  if ! isClass(node)
    state =  LOOKUP_STATE_COMP_CLASS()
    return (node, state)
  end
  if ! selfReference
    node = instPackage(node)
  end
  #=  Look up the path in the scope.
  =#
  () = begin
    @match name begin
      Absyn.IDENT(__)  => begin
        (node, is_import) = lookupLocalSimpleName(name.name, node)
        @debug "HERE WE ARE!"
        if is_import
          state = LOOKUP_STATE_ERROR(LOOKUP_STATE_IMPORT())
        else
          state = next(node, state, checkAccessViolations)
        end
        ()
      end

      Absyn.QUALIFIED(__)  => begin
        (node, is_import) = lookupLocalSimpleName(name.name, node)
        if is_import
          state = LOOKUP_STATE_ERROR(LOOKUP_STATE_IMPORT())
        else
          state = next(node, state, checkAccessViolations)
          (node, state) = lookupLocalName(name.path, node, state, checkAccessViolations)
        end
        ()
      end

      _  => begin
        #Error.assertion(false, getInstanceName() + " was called with an invalid path.", sourceInfo())
        fail()
      end
    end
  end
  (node, state)
end

""" #= Looks up a path in the given scope, without continuing the search in any
                 enclosing scopes if the path isn't found. =#"""
function lookupLocalNames(name::Absyn.Path, scope::InstNode, nodes::List{<:InstNode}, state::LookupState, selfReference::Bool = false) ::Tuple{List{InstNode}, LookupState}
  local node::InstNode = scope
  if ! isClass(scope)
    state = LOOKUP_STATE_COMP_CLASS()
    return (nodes, state)
  end
  if ! selfReference
    node = instPackage(node)
  end
  (nodes, state) = begin
    @match name begin
      Absyn.IDENT(__)  => begin
        (node, _) = lookupLocalSimpleName(name.name, node)
        @debug "Here we are!"
        state = next(node, state)
        (_cons(node, nodes), state)
      end

      Absyn.QUALIFIED(__)  => begin
        (node, _) = lookupLocalSimpleName(name.name, node)
        state = next(node, state)
        lookupLocalNames(name.path, node, _cons(node, nodes), state)
      end

      _  => begin
        #                           Error.assertion(false, getInstanceName() + " was called with an invalid path.", sourceInfo())
        fail()
      end
    end
  end
  (nodes, state)
end

function lookupSimpleBuiltinName(name::String) ::InstNode
  local builtin::InstNode
  @debug "Calling lookupSimpleBuiltinName with $name"
  builtin = begin
    @match name begin
      "Real"  => begin
        NFBuiltin.REAL_NODE
      end
      "Integer"  => begin
        NFBuiltin.INTEGER_NODE
      end
      "Boolean"  => begin
        NFBuiltin.BOOLEAN_NODE
      end
      "String"  => begin
        NFBuiltin.STRING_NODE
      end
      "Clock"  => begin
        NFBuiltin.CLOCK_NODE
      end
      "polymorphic"  => begin
        NFBuiltin.POLYMORPHIC_NODE
      end
    end
  end
  builtin
end

function lookupSimpleBuiltinCref(name::String, subs::List{<:Absyn.Subscript}) ::Tuple{InstNode, ComponentRef, LookupState}
  local state::LookupState
  local cref::ComponentRef
  local node::InstNode
  @debug "Looking up $name in lookupSimpleBuiltinCref"
  (node, cref, state) = begin
    @match name begin
      "time"  => begin
        (NFBuiltin.TIME, NFBuiltin.TIME_CREF, LOOKUP_STATE_PREDEF_COMP())
      end

      "Boolean"  => begin
        (NFBuiltin.BOOLEAN_NODE, NFBuiltin.BOOLEAN_CREF, PREDEF_CLASS())
      end

      "Integer"  => begin
        (NFBuiltinFuncs.INTEGER_NODE, NFBuiltinFuncs.INTEGER_CREF, FUNC())
      end

      "String"  => begin
        (NFBuiltinFuncs.STRING_NODE, NFBuiltinFuncs.STRING_CREF, FUNC())
      end

      "Clock" where (Config.synchronousFeaturesAllowed())  => begin
        (NFBuiltinFuncs.CLOCK_NODE, NFBuiltinFuncs.CLOCK_CREF, FUNC())
      end
    end
  end
  if ! listEmpty(subs)
    cref = setSubscripts(list(SUBSCRIPT_RAW_SUBSCRIPT(s) for s in subs), cref)
  end
  (node, cref, state)
end

""" #= This function look up a simple name as a cref in a given component. =#"""
function lookupSimpleCref(name::String, subs::List{<:Absyn.Subscript}, scope::InstNode) ::Tuple{InstNode, ComponentRef, InstNode, LookupState}
  local state::LookupState
  local foundScope::InstNode = scope
  local cref::ComponentRef
  local node::InstNode
  local is_import::Bool
  try
    (node, cref, state) = lookupSimpleBuiltinCref(name, subs)
    foundScope = topScope(foundScope)
  catch e
    @debug "Searching for scope in lookupSimplecref.. with $(typeof(scope))"
#    @error "Another DBG error message: $(e)"
    for i in 1:Global.recursionDepthLimit
      try
        @debug "Searching..."
        (node, is_import) = begin
          @match foundScope begin
            IMPLICIT_SCOPE(__)  => begin
              (lookupIterator(name, foundScope.locals), false)
            end
            CLASS_NODE(__)  => begin
              @debug "Hit CLASS_NODE. Fetching class"
              c = getClass(foundScope)
              @debug "Class fetched. Looking up element"
              e = lookupElement(name, c)
              @debug "After looking up element"
              e
            end
            COMPONENT_NODE(__)  => begin
              lookupElement(name, getClass(foundScope))
            end
            INNER_OUTER_NODE(__)  => begin
              lookupElement(name, getClass(foundScope.innerNode))
            end
          end
        end
        @debug "Checking imports and other things.."
        if is_import
          foundScope = parent(node)
        elseif isInnerOuterNode(node)
          @debug "Not a import checking inner"
          node = resolveInner(node)
          foundScope = parent(node)
        end
        @debug "Not inner outer. Checking state"
        state = nodeState(node)
        @debug "State checked. Checking fromAbsyn"
        cref = fromAbsyn(node, subs)
        @debug "After from absyn. Returning..."
        return (node, cref, foundScope, state)
      catch e
        foundScope = parentScope(foundScope)
#        @error "DBG error message to be removed since the error is used for control flow: $(e)"
        #fail()
      end
    end
    #    Error.addMessage(Error.RECURSION_DEPTH_REACHED, list(String(Global.recursionDepthLimit), scopeName(foundScope)))
    @error "Recursion depth reached failing.."
    fail()
  end
  (node, cref, foundScope, state)
end

""" #= This function look up a simple name as a cref in a given component, without
                 searching in any enclosing scope. =#"""
                   function lookupLocalSimpleCref(name::String, scope::InstNode) ::Tuple{InstNode, InstNode}
                     local foundScope::InstNode = scope
                     local node::InstNode

                     local is_import::Bool

                     (node, is_import) = begin
                       @match foundScope begin
                         IMPLICIT_SCOPE(__)  => begin
                           (lookupIterator(name, foundScope.locals), false)
                         end

                         CLASS_NODE(__)  => begin
                           lookupElement(name, getClass(foundScope))
                         end

                         COMPONENT_NODE(__)  => begin
                           lookupElement(name, getClass(foundScope))
                         end

                         INNER_OUTER_NODE(__)  => begin
                           lookupElement(name, getClass(foundScope.innerNode))
                         end
                       end
                     end
                     if is_import
                       foundScope = parent(node)
                     elseif isInnerOuterNode(node)
                       node = resolveInner(node)
                       foundScope = parent(node)
                     end
                     #=  If the node is an outer node, return the inner instead.
                     =#
                     (node, foundScope)
                   end

function lookupIterator(iteratorName::String, iterators::List{<:InstNode})::InstNode
  local it::InstNode
  for i in iterators
    if iteratorName == name(i)
#      @info "Iterator located"
      it = i
      return it
    end
  end
  fail()
#  throw("Iterator lookup error") #Addition by me, John May 2021
end

function lookupCrefInNode(cref::Absyn.ComponentRef #=modification-040321=#, node::InstNode, foundCref::ComponentRef, foundScope::InstNode, state::LookupState) ::Tuple{ComponentRef, InstNode, LookupState}
  local scope::InstNode
  local n::InstNode
  local name::String
  local cls::Class
  local is_import::Bool
  if isError(state)
    return (foundCref, foundScope, state)
  end
  scope = begin
    @match node begin
      CLASS_NODE(__)  => begin
        Inst.instPackage(node)
      end
      _  => begin
        node
      end
    end
  end
  name = AbsynUtil.crefFirstIdent(cref)
  cls = getClass(scope)
  try
    (n, is_import) = lookupElement(name, cls)
  catch e
    @error "DBG Error $e"
    @match true = isComponent(node)
    @match true = isExpandableConnectorClass(cls)
    foundCref = fromAbsynCref(cref, foundCref)
    return (foundCref, foundScope, state)
  end
  if is_import
    state = P_LookupState.ERROR(P_LookupState.IMPORT())
    foundCref = fromAbsyn(n, nil, foundCref)
    return (foundCref, foundScope, state)
  end
  (n, foundCref, foundScope) = resolveInnerCref(n, foundCref, foundScope)
  state = next(n, state)
  (foundCref, foundScope, state) = begin
    @match cref begin
      Absyn.CREF_IDENT(__)  => begin
        (fromAbsyn(n, cref.subscripts, foundCref), foundScope, state)
      end

      Absyn.CREF_QUAL(__)  => begin
        foundCref = fromAbsyn(n, cref.subscripts, foundCref)
        lookupCrefInNode(cref.componentRef, n, foundCref, foundScope, state)
      end
    end
  end
  (foundCref, foundScope, state)
end

""" #= If given an outer node, resolves it to the corresponding inner node and
                     collapses the given cref so that it refers to the correct node. The scope a
                   cref is found in may also change if the inner is outside the scope found by
                     lookupCref. =#"""
                       function resolveInnerCref(node::InstNode, cref::ComponentRef, foundScope::InstNode) ::Tuple{InstNode, ComponentRef, InstNode}
                         local prev_node::InstNode
                         local scope::InstNode
                         if isInnerOuterNode(node)
                           node = resolveInner(node)
                           scope = parent(node)
                           while ! isEmpty(cref)
                             if referenceEq(node(cref), scope)
                               break
                             else
                               cref = rest(cref)
                             end
                           end
                           if isEmpty(cref)
                             foundScope = scope
                           end
                         end
                         (node, cref, foundScope)
                       end

""" #= Generates an inner element given an outer one, or returns the already
                   generated inner element if one has already been generated. =#"""
                     function generateInner(outerNode::InstNode, topScope::InstNode) ::InstNode
                       local innerNode::InstNode
                       local cache::CachedData
                       local name::String
                       local inner_node_opt::Option{InstNode}
                       local inner_node::InstNode
                       cache = getInnerOuterCache(topScope)
                       () = begin
                         @match cache begin
                           P_CachedData.TOP_SCOPE(__)  => begin
                             name = name(outerNode)
                             inner_node_opt = NodeTree.getOpt(cache.addedInner, name)
                             if isSome(inner_node_opt)
                               @match SOME(innerNode) = inner_node_opt
                             else
                               innerNode = makeInnerNode(outerNode)
                               innerNode = setParent(cache.rootClass, innerNode)
                               #= Complex assign=#@assign cache.addedInner = NodeTree.add(cache.addedInner, name, innerNode)
                               setInnerOuterCache(topScope, cache)
                             end
                             ()
                           end

                           _  => begin
#                             Error.assertion(false, getInstanceName() + " got top node with missing cache", sourceInfo())
                             fail()
                           end
                         end
                       end
                       innerNode
                     end

"Returns a copy of the given node where the element definition has been
                   changed to have the inner prefix."
function makeInnerNode(node::InstNode) ::InstNode
  node = begin
    local def::SCode.Element
    local prefs::SCode.Prefixes
    local comp::Component
    @match node begin
      CLASS_NODE(definition = def && SCode.CLASS(prefixes = prefs))  => begin
        #= Complex assign=#@assign prefs.innerOuter = Absyn.INNER()
        #= complex assign=#@assign def.prefixes = prefs
        #= complex assign=#@assign node.definition = def
        node
      end
      COMPONENT_NODE(__)  => begin
        comp = component(node)
        comp = begin
          @match comp begin
            P_Component.COMPONENT_DEF(definition = def && SCode.COMPONENT(prefixes = prefs))  => begin
              #= Complex assign=#@assign prefs.innerOuter = Absyn.INNER()
              #= complex assign=#@assign def.prefixes = prefs
              #= complex assign=#@assign comp.definition = def
              comp
            end
            _  => begin
              #             Error.assertion(false, getInstanceName() + " got unknown component", sourceInfo())
              @error "Unknown component in makeInnerNode"
              fail()
            end
          end
        end
        replaceComponent(comp, node)
      end
      _  => begin
        #        Error.assertion(false, getInstanceName() + " got unknown node", sourceInfo())
        @error "Unknown node in makeInnerNode"
        fail()
      end
    end
  end
  node
end
