  module NFOCConnectionGraph

    using MetaModelica
    using ExportAll
    #= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

    @UniontypeDecl NFOCConnectionGraph

         #= /*
         * This file is part of OpenModelica.
         *
         * Copyright (c) 1998-CurrentYear, Open Source Modelica Consortium (OSMC),
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

        import ..NFFlatModel; FlatModel=NFFlatModel
         import ..P_NFComponentRef
         P_ComponentRef=P_NFComponentRef
         ComponentRef=P_NFComponentRef.NFComponentRef
         import ..P_NFEquation
         P_Equation=P_NFEquation
         Equation=P_NFEquation.NFEquation
        import ..P_NFConnections
        FlatEdge = P_NFConnections.BrokenEdge  #= a tuple with two crefs and equation(s) for calling the equalityConstraint function call =#
        FlatEdges = P_NFConnections.BrokenEdges  #= a lit of broken edges =#

        import Absyn
        import ..NFBuiltin
        import ..NFCall.P_Call
        import ..NFClass.P_Class
         import ..P_NFDimension
         P_Dimension=P_NFDimension
         Dimension=P_NFDimension.NFDimension
        import ..NFFunction.P_Function
        import ..NFHashTable
        import ..NFHashTable3
        import ..NFHashTableCG
        import ..NFInstNode.P_InstNode
         import ..P_NFOperator
         P_Operator=P_NFOperator
         Operator=P_NFOperator.NFOperator
        import ..P_NFOperator.Op
        import ..DAE.Connect
         import ..P_NFExpression
         P_Expression=P_NFExpression
         Expression=P_NFExpression.NFExpression
         import ..P_NFType
         P_M_Type=P_NFType
         M_Type=NFType
        import ..MetaModelica.Dangerous.listReverseInPlace
         import ..P_NFConnector
         P_NFConnector
         Connector=NFConnector
        import ..ElementSource
        import ..NFTyping.ExpOrigin
        import ..NFTyping; Typing=NFTyping
        import ..NFPrefixes.Variability
        import ..Error
         import ..P_NFConnections
         P_Connections=P_NFConnections
         Connections=P_NFConnections.NFConnections
         import ..P_NFConnection
         P_Connection=P_NFConnection
         Connection=P_NFConnection.NFConnection
        Edge = Tuple  #= an edge is a tuple with two component references =#
        Edges = List  #= A list of edges =#
        DefiniteRoot = ComponentRef  #= root defined with Connection.root =#
        DefiniteRoots = List  #= roots defined with Connection.root =#
        UniqueRoots = List  #= roots defined with Connection.uniqueRoot =#
        PotentialRoot = Tuple  #= potential root defined with Connections.potentialRoot =#
        PotentialRoots = List  #= potential roots defined with Connections.potentialRoot =#

          #= Input structure for connection breaking algorithm. It is collected during instantiation phase. =#
         @Uniontype NFOCConnectionGraph begin
              @Record GRAPH begin

                      updateGraph::Bool
                      definiteRoots #= Roots defined with Connection.root =#::DefiniteRoots
                      potentialRoots #= Roots defined with Connection.potentialRoot =#::PotentialRoots
                      uniqueRoots #= Roots defined with Connection.uniqueRoot =#::UniqueRoots
                      branches #= Edges defined with Connection.branch =#::Edges
                      connections #= Edges defined with connect statement =#::FlatEdges
              end
         end

        const EMPTY = GRAPH(true, nil, nil, nil, nil, nil) #= Initial connection graph with no edges in it. =#::NFOCConnectionGraph
        const NOUPDATE_EMPTY = GRAPH(false, nil, nil, nil, nil, nil) #= Initial connection graph with updateGraph set to false. =#::NFOCConnectionGraph
          ConnectionsOperator = #= Enumeration =# (() -> begin
          BRANCH  = 1
          ROOT  = 2
          POTENTIAL_ROOT  = 3
          IS_ROOT  = 4
          ROOTED  = 5
          UNIQUE_ROOT  = 6
          UNIQUE_ROOT_INDICES  = 7
          NOT_OPERATOR  = 8
           ()->(BRANCH ;ROOT ;POTENTIAL_ROOT ;IS_ROOT ;ROOTED ;UNIQUE_ROOT ;UNIQUE_ROOT_INDICES ;NOT_OPERATOR )
          end)()

        """ #= @author: adrpo
         goes over all equations from the FlatModel and:
         1. builds the overconstrained connection graph from:
           - connect, Connections.branch
           - Connections.root, Connections.potentialRoot
         2. Breaks the overconstrained connection graph
            and replaces the broken connects with a call
            to the equalityConstraint function
         3. using the graph evaluates:
           - Connections.isRoot, Connections.rooted, rooted
         4. partially handles non-standard
           - Connections.uniqueRoot
           - Connections.uniqueRootIndices =#"""
        function handleOverconstrainedConnections(flatModel::FlatModel, conns::Connections, modelNameQualified::String) ::Tuple{FlatModel, FlatEdges}
              local outBroken::FlatEdges


              local lhs::ComponentRef
              local rhs::ComponentRef
              local cref::ComponentRef
              local source::DAE.ElementSource
              local graph::NFOCConnectionGraph = EMPTY
              local eql::List{Equation} = nil
              local eqlBroken::List{Equation}
              local ieql::List{Equation}
              local broken::FlatEdges
              local connected::FlatEdges
              local call::Call
              local lst::List{Expression}
              local priority::Integer
              local root::Expression
              local msg::Expression
              local c1::Connector
              local c2::Connector
              local lhs_crefs::List{ComponentRef}
              local rhs_crefs::List{ComponentRef}
              local print_trace::Bool = Flags.isSet(Flags.CGRAPH)
              local origin::ORIGIN_Type
              local name::String

              @assign origin = intBitOr(ExpOrigin.EQUATION, ExpOrigin.CONNECT)
               #=  go over all equations, connect, Connection.branch
               =#
              for conn in conns.connections
                @match P_Connection.Connection.CONNECTION(lhs = c1, rhs = c2) = conn
                @assign lhs_crefs = getOverconstrainedCrefs(c1)
                @assign rhs_crefs = getOverconstrainedCrefs(c2)
                if ! listEmpty(lhs_crefs)
                  @assign eqlBroken = generateEqualityConstraintEquation(c1.name, c1.ty, c2.name, c2.ty, origin, c1.source)
                  @assign graph = ListUtil.threadFold(lhs_crefs, rhs_crefs, (eqlBroken, print_trace) -> addConnection(brokenEquations = eqlBroken, printTrace = print_trace), graph)
                end
              end
              for eq in flatModel.equations
                @assign eql = begin
                  @match eq begin
                    P_Equation.Equation.NORETCALL(exp = CALL_EXPRESSION(call && P_Call.TYPED_CALL(arguments = lst)), source = source)  => begin
                      begin
                        @match identifyConnectionsOperator(P_Function.name(call.fn)) begin
                          ConnectionsOperator.ROOT  => begin
                              @match list(CREF_EXPRESSION(cref = cref)) = lst
                              @assign graph = addDefiniteRoot(cref, print_trace, graph)
                            eql
                          end

                          ConnectionsOperator.POTENTIAL_ROOT  => begin
                              @assign graph = begin
                                @match lst begin
                                  CREF_EXPRESSION(cref = cref) <|  nil()  => begin
                                    addPotentialRoot(cref, 0, print_trace, graph)
                                  end

                                  CREF_EXPRESSION(cref = cref) <| INTEGER_EXPRESSION(priority) <|  nil()  => begin
                                    addPotentialRoot(cref, priority, print_trace, graph)
                                  end
                                end
                              end
                            eql
                          end

                          ConnectionsOperator.UNIQUE_ROOT  => begin
                              @assign graph = begin
                                @match lst begin
                                  root && CREF_EXPRESSION(cref = cref) <|  nil()  => begin
                                    addUniqueRoots(root, STRING_EXPRESSION(""), print_trace, graph)
                                  end

                                  root && CREF_EXPRESSION(cref = cref) <| msg <|  nil()  => begin
                                    addUniqueRoots(root, msg, print_trace, graph)
                                  end
                                end
                              end
                            eql
                          end

                          ConnectionsOperator.BRANCH  => begin
                              @match list(CREF_EXPRESSION(cref = lhs), CREF_EXPRESSION(cref = rhs)) = lst
                              @assign graph = addBranch(lhs, rhs, print_trace, graph)
                            eql
                          end

                          _  => begin
                              _cons(eq, eql)
                          end
                        end
                      end
                    end

                    _  => begin
                        _cons(eq, eql)
                    end
                  end
                end
              end
               #=  now we have the graph, remove the broken connects and evaluate the equation operators
               =#
              @assign eql = listReverseInPlace(eql)
              @assign ieql = flatModel.initialEquations
              @assign (eql, ieql, connected, broken) = handleOverconstrainedConnections_dispatch(graph, modelNameQualified, eql, ieql)
              @assign eql = removeBrokenConnects(eql, connected, broken)
              @assign flatModel.equations = eql
              @assign flatModel.initialEquations = ieql
              @assign outBroken = broken
          (flatModel, outBroken)
        end

        function generateEqualityConstraintEquation(clhs::ComponentRef, lhs_ty::M_Type, crhs::ComponentRef, rhs_ty::M_Type, origin::ORIGIN_Type, source::DAE.ElementSource) ::List{Equation}
              local eqsEqualityConstraint::List{Equation} = nil

              local lhs::ComponentRef
              local rhs::ComponentRef
              local cref::ComponentRef
              local fcref_rhs::ComponentRef
              local fcref_lhs::ComponentRef
              local lhsArr::ComponentRef
              local rhsArr::ComponentRef
              local eql::List{Equation} = nil
              local lst::List{Expression}
              local ty::M_Type
              local ty1::M_Type
              local ty2::M_Type
              local priority::Integer
              local root::Expression
              local msg::Expression
              local c1::Connector
              local c2::Connector
              local cc1::Connector
              local cc2::Connector
              local cl1::List{Connector}
              local cl2::List{Connector}
              local lhsl::List{Connector}
              local rhsl::List{Connector}
              local replaceEq::Equation
              local expLHS::Expression
              local expRHS::Expression
              local fn_node_lhs::InstNode
              local fn_node_rhs::InstNode
              local var::VariabilityType

              if ! System.getHasOverconstrainedConnectors()
                return eqsEqualityConstraint
              end
              if ! (isDeleted(clhs) || isDeleted(crhs))
                @assign cl1 = P_NFConnections.makeConnectors(clhs, lhs_ty, source)
                @assign cl2 = P_NFConnections.makeConnectors(crhs, rhs_ty, source)
                for c1 in cl1
                  @match _cons(c2, cl2) = cl2
                  @assign lhsl = Connector.split(c1)
                  @assign rhsl = Connector.split(c2)
                  for cc1 in lhsl
                    @match _cons(cc2, rhsl) = rhsl
                    if ! (Connector.isDeleted(cc1) || Connector.isDeleted(cc2))
                      @assign lhs = Connector.name(cc1)
                      @assign rhs = Connector.name(cc2)
                      if isOverconstrainedCref(lhs) && isOverconstrainedCref(rhs)
                        @assign lhs = getOverconstrainedCref(lhs)
                        @assign rhs = getOverconstrainedCref(rhs)
                        @assign lhsArr = stripSubscripts(lhs)
                        @assign rhsArr = stripSubscripts(rhs)
                        @assign ty1 = getComponentType(lhsArr)
                        @assign ty2 = getComponentType(rhsArr)
                        @assign fcref_rhs = P_Function.lookupFunctionSimple("equalityConstraint", classScope(node(lhs)))
                        @assign (fcref_rhs, fn_node_rhs, _) = P_Function.instFunctionRef(fcref_rhs, ElementSource.getInfo(source))
                        @assign expRHS = CALL_EXPRESSION(P_Call.UNTYPED_CALL(fcref_rhs, list(CREF_EXPRESSION(ty1, lhsArr), CREF_EXPRESSION(ty2, rhsArr)), nil, fn_node_rhs))
                        @assign (expRHS, ty, var) = Typing.typeExp(expRHS, origin, ElementSource.getInfo(source))
                        @assign fcref_lhs = P_Function.lookupFunctionSimple("fill", topScope(node(clhs)))
                        @assign (fcref_lhs, fn_node_lhs, _) = P_Function.instFunctionRef(fcref_lhs, ElementSource.getInfo(source))
                        @assign expLHS = CALL_EXPRESSION(P_Call.UNTYPED_CALL(fcref_lhs, _cons(P_Expression.REAL_EXPRESSION(0.0), ListUtil.map(arrayDims(ty), P_Dimension.Dimension.sizeExp)), nil, fn_node_lhs))
                        @assign (expLHS, ty, var) = Typing.typeExp(expLHS, origin, ElementSource.getInfo(source))
                        @assign replaceEq = EQUATION_EQUALITY(expRHS, expLHS, ty, source)
                        @assign eqsEqualityConstraint = list(replaceEq)
                        return eqsEqualityConstraint
                      end
                    end
                  end
                end
              end
          eqsEqualityConstraint
        end

        function getOverconstrainedCrefs(conn::Connector) ::List{ComponentRef}
              local crefs::List{ComponentRef}

              local conns::List{Connector}

              @assign conns = Connector.split(conn, scalarize = ScalarizeSetting.PREFIX)
              @assign crefs = List(getOverconstrainedCref(c.name) for c in conns if ! Connector.isDeleted(c) && isOverconstrainedCref(c.name))
              @assign crefs = ListUtil.uniqueOnTrue(crefs, isEqual)
          crefs
        end

        function isOverconstrainedCref(cref::ComponentRef) ::Bool
              local b::Bool = false

              local node::InstNode
              local rest::ComponentRef

              @assign b = begin
                @match cref begin
                  CREF(node = node, origin = P_NFComponentRef.Origin.CREF, restCref = rest)  => begin
                    isOverdetermined(getClass(node)) || isOverconstrainedCref(rest)
                  end

                  _  => begin
                      false
                  end
                end
              end
          b
        end

        function getOverconstrainedCref(cref::ComponentRef) ::ComponentRef
              local c::ComponentRef

              local node::InstNode
              local rest::ComponentRef

              @assign c = begin
                @match cref begin
                  CREF(node = node, origin = P_NFComponentRef.Origin.CREF, restCref = rest)  => begin
                    if isOverdetermined(getClass(node))
                          cref
                        else
                          getOverconstrainedCref(rest)
                        end
                  end
                end
              end
          c
        end

        """ #= author: adrpo
         this function gets the connection graph and the existing DAE and:
         - returns a list of broken connects and one list of connected connects
         - evaluates Connections.isRoot in the input DAE
         - evaluates Connections.uniqueRootIndices in the input DAE
         - evaluates the rooted operator in the input DAE =#"""
        function handleOverconstrainedConnections_dispatch(inGraph::NFOCConnectionGraph, modelNameQualified::String, inEquations::List{<:Equation}, inInitialEquations::List{<:Equation}) ::Tuple{List{Equation}, List{Equation}, FlatEdges, FlatEdges}
              local outBroken::FlatEdges
              local outConnected::FlatEdges
              local outInitialEquations::List{Equation}
              local outEquations::List{Equation}

              @assign (outEquations, outInitialEquations, outConnected, outBroken) = begin
                  local graph::NFOCConnectionGraph
                  local eqs::List{Equation}
                  local ieqs::List{Equation}
                  local roots::List{ComponentRef}
                  local broken::FlatEdges
                  local connected::FlatEdges
                   #=  handle the connection breaking
                   =#
                @matchcontinue (inGraph, modelNameQualified, inEquations, inInitialEquations) begin
                  (graph, _, eqs, ieqs)  => begin
                      if Flags.isSet(Flags.CGRAPH)
                        print("Summary: \\n\\t" + "Nr Roots:           " + intString(listLength(getDefiniteRoots(graph))) + "\\n\\t" + "Nr Potential Roots: " + intString(listLength(getPotentialRoots(graph))) + "\\n\\t" + "Nr Unique Roots:    " + intString(listLength(getUniqueRoots(graph))) + "\\n\\t" + "Nr Branches:        " + intString(listLength(getBranches(graph))) + "\\n\\t" + "Nr Connections:     " + intString(listLength(getConnections(graph))) + "\\n")
                      end
                      @assign (roots, connected, broken) = findResultGraph(graph, modelNameQualified)
                      if Flags.isSet(Flags.CGRAPH)
                        print("Roots: " + stringDelimitList(ListUtil.map(roots, toString), ", ") + "\\n")
                        print("Broken connections: " + stringDelimitList(ListUtil.map1(broken, printConnectionStr, "broken"), ", ") + "\\n")
                        print("Allowed connections: " + stringDelimitList(ListUtil.map1(connected, printConnectionStr, "allowed"), ", ") + "\\n")
                      end
                      @assign eqs = evalConnectionsOperators(roots, graph, eqs)
                      @assign ieqs = evalConnectionsOperators(roots, graph, ieqs)
                    (eqs, ieqs, connected, broken)
                  end

                  _  => begin
                         #=  handle the connection breaking
                         =#
                        @match true = Flags.isSet(Flags.CGRAPH)
                        print("- NFOCConnectionGraph.handleOverconstrainedConnections failed for model: " + modelNameQualified + "\\n")
                      fail()
                  end
                end
              end
          (outEquations, outInitialEquations, outConnected, outBroken)
        end

        """ #= Adds a new definite root to NFOCConnectionGraph =#"""
        function addDefiniteRoot(root::ComponentRef, printTrace::Bool, graph::NFOCConnectionGraph) ::NFOCConnectionGraph


              if printTrace
                print("- NFOCConnectionGraph.addDefiniteRoot(" + toString(root) + ")\\n")
              end
              @assign graph.definiteRoots = _cons(root, graph.definiteRoots)
          graph
        end

        """ #= Adds a new potential root to NFOCConnectionGraph =#"""
        function addPotentialRoot(root::ComponentRef, priority::AbstractFloat, printTrace::Bool, graph::NFOCConnectionGraph) ::NFOCConnectionGraph


              if printTrace
                print("- NFOCConnectionGraph.addPotentialRoot(" + toString(root) + ", " + realString(priority) + ")" + "\\n")
              end
              @assign graph.potentialRoots = _cons((root, priority), graph.potentialRoots)
          graph
        end

        """ #= Adds a new unique root to NFOCConnectionGraph =#"""
        function addUniqueRoots(roots::Expression, message::Expression, printTrace::Bool, graph::NFOCConnectionGraph) ::NFOCConnectionGraph


              local unique_roots::UniqueRoots = graph.uniqueRoots

              for root in P_Expression.Expression.arrayScalarElements(roots)
                @assign unique_roots = begin
                  @match root begin
                    CREF_EXPRESSION(__)  => begin
                        if printTrace
                          print("- NFOCConnectionGraph.addUniqueRoots(" + toString(root) + ", " + toString(message) + ")\\n")
                        end
                      _cons((root.cref, message), unique_roots)
                    end

                    _  => begin
                           #=  TODO! FIXME! print some meaningful error message here that the input is not an array of roots or a cref
                           =#
                        unique_roots
                    end
                  end
                end
              end
          graph
        end

        function addBranch(ref1::ComponentRef, ref2::ComponentRef, printTrace::Bool, graph::NFOCConnectionGraph) ::NFOCConnectionGraph


              if printTrace
                print("- NFOCConnectionGraph.addBranch(" + toString(ref1) + ", " + toString(ref2) + ")\\n")
              end
              @assign graph.branches = _cons((ref1, ref2), graph.branches)
          graph
        end

        """ #= Adds a new connection to NFOCConnectionGraph =#"""
        function addConnection(ref1::ComponentRef, ref2::ComponentRef, brokenEquations::List{<:Equation}, printTrace::Bool, graph::NFOCConnectionGraph) ::NFOCConnectionGraph


              if printTrace
                print("- NFOCConnectionGraph.addConnection(" + toString(ref1) + ", " + toString(ref2) + ")\\n")
              end
              @assign graph.connections = _cons((ref1, ref2, brokenEquations), graph.connections)
          graph
        end

         #=  *************************************
         =#
         #=  ********* protected section *********
         =#
         #=  *************************************
         =#

        import ..BaseHashTable

        import ..Debug

        import ..Flags

        import ListUtil

        import ..Util

        import ..System

        import ..IOStream

        import ..Settings

        """ #= Returns the canonical element of the component where input element belongs to.
         See explanation at the top of file. =#"""
        function canonical(inPartition::NFHashTableCG.HashTable, inRef::ComponentRef) ::ComponentRef
              local outCanonical::ComponentRef

              @assign outCanonical = begin
                   #= /*outPartition,*/ =#
                  local partition::NFHashTableCG.HashTable
                  local ref::ComponentRef
                  local parent::ComponentRef
                  local parentCanonical::ComponentRef
                @matchcontinue (inPartition, inRef) begin
                  (partition, ref)  => begin
                      @assign parent = BaseHashTable.get(ref, partition)
                      @assign parentCanonical = canonical(partition, parent)
                    parentCanonical
                  end

                  (_, ref)  => begin
                    ref
                  end
                end
              end
               #= fprintln(Flags.CGRAPH,
               =#
               #=   \"- NFOCConnectionGraph.canonical_case1(\" + ComponentRef.toString(ref) + \") = \" +
               =#
               #=   ComponentRef.toString(parentCanonical));
               =#
               #= partition2 = BaseHashTable.add((ref, parentCanonical), partition);
               =#
               #= fprintln(Flags.CGRAPH,
               =#
               #=   \"- NFOCConnectionGraph.canonical_case2(\" + ComponentRef.toString(ref) + \") = \" +
               =#
               #=   ComponentRef.toString(ref));
               =#
          outCanonical
        end

        """ #= Tells whether the elements belong to the same component.
         See explanation at the top of file. =#"""
        function areInSameComponent(inPartition::NFHashTableCG.HashTable, inRef1::ComponentRef, inRef2::ComponentRef) ::Bool
              local outResult::Bool

               #=  canonical(inPartition,inRef1) = canonical(inPartition,inRef2);
               =#
              @assign outResult = begin
                  local partition::NFHashTableCG.HashTable
                  local ref1::ComponentRef
                  local ref2::ComponentRef
                  local canon1::ComponentRef
                  local canon2::ComponentRef
                @matchcontinue (inPartition, inRef1, inRef2) begin
                  (partition, ref1, ref2)  => begin
                      @assign canon1 = canonical(partition, ref1)
                      @assign canon2 = canonical(partition, ref2)
                      @match true = isEqual(canon1, canon2)
                    true
                  end

                  _  => begin
                      false
                  end
                end
              end
          outResult
        end

        """ #= Tries to connect two components whose elements are given. Depending
         on wheter the connection success or not (i.e are the components already
         connected), adds either inConnectionDae or inBreakDae to the list of
         DAE elements. =#"""
        function connectBranchComponents(inPartition::NFHashTableCG.HashTable, inRef1::ComponentRef, inRef2::ComponentRef) ::NFHashTableCG.HashTable
              local outPartition::NFHashTableCG.HashTable

              @assign outPartition = begin
                  local partition::NFHashTableCG.HashTable
                  local ref1::ComponentRef
                  local ref2::ComponentRef
                  local canon1::ComponentRef
                  local canon2::ComponentRef
                   #=  can connect them
                   =#
                @matchcontinue (inPartition, inRef1, inRef2) begin
                  (partition, ref1, ref2)  => begin
                      @assign canon1 = canonical(partition, ref1)
                      @assign canon2 = canonical(partition, ref2)
                      @match (partition, true) = connectCanonicalComponents(partition, canon1, canon2)
                    partition
                  end

                  (partition, _, _)  => begin
                    partition
                  end
                end
              end
               #=  cannot connect them
               =#
          outPartition
        end

        """ #= Tries to connect two components whose elements are given. Depending
         on wheter the connection success or not (i.e are the components already
         connected), adds either inConnectionDae or inBreakDae to the list of
         DAE elements. =#"""
        function connectComponents(inPartition::NFHashTableCG.HashTable, inFlatEdge::FlatEdge) ::Tuple{NFHashTableCG.HashTable, FlatEdges, FlatEdges}
              local outBrokenConnections::FlatEdges
              local outConnectedConnections::FlatEdges
              local outPartition::NFHashTableCG.HashTable

              @assign (outPartition, outConnectedConnections, outBrokenConnections) = begin
                  local partition::NFHashTableCG.HashTable
                  local ref1::ComponentRef
                  local ref2::ComponentRef
                  local canon1::ComponentRef
                  local canon2::ComponentRef
                   #=  leave the connect(ref1,ref2)
                   =#
                @matchcontinue (inPartition, inFlatEdge) begin
                  (partition, (ref1, _, _))  => begin
                      @shouldFail @assign _ = canonical(partition, ref1)
                    (partition, list(inFlatEdge), nil)
                  end

                  (partition, (_, ref2, _))  => begin
                      @shouldFail @assign _ = canonical(partition, ref2)
                    (partition, list(inFlatEdge), nil)
                  end

                  (partition, (ref1, ref2, _))  => begin
                      @assign canon1 = canonical(partition, ref1)
                      @assign canon2 = canonical(partition, ref2)
                      @match (partition, true) = connectCanonicalComponents(partition, canon1, canon2)
                    (partition, list(inFlatEdge), nil)
                  end

                  (partition, (ref1, ref2, _))  => begin
                      if Flags.isSet(Flags.CGRAPH)
                        Debug.trace("- NFOCConnectionGraph.connectComponents: should remove equations generated from: connect(" + toString(ref1) + ", " + toString(ref2) + ") and add {0, ..., 0} = equalityConstraint(cr1, cr2) instead.\\n")
                      end
                    (partition, nil, list(inFlatEdge))
                  end
                end
              end
          (outPartition, outConnectedConnections, outBrokenConnections)
        end

        """ #= Tries to connect two components whose canonical elements are given.
         Helper function for connectionComponents. =#"""
        function connectCanonicalComponents(inPartition::NFHashTableCG.HashTable, inRef1::ComponentRef, inRef2::ComponentRef) ::Tuple{NFHashTableCG.HashTable, Bool}
              local outReallyConnected::Bool
              local outPartition::NFHashTableCG.HashTable

              @assign (outPartition, outReallyConnected) = begin
                  local partition::NFHashTableCG.HashTable
                  local ref1::ComponentRef
                  local ref2::ComponentRef
                   #=  they are the same
                   =#
                @matchcontinue (inPartition, inRef1, inRef2) begin
                  (partition, ref1, ref2)  => begin
                      @match true = isEqual(ref1, ref2)
                    (partition, false)
                  end

                  (partition, ref1, ref2)  => begin
                      @assign partition = BaseHashTable.add((ref1, ref2), partition)
                    (partition, true)
                  end
                end
              end
               #=  not the same, add it
               =#
          (outPartition, outReallyConnected)
        end

        """ #= Adds a root the the graph. This is implemented by connecting the root to inFirstRoot element. =#"""
        function addRootsToTable(inTable::NFHashTableCG.HashTable, inRoots::List{<:ComponentRef}, inFirstRoot::ComponentRef) ::NFHashTableCG.HashTable
              local outTable::NFHashTableCG.HashTable

              @assign outTable = begin
                  local table::NFHashTableCG.HashTable
                  local root::ComponentRef
                  local firstRoot::ComponentRef
                  local tail::List{ComponentRef}
                @match (inTable, inRoots, inFirstRoot) begin
                  (table, root <| tail, firstRoot)  => begin
                      @assign table = BaseHashTable.add((root, firstRoot), table)
                      @assign table = addRootsToTable(table, tail, firstRoot)
                    table
                  end

                  (table,  nil(), _)  => begin
                    table
                  end
                end
              end
          outTable
        end

        """ #= Creates an initial graph with given definite roots. =#"""
        function resultGraphWithRoots(roots::List{<:ComponentRef}) ::NFHashTableCG.HashTable
              local outTable::NFHashTableCG.HashTable

              local table0::NFHashTableCG.HashTable
              local dummyRoot::ComponentRef

              @assign dummyRoot = NFBuiltin.TIME_CREF
              @assign table0 = NFHashTableCG.emptyHashTable()
              @assign outTable = addRootsToTable(table0, roots, dummyRoot)
          outTable
        end

        """ #= Adds all branches to the graph. =#"""
        function addBranchesToTable(inTable::NFHashTableCG.HashTable, inBranches::Edges) ::NFHashTableCG.HashTable
              local outTable::NFHashTableCG.HashTable

              @assign outTable = begin
                  local table::NFHashTableCG.HashTable
                  local table1::NFHashTableCG.HashTable
                  local table2::NFHashTableCG.HashTable
                  local ref1::ComponentRef
                  local ref2::ComponentRef
                  local tail::Edges
                @match (inTable, inBranches) begin
                  (table, (ref1, ref2) <| tail)  => begin
                      @assign table1 = connectBranchComponents(table, ref1, ref2)
                      @assign table2 = addBranchesToTable(table1, tail)
                    table2
                  end

                  (table,  nil())  => begin
                    table
                  end
                end
              end
          outTable
        end

        """ #= An ordering function for potential roots. =#"""
        function ord(inEl1::PotentialRoot, inEl2::PotentialRoot) ::Bool
              local outBoolean::Bool

              @assign outBoolean = begin
                  local r1::AbstractFloat
                  local r2::AbstractFloat
                  local c1::ComponentRef
                  local c2::ComponentRef
                  local s1::String
                  local s2::String
                @matchcontinue (inEl1, inEl2) begin
                  ((c1, r1), (c2, r2))  => begin
                      @match true = realEq(r1, r2)
                      @assign s1 = toString(c1)
                      @assign s2 = toString(c2)
                      @match 1 = stringCompare(s1, s2)
                    true
                  end

                  ((_, r1), (_, r2))  => begin
                    r1 > r2
                  end
                end
              end
               #=  if equal order by cref
               =#
          outBoolean
        end

        """ #= Adds all potential roots to graph. =#"""
        function addPotentialRootsToTable(inTable::NFHashTableCG.HashTable, inPotentialRoots::PotentialRoots, inRoots::DefiniteRoots, inFirstRoot::ComponentRef) ::Tuple{NFHashTableCG.HashTable, DefiniteRoots}
              local outRoots::DefiniteRoots
              local outTable::NFHashTableCG.HashTable

              @assign (outTable, outRoots) = begin
                  local table::NFHashTableCG.HashTable
                  local potentialRoot::ComponentRef
                  local firstRoot::ComponentRef
                  local canon1::ComponentRef
                  local canon2::ComponentRef
                  local roots::DefiniteRoots
                  local finalRoots::DefiniteRoots
                  local tail::PotentialRoots
                @matchcontinue (inTable, inPotentialRoots, inRoots, inFirstRoot) begin
                  (table,  nil(), roots, _)  => begin
                    (table, roots)
                  end

                  (table, (potentialRoot, _) <| tail, roots, firstRoot)  => begin
                      @assign canon1 = canonical(table, potentialRoot)
                      @assign canon2 = canonical(table, firstRoot)
                      @match (table, true) = connectCanonicalComponents(table, canon1, canon2)
                      @assign (table, finalRoots) = addPotentialRootsToTable(table, tail, _cons(potentialRoot, roots), firstRoot)
                    (table, finalRoots)
                  end

                  (table, _ <| tail, roots, firstRoot)  => begin
                      @assign (table, finalRoots) = addPotentialRootsToTable(table, tail, roots, firstRoot)
                    (table, finalRoots)
                  end
                end
              end
          (outTable, outRoots)
        end

        """ #= Adds all connections to graph. =#"""
        function addConnections(inTable::NFHashTableCG.HashTable, inConnections::FlatEdges) ::Tuple{NFHashTableCG.HashTable, FlatEdges, FlatEdges}
              local outBrokenConnections::FlatEdges
              local outConnectedConnections::FlatEdges
              local outTable::NFHashTableCG.HashTable

              @assign (outTable, outConnectedConnections, outBrokenConnections) = begin
                  local table::NFHashTableCG.HashTable
                  local tail::FlatEdges
                  local broken1::FlatEdges
                  local broken2::FlatEdges
                  local broken::FlatEdges
                  local connected1::FlatEdges
                  local connected2::FlatEdges
                  local connected::FlatEdges
                  local e::FlatEdge
                   #=  empty case
                   =#
                @match (inTable, inConnections) begin
                  (table,  nil())  => begin
                    (table, nil, nil)
                  end

                  (table, e <| tail)  => begin
                      @assign (table, connected1, broken1) = connectComponents(table, e)
                      @assign (table, connected2, broken2) = addConnections(table, tail)
                      @assign connected = listAppend(connected1, connected2)
                      @assign broken = listAppend(broken1, broken2)
                    (table, connected, broken)
                  end
                end
              end
               #=  normal case
               =#
          (outTable, outConnectedConnections, outBrokenConnections)
        end

        """ #= Given NFOCConnectionGraph structure, breaks all connections,
         determines roots and generates a list of dae elements. =#"""
        function findResultGraph(inGraph::NFOCConnectionGraph, modelNameQualified::String) ::Tuple{DefiniteRoots, FlatEdges, FlatEdges}
              local outBrokenConnections::FlatEdges
              local outConnectedConnections::FlatEdges
              local outRoots::DefiniteRoots

              @assign (outRoots, outConnectedConnections, outBrokenConnections) = begin
                  local definiteRoots::DefiniteRoots
                  local finalRoots::DefiniteRoots
                  local potentialRoots::PotentialRoots
                  local orderedPotentialRoots::PotentialRoots
                  local uniqueRoots::UniqueRoots
                  local branches::Edges
                  local connections::FlatEdges
                  local broken::FlatEdges
                  local connected::FlatEdges
                  local table::NFHashTableCG.HashTable
                  local dummyRoot::ComponentRef
                  local brokenConnectsViaGraphViz::String
                  local userBrokenLst::List{String}
                  local userBrokenLstLst::List{List{String}}
                  local userBrokenTplLst::List{Tuple{String, String}}
                   #=  deal with empty connection graph
                   =#
                @matchcontinue (inGraph, modelNameQualified) begin
                  (GRAPH(definiteRoots =  nil(), potentialRoots =  nil(), uniqueRoots =  nil(), branches =  nil(), connections =  nil()), _)  => begin
                    (nil, nil, nil)
                  end

                  (GRAPH(definiteRoots = definiteRoots, potentialRoots = potentialRoots, uniqueRoots = uniqueRoots, branches = branches, connections = connections), _)  => begin
                      @assign connections = listReverse(connections)
                      @assign table = resultGraphWithRoots(definiteRoots)
                      @assign table = addBranchesToTable(table, branches)
                      @assign orderedPotentialRoots = ListUtil.sort(potentialRoots, ord)
                      if Flags.isSet(Flags.CGRAPH)
                        print("Ordered Potential Roots: " + stringDelimitList(ListUtil.map(orderedPotentialRoots, printPotentialRootTuple), ", ") + "\\n")
                      end
                      @assign (table, connected, broken) = addConnections(table, connections)
                      @assign dummyRoot = NFBuiltin.TIME_CREF
                      @assign (table, finalRoots) = addPotentialRootsToTable(table, orderedPotentialRoots, definiteRoots, dummyRoot)
                      @assign brokenConnectsViaGraphViz = generateGraphViz(modelNameQualified, definiteRoots, potentialRoots, uniqueRoots, branches, connections, finalRoots, broken)
                      if stringEq(brokenConnectsViaGraphViz, "")
                      else
                        @assign userBrokenLst = Util.stringSplitAtChar(brokenConnectsViaGraphViz, "#")
                        @assign userBrokenLstLst = ListUtil.map1(userBrokenLst, Util.stringSplitAtChar, "|")
                        @assign userBrokenTplLst = makeTuple(userBrokenLstLst)
                        print("User selected the following connect edges for breaking:\\n\\t" + stringDelimitList(ListUtil.map(userBrokenTplLst, printTupleStr), "\\n\\t") + "\\n")
                        printFlatEdges(connections)
                        @assign connections = orderConnectsGuidedByUser(connections, userBrokenTplLst)
                        @assign connections = listReverse(connections)
                        print("\\nAfer ordering:\\n")
                        @assign (finalRoots, connected, broken) = findResultGraph(GRAPH(false, definiteRoots, potentialRoots, uniqueRoots, branches, connections), modelNameQualified)
                      end
                    (finalRoots, connected, broken)
                  end
                end
              end
          (outRoots, outConnectedConnections, outBrokenConnections)
        end

        function orderConnectsGuidedByUser(inConnections::FlatEdges, inUserSelectedBreaking::List{<:Tuple{<:String, String}}) ::FlatEdges
              local outOrderedConnections::FlatEdges

              local front::FlatEdges = nil
              local back::FlatEdges = nil
              local c1::ComponentRef
              local c2::ComponentRef
              local sc1::String
              local sc2::String

              for e in inConnections
                @assign (c1, c2, _) = e
                @assign sc1 = toString(c1)
                @assign sc2 = toString(c2)
                if listMember((sc1, sc2), inUserSelectedBreaking) || listMember((sc2, sc1), inUserSelectedBreaking)
                  @assign back = _cons(e, back)
                else
                  @assign front = _cons(e, front)
                end
              end
               #=  put them at the end to be tried last (more chance to be broken)
               =#
               #=  put them at the front to be tried first (less chance to be broken)
               =#
              @assign outOrderedConnections = ListUtil.append_reverse(front, back)
          outOrderedConnections
        end

        function printTupleStr(inTpl::Tuple{<:String, String}) ::String
              local out::String

              @assign out = begin
                  local c1::String
                  local c2::String
                @match inTpl begin
                  (c1, c2)  => begin
                    c1 + " -- " + c2
                  end
                end
              end
          out
        end

        function makeTuple(inLstLst::List{<:List{<:String}}) ::List{Tuple{String, String}}
              local outLst::List{Tuple{String, String}}

              @assign outLst = begin
                  local c1::String
                  local c2::String
                  local rest::List{List{String}}
                  local lst::List{Tuple{String, String}}
                  local bad::List{String}
                   #=  empty case
                   =#
                @matchcontinue inLstLst begin
                   nil()  => begin
                    nil
                  end

                  c1 <| c2 <|  nil() <| rest  => begin
                      @assign lst = makeTuple(rest)
                    _cons((c1, c2), lst)
                  end

                  "" <|  nil() <| rest  => begin
                      @assign lst = makeTuple(rest)
                    lst
                  end

                   nil() <| rest  => begin
                      @assign lst = makeTuple(rest)
                    lst
                  end

                  bad <| rest  => begin
                      print("The following output from GraphViz OpenModelica assistant cannot be parsed:" + stringDelimitList(bad, ", ") + "\\nExpected format from GrapViz: cref1|cref2#cref3|cref4#. Ignoring malformed input.\\n")
                      @assign lst = makeTuple(rest)
                    lst
                  end
                end
              end
               #=  somthing case
               =#
               #=  ignore empty strings
               =#
               #=  ignore empty list
               =#
               #=  somthing case
               =#
          outLst
        end

        function printPotentialRootTuple(potentialRoot::PotentialRoot) ::String
              local outStr::String

              @assign outStr = begin
                  local cr::ComponentRef
                  local priority::AbstractFloat
                  local str::String
                @match potentialRoot begin
                  (cr, priority)  => begin
                      @assign str = toString(cr) + "(" + realString(priority) + ")"
                    str
                  end
                end
              end
          outStr
        end

        function setRootDistance(finalRoots::List{<:ComponentRef}, table::NFHashTable3.HashTable, distance::Integer, nextLevel::List{<:ComponentRef}, irooted::NFHashTable.HashTable) ::NFHashTable.HashTable
              local orooted::NFHashTable.HashTable

              @assign orooted = begin
                  local rooted::NFHashTable.HashTable
                  local rest::List{ComponentRef}
                  local next::List{ComponentRef}
                  local cr::ComponentRef
                @matchcontinue (finalRoots, table, distance, nextLevel, irooted) begin
                  ( nil(), _, _,  nil(), _)  => begin
                    irooted
                  end

                  ( nil(), _, _, _, _)  => begin
                    setRootDistance(nextLevel, table, distance + 1, nil, irooted)
                  end

                  (cr <| rest, _, _, _, _)  => begin
                      @match false = BaseHashTable.hasKey(cr, irooted)
                      @assign rooted = BaseHashTable.add((cr, distance), irooted)
                      @assign next = BaseHashTable.get(cr, table)
                      @assign next = listAppend(nextLevel, next)
                    setRootDistance(rest, table, distance, next, rooted)
                  end

                  (cr <| rest, _, _, _, _)  => begin
                      @match false = BaseHashTable.hasKey(cr, irooted)
                      @assign rooted = BaseHashTable.add((cr, distance), irooted)
                    setRootDistance(rest, table, distance, nextLevel, rooted)
                  end

                  (_ <| rest, _, _, _, _)  => begin
                    setRootDistance(rest, table, distance, nextLevel, irooted)
                  end
                end
              end
               #= print(\"- NFOCConnectionGraph.setRootDistance: Set Distance \" +
               =#
               #=    ComponentRef.toString(cr) + \" , \" + intString(distance) + \"\\n\");
               =#
               #= print(\"- NFOCConnectionGraph.setRootDistance: add \" +
               =#
               #=    stringDelimitList(List.map(next,ComponentRef.toString),\"\\n\") + \" to the queue\\n\");
               =#
               #= print(\"- NFOCConnectionGraph.setRootDistance: Set Distance \" +
               =#
               #=    ComponentRef.toString(cr) + \" , \" + intString(distance) + \"\\n\");
               =#
               #= /*    case(cr::rest,_,_,_,_)
                    equation
                      i = BaseHashTable.get(cr, irooted);
                      print(\"- NFOCConnectionGraph.setRootDistance: found \" +
                         ComponentRef.toString(cr) + \" twice, value is \" + intString(i) + \"\\n\");
                    then
                      setRootDistance(rest,table,distance,nextLevel,irooted);
              */ =#
               #= equation
               =#
               #=   print(\"- NFOCConnectionGraph.setRootDistance: cannot found \" + ComponentRef.toString(cr) + \"\\n\");
               =#
          orooted
        end

        function addBranches(edge::Edge, itable::NFHashTable3.HashTable) ::NFHashTable3.HashTable
              local otable::NFHashTable3.HashTable

              local cref1::ComponentRef
              local cref2::ComponentRef

              @assign (cref1, cref2) = edge
              @assign otable = addConnectionRooted(cref1, cref2, itable)
              @assign otable = addConnectionRooted(cref2, cref1, otable)
          otable
        end

        function addConnectionsRooted(connection::FlatEdge, itable::NFHashTable3.HashTable) ::NFHashTable3.HashTable
              local otable::NFHashTable3.HashTable

              local cref1::ComponentRef
              local cref2::ComponentRef

              @assign (cref1, cref2, _) = connection
              @assign otable = addConnectionRooted(cref1, cref2, itable)
              @assign otable = addConnectionRooted(cref2, cref1, otable)
          otable
        end

        function addConnectionRooted(cref1::ComponentRef, cref2::ComponentRef, itable::NFHashTable3.HashTable) ::NFHashTable3.HashTable
              local otable::NFHashTable3.HashTable

              @assign otable = begin
                  local table::NFHashTable3.HashTable
                  local crefs::List{ComponentRef}
                @match (cref1, cref2, itable) begin
                  (_, _, _)  => begin
                      @assign crefs = begin
                        @matchcontinue () begin
                          ()  => begin
                            BaseHashTable.get(cref1, itable)
                          end

                          _  => begin
                              nil
                          end
                        end
                      end
                      @assign table = BaseHashTable.add((cref1, _cons(cref2, crefs)), itable)
                    table
                  end
                end
              end
          otable
        end

        """ #= evaluation of Connections.rooted, Connections.isRoot, Connections.uniqueRootIndices
         - replaces all [Connections.]rooted calls by true or false depending on wheter branche frame_a or frame_b is closer to root
         - return true or false for Connections.isRoot operator if is a root or not
         - return an array of indices for Connections.uniqueRootIndices, see Modelica_StateGraph2
           See Modelica_StateGraph2:
            https:github.com/modelica/Modelica_StateGraph2 and
            https:trac.modelica.org/Modelica/ticket/984 and
            http:www.ep.liu.se/ecp/043/041/ecp09430108.pdf
           for a specification of this operator =#"""
        function evalConnectionsOperators(inRoots::List{<:ComponentRef}, graph::NFOCConnectionGraph, inEquations::List{<:Equation}) ::List{Equation}
              local outEquations::List{Equation}

              @assign outEquations = begin
                  local rooted::NFHashTable.HashTable
                  local table::NFHashTable3.HashTable
                  local branches::Edges
                  local connections::FlatEdges
                @matchcontinue (inRoots, graph, inEquations) begin
                  (_, _,  nil())  => begin
                    nil
                  end

                  _  => begin
                        @assign table = NFHashTable3.emptyHashTable()
                        @assign branches = getBranches(graph)
                        @assign table = ListUtil.fold(branches, addBranches, table)
                        @assign connections = getConnections(graph)
                        @assign table = ListUtil.fold(connections, addConnectionsRooted, table)
                        @assign rooted = setRootDistance(inRoots, table, 0, nil, NFHashTable.emptyHashTable())
                        @assign outEquations = List(P_Equation.Equation.mapExp(eq, (rooted, inRoots, graph, P_Equation.Equation.info(eq)) -> evaluateOperators(rooted = rooted, roots = inRoots, graph = graph, info = P_Equation.Equation.info(eq))) for eq in inEquations)
                      outEquations
                  end
                end
              end
               #=  built table
               =#
               #=  add branches to table
               =#
               #=  add connections to table
               =#
               #=  get distance to root
               =#
               #=   print(\"Roots: \" + stringDelimitList(List.map(inRoots,ComponentRef.toString),\"\\n\") + \"\\n\");
               =#
               #=   BaseHashTable.dumpHashTable(table);
               =#
               #=   BaseHashTable.dumpHashTable(rooted);
               =#
          outEquations
        end

        function evaluateOperators(exp::Expression, rooted::NFHashTable.HashTable, roots::List{<:ComponentRef}, graph::NFOCConnectionGraph, info::SourceInfo) ::Expression


              @assign exp = map(exp, (rooted, roots, graph, info) -> evalConnectionsOperatorsHelper(rooted = rooted, roots = roots, graph = graph, info = info))
          exp
        end

        """ #= Helper function for evaluation of Connections.rooted, Connections.isRoot, Connections.uniqueRootIndices =#"""
        function evalConnectionsOperatorsHelper(exp::Expression, rooted::NFHashTable.HashTable, roots::List{<:ComponentRef}, graph::NFOCConnectionGraph, info::SourceInfo) ::Expression
              local outExp::Expression

              @assign outExp = begin
                  local uroots::Expression
                  local nodes::Expression
                  local message::Expression
                  local res::Expression
                  local cref::ComponentRef
                  local cref1::ComponentRef
                  local result::Bool
                  local branches::Edges
                  local lst::List{Expression}
                  local call::Call
                  local str::String
                @match exp begin
                  CALL_EXPRESSION(call = call && P_Call.TYPED_CALL(__))  => begin
                    begin
                      @match identifyConnectionsOperator(P_Function.name(call.fn)) begin
                        ConnectionsOperator.ROOTED  => begin
                             #=  handle rooted - with zero size array or the normal call
                             =#
                            @assign res = begin
                              @match call.arguments begin
                                P_Expression.Expression.ARRAY(elements =  nil()) <|  nil()  => begin
                                    if Flags.isSet(Flags.CGRAPH)
                                      print("- NFOCConnectionGraph.evalConnectionsOperatorsHelper: " + toString(exp) + " = false\\n")
                                    end
                                  P_Expression.Expression.BOOLEAN(false)
                                end

                                CREF_EXPRESSION(cref = cref) <|  nil()  => begin
                                     #=  normal call
                                     =#
                                     #=  find partner in branches
                                     =#
                                    @assign branches = getBranches(graph)
                                    try
                                      @assign cref1 = getEdge(cref, branches)
                                      if Flags.isSet(Flags.CGRAPH)
                                        print("- NFOCConnectionGraph.evalConnectionsOperatorsHelper: Found Branche Partner " + toString(cref) + ", " + toString(cref1) + "\\n")
                                      end
                                      @assign result = getRooted(cref, cref1, rooted)
                                      if Flags.isSet(Flags.CGRAPH)
                                        print("- NFOCConnectionGraph.evalConnectionsOperatorsHelper: " + toString(exp) + " = " + boolString(result) + "\\n")
                                      end
                                    catch
                                      @assign str = toString(cref)
                                      Error.addSourceMessage(Error.OCG_MISSING_BRANCH, list(str, str, str), info)
                                      @assign result = false
                                    end
                                     #=  print(\"- NFOCConnectionGraph.evalConnectionsOperatorsHelper: Found Branche Partner \" +
                                     =#
                                     #=    ComponentRef.toString(cref) + \", \" + ComponentRef.toString(cref1) + \"\\n\");
                                     =#
                                     #= print(\"- NFOCConnectionGraph.evalRootedAndIsRootHelper: \" +
                                     =#
                                     #=    ComponentRef.toString(cref) + \" is \" + boolString(result) + \" rooted\\n\");
                                     =#
                                     #=  add an error message:
                                     =#
                                  P_Expression.Expression.BOOLEAN(result)
                                end
                              end
                            end
                          res
                        end

                        ConnectionsOperator.IS_ROOT  => begin
                             #=  deal with Connections.isRoot - with zero size array and normal
                             =#
                            @assign res = begin
                              @match call.arguments begin
                                P_Expression.Expression.ARRAY(elements =  nil()) <|  nil()  => begin
                                    if Flags.isSet(Flags.CGRAPH)
                                      print("- NFOCConnectionGraph.evalConnectionsOperatorsHelper: " + toString(exp) + " = false\\n")
                                    end
                                  P_Expression.Expression.BOOLEAN(false)
                                end

                                CREF_EXPRESSION(cref = cref) <|  nil()  => begin
                                    @assign result = ListUtil.isMemberOnTrue(cref, roots, isEqual)
                                    if Flags.isSet(Flags.CGRAPH)
                                      print("- NFOCConnectionGraph.evalConnectionsOperatorsHelper: " + toString(exp) + " = " + boolString(result) + "\\n")
                                    end
                                  P_Expression.Expression.BOOLEAN(result)
                                end
                              end
                            end
                          res
                        end

                        ConnectionsOperator.UNIQUE_ROOT_INDICES  => begin
                             #=  deal with Connections.uniqueRootIndices, TODO! FIXME! actually implement this
                             =#
                            @assign res = begin
                              @match call.arguments begin
                                uroots && P_Expression.Expression.ARRAY(elements = lst) <| nodes <| message <|  nil()  => begin
                                    if Flags.isSet(Flags.CGRAPH)
                                      print("- NFOCConnectionGraph.evalConnectionsOperatorsHelper: Connections.uniqueRootsIndicies(" + toString(uroots) + "," + toString(nodes) + "," + toString(message) + ")\\n")
                                    end
                                    @assign lst = ListUtil.fill(INTEGER_EXPRESSION(1), listLength(lst))
                                  P_Expression.Expression.makeArray(TYPE_INTEGER(), lst)
                                end
                              end
                            end
                             #=  TODO! FIXME! actually implement this correctly
                             =#
                          res
                        end

                        _  => begin
                            exp
                        end
                      end
                    end
                  end

                  _  => begin
                      exp
                  end
                end
              end
               #=  no replacement needed
               =#
          outExp
        end

        function getRooted(cref1::ComponentRef, cref2::ComponentRef, rooted::NFHashTable.HashTable) ::Bool
              local result::Bool

              @assign result = begin
                  local i1::Integer
                  local i2::Integer
                @matchcontinue (cref1, cref2, rooted) begin
                  (_, _, _)  => begin
                      @assign i1 = BaseHashTable.get(cref1, rooted)
                      @assign i2 = BaseHashTable.get(cref2, rooted)
                    intLt(i1, i2)
                  end

                  _  => begin
                      true
                  end
                end
              end
               #=  in fail case return true
               =#
          result
        end

        """ #= return the Edge partner of a edge, fails if not found =#"""
        function getEdge(cr::ComponentRef, edges::Edges) ::ComponentRef
              local ocr::ComponentRef

              @assign ocr = begin
                  local rest::Edges
                  local cref1::ComponentRef
                  local cref2::ComponentRef
                @matchcontinue (cr, edges) begin
                  (_, (cref1, cref2) <| _)  => begin
                      @assign cref1 = getEdge1(cr, cref1, cref2)
                    cref1
                  end

                  (_, _ <| rest)  => begin
                    getEdge(cr, rest)
                  end
                end
              end
          ocr
        end

        """ #= return the Edge partner of a edge, fails if not found =#"""
        function getEdge1(cr::ComponentRef, cref1::ComponentRef, cref2::ComponentRef) ::ComponentRef
              local ocr::ComponentRef

              @assign ocr = begin
                @matchcontinue (cr, cref1, cref2) begin
                  (_, _, _)  => begin
                      @match true = isEqual(cr, cref1)
                    cref2
                  end

                  _  => begin
                        @match true = isEqual(cr, cref2)
                      cref1
                  end
                end
              end
          ocr
        end

        """ #= prints the connection str =#"""
        function printConnectionStr(connectTuple::FlatEdge, ty::String) ::String
              local outStr::String

              @assign outStr = begin
                  local c1::ComponentRef
                  local c2::ComponentRef
                  local str::String
                @match (connectTuple, ty) begin
                  ((c1, c2, _), _)  => begin
                      @assign str = ty + "(" + toString(c1) + ", " + toString(c2) + ")"
                    str
                  end
                end
              end
          outStr
        end

        """ #= Prints a list of edges to stdout. =#"""
        function printEdges(inEdges::Edges)
              @assign _ = begin
                  local c1::ComponentRef
                  local c2::ComponentRef
                  local tail::Edges
                @match inEdges begin
                   nil()  => begin
                    ()
                  end

                  (c1, c2) <| tail  => begin
                      print("    ")
                      print(toString(c1))
                      print(" -- ")
                      print(toString(c2))
                      print("\\n")
                      printEdges(tail)
                    ()
                  end
                end
              end
        end

        """ #= Prints a list of dae edges to stdout. =#"""
        function printFlatEdges(inEdges::FlatEdges)
              @assign _ = begin
                  local c1::ComponentRef
                  local c2::ComponentRef
                  local tail::FlatEdges
                @match inEdges begin
                   nil()  => begin
                    ()
                  end

                  (c1, c2, _) <| tail  => begin
                      print("    ")
                      print(toString(c1))
                      print(" -- ")
                      print(toString(c2))
                      print("\\n")
                      printFlatEdges(tail)
                    ()
                  end
                end
              end
        end

        """ #= Prints the content of NFOCConnectionGraph structure. =#"""
        function printNFOCConnectionGraph(inGraph::NFOCConnectionGraph)
              @assign _ = begin
                  local connections::FlatEdges
                  local branches::Edges
                @match inGraph begin
                  GRAPH(connections = connections, branches = branches)  => begin
                      print("Connections:\\n")
                      printFlatEdges(connections)
                      print("Branches:\\n")
                      printEdges(branches)
                    ()
                  end
                end
              end
        end

        """ #= Accessor for NFOCConnectionGraph.definiteRoots. =#"""
        function getDefiniteRoots(inGraph::NFOCConnectionGraph) ::DefiniteRoots
              local outResult::DefiniteRoots

              @assign outResult = begin
                  local result::DefiniteRoots
                @match inGraph begin
                  GRAPH(definiteRoots = result)  => begin
                    result
                  end
                end
              end
          outResult
        end

        """ #= Accessor for NFOCConnectionGraph.uniqueRoots. =#"""
        function getUniqueRoots(inGraph::NFOCConnectionGraph) ::UniqueRoots
              local outResult::UniqueRoots

              @assign outResult = begin
                  local result::UniqueRoots
                @match inGraph begin
                  GRAPH(uniqueRoots = result)  => begin
                    result
                  end
                end
              end
          outResult
        end

        """ #= Accessor for NFOCConnectionGraph.potentialRoots. =#"""
        function getPotentialRoots(inGraph::NFOCConnectionGraph) ::PotentialRoots
              local outResult::PotentialRoots

              @assign outResult = begin
                  local result::PotentialRoots
                @match inGraph begin
                  GRAPH(potentialRoots = result)  => begin
                    result
                  end
                end
              end
          outResult
        end

        """ #= Accessor for NFOCConnectionGraph.branches. =#"""
        function getBranches(inGraph::NFOCConnectionGraph) ::Edges
              local outResult::Edges

              @assign outResult = begin
                  local result::Edges
                @match inGraph begin
                  GRAPH(branches = result)  => begin
                    result
                  end
                end
              end
          outResult
        end

        """ #= Accessor for NFOCConnectionGraph.connections. =#"""
        function getConnections(inGraph::NFOCConnectionGraph) ::FlatEdges
              local outResult::FlatEdges

              @assign outResult = begin
                  local result::FlatEdges
                @match inGraph begin
                  GRAPH(connections = result)  => begin
                    result
                  end
                end
              end
          outResult
        end

        """ #= merge two NFOCConnectionGraphs =#"""
        function merge(inGraph1::NFOCConnectionGraph, inGraph2::NFOCConnectionGraph) ::NFOCConnectionGraph
              local outGraph::NFOCConnectionGraph

              @assign outGraph = begin
                  local updateGraph::Bool
                  local updateGraph1::Bool
                  local updateGraph2::Bool
                  local definiteRoots::DefiniteRoots
                  local definiteRoots1::DefiniteRoots
                  local definiteRoots2::DefiniteRoots
                  local uniqueRoots::UniqueRoots
                  local uniqueRoots1::UniqueRoots
                  local uniqueRoots2::UniqueRoots
                  local potentialRoots::PotentialRoots
                  local potentialRoots1::PotentialRoots
                  local potentialRoots2::PotentialRoots
                  local branches::Edges
                  local branches1::Edges
                  local branches2::Edges
                  local connections::FlatEdges
                  local connections1::FlatEdges
                  local connections2::FlatEdges
                   #=  left is empty, return right
                   =#
                @matchcontinue (inGraph1, inGraph2) begin
                  (_, GRAPH(definiteRoots =  nil(), potentialRoots =  nil(), uniqueRoots =  nil(), branches =  nil(), connections =  nil()))  => begin
                    inGraph1
                  end

                  (GRAPH(definiteRoots =  nil(), potentialRoots =  nil(), uniqueRoots =  nil(), branches =  nil(), connections =  nil()), _)  => begin
                    inGraph2
                  end

                  (_, _)  => begin
                      equality(inGraph1, inGraph2)
                    inGraph1
                  end

                  (GRAPH(updateGraph = updateGraph1, definiteRoots = definiteRoots1, potentialRoots = potentialRoots1, uniqueRoots = uniqueRoots1, branches = branches1, connections = connections1), GRAPH(updateGraph = updateGraph2, definiteRoots = definiteRoots2, potentialRoots = potentialRoots2, uniqueRoots = uniqueRoots2, branches = branches2, connections = connections2))  => begin
                      if Flags.isSet(Flags.CGRAPH)
                        Debug.trace("- NFOCConnectionGraph.merge()\\n")
                      end
                      @assign updateGraph = boolOr(updateGraph1, updateGraph2)
                      @assign definiteRoots = ListUtil.union(definiteRoots1, definiteRoots2)
                      @assign potentialRoots = ListUtil.union(potentialRoots1, potentialRoots2)
                      @assign uniqueRoots = ListUtil.union(uniqueRoots1, uniqueRoots2)
                      @assign branches = ListUtil.union(branches1, branches2)
                      @assign connections = ListUtil.union(connections1, connections2)
                    GRAPH(updateGraph, definiteRoots, potentialRoots, uniqueRoots, branches, connections)
                  end
                end
              end
          outGraph
        end

         #= /***********************************************************************************************************************/ =#
         #= /******************************************* GraphViz generation *******************************************************/ =#
         #= /***********************************************************************************************************************/ =#

        function graphVizEdge(inEdge::Edge) ::String
              local out::String

              @assign out = begin
                  local c1::ComponentRef
                  local c2::ComponentRef
                  local strEdge::String
                @match inEdge begin
                  (c1, c2)  => begin
                      @assign strEdge = "\\" + toString(c1) + "\\ -- \\" + toString(c2) + "\\" + " [color = blue, dir = \\none\\, fontcolor=blue, label = \\branch\\];\\n\\t"
                    strEdge
                  end
                end
              end
          out
        end

        function graphVizFlatEdge(inFlatEdge::FlatEdge, inBrokenFlatEdges::FlatEdges) ::String
              local out::String

              @assign out = begin
                  local c1::ComponentRef
                  local c2::ComponentRef
                  local sc1::String
                  local sc2::String
                  local strFlatEdge::String
                  local label::String
                  local labelFontSize::String
                  local decorate::String
                  local color::String
                  local style::String
                  local fontColor::String
                  local isBroken::Bool
                @match (inFlatEdge, inBrokenFlatEdges) begin
                  ((c1, c2, _), _)  => begin
                      @assign isBroken = ListUtil.isMemberOnTrue(inFlatEdge, inBrokenFlatEdges, FlatEdgeIsEqual)
                      @assign label = if isBroken
                            "[[broken connect]]"
                          else
                            "connect"
                          end
                      @assign color = if isBroken
                            "red"
                          else
                            "green"
                          end
                      @assign style = if isBroken
                            "\\bold, dashed\\"
                          else
                            "solid"
                          end
                      @assign decorate = boolString(isBroken)
                      @assign fontColor = if isBroken
                            "red"
                          else
                            "green"
                          end
                      @assign labelFontSize = if isBroken
                            "labelfontsize = 20.0, "
                          else
                            ""
                          end
                      @assign sc1 = toString(c1)
                      @assign sc2 = toString(c2)
                      @assign strFlatEdge = stringAppendList(list("\\", sc1, "\\ -- \\", sc2, "\\ [", "dir = \\none\\, ", "style = ", style, ", ", "decorate = ", decorate, ", ", "color = ", color, ", ", labelFontSize, "fontcolor = ", fontColor, ", ", "label = \\", label, "\\", "];\\n\\t"))
                    strFlatEdge
                  end
                end
              end
          out
        end

        function FlatEdgeIsEqual(inEdge1::FlatEdge, inEdge2::FlatEdge) ::Bool
              local isEqual::Bool

              @assign isEqual = isEqual(Util.tuple31(inEdge1), Util.tuple31(inEdge2)) && isEqual(Util.tuple32(inEdge1), Util.tuple32(inEdge2))
          isEqual
        end

        function graphVizDefiniteRoot(inDefiniteRoot::DefiniteRoot, inFinalRoots::DefiniteRoots) ::String
              local out::String

              @assign out = begin
                  local c::ComponentRef
                  local strDefiniteRoot::String
                  local isSelectedRoot::Bool
                @match (inDefiniteRoot, inFinalRoots) begin
                  (c, _)  => begin
                      @assign isSelectedRoot = ListUtil.isMemberOnTrue(c, inFinalRoots, isEqual)
                      @assign strDefiniteRoot = "\\" + toString(c) + "\\" + " [fillcolor = red, rank = \\source\\, label = " + "\\" + toString(c) + "\\, " + (if isSelectedRoot
                            "shape=polygon, sides=8, distortion=\\0.265084\\, orientation=26, skew=\\0.403659\\"
                          else
                            "shape=box"
                          end) + "];\\n\\t"
                    strDefiniteRoot
                  end
                end
              end
          out
        end

        function graphVizPotentialRoot(inPotentialRoot::PotentialRoot, inFinalRoots::DefiniteRoots) ::String
              local out::String

              @assign out = begin
                  local c::ComponentRef
                  local priority::AbstractFloat
                  local strPotentialRoot::String
                  local isSelectedRoot::Bool
                @match (inPotentialRoot, inFinalRoots) begin
                  ((c, priority), _)  => begin
                      @assign isSelectedRoot = ListUtil.isMemberOnTrue(c, inFinalRoots, isEqual)
                      @assign strPotentialRoot = "\\" + toString(c) + "\\" + " [fillcolor = orangered, rank = \\min\\ label = " + "\\" + toString(c) + "\\\\n" + realString(priority) + "\\, " + (if isSelectedRoot
                            "shape=ploygon, sides=7, distortion=\\0.265084\\, orientation=26, skew=\\0.403659\\"
                          else
                            "shape=box"
                          end) + "];\\n\\t"
                    strPotentialRoot
                  end
                end
              end
          out
        end

        """ #= @author: adrpo
          Generate a graphviz file out of the connection graph =#"""
        function generateGraphViz(modelNameQualified::String, definiteRoots::DefiniteRoots, potentialRoots::PotentialRoots, uniqueRoots::UniqueRoots, branches::Edges, connections::FlatEdges, finalRoots::DefiniteRoots, broken::FlatEdges) ::String
              local brokenConnectsViaGraphViz::String

              @assign brokenConnectsViaGraphViz = begin
                  local fileName::String
                  local i::String
                  local nrDR::String
                  local nrPR::String
                  local nrUR::String
                  local nrBR::String
                  local nrCO::String
                  local nrFR::String
                  local nrBC::String
                  local timeStr::String
                  local infoNodeStr::String
                  local brokenConnects::String
                  local tStart::AbstractFloat
                  local tEnd::AbstractFloat
                  local t::AbstractFloat
                  local graphVizStream
                  local infoNode::List{String}
                   #=  don't do anything if we don't have -d=cgraphGraphVizFile or -d=cgraphGraphVizShow
                   =#
                @matchcontinue (modelNameQualified, definiteRoots, potentialRoots, uniqueRoots, branches, connections, finalRoots, broken) begin
                  (_, _, _, _, _, _, _, _)  => begin
                      @match false = boolOr(Flags.isSet(Flags.CGRAPH_GRAPHVIZ_FILE), Flags.isSet(Flags.CGRAPH_GRAPHVIZ_SHOW))
                    ""
                  end

                  (_, _, _, _, _, _, _, _)  => begin
                      @assign tStart = clock()
                      @assign i = "\\t"
                      @assign fileName = stringAppend(modelNameQualified, ".gv")
                      @assign graphVizStream = IOStream.create(fileName, IOStream.LIST())
                      @assign nrDR = intString(listLength(definiteRoots))
                      @assign nrPR = intString(listLength(potentialRoots))
                      @assign nrUR = intString(listLength(uniqueRoots))
                      @assign nrBR = intString(listLength(branches))
                      @assign nrCO = intString(listLength(connections))
                      @assign nrFR = intString(listLength(finalRoots))
                      @assign nrBC = intString(listLength(broken))
                      @assign infoNode = list("// Generated by OpenModelica. \\n", "// Overconstrained connection graph for model: \\n//    ", modelNameQualified, "\\n", "// \\n", "// Summary: \\n", "//   Roots:              ", nrDR, "\\n", "//   Potential Roots:    ", nrPR, "\\n", "//   Unique Roots:       ", nrUR, "\\n", "//   Branches:           ", nrBR, "\\n", "//   Connections:        ", nrCO, "\\n", "//   Final Roots:        ", nrFR, "\\n", "//   Broken Connections: ", nrBC, "\\n")
                      @assign infoNodeStr = stringAppendList(infoNode)
                      @assign infoNodeStr = System.stringReplace(infoNodeStr, "\\n", "\\\\l")
                      @assign infoNodeStr = System.stringReplace(infoNodeStr, "\\t", " ")
                      @assign infoNodeStr = System.stringReplace(infoNodeStr, "/", "")
                      @assign graphVizStream = IOStream.appendList(graphVizStream, infoNode)
                      @assign graphVizStream = IOStream.appendList(graphVizStream, list("\\n\\n"))
                      @assign graphVizStream = IOStream.appendList(graphVizStream, list("graph \\", modelNameQualified, "\\\\n{\\n\\n"))
                      @assign graphVizStream = IOStream.appendList(graphVizStream, list(i, "overlap=false;\\n"))
                      @assign graphVizStream = IOStream.appendList(graphVizStream, list(i, "layout=dot;\\n\\n"))
                      @assign graphVizStream = IOStream.appendList(graphVizStream, list(i, "node [", "fillcolor = \\lightsteelblue1\\, ", "shape = box, ", "style = \\bold, filled\\, ", "rank = \\max\\", "]\\n\\n"))
                      @assign graphVizStream = IOStream.appendList(graphVizStream, list(i, "edge [", "color = \\black\\, ", "style = bold", "]\\n\\n"))
                      @assign graphVizStream = IOStream.appendList(graphVizStream, list(i, "graph [fontsize=20, fontname = \\Courier Bold\\ label= \\\\\\n\\\\n", infoNodeStr, "\\, size=\\6,6\\];\\n", i))
                      @assign graphVizStream = IOStream.appendList(graphVizStream, list("\\n", i, "// Definite Roots (Connections.root)", "\\n", i))
                      @assign graphVizStream = IOStream.appendList(graphVizStream, ListUtil.map1(definiteRoots, graphVizDefiniteRoot, finalRoots))
                      @assign graphVizStream = IOStream.appendList(graphVizStream, list("\\n", i, "// Potential Roots (Connections.potentialRoot)", "\\n", i))
                      @assign graphVizStream = IOStream.appendList(graphVizStream, ListUtil.map1(potentialRoots, graphVizPotentialRoot, finalRoots))
                      @assign graphVizStream = IOStream.appendList(graphVizStream, list("\\n", i, "// Branches (Connections.branch)", "\\n", i))
                      @assign graphVizStream = IOStream.appendList(graphVizStream, ListUtil.map(branches, graphVizEdge))
                      @assign graphVizStream = IOStream.appendList(graphVizStream, list("\\n", i, "// Connections (connect)", "\\n", i))
                      @assign graphVizStream = IOStream.appendList(graphVizStream, ListUtil.map1(connections, graphVizFlatEdge, broken))
                      @assign graphVizStream = IOStream.appendList(graphVizStream, list("\\n}\\n"))
                      @assign tEnd = clock()
                      @assign t = tEnd - tStart
                      @assign timeStr = realString(t)
                      @assign graphVizStream = IOStream.appendList(graphVizStream, list("\\n\\n\\n// graph generation took: ", timeStr, " seconds\\n"))
                      System.writeFile(fileName, IOStream.string(graphVizStream))
                      print("GraphViz with connection graph for model: " + modelNameQualified + " was writen to file: " + fileName + "\\n")
                      @assign brokenConnects = showGraphViz(fileName, modelNameQualified)
                    brokenConnects
                  end
                end
              end
               #=  create a stream
               =#
               #=  replace \\n with \\\\l (left align), replace \\t with \" \"
               =#
               #=  replace / with \"\"
               =#
               #=  output header
               =#
               #=  output command to be used
               =#
               #=  output graphviz header
               =#
               #=  output global settings
               =#
               #=  output settings for nodes
               =#
               #=  output settings for edges
               =#
               #=  output summary node
               =#
               #=  output definite roots
               =#
               #=  output potential roots
               =#
               #=  output branches
               =#
               #=  output connections
               =#
               #=  output graphviz footer
               =#
          brokenConnectsViaGraphViz
        end

        function showGraphViz(fileNameGraphViz::String, modelNameQualified::String) ::String
              local brokenConnectsViaGraphViz::String

              @assign brokenConnectsViaGraphViz = begin
                  local leftyCMD::String
                  local fileNameTraceRemovedConnections::String
                  local omhome::String
                  local brokenConnects::String
                  local leftyExitStatus::Integer
                   #=  do not start graphviz if we don't have -d=cgraphGraphVizShow
                   =#
                @matchcontinue (fileNameGraphViz, modelNameQualified) begin
                  (_, _)  => begin
                      @match false = Flags.isSet(Flags.CGRAPH_GRAPHVIZ_SHOW)
                    ""
                  end

                  _  => begin
                        @assign fileNameTraceRemovedConnections = modelNameQualified + "_removed_connections.txt"
                        print("Tyring to start GraphViz *lefty* to visualize the graph. You need to have lefty in your PATH variable\\n")
                        print("Make sure you quit GraphViz *lefty* via Right Click->quit to be sure the process will be exited.\\n")
                        print("If you quit the GraphViz *lefty* window via X, please kill the process in task manager to continue.\\n")
                        @assign omhome = Settings.getInstallationDirectoryPath()
                        @assign omhome = System.stringReplace(omhome, "\\", "")
                        @assign leftyCMD = "load('" + omhome + "/share/omc/scripts/openmodelica.lefty');" + "openmodelica.init();openmodelica.createviewandgraph('" + fileNameGraphViz + "','file',null,null);txtview('off');"
                        print("Running command: " + "lefty -e " + leftyCMD + " > " + fileNameTraceRemovedConnections + "\\n")
                        @assign leftyExitStatus = System.systemCall("lefty -e " + leftyCMD, fileNameTraceRemovedConnections)
                        print("GraphViz *lefty* exited with status:" + intString(leftyExitStatus) + "\\n")
                        @assign brokenConnects = System.readFile(fileNameTraceRemovedConnections)
                        print("GraphViz OpenModelica assistant returned the following broken connects: " + brokenConnects + "\\n")
                      brokenConnects
                  end
                end
              end
               #=  omhome = System.stringReplace(omhome, \"\\\\\", \"/\");
               =#
               #=  create a lefty command and execute it
               =#
               #=  execute lefty
               =#
               #=  show the exit status
               =#
          brokenConnectsViaGraphViz
        end

        """ #= @author adrpo:
         this function removes the BROKEN connects from the equation list
         and keeps the CONNECTED ones. =#"""
        function removeBrokenConnects(inEquations::List{<:Equation}, inConnected::FlatEdges, inBroken::FlatEdges) ::List{Equation}
              local outEquations::List{Equation}

              @assign outEquations = begin
                  local toRemove::List{ComponentRef}
                  local toKeep::List{ComponentRef}
                  local intersect::List{ComponentRef}
                  local c1::ComponentRef
                  local c2::ComponentRef
                  local lhs::ComponentRef
                  local rhs::ComponentRef
                  local eql::List{Equation} = nil
                  local isThere::Bool
                  local str::String
                  local ty1::M_Type
                  local ty2::M_Type
                  local source::DAE.ElementSource
                   #=  if we have no broken then we don't care!
                   =#
                @match (inEquations, inConnected, inBroken) begin
                  (_, _,  nil())  => begin
                    inEquations
                  end

                  (_, _, _)  => begin
                       #=  if we have nothing toRemove then we don't care!
                       =#
                      for eq in inEquations
                        @assign eql = begin
                          @match eq begin
                            EQUATION_CONNECT(lhs = CREF_EXPRESSION(ty = ty1, cref = lhs), rhs = CREF_EXPRESSION(ty = ty2, cref = rhs), source = source)  => begin
                                if ! (isDeleted(lhs) || isDeleted(rhs))
                                  @assign isThere = false
                                  for tpl in inBroken
                                    @assign c1 = Util.tuple31(tpl)
                                    @assign c2 = Util.tuple32(tpl)
                                    if isEqual(c1, lhs) && isEqual(c2, rhs) || isEqual(c2, lhs) && isEqual(c1, rhs)
                                      @assign isThere = true
                                      break
                                    end
                                  end
                                end
                                 #=  check for equality
                                 =#
                                if ! isThere
                                  @assign eql = _cons(eq, eql)
                                end
                              eql
                            end

                            _  => begin
                                _cons(eq, eql)
                            end
                          end
                        end
                      end
                      @assign eql = listReverseInPlace(eql)
                      if Flags.isSet(Flags.CGRAPH)
                        @assign str = ""
                        for tpl in inBroken
                          @assign c1 = Util.tuple31(tpl)
                          @assign c2 = Util.tuple32(tpl)
                          @assign str = str + "connect(" + toString(c1) + ", " + toString(c2) + ")\\n"
                        end
                        print("- NFOCConnectionGraph.removeBrokenConnects:\\n" + str + "\\n")
                      end
                    eql
                  end
                end
              end
          outEquations
        end

        """ #= @author: adrpo
         adds all the equalityConstraint equations from broken connections =#"""
        function addBrokenEqualityConstraintEquations(inEquations::List{<:Equation}, inBroken::FlatEdges) ::List{Equation}
              local outEquations::List{Equation}

              @assign outEquations = begin
                  local equalityConstraintElements::List{Equation}
                  local eqs::List{Equation}
                @matchcontinue (inEquations, inBroken) begin
                  (_,  nil())  => begin
                    inEquations
                  end

                  _  => begin
                        @assign equalityConstraintElements = ListUtil.flatten(ListUtil.map(inBroken, Util.tuple33))
                        @assign eqs = listAppend(equalityConstraintElements, inEquations)
                      eqs
                  end
                end
              end
          outEquations
        end

        function identifyConnectionsOperator(functionName::Absyn.Path) ::ConnectionsOperator
              local call::ConnectionsOperator

              @assign call = begin
                  local name::String
                @match functionName begin
                  Absyn.QUALIFIED(name = "Connections", path = Absyn.IDENT(name = name))  => begin
                    begin
                      @match name begin
                        "branch"  => begin
                          ConnectionsOperator.BRANCH
                        end

                        "root"  => begin
                          ConnectionsOperator.ROOT
                        end

                        "potentialRoot"  => begin
                          ConnectionsOperator.POTENTIAL_ROOT
                        end

                        "isRoot"  => begin
                          ConnectionsOperator.IS_ROOT
                        end

                        "rooted"  => begin
                          ConnectionsOperator.ROOTED
                        end

                        "uniqueRoot"  => begin
                          ConnectionsOperator.UNIQUE_ROOT
                        end

                        "uniqueRootIndices"  => begin
                          ConnectionsOperator.UNIQUE_ROOT_INDICES
                        end

                        _  => begin
                            ConnectionsOperator.NOT_OPERATOR
                        end
                      end
                    end
                  end

                  Absyn.IDENT(name = "rooted")  => begin
                    ConnectionsOperator.ROOTED
                  end

                  _  => begin
                      ConnectionsOperator.NOT_OPERATOR
                  end
                end
              end
          call
        end

    @exportAll()
  end
