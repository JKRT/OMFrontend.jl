EquationFn = Function
AlgorithmFn = Function
FoldFn = Function
MapFn = Function
EquationFn = Function
AlgorithmFn = Function
EquationFn = Function
AlgorithmFn = Function

@UniontypeDecl NFSections
#=Necessary redefinitions=#
Sections = NFSections
Equation = NFEquation
Algorithm = NFAlgorithm

@Uniontype NFSections begin
  @Record SECTIONS_EMPTY begin
  end
  @Record SECTIONS_EXTERNAL begin
    name::String
    args::List{Expression}
    outputRef::ComponentRef
    language::String
    ann::Option{SCode.Annotation}
    explicit::Bool
  end
  @Record SECTIONS begin
    equations::List{Equation}
    initialEquations::List{Equation}
    algorithms::List{Algorithm}
    initialAlgorithms::List{Algorithm}
  end
end

function isEmpty(sections::Sections)::Bool
  local isEmpty::Bool
  @assign isEmpty = begin
    @match sections begin
      EMPTY(__) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return isEmpty
end

function apply(
  sections::Sections,
  eqFn::EquationFn,
  algFn::AlgorithmFn,
  ieqFn::EquationFn = eqFn,
  ialgFn::AlgorithmFn = algFn,
)
  return @assign () = begin
    @match sections begin
      SECTIONS(__) => begin
        for eq in sections.equations
          eqFn(eq)
        end
        for ieq in sections.initialEquations
          ieqFn(ieq)
        end
        for alg in sections.algorithms
          algFn(alg)
        end
        for ialg in sections.initialAlgorithms
          ialgFn(ialg)
        end
        ()
      end
      _ => begin
        ()
      end
    end
  end
end

function foldExp(sections::Sections, foldFn::FoldFn, arg::ArgT) where {ArgT}
  @assign arg = begin
    @match sections begin
      SECTIONS(__) => begin
        @assign arg = P_Equation.Equation.foldExpList(sections.equations, foldFn, arg)
        @assign arg =
          P_Equation.Equation.foldExpList(sections.initialEquations, foldFn, arg)
        @assign arg = P_Algorithm.Algorithm.foldExpList(sections.algorithms, foldFn, arg)
        @assign arg =
          P_Algorithm.Algorithm.foldExpList(sections.initialAlgorithms, foldFn, arg)
        arg
      end
      SECTIONS_EXTERNAL(__) => begin
        ListUtil.fold(sections.args, foldFn, arg)
      end
      _ => begin
        arg
      end
    end
  end
  return arg
end

function mapExp(sections::Sections, mapFn::MapFn)::Sections
  local eq::List{Equation}
  local ieq::List{Equation}
  local alg::List{Algorithm}
  local ialg::List{Algorithm}
  @assign sections = begin
    @match sections begin
      SECTIONS(__) => begin
        @assign eq = mapExpList(sections.equations, mapFn)
        @assign ieq = mapExpList(sections.initialEquations, mapFn)
        @assign alg = P_Algorithm.Algorithm.mapExpList(sections.algorithms, mapFn)
        @assign ialg = P_Algorithm.Algorithm.mapExpList(sections.initialAlgorithms, mapFn)
        SECTIONS(eq, ieq, alg, ialg)
      end
      SECTIONS_EXTERNAL(__) => begin
        @assign sections.args = List(mapFn(e) for e in sections.args)
        sections
      end
      _ => begin
        sections
      end
    end
  end
  return sections
end

function map1(
  sections::Sections,
  arg::ArgT,
  eqFn::EquationFn,
  algFn::AlgorithmFn,
  ieqFn::EquationFn = eqFn,
  ialgFn::AlgorithmFn = algFn,
) where {ArgT}

  local eq::List{Equation}
  local ieq::List{Equation}
  local alg::List{Algorithm}
  local ialg::List{Algorithm}

  @assign () = begin
    @match sections begin
      SECTIONS(__) => begin
        @assign eq = List(eqFn(e, arg) for e in sections.equations)
        @assign ieq = List(ieqFn(e, arg) for e in sections.initialEquations)
        @assign alg = List(algFn(a, arg) for a in sections.algorithms)
        @assign ialg = List(ialgFn(a, arg) for a in sections.initialAlgorithms)
        @assign sections = SECTIONS(eq, ieq, alg, ialg)
        ()
      end
      _ => begin
        ()
      end
    end
  end
  return sections
end

function map(
  sections::Sections,
  eqFn::EquationFn,
  algFn::AlgorithmFn,
  ieqFn::EquationFn = eqFn,
  ialgFn::AlgorithmFn = algFn,
)::Sections
  local eq::List{Equation}
  local ieq::List{Equation}
  local alg::List{Algorithm}
  local ialg::List{Algorithm}
  @assign () = begin
    @match sections begin
      SECTIONS(__) => begin
        @assign eq = list(eqFn(e) for e in sections.equations)
        @assign ieq = list(ieqFn(e) for e in sections.initialEquations)
        @assign alg = list(algFn(a) for a in sections.algorithms)
        @assign ialg = list(ialgFn(a) for a in sections.initialAlgorithms)
        @assign sections = SECTIONS(eq, ieq, alg, ialg)
        ()
      end
      _ => begin
        ()
      end
    end
  end
  return sections
end

function join(sections1::Sections, sections2::Sections)::Sections
  local sections::Sections

  @assign sections = begin
    @match (sections1, sections2) begin
      (SECTIONS_EMPTY(__), _) => begin
        sections2
      end

      (_, SECTIONS_EMPTY(__)) => begin
        sections1
      end

      (SECTIONS(__), SECTIONS(__)) => begin
        SECTIONS(
          listAppend(sections1.equations, sections2.equations),
          listAppend(sections1.initialEquations, sections2.initialEquations),
          listAppend(sections1.algorithms, sections2.algorithms),
          listAppend(sections1.initialAlgorithms, sections2.initialAlgorithms),
        )
      end
    end
  end
  return sections
end

function append(
  equations::List{<:Equation},
  initialEquations::List{<:Equation},
  algorithms::List{<:Algorithm},
  initialAlgorithms::List{<:Algorithm},
  sections::Sections,
)::Sections

  @assign sections = begin
    @match sections begin
      SECTIONS(__) => begin
        SECTIONS(
          listAppend(sections.equations, equations),
          listAppend(sections.initialEquations, initialEquations),
          listAppend(sections.algorithms, algorithms),
          listAppend(sections.initialAlgorithms, initialAlgorithms),
        )
      end

      _ => begin
        SECTIONS(equations, initialEquations, algorithms, initialAlgorithms)
      end
    end
  end
  return sections
end

function prependAlgorithm(
  alg::Algorithm,
  sections::Sections,
  isInitial::Bool = false,
)::Sections

  @assign sections = begin
    @match sections begin
      SECTIONS(__) => begin
        if isInitial
          @assign sections.initialAlgorithms = _cons(alg, sections.initialAlgorithms)
        else
          @assign sections.algorithms = _cons(alg, sections.algorithms)
        end
        sections
      end

      EMPTY(__) => begin
        if isInitial
          SECTIONS(nil, nil, nil, list(alg))
        else
          SECTIONS(nil, nil, list(alg), nil)
        end
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got invalid Sections to prepend algorithm to",
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return sections
end

function prependEquation(
  eq::Equation,
  sections::Sections,
  isInitial::Bool = false,
)::Sections

  @assign sections = begin
    @match sections begin
      SECTIONS(__) => begin
        if isInitial
          @assign sections.initialEquations = _cons(eq, sections.initialEquations)
        else
          @assign sections.equations = _cons(eq, sections.equations)
        end
        sections
      end

      EMPTY(__) => begin
        if isInitial
          SECTIONS(nil, list(eq), nil, nil)
        else
          SECTIONS(list(eq), nil, nil, nil)
        end
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got invalid Sections to prepend equation to",
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return sections
end

function prepend(
  equations::List{<:Equation},
  initialEquations::List{<:Equation},
  algorithms::List{<:Algorithm},
  initialAlgorithms::List{<:Algorithm},
  sections::Sections,
)::Sections

  @assign sections = begin
    @match sections begin
      SECTIONS(__) => begin
        SECTIONS(
          listAppend(equations, sections.equations),
          listAppend(initialEquations, sections.initialEquations),
          listAppend(algorithms, sections.algorithms),
          listAppend(initialAlgorithms, sections.initialAlgorithms),
        )
      end

      _ => begin
        SECTIONS(equations, initialEquations, algorithms, initialAlgorithms)
      end
    end
  end
  return sections
end

function new(
  equations::List{<:Equation},
  initialEquations::List{<:Equation},
  algorithms::List{<:Algorithm},
  initialAlgorithms::List{<:Algorithm},
)::Sections
  local sections::Sections

  if listEmpty(equations) &&
     listEmpty(initialEquations) &&
     listEmpty(algorithms) &&
     listEmpty(initialAlgorithms)
    @assign sections = SECTIONS_EMPTY()
  else
    @assign sections = SECTIONS(equations, initialEquations, algorithms, initialAlgorithms)
  end
  return sections
end
