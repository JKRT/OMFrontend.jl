FuncT = Function

M_Type = NFType
ComponentRef = NFComponentRef
ComplexType = NFComplexType
Subscript = NFSubscript
Dimension = NFDimension


FunctionType = (() -> begin #= Enumeration =#
  FUNCTIONAL_PARAMETER = 1  #= Function parameter of function type. =#
  FUNCTION_REFERENCE = 2  #= Function name used to reference a function. =#
  FUNCTIONAL_VARIABLE = 3  #= A variable that contains a function reference. =#
  () -> (FUNCTIONAL_PARAMETER; FUNCTION_REFERENCE; FUNCTIONAL_VARIABLE)  #= A variable that contains a function reference. =#
end)()

M_Type=NFType

@UniontypeDecl NFType
@Uniontype NFType begin
  @Record SUBSCRIPTED_TYPE begin
    name::String
    ty::M_Type
    subs::List{M_Type}
    subscriptedTy::M_Type
  end

  @Record TYPE_ANY begin
  end

  @Record TYPE_POLYMORPHIC begin
    name::String
  end

  @Record TYPE_METABOXED begin
    ty::M_Type
  end

  @Record TYPE_FUNCTION begin
    fn::M_Function
    fnType
  end

  @Record TYPE_COMPLEX begin
    cls::InstNode
    complexTy::ComplexType
  end

  @Record TYPE_UNKNOWN begin
  end

  @Record TYPE_NORETCALL begin
  end

  @Record TYPE_TUPLE begin
    types::List{M_Type}
    names::Option{List{String}}
  end

  @Record TYPE_ARRAY begin
    elementType::M_Type
    dimensions::List{Dimension}
  end

  @Record TYPE_ENUMERATION_ANY begin
  end

  @Record TYPE_ENUMERATION begin
    typePath::Absyn.Path
    literals::List{String}
  end

  @Record TYPE_CLOCK begin
  end

  @Record TYPE_BOOLEAN begin
  end

  @Record TYPE_STRING begin
  end

  @Record TYPE_REAL begin
  end

  @Record TYPE_INTEGER begin
  end
end



function subscriptedTypeName(expType::M_Type, subscriptTypes::List{<:M_Type})::String
  local str::String

  local strl::List{String}

  @assign strl = List(toString(t) for t in subscriptTypes)
  @assign strl = _cons("_", strl)
  @assign strl = _cons(toString(expType), strl)
  @assign strl = _cons("subscript", strl)
  @assign str = stringAppendList(strl)
  return str
end

function sizeType(arrayTy::M_Type)::M_Type
  local sizeTy::M_Type

  if Type.isUnknown(arrayTy)
    @assign sizeTy = TYPE_UNKNOWN()
  else
    @assign sizeTy = Type.ARRAY(
      TYPE_INTEGER(),
      list(P_Dimension.Dimension.fromInteger(dimensionCount(arrayTy))),
    )
  end
  #=  Return unknown type if the type is unknown, to avoid returning Array[0]
  =#
  #=  for untyped expressions.
  =#
  return sizeTy
end

function isBoxed(ty::M_Type)::Bool
  local isBoxed::Bool

  @assign isBoxed = begin
    @match ty begin
      METABOXED(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isBoxed
end

function unbox(ty::M_Type)::M_Type
  local unboxedType::M_Type

  @assign unboxedType = begin
    @match ty begin
      METABOXED(__) => begin
        ty.ty
      end

      _ => begin
        ty
      end
    end
  end
  return unboxedType
end

function box(ty::M_Type)::M_Type
  local boxedType::M_Type

  @assign boxedType = begin
    @match ty begin
      METABOXED(__) => begin
        ty
      end

      _ => begin
        METABOXED(ty)
      end
    end
  end
  return boxedType
end

function enumSize(ty::M_Type)::Integer
  local size::Integer

  local literals::List{String}

  @match ENUMERATION(literals = literals) = ty
  @assign size = listLength(literals)
  return size
end

function enumName(ty::M_Type)::Absyn.Path
  local name::Absyn.Path

  @match ENUMERATION(typePath = name) = ty
  return name
end

function setRecordFields(fields::List{<:Field}, recordType::M_Type)::M_Type

  @assign recordType = begin
    local rec_node::InstNode
    @match recordType begin
      COMPLEX(complexTy = ComplexType.RECORD(constructor = rec_node)) => begin
        COMPLEX(recordType.cls, ComplexType.RECORD(rec_node, fields))
      end

      _ => begin
        recordType
      end
    end
  end
  return recordType
end

function recordFields(recordType::M_Type)::List{Field}
  local fields::List{Field}

  @assign fields = begin
    @match recordType begin
      COMPLEX(complexTy = ComplexType.RECORD(fields = fields)) => begin
        fields
      end

      _ => begin
        nil
      end
    end
  end
  return fields
end

function lookupRecordFieldType(name::String, recordType::M_Type)::M_Type
  local fieldType::M_Type

  @assign fieldType = begin
    @match recordType begin
      COMPLEX(__) => begin
        getType(lookupElement(name, getClass(recordType.cls)))
      end

      ARRAY(__) => begin
        liftArrayLeftList(
          lookupRecordFieldType(name, recordType.elementType),
          recordType.dimensions,
        )
      end
    end
  end
  return fieldType
end

function isDiscrete(ty::M_Type)::Bool
  local isDiscrete::Bool

  @assign isDiscrete = begin
    @match ty begin
      TYPE_INTEGER(__) => begin
        true
      end

      TYPE_STRING(__) => begin
        true
      end

      TYPE_BOOLEAN(__) => begin
        true
      end

      TYPE_ENUMERATION(__) => begin
        true
      end

      TYPE_ARRAY(__) => begin
        isDiscrete(ty.elementType)
      end

      TYPE_FUNCTION(__) => begin
        isDiscrete(P_Function.returnType(ty.fn))
      end
      _ => begin
        false
      end
    end
  end
  return isDiscrete
end

function isEqual(ty1::M_Type, ty2::M_Type)::Bool
  local equal::Bool

  if referenceEq(ty1, ty2)
    @assign equal = true
    return equal
  end
  if valueConstructor(ty1) != valueConstructor(ty2)
    @assign equal = false
    return equal
  end
  @assign equal = begin
    local names1::List{String}
    local names2::List{String}
    @match (ty1, ty2) begin
      (ENUMERATION(__), ENUMERATION(__)) => begin
        ListUtil.isEqualOnTrue(ty1.literals, ty2.literals, stringEq)
      end

      (ARRAY(__), ARRAY(__)) => begin
        isEqual(ty1.elementType, ty2.elementType) && ListUtil.isEqualOnTrue(
          ty1.dimensions,
          ty2.dimensions,
          P_Dimension.Dimension.isEqualKnown,
        )
      end

      (TUPLE(names = SOME(names1)), TUPLE(names = SOME(names2))) => begin
        ListUtil.isEqualOnTrue(names1, names2, stringEq) &&
        ListUtil.isEqualOnTrue(ty1.types, ty2.types, isEqual)
      end

      (TUPLE(names = NONE()), TUPLE(names = NONE())) => begin
        ListUtil.isEqualOnTrue(ty1.types, ty2.types, isEqual)
      end

      (TUPLE(__), TUPLE(__)) => begin
        false
      end

      (COMPLEX(__), COMPLEX(__)) => begin
        isSame(ty1.cls, ty2.cls)
      end

      _ => begin
        true
      end
    end
  end
  return equal
end

""" #= Reduces a type's dimensions based on the given list of subscripts. =#"""
function subscript(ty::M_Type, subs::List{<:Subscript})::M_Type

  local dim::Dimension
  local dims::List{Dimension}
  local subbed_dims::List{Dimension} = nil

  if listEmpty(subs) || isUnknown(ty)
    return ty
  end
  @assign dims = arrayDims(ty)
  for sub in subs
    @match _cons(dim, dims) = dims
    @assign subbed_dims = begin
      @match sub begin
        SUBSCRIPT_INDEX(__) => begin
          subbed_dims
        end

        SUBSCRIPT_SLICE(__) => begin
          _cons(SUBSCRIPT_toDimension(sub), subbed_dims)
        end

        SUBSCRIPT_WHOLE(__) => begin
          _cons(dim, subbed_dims)
        end
      end
    end
  end
  @assign ty = arrayElementType(ty)
  if !(listEmpty(subbed_dims) && listEmpty(dims))
    @assign ty = ARRAY(ty, listAppend(listReverse(subbed_dims), dims))
  end
  return ty
end

function toDAE(ty::M_Type, makeTypeVars::Bool = true)::DAE.Type
  local daeTy::DAE.Type

  @assign daeTy = begin
    @match ty begin
      TYPE_INTEGER(__) => begin
        DAE.T_INTEGER_DEFAULT
      end

      TYPE_REAL(__) => begin
        DAE.T_REAL_DEFAULT
      end

      TYPE_STRING(__) => begin
        DAE.T_STRING_DEFAULT
      end

      TYPE_BOOLEAN(__) => begin
        DAE.T_BOOL_DEFAULT
      end

      TYPE_ENUMERATION(__) => begin
        DAE.T_ENUMERATION(NONE(), ty.typePath, ty.literals, nil, nil)
      end

      TYPE_CLOCK(__) => begin
        DAE.T_CLOCK_DEFAULT
      end

      Type.ARRAY(__) => begin
        DAE.T_ARRAY(
          toDAE(ty.elementType, makeTypeVars),
          List(P_Dimension.Dimension.toDAE(d) for d in ty.dimensions),
        )
      end

      TYPE_TUPLE(__) => begin
        DAE.T_TUPLE(List(toDAE(t) for t in ty.types), ty.names)
      end

      TYPE_FUNCTION(__) => begin
        begin
          @match ty.fnType begin
            FunctionTYPE_FUNCTIONAL_PARAMETER => begin
              P_Function.makeDAEType(ty.fn)
            end

            FunctionTYPE_FUNCTION_REFERENCE => begin
              DAE.T_FUNCTION_REFERENCE_FUNC(
                P_Function.isBuiltin(ty.fn),
                P_Function.makeDAEType(ty.fn),
              )
            end

            FunctionTYPE_FUNCTIONAL_VARIABLE => begin
              DAE.T_FUNCTION_REFERENCE_VAR(P_Function.makeDAEType(ty.fn, true))
            end
          end
        end
      end

      TYPE_NORETCALL(__) => begin
        DAE.T_NORETCALL_DEFAULT
      end

      TYPE_UNKNOWN(__) => begin
        DAE.T_UNKNOWN_DEFAULT
      end

      TYPE_COMPLEX(__) => begin
        if makeTypeVars
          toFullDAEType(ty.cls)
        else
          toPartialDAEType(ty.cls)
        end
      end

      TYPE_METABOXED(__) => begin
        DAE.T_METABOXED(toDAE(ty.ty))
      end

      TYPE_POLYMORPHIC(__) => begin
        DAE.T_METAPOLYMORPHIC(ty.name)
      end

      TYPE_ANY(__) => begin
        DAE.T_ANYTYPE(NONE())
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got unknown type: " + anyString(ty),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return daeTy
end

function typenameString(ty::M_Type)::String
  local str::String

  @assign str = begin
    @match ty begin
      TYPE_ENUMERATION(__) => begin
        AbsynUtil.pathString(ty.typePath)
      end

      _ => begin
        toString(ty)
      end
    end
  end
  return str
end


function toFlatString(ty::M_Type)::String
  local str::String

  @assign str = begin
    @match ty begin
      TYPE_INTEGER(__) => begin
        "Integer"
      end

      TYPE_REAL(__) => begin
        "Real"
      end

      TYPE_STRING(__) => begin
        "String"
      end

      TYPE_BOOLEAN(__) => begin
        "Boolean"
      end

      TYPE_CLOCK(__) => begin
        "Clock"
      end

      TYPE_ENUMERATION(__) => begin
        "'" + AbsynUtil.pathString(ty.typePath)
        +"'"
      end

      TYPE_ENUMERATION_ANY(__) => begin
        "enumeration(:)"
      end

      Type.ARRAY(__) => begin
        toString(ty.elementType) +
        "[" +
        stringDelimitList(
          ListUtil.map(ty.dimensions, P_Dimension.Dimension.toString),
          ", ",
        ) +
        "]"
      end

      TYPE_TUPLE(__) => begin
        "(" + stringDelimitList(ListUtil.map(ty.types, toString), ", ")
        +")"
      end

      TYPE_NORETCALL(__) => begin
        "()"
      end

      TYPE_UNKNOWN(__) => begin
        "unknown()"
      end

      TYPE_COMPLEX(__) => begin
        "'" + AbsynUtil.pathString(scopePath(ty.cls))
        +"'"
      end

      TYPE_FUNCTION(__) => begin
        P_Function.typeString(ty.fn)
      end

      TYPE_METABOXED(__) => begin
        "#" + toFlatString(ty.ty)
      end

      TYPE_POLYMORPHIC(__) => begin
        "<" + ty.name
        +">"
      end

      TYPE_ANY(__) => begin
        "ANY"
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got unknown type: " + anyString(ty),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return str
end

function toString(ty::M_Type)::String
  local str::String

  @assign str = begin
    @match ty begin
      TYPE_INTEGER(__) => begin
        "Integer"
      end

      TYPE_REAL(__) => begin
        "Real"
      end

      TYPE_STRING(__) => begin
        "String"
      end

      TYPE_BOOLEAN(__) => begin
        "Boolean"
      end

      TYPE_CLOCK(__) => begin
        "Clock"
      end

      TYPE_ENUMERATION(__) => begin
        "enumeration " + AbsynUtil.pathString(ty.typePath)
        +"(" + stringDelimitList(ty.literals, ", ") + ")"
      end

      TYPE_ENUMERATION_ANY(__) => begin
        "enumeration(:)"
      end

      TYPE_ARRAY(__) => begin
        toString(ty.elementType) +
        "[" +
        stringDelimitList(
          ListUtil.map(ty.dimensions, P_Dimension.Dimension.toString),
          ", ",
        ) +
        "]"
      end

      TYPE_TUPLE(__) => begin
        "(" + stringDelimitList(ListUtil.map(ty.types, toString), ", ")
        +")"
      end

      TYPE_NORETCALL(__) => begin
        "()"
      end

      TYPE_UNKNOWN(__) => begin
        "unknown()"
      end

      TYPE_COMPLEX(__) => begin
        AbsynUtil.pathString(scopePath(ty.cls))
      end

      TYPE_FUNCTION(__) => begin
        P_Function.typeString(ty.fn)
      end

      Type.METABOXED(__) => begin
        "#" + toString(ty.ty)
      end

      TYPE_POLYMORPHIC(__) => begin
        "<" + ty.name
        +">"
      end

      TYPE_ANY(__) => begin
        "ANY"
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got unknown type: " + anyString(ty),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return str
end

function nthEnumLiteral(ty::M_Type, index::Integer)::String
  local literal::String

  local literals::List{String}

  @match ENUMERATION(literals = literals) = ty
  @assign literal = listGet(literals, index)
  return literal
end

function foldDims(ty::M_Type, func::FuncT, arg::ArgT) where {ArgT}

  @assign arg = begin
    @match ty begin
      ARRAY(__) => begin
        ListUtil.fold(ty.dimensions, func, arg)
      end

      TUPLE(__) => begin
        ListUtil.fold(ty.types, (func) -> foldDims(func = func), arg)
      end

      FUNCTION(__) => begin
        foldDims(P_Function.returnType(ty.fn), func, arg)
      end

      METABOXED(__) => begin
        foldDims(ty.ty, func, arg)
      end

      _ => begin
        arg
      end
    end
  end
  return arg
end

function mapDims(ty::M_Type, func::FuncT)::M_Type

  @assign () = begin
    local fn::M_Function
    @match ty begin
      ARRAY(__) => begin
        @assign ty.dimensions = List(func(d) for d in ty.dimensions)
        ()
      end

      TUPLE(__) => begin
        @assign ty.types = List(mapDims(t, func) for t in ty.types)
        ()
      end

      FUNCTION(fn = fn) => begin
        @assign ty.fn =
          P_Function.setReturnType(mapDims(P_Function.returnType(fn), func), fn)
        ()
      end

      METABOXED(__) => begin
        @assign ty.ty = mapDims(ty.ty, func)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return ty
end

function hasZeroDimension(ty::M_Type)::Bool
  local hasZero::Bool

  @assign hasZero = begin
    @match ty begin
      ARRAY(__) => begin
        ListUtil.exist(ty.dimensions, P_Dimension.Dimension.isZero)
      end

      _ => begin
        false
      end
    end
  end
  return hasZero
end

function hasKnownSize(ty::M_Type)::Bool
  local isKnown::Bool

  @assign isKnown = begin
    @match ty begin
      ARRAY(__) => begin
        ListUtil.all(
          ty.dimensions,
          (x) -> P_Dimension.Dimension.isKnown(allowExp = false),
        )
      end

      FUNCTION(__) => begin
        hasKnownSize(P_Function.returnType(ty.fn))
      end

      _ => begin
        true
      end
    end
  end
  return isKnown
end

function dimensionDiff(ty1::M_Type, ty2::M_Type)::Integer
  local diff::Integer = dimensionCount(ty1) - dimensionCount(ty2)
  return diff
end

function dimensionCount(ty::M_Type)::Integer
  local dimCount::Integer

  @assign dimCount = begin
    @match ty begin
      ARRAY(__) => begin
        listLength(ty.dimensions)
      end

      FUNCTION(__) => begin
        dimensionCount(P_Function.returnType(ty.fn))
      end

      METABOXED(__) => begin
        dimensionCount(ty.ty)
      end

      _ => begin
        0
      end
    end
  end
  return dimCount
end

function nthDimension(ty::M_Type, index::Integer)::Dimension
  local dim::Dimension

  @assign dim = begin
    @match ty begin
      ARRAY(__) => begin
        listGet(ty.dimensions, index)
      end

      FUNCTION(__) => begin
        nthDimension(P_Function.returnType(ty.fn), index)
      end

      METABOXED(__) => begin
        nthDimension(ty.ty, index)
      end
    end
  end
  return dim
end

""" #= Copies array dimensions from one type to another, discarding the existing
     dimensions of the destination type but keeping its element type. =#"""
function copyDims(srcType::M_Type, dstType::M_Type)::M_Type
  local ty::M_Type

  if listEmpty(arrayDims(srcType))
    @assign ty = arrayElementType(dstType)
  else
    @assign ty = begin
      @match dstType begin
        ARRAY(__) => begin
          ARRAY(dstType.elementType, arrayDims(srcType))
        end

        _ => begin
          ARRAY(dstType, arrayDims(srcType))
        end
      end
    end
  end
  return ty
end

function arrayDims(ty::M_Type)::List{Dimension}
  local dims::List{Dimension}

  @assign dims = begin
    @match ty begin
      ARRAY(__) => begin
        ty.dimensions
      end

      FUNCTION(__) => begin
        arrayDims(P_Function.returnType(ty.fn))
      end

      METABOXED(__) => begin
        arrayDims(ty.ty)
      end

      _ => begin
        nil
      end
    end
  end
  return dims
end

function elementType(ty::M_Type)::M_Type
  local elementTy::M_Type

  @assign elementTy = begin
    @match ty begin
      ARRAY(__) => begin
        ty.elementType
      end

      FUNCTION(__) => begin
        elementType(P_Function.returnType(ty.fn))
      end

      _ => begin
        ty
      end
    end
  end
  return elementTy
end

""" #= Sets the common type of the elements in an array, if the type is an array
     type. Otherwise it just returns the given element type. =#"""
function setArrayElementType(arrayTy::M_Type, elementTy::M_Type)::M_Type
  local ty::M_Type

  @assign ty = begin
    @match arrayTy begin
      ARRAY(__) => begin
        liftArrayLeftList(elementTy, arrayTy.dimensions)
      end

      _ => begin
        elementTy
      end
    end
  end
  return ty
end

""" #= Returns the common type of the elements in an array, or just the type
     itself if it's not an array type. =#"""
function arrayElementType(ty::M_Type)::M_Type
  local elementTy::M_Type

  @assign elementTy = begin
    @match ty begin
      ARRAY(__) => begin
        ty.elementType
      end

      _ => begin
        ty
      end
    end
  end
  return elementTy
end

function nthTupleType(ty::M_Type, n::Integer)::M_Type
  local outTy::M_Type

  @assign outTy = begin
    @match ty begin
      TUPLE(__) => begin
        listGet(ty.types, n)
      end

      ARRAY(__) => begin
        Type.ARRAY(nthTupleType(ty.elementType, n), ty.dimensions)
      end

      _ => begin
        ty
      end
    end
  end
  return outTy
end

function firstTupleType(ty::M_Type)::M_Type
  local outTy::M_Type

  @assign outTy = begin
    @match ty begin
      TUPLE(__) => begin
        listHead(ty.types)
      end

      ARRAY(__) => begin
        Type.ARRAY(firstTupleType(ty.elementType), ty.dimensions)
      end

      _ => begin
        ty
      end
    end
  end
  return outTy
end

function isPolymorphic(ty::M_Type)::Bool
  local isPolymorphic::Bool

  @assign isPolymorphic = begin
    @match ty begin
      POLYMORPHIC(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isPolymorphic
end

function isKnown(ty::M_Type)::Bool
  local isKnown::Bool

  @assign isKnown = begin
    @match ty begin
      UNKNOWN(__) => begin
        false
      end

      _ => begin
        true
      end
    end
  end
  return isKnown
end

function isUnknown(ty::M_Type)::Bool
  local isUnknown::Bool

  @assign isUnknown = begin
    @match ty begin
      UNKNOWN(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isUnknown
end

function isTuple(ty::M_Type)::Bool
  local isTuple::Bool

  @assign isTuple = begin
    @match ty begin
      TUPLE(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isTuple
end

""" #= Returns true for all the builtin scalar types such as Integer, Real, etc. =#"""
function isScalarBuiltin(ty::M_Type)::Bool
  local isScalarBuiltin::Bool

  @assign isScalarBuiltin = begin
    @match ty begin
      INTEGER(__) => begin
        true
      end

      REAL(__) => begin
        true
      end

      STRING(__) => begin
        true
      end

      BOOLEAN(__) => begin
        true
      end

      CLOCK(__) => begin
        true
      end

      ENUMERATION(__) => begin
        true
      end

      ENUMERATION_ANY(__) => begin
        true
      end

      FUNCTION(__) => begin
        isScalarBuiltin(P_Function.returnType(ty.fn))
      end

      _ => begin
        false
      end
    end
  end
  return isScalarBuiltin
end

function isNumeric(ty::M_Type)::Bool
  local isNumeric::Bool

  @assign isNumeric = begin
    @match ty begin
      ARRAY(__) => begin
        isBasicNumeric(ty.elementType)
      end

      _ => begin
        isBasicNumeric(ty)
      end
    end
  end
  return isNumeric
end

function isBasicNumeric(ty::M_Type)::Bool
  local isNumeric::Bool

  @assign isNumeric = begin
    @match ty begin
      REAL(__) => begin
        true
      end

      INTEGER(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isNumeric
end

function isBasic(ty::M_Type)::Bool
  local isNumeric::Bool

  @assign isNumeric = begin
    @match ty begin
      REAL(__) => begin
        true
      end

      INTEGER(__) => begin
        true
      end

      BOOLEAN(__) => begin
        true
      end

      STRING(__) => begin
        true
      end

      ENUMERATION(__) => begin
        true
      end

      CLOCK(__) => begin
        true
      end

      FUNCTION(__) => begin
        isBasic(P_Function.returnType(ty.fn))
      end

      _ => begin
        false
      end
    end
  end
  return isNumeric
end

function isScalarArray(ty::M_Type)::Bool
  local isScalar::Bool

  @assign isScalar = begin
    @match ty begin
      ARRAY(dimensions = _ <| nil()) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isScalar
end

function isRecord(ty::M_Type)::Bool
  local isRecord::Bool

  @assign isRecord = begin
    @match ty begin
      COMPLEX(complexTy = ComplexType.RECORD(__)) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isRecord
end

function isExternalObject(ty::M_Type)::Bool
  local isEO::Bool

  @assign isEO = begin
    @match ty begin
      COMPLEX(complexTy = ComplexType.EXTERNAL_OBJECT(__)) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isEO
end

function isExpandableConnector(ty::M_Type)::Bool
  local isExpandable::Bool

  @assign isExpandable = begin
    @match ty begin
      COMPLEX(complexTy = ComplexType.EXPANDABLE_CONNECTOR(__)) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isExpandable
end

function isConnector(ty::M_Type)::Bool
  local isConnector::Bool

  @assign isConnector = begin
    @match ty begin
      COMPLEX(complexTy = ComplexType.CONNECTOR(__)) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isConnector
end

function isComplex(ty::M_Type)::Bool
  local isComplex::Bool

  @assign isComplex = begin
    @match ty begin
      COMPLEX(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isComplex
end

function isEnumeration(ty::M_Type)::Bool
  local isEnum::Bool

  @assign isEnum = begin
    @match ty begin
      ENUMERATION(__) => begin
        true
      end

      ENUMERATION_ANY(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isEnum
end

function isSingleElementArray(ty::M_Type)::Bool
  local isSingleElement::Bool

  @assign isSingleElement = begin
    local d::Dimension
    @match ty begin
      ARRAY(dimensions = d <| nil()) => begin
        P_Dimension.Dimension.isKnown(d) && P_Dimension.Dimension.size(d) == 1
      end

      _ => begin
        false
      end
    end
  end
  return isSingleElement
end

function isEmptyArray(ty::M_Type)::Bool
  local isEmpty::Bool

  @assign isEmpty = begin
    @match ty begin
      ARRAY(__) => begin
        ListUtil.exist(ty.dimensions, P_Dimension.Dimension.isZero)
      end

      _ => begin
        false
      end
    end
  end
  return isEmpty
end

function isSquareMatrix(ty::M_Type)::Bool
  local isSquareMatrix::Bool

  @assign isSquareMatrix = begin
    local d1::Dimension
    local d2::Dimension
    @match ty begin
      ARRAY(dimensions = d1 <| d2 <| nil()) => begin
        P_Dimension.Dimension.isEqualKnown(d1, d2)
      end

      _ => begin
        false
      end
    end
  end
  return isSquareMatrix
end

function isMatrix(ty::M_Type)::Bool
  local isMatrix::Bool

  @assign isMatrix = begin
    @match ty begin
      ARRAY(dimensions = _ <| _ <| nil()) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isMatrix
end

""" #= Return whether the type is a vector type or not, i.e. a 1-dimensional array. =#"""
function isVector(ty::M_Type)::Bool
  local isVector::Bool

  @assign isVector = begin
    @match ty begin
      ARRAY(dimensions = _ <| nil()) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isVector
end

function isArray(ty::M_Type)::Bool
  local isArray::Bool

  @assign isArray = begin
    @match ty begin
      ARRAY(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isArray
end

function isScalar(ty::M_Type)::Bool
  local isScalar::Bool

  @assign isScalar = begin
    @match ty begin
      ARRAY(__) => begin
        false
      end

      _ => begin
        true
      end
    end
  end
  return isScalar
end

function isClock(ty::M_Type)::Bool
  local isClock::Bool

  @assign isClock = begin
    @match ty begin
      CLOCK(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isClock
end

function isString(ty::M_Type)::Bool
  local isString::Bool

  @assign isString = begin
    @match ty begin
      STRING(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isString
end

function isBoolean(ty::M_Type)::Bool
  local isBool::Bool

  @assign isBool = begin
    @match ty begin
      BOOLEAN(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isBool
end

function isReal(ty::M_Type)::Bool
  local isReal::Bool

  @assign isReal = begin
    @match ty begin
      REAL(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isReal
end

function isInteger(ty::M_Type)::Bool
  local isInteger::Bool

  @assign isInteger = begin
    @match ty begin
      INTEGER(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isInteger
end

function unliftArrayN(N::Integer, ty::M_Type)::M_Type

  local el_ty::M_Type
  local dims::List{Dimension}

  @match ARRAY(el_ty, dims) = ty
  for i = 1:N
    @assign dims = listRest(dims)
  end
  if listEmpty(dims)
    @assign ty = el_ty
  else
    @assign ty = ARRAY(el_ty, dims)
  end
  return ty
end

function unliftArray(ty::M_Type)::M_Type

  local el_ty::M_Type
  local dims::List{Dimension}

  @match ARRAY(el_ty, _cons(_, dims)) = ty
  if listEmpty(dims)
    @assign ty = el_ty
  else
    @assign ty = ARRAY(el_ty, dims)
  end
  return ty
end

""" #= Adds array dimensions to a type on the left side, e.g.
       listArrayLeft(Real[2, 3], [4, 5]) => Real[2, 3, 4, 5]. =#"""
function liftArrayRightList(ty::M_Type, dims::List{<:Dimension})::M_Type

  if listEmpty(dims)
    return ty
  end
  @assign ty = begin
    @match ty begin
      ARRAY(__) => begin
        ARRAY(ty.elementType, listAppend(ty.dimensions, dims))
      end

      _ => begin
        ARRAY(ty, dims)
      end
    end
  end
  return ty
end

""" #= Adds array dimensions to a type on the left side, e.g.
       listArrayLeft(Real[2, 3], [4, 5]) => Real[4, 5, 2, 3]. =#"""
function liftArrayLeftList(ty::M_Type, dims::List{<:Dimension})::M_Type

  if listEmpty(dims)
    return ty
  end
  @assign ty = begin
    @match ty begin
      ARRAY(__) => begin
        ARRAY(ty.elementType, listAppend(dims, ty.dimensions))
      end

      _ => begin
        ARRAY(ty, dims)
      end
    end
  end
  return ty
end

""" #= Adds an array dimension to a type on the left side, e.g.
       listArrayLeft(Real[2, 3], [4]) => Real[4, 2, 3]. =#"""
function liftArrayLeft(ty::M_Type, dim::Dimension)::M_Type

  @assign ty = begin
    @match ty begin
      ARRAY(__) => begin
        ARRAY(ty.elementType, _cons(dim, ty.dimensions))
      end
      _ => begin
        ARRAY(ty, list(dim))
      end
    end
  end
  return ty
end
