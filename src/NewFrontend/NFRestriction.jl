
@UniontypeDecl NFRestriction

@Uniontype NFRestriction begin
  @Record RESTRICTION_UNKNOWN begin
  end

  @Record RESTRICTION_CLOCK begin
  end

  @Record RESTRICTION_TYPE begin
  end

  @Record RESTRICTION_RECORD_CONSTRUCTOR begin
  end

  @Record RESTRICTION_RECORD begin
    isOperator::Bool
  end

  @Record RESTRICTION_OPERATOR begin
  end

  @Record RESTRICTION_MODEL begin
  end

  @Record RESTRICTION_FUNCTION begin
  end

  @Record RESTRICTION_EXTERNAL_OBJECT begin
  end

  @Record RESTRICTION_ENUMERATION begin
  end

  @Record RESTRICTION_CONNECTOR begin
    isExpandable::Bool
  end

  @Record RESTRICTION_CLASS begin
  end
end

Restriction = NFRestriction

function toString(res::Restriction)::String
  local str::String
  @assign str = begin
    @match res begin
      Restriction_CLASS(__) => begin
        "class"
      end

      Restriction_CONNECTOR(__) => begin
        if res.isExpandable
          "expandable connector"
        else
          "connector"
        end
      end

      RESTRICTION_ENUMERATION(__) => begin
        "enumeration"
      end

      RESTRICTION_EXTERNAL_OBJECT(__) => begin
        "ExternalObject"
      end

      RESTRICTION_FUNCTION(__) => begin
        "function"
      end

      RESTRICTION_MODEL(__) => begin
        "model"
      end

      RESTRICTION_OPERATOR(__) => begin
        "operator"
      end
      RESTRICTION_RECORD(__) => begin
        "record"
      end
      RESTRICTION_RECORD_CONSTRUCTOR(__) => begin
        "record"
      end
      RESTRICTION_TYPE(__) => begin
        "type"
      end
      RESTRICTION_CLOCK(__) => begin
        "clock"
      end

      _ => begin
        "unknown"
      end
    end
  end
  return str
end

function isModel(res::Restriction)::Bool
  local isModel::Bool
  @assign isModel = begin
    @match res begin
      RESTRICITON_MODEL(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isModel
end

function isClock(res::Restriction)::Bool
  local isClock::Bool
  @assign isClock = begin
    @match res begin
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

function isType(res::Restriction)::Bool
  local isType::Bool
  @assign isType = begin
    @match res begin
      RESTRICTION_TYPE(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isType
end

function isOperator(res::Restriction)::Bool
  local isOperator::Bool
  @assign isOperator = begin
    @match res begin
      RESTRICTION_OPERATOR(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isOperator
end

function isOperatorRecord(res::Restriction)::Bool
  local isOpRecord::Bool
  @assign isOpRecord = begin
    @match res begin
      RESTRICTION_RECORD(__) => begin
        res.isOperator
      end
      _ => begin
        false
      end
    end
  end
  return isOpRecord
end

function isRecord(res::Restriction)::Bool
  local isRecord::Bool
  @assign isRecord = begin
    @match res begin
      RESTRICTION_RECORD(__) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return isRecord
end

function isFunction(res::Restriction)::Bool
  local isFunction::Bool
  @assign isFunction = begin
    @match res begin
      RESTRICTION_FUNCTION(__) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return isFunction
end

function isExternalObject(res::Restriction)::Bool
  local isExternalObject::Bool
  @assign isExternalObject = begin
    @match res begin
      RESTRICTION_EXTERNAL_OBJECT(__) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return isExternalObject
end

function isNonexpandableConnector(res::Restriction)::Bool
  local isNonexpandable::Bool

  @assign isNonexpandable = begin
    @match res begin
      RESTRICTION_CONNECTOR(__) => begin
        !res.isExpandable
      end
      _ => begin
        false
      end
    end
  end
  return isNonexpandable
end

function isExpandableConnector(res::Restriction)::Bool
  local isConnector::Bool
  @assign isConnector = begin
    @match res begin
      RESTRICTION_CONNECTOR(__) => begin
        res.isExpandable
      end

      _ => begin
        false
      end
    end
  end
  return isConnector
end

function isConnector(res::Restriction)::Bool
  local isConnector::Bool
  @assign isConnector = begin
    @match res begin
      RESTRICTION_CONNECTOR(__) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return isConnector
end

function toDAE(res::Restriction, path::Absyn.Path)::ClassInf.State
  local state::ClassInf.State

  @assign state = begin
    @match res begin
      RESTRICTION_CLASS(__) => begin
        ClassInf.State.UNKNOWN(path)
      end
      RESTRICTION_CONNECTOR(__) => begin
        ClassInf.State.CONNECTOR(path, res.isExpandable)
      end
      RESTRICTION_ENUMERATION(__) => begin
        ClassInf.State.ENUMERATION(path)
      end
      RESTRICTION_EXTERNAL_OBJECT(__) => begin
        ClassInf.State.EXTERNAL_OBJ(path)
      end
      RESTRICTION_FUNCTION(__) => begin
        ClassInf.State.FUNCTION(path, false)
      end
      RESTRICTION_MODEL(__) => begin
        ClassInf.State.MODEL(path)
      end
      RESTRICTION_OPERATOR(__) => begin
        ClassInf.State.FUNCTION(path, false)
      end
      RESTRICTION_RECORD(__) => begin
        ClassInf.State.RECORD(path)
      end
      RESTRICTION_TYPE(__) => begin
        ClassInf.State.TYPE(path)
      end
      RESTRICTION_CLOCK(__) => begin
        ClassInf.State.TYPE_CLOCK(path)
      end
      _ => begin
        ClassInf.State.UNKNOWN(path)
      end
    end
  end
  return state
end

function fromSCode(sres::SCode.Restriction)::Restriction
  local res::Restriction
  @assign res = begin
    @match sres begin
      SCode.R_CLASS(__) => begin
        RESTRICTION_CLASS()
      end
      SCode.R_CONNECTOR(__) => begin
        RESTRICTION_CONNECTOR(sres.isExpandable)
      end
      SCode.R_ENUMERATION(__) => begin
        RESTRICTION_ENUMERATION()
      end
      SCode.R_FUNCTION(__) => begin
        RESTRICTION_FUNCTION()
      end
      SCode.R_MODEL(__) => begin
        RESTRICTION_MODEL()
      end
      SCode.R_OPERATOR(__) => begin
        RESTRICTION_OPERATOR()
      end
      SCode.R_RECORD(__) => begin
        RESTRICTION_RECORD(sres.isOperator)
      end
      SCode.R_TYPE(__) => begin
        RESTRICTION_TYPE()
      end
      SCode.R_PREDEFINED_CLOCK(__) => begin
        RESTRICTION_CLOCK()
      end
      _ => begin
        RESTRICTION_MODEL()
      end
    end
  end
  return res
end
