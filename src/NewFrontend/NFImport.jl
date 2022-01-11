@UniontypeDecl NFImport
Import = NFImport

@Uniontype NFImport begin
  @Record CONFLICTING_IMPORT begin
    imp1::Import
    imp2::Import
  end

  @Record RESOLVED_IMPORT begin
    node::InstNode
    info::SourceInfo
  end

  @Record UNRESOLVED_IMPORT begin
    imp::Absyn.Import
    scope::InstNode
    info::SourceInfo
  end
end

function printImportError(imp1::Import, imp2::Import)
  local err_msg::ErrorTypes.Message
  Error.addSourceMessage(Error.ERROR_FROM_HERE, nil, Import_info(imp1))
  @assign err_msg = begin
    @match imp2 begin
      UNRESOLVED_IMPORT(__) => begin
        Error.MULTIPLE_QUALIFIED_IMPORTS_WITH_SAME_NAME
      end

      RESOLVED_IMPORT(__) => begin
        Error.IMPORT_SEVERAL_NAMES
      end
    end
  end
  return Error.addSourceMessage(err_msg, list(name(imp2)), info(imp2))
end

function instUnqualified(
  imp::Absyn.Import,
  scope::InstNode,
  info::SourceInfo,
  imps::List{<:Import} = nil,
)::List{Import}

  local path::Absyn.Path
  local node::InstNode
  local tree::ClassTree
  local elements::List{InstNode}

  @match Absyn.Import.UNQUAL_IMPORT(path = path) = imp
  @assign node = lookupImport(path, scope, info)
  @assign node = Inst.instPackage(node)
  @assign tree = classTree(getClass(node))
  @assign () = begin
    @match tree begin
      CLASS_TREE_FLAT_TREE(__) => begin
        for cls in tree.classes
          @assign imps = _cons(RESOLVED_IMPORT(cls, info), imps)
        end
        for comp in tree.components
          @assign imps = _cons(RESOLVED_IMPORT(comp, info), imps)
        end
        ()
      end

      _ => begin
        Error.assertion(false, getInstanceName() + " got invalid class tree", sourceInfo())
        ()
      end
    end
  end
  return imps
end

function instQualified(
  imp::Absyn.Import,
  scope::InstNode,
  info::SourceInfo,
)::Tuple{Import, InstNode}
  local node::InstNode
  local outImport::Import

  @assign node = begin
    @match imp begin
      Absyn.NAMED_IMPORT(__) => begin
        lookupImport(imp.path, scope, info)
      end

      Absyn.QUAL_IMPORT(__) => begin
        lookupImport(imp.path, scope, info)
      end
    end
  end
  @assign outImport = RESOLVED_IMPORT(node, info)
  return (outImport, node)
end

function resolve(imp::Import)::Tuple{InstNode, Bool, Import}
  local outImport::Import
  local changed::Bool
  local node::InstNode

  @assign (outImport, node, changed) = begin
    @match imp begin
      UNRESOLVED_IMPORT(__) => begin
        @assign (outImport, node) = instQualified(imp.imp, imp.scope, imp.info)
        (outImport, node, true)
      end

      RESOLVED_IMPORT(__) => begin
        (imp, imp.node, false)
      end

      CONFLICTING_IMPORT(__) => begin
        printImportError(imp.imp1, imp.imp2)
        fail()
      end
    end
  end
  return (node, changed, outImport)
end

function Import_info(imp::Import)::SourceInfo
  local info::SourceInfo

  @assign info = begin
    @match imp begin
      UNRESOLVED_IMPORT(__) => begin
        imp.info
      end

      RESOLVED_IMPORT(__) => begin
        imp.info
      end
    end
  end
  return info
end

function name(imp::Import)::String
  local name::String

  @assign name = begin
    @match imp begin
      UNRESOLVED_IMPORT(__) => begin
        AbsynUtil.importName(imp.imp)
      end

      RESOLVED_IMPORT(__) => begin
        name(imp.node)
      end
    end
  end
  return name
end
