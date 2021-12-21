
include("LookupTree.jl")
include("DuplicateTree.jl")

@Uniontype ClassTree begin
  @Record CLASS_TREE_EMPTY_TREE begin
  end
  @Record CLASS_TREE_FLAT_TREE begin
    tree::LookupTree.Tree
    classes::Array{InstNode}
    components::Array{InstNode}
    imports::Array{Import}
    duplicates::DuplicateTree.Tree
  end

  @Record CLASS_TREE_INSTANTIATED_TREE begin
    tree::LookupTree.Tree
    classes::Array{Pointer{InstNode}}
    components::Array{Pointer{InstNode}}
    localComponents::List{Int}
    exts::Array{InstNode}
    imports::Array{Import}
    duplicates::DuplicateTree.Tree
  end

  @Record CLASS_TREE_EXPANDED_TREE begin
    tree::LookupTree.Tree
    classes::Array{InstNode}
    components::Array{InstNode}
    exts::Array{InstNode}
    imports::Array{Import}
    duplicates::DuplicateTree.Tree
  end

  @Record CLASS_TREE_PARTIAL_TREE begin
    tree::LookupTree.Tree
    classes::Array{InstNode}
    components::Array{InstNode}
    exts::Array{InstNode}
    imports::Array{Import}
    duplicates::DuplicateTree.Tree
  end
end

const EMPTY_CLASS_TREE =
  CLASS_TREE_PARTIAL_TREE(
    LookupTree.EMPTY(),
    listArray(nil),
    listArray(nil),
    listArray(nil),
    listArray(nil),
    DuplicateTree.EMPTY(),
  )::ClassTree

const EMPTY_FLAT_CLASS_TREE =
  CLASS_TREE_FLAT_TREE(
    LookupTree.EMPTY(),
    listArray(nil),
    listArray(nil),
    listArray(nil),
    DuplicateTree.EMPTY(),
  )::ClassTree

FuncT = Function

function isEmptyTree(tree::ClassTree)::Bool
  local isEmpty::Bool

  @assign isEmpty = begin
    @match tree begin
      CLASS_TREE_EMPTY_TREE(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isEmpty
end

function getComponents(tree::ClassTree)::Array{InstNode}
  local comps::Array{InstNode}

  @assign comps = begin
    @match tree begin
      CLASS_TREE_PARTIAL_TREE(__) => begin
        tree.components
      end

      CLASS_TREE_EXPANDED_TREE(__) => begin
        tree.components
      end

      CLASS_TREE_FLAT_TREE(__) => begin
        tree.components
      end
    end
  end
  return comps
end

function getExtends(tree::ClassTree)::Array{InstNode}
  local exts::Array{InstNode}
  @assign exts = begin
    @match tree begin
      CLASS_TREE_PARTIAL_TREE(__) => begin
        tree.exts
      end

      CLASS_TREE_EXPANDED_TREE(__) => begin
        tree.exts
      end

      CLASS_TREE_INSTANTIATED_TREE(__) => begin
        tree.exts
      end
    end
  end
  return exts
end

function getClasses(tree::ClassTree)::Array{InstNode}
  local clss::Array{InstNode}
  @assign clss = begin
    @match tree begin
      CLASS_TREE_PARTIAL_TREE(__) => begin
        tree.classes
      end
      CLASS_TREE_EXPANDED_TREE(__) => begin
        tree.classes
      end
      CLASS_TREE_FLAT_TREE(__) => begin
        tree.classes
      end
    end
  end
  return clss
end

function enumerateComponents2(
  name::String,
  entry::LookupTree.Entry,
  comps::Array{<:InstNode},
  components::List{<:InstNode},
)::List{InstNode}

  @assign () = begin
    @match entry begin
      LookupTree.COMPONENT(__) => begin
        @assign components = _cons(comps[entry.index], components)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return components
end

function enumerateComponents(tree::ClassTree)::List{InstNode}
  local components::List{InstNode}

  local ltree::LookupTree.Tree
  local comps::Array{InstNode}

  @match CLASS_TREE_FLAT_TREE(tree = ltree, components = comps) = tree
  components = LookupTree.fold(ltree, (name, entry, components) -> enumerateComponents2(name, entry, comps, components), nil)
  return components
end

function setClassExtends(extNode::InstNode, tree::ClassTree)::ClassTree

  arrayUpdate(getExtends(tree), 1, extNode)
  return tree
end

function getRedeclaredNode(name::String, tree::ClassTree)::InstNode
  local node::InstNode

  local entry::DuplicateTree.Entry

  try
    @assign entry = DuplicateTree.get(getDuplicates(tree), name)
    @assign entry = listHead(entry.children)
    if isSome(entry.node)
      @match SOME(node) = entry.node
    else
      @assign node = resolveEntry(entry.entry, tree)
    end
  catch
    Error.assertion(false, getInstanceName() + " failed on " + name, sourceInfo())
  end
  return node
end

function isIdentical(tree1::ClassTree, tree2::ClassTree)::Bool
  local identical::Bool

  @assign identical = true
  return identical
end

function checkDuplicates2(
  name::String,
  entry::DuplicateTree.Entry,
  tree::ClassTree,
)::ClassTree

  local kept::InstNode
  local dup::InstNode

  @match SOME(kept) = entry.node
  @assign () = begin
    @match entry.ty begin
      DuplicateTree.EntryType.REDECLARE => begin
        ()
      end

      _ => begin
        for c in entry.children
          @match SOME(dup) = c.node
          checkIdentical(kept, dup)
        end
        ()
      end
    end
  end
  return tree
end

function checkDuplicates(tree::ClassTree)
  return @assign () = begin
    @match tree begin
      CLASS_TREE_INSTANTIATED_TREE(__) where {(!DuplicateTree.isEmpty(tree.duplicates))} => begin
        DuplicateTree.fold(tree.duplicates, checkDuplicates2, tree)
        ()
      end

      _ => begin
        ()
      end
    end
  end
end

function extendsCount(tree::ClassTree)::Int
  local count::Int = arrayLength(getExtends(tree))
  return count
end

function componentCount(tree::ClassTree)::Int
  local count::Int

  @assign count = begin
    @match tree begin
      CLASS_TREE_PARTIAL_TREE(__) => begin
        arrayLength(tree.components) - arrayLength(tree.exts)
      end

      CLASS_TREE_EXPANDED_TREE(__) => begin
        arrayLength(tree.components) - arrayLength(tree.exts)
      end

      CLASS_TREE_INSTANTIATED_TREE(__) => begin
        arrayLength(tree.components)
      end

      CLASS_TREE_FLAT_TREE(__) => begin
        arrayLength(tree.components)
      end
    end
  end
  return count
end

function classCount(tree::ClassTree)::Int
  local count::Int

  @assign count = begin
    @match tree begin
      CLASS_TREE_PARTIAL_TREE(__) => begin
        arrayLength(tree.classes)
      end

      CLASS_TREE_EXPANDED_TREE(__) => begin
        arrayLength(tree.classes)
      end

      CLASS_TREE_INSTANTIATED_TREE(__) => begin
        arrayLength(tree.classes)
      end

      CLASS_TREE_FLAT_TREE(__) => begin
        arrayLength(tree.classes)
      end
    end
  end
  return count
end

function foldComponents(tree::ClassTree, func::FuncT, arg::ArgT) where {ArgT}

  @assign () = begin
    @match tree begin
      CLASS_TREE_PARTIAL_TREE(__) => begin
        for c in tree.components
          @assign arg = func(c, arg)
        end
        ()
      end

      CLASS_TREE_EXPANDED_TREE(__) => begin
        for c in tree.components
          @assign arg = func(c, arg)
        end
        ()
      end

      CLASS_TREE_INSTANTIATED_TREE(__) => begin
        for c in tree.components
          @assign arg = func(P_Pointer.access(c), arg)
        end
        ()
      end

      CLASS_TREE_FLAT_TREE(__) => begin
        for c in tree.components
          @assign arg = func(c, arg)
        end
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return arg
end

function applyComponents(tree::ClassTree, func::FuncT)
  return @assign () = begin
    @match tree begin
      CLASS_TREE_PARTIAL_TREE(__) => begin
        for c in tree.components
          func(c)
        end
        ()
      end

      CLASS_TREE_EXPANDED_TREE(__) => begin
        for c in tree.components
          func(c)
        end
        ()
      end

      CLASS_TREE_INSTANTIATED_TREE(__) => begin
        for c in tree.components
          func(P_Pointer.access(c))
        end
        ()
      end

      CLASS_TREE_FLAT_TREE(__) => begin
        for c in tree.components
          func(c)
        end
        ()
      end

      _ => begin
        ()
      end
    end
  end
end

function applyLocalComponents(tree::CLASS_TREE_INSTANTIATED_TREE, func::FuncT)
  for i in tree.localComponents
    func(P_Pointer.access(arrayGetNoBoundsChecking(tree.components, i)))
  end
  ()
end

function applyLocalComponents(tree::CLASS_TREE_PARTIAL_TREE, func::FuncT)
  for c in tree.components
    func(c)
  end
  ()
end

function applyLocalComponents(tree::CLASS_TREE_EXPANDED_TREE, func::FuncT)
  for c in tree.components
    func(c)
  end
  ()
end

""" #= Applies a mutating function to each extends node in the class tree.
       A given argument is also folded and returned. =#"""
function mapFoldExtends(tree::ClassTree, func::FuncT, arg::ArgT) where {ArgT}

  local exts::Array{InstNode} = getExtends(tree)
  local ext::InstNode

  for i = 1:arrayLength(exts)
    @assign (ext, arg) = func(arrayGetNoBoundsChecking(exts, i), arg)
    arrayUpdateNoBoundsChecking(exts, i, ext)
  end
  return arg
end

function foldExtends(tree::ClassTree, func::FuncT, arg::ArgT) where {ArgT}

  local exts::Array{InstNode} = getExtends(tree)

  for ext in exts
    @assign arg = func(ext, arg)
  end
  return arg
end

"""  
  Applies a function to each extends node in the class tree, and updates
  the extends array with the returned nodes. 
"""
function mapExtends(tree::ClassTree, func::FuncT)
  local exts::Array{InstNode} = getExtends(tree)
  for i = 1:arrayLength(exts)
    local res = arrayGetNoBoundsChecking(exts, i)
    arrayUpdateNoBoundsChecking(exts, i, func(res))
  end
  return
end

function applyExtends(tree::ClassTree, func::FuncT)
  local exts::Array{InstNode} = getExtends(tree)

  return for ext in exts
    func(ext)
  end
end

function foldClasses(tree::ClassTree, func::FuncT, arg::ArgT) where {ArgT}

  local clss::Array{InstNode} = getClasses(tree)

  for cls in clss
    @assign arg = func(cls, arg)
  end
  return arg
end

function mapClasses(tree::ClassTree, func::FuncT)
  local clss::Array{InstNode} = getClasses(tree)

  return for i = 1:arrayLength(clss)
    arrayUpdateNoBoundsChecking(clss, i, func(arrayGetNoBoundsChecking(clss, i)))
  end
end

function nthComponent(index::Int, tree::ClassTree)::InstNode
  local component::InstNode

  @assign component = begin
    @match tree begin
      CLASS_TREE_PARTIAL_TREE(__) => begin
        arrayGet(tree.components, index)
      end

      CLASS_TREE_EXPANDED_TREE(__) => begin
        arrayGet(tree.components, index)
      end

      CLASS_TREE_INSTANTIATED_TREE(__) => begin
        P_Pointer.access(arrayGet(tree.components, index))
      end

      CLASS_TREE_FLAT_TREE(__) => begin
        arrayGet(tree.components, index)
      end
    end
  end
  return component
end

function lookupComponentIndex(name::String, tree::ClassTree)::Int
  local index::Int

  @match LookupTree.COMPONENT(index = index) =
    LookupTree.get(lookupTree(tree), name)
  return index
end

function lookupElementsPtr(name::String, tree::ClassTree)::List{Pointer{InstNode}}
  local elements::List{Pointer{InstNode}}

  local dup_entry::DuplicateTree.Entry

  try
    @assign dup_entry = DuplicateTree.get(getDuplicates(tree), name)
    @assign elements = resolveDuplicateEntriesPtr(dup_entry, tree)
  catch
    @assign elements = list(lookupElementPtr(name, tree))
  end
  return elements
end

function lookupElementPtr(name::String, tree::ClassTree)::Pointer{InstNode}
  local element::Pointer{InstNode}
  local entry::LookupTree.Entry

  @assign entry = LookupTree.get(lookupTree(tree), name)
  @assign element = resolveEntryPtr(entry, tree)
  return element
end

""" #= Returns the class or component with the given name in the class tree. =#"""
function lookupElement(name::String, tree::ClassTree)::Tuple{InstNode, Bool}
  local isImport::Bool
  local element::InstNode
  local entry::LookupTree.Entry
  @debug "Looking up element $name in class tree!"
  @debug "Fetching from tree. Soon to report entry"
  str = LookupTree.printTreeStr(lookupTree(tree))
  @debug str
  @assign entry = LookupTree.get(lookupTree(tree), name)
  @debug "Our entry is $entry"
  @assign (element, isImport) = resolveEntry(entry, tree)
  return (element, isImport)
end

function flattenLookupTree2(
  key::LookupTree.Key,
  entry::LookupTree.Entry,
  offsets::Array{<:Int},
)::LookupTree.Entry
  local outEntry::LookupTree.Entry

  @assign outEntry = begin
    @match entry begin
      LookupTree.COMPONENT(__) => begin
        LookupTree.COMPONENT(
          entry.index - arrayGetNoBoundsChecking(offsets, entry.index),
        )
      end

      _ => begin
        entry
      end
    end
  end
  return outEntry
end

""" #= Traverses a lookup tree and shifts the index of each component entry by
       using the given offset array, such that the lookup tree can be used to
       look up components when any duplicates have been removed from the
       component array. =#"""
function flattenLookupTree(
  tree::LookupTree.Tree,
  offsets::Array{<:Int},
)::LookupTree.Tree
  @assign tree = LookupTree.map(tree, (key, entry) -> flattenLookupTree2(key, entry, offsets))
  return tree
end

""" #= Creates an array of offsets given an element count and a sorted list of
       duplicate indices. The offsets indicate how many positions each element
       is shifted when removing the duplicate elements. The duplicates are
       marked with -1 in the array. For example:
         createFlatOffsets(7, {2, 4, 5}) => {0, -1, 1, -1, -1, 3, 3}
       =#"""
function createFlatOffsets(
  elementCount::Int,
  duplicates::List{<:Int},
)::Array{Int}
  local offsets::Array{Int}

  local offset::Int = 0
  local dup::Int
  local rest_dups::List{Int}

  @assign offsets = arrayCreateNoInit(elementCount, 0)
  @match _cons(dup, rest_dups) = duplicates
  for i = 1:elementCount
    if i == dup
      if listEmpty(rest_dups)
        @assign dup = 0
      else
        @match _cons(dup, rest_dups) = rest_dups
      end
      @assign offset = offset + 1
      arrayUpdateNoBoundsChecking(offsets, i, -1)
    else
      arrayUpdateNoBoundsChecking(offsets, i, offset)
    end
  end
  return offsets
end

function flattenElementsWithOffset(
  elements::Array{<:Pointer{<:InstNode}},
  flatElements::Array{<:InstNode},
  offsets::Array{<:Int},
)
  local offset::Int

  return for i = 1:arrayLength(elements)
    @assign offset = arrayGetNoBoundsChecking(offsets, i)
    if offset >= 0
      arrayUpdateNoBoundsChecking(
        flatElements,
        i - offset,
        P_Pointer.access(arrayGetNoBoundsChecking(elements, i)),
      )
    end
  end
end

""" #= Copies elements from one array to another while removing the Mutable
       container for each element. =#"""
function flattenElements(
  elements::Array,
  flatElements::Array{<:InstNode},
)
  return for i = 1:arrayLength(elements)
    arrayUpdateNoBoundsChecking(
      flatElements,
      i,
      P_Pointer.access(arrayGetNoBoundsChecking(elements, i)),
    )
  end
end

""" #= Flattens a class tree by creating new arrays for the classes and
       components with any duplicates removed and with the elements no longer
       being mutable references. =#"""
function flatten(tree::ClassTree)::ClassTree

  @assign tree = begin
    local clss::Array{InstNode}
    local comps::Array{InstNode}
    local comp_offsets::Array{Int}
    local clsc::Int
    local compc::Int
    local dup_comp::List{Int}
    local ltree::LookupTree.Tree
    @match tree begin
      CLASS_TREE_INSTANTIATED_TREE(__) => begin
        #=  Create a list of indices for any duplicates.
        =#
        @assign (_, dup_comp) = enumerateDuplicates(tree.duplicates)
        #=  Allocate new arrays for classes and components.
        =#
        @assign clsc = arrayLength(tree.classes)
        @assign compc = arrayLength(tree.components) - listLength(dup_comp)
        @assign clss = arrayCreateNoInit(clsc, EMPTY_NODE())
        @assign comps = arrayCreateNoInit(compc, EMPTY_NODE())
        #=  Class duplicates can be ignored since classes are only accessed
        =#
        #=  through name lookup and not index, so there's no need to spend
        =#
        #=  time on filtering them out.
        =#
        flattenElements(tree.classes, clss)
        #=  Component duplicates should be removed though, since we don't
        =#
        #=  want any duplicates in the flat model.
        =#
        if listEmpty(dup_comp)
          flattenElements(tree.components, comps)
          @assign ltree = tree.tree
        else
          @assign comp_offsets = createFlatOffsets(arrayLength(tree.components), dup_comp)
          flattenElementsWithOffset(tree.components, comps, comp_offsets)
          @assign ltree = flattenLookupTree(tree.tree, comp_offsets)
        end
        #=  No duplicates, just copy to new array.
        =#
        #=  Duplicates, create an array of offsets and use it to fill the
        =#
        #=  new array and update the lookup tree.
        =#
        CLASS_TREE_FLAT_TREE(ltree, clss, comps, tree.imports, tree.duplicates)
      end

      _ => begin
        tree
      end
    end
  end
  return tree
end

""" #= Appens a list of local components to an instantiated class tree. =#"""
function appendComponentsToInstTree(
  components::List{<:Pointer{<:InstNode}},
  tree::ClassTree,
)::ClassTree

  if listEmpty(components)
    return tree
  else
    @assign () = begin
      local comp_idx::Int
      local local_comps::List{Int}
      @match tree begin
        CLASS_TREE_INSTANTIATED_TREE(__) => begin
          @assign comp_idx = arrayLength(tree.components)
          @assign tree.components = ArrayUtil.appendList(tree.components, components)
          @assign local_comps = tree.localComponents
          for i = (comp_idx + 1):(comp_idx + listLength(components))
            @assign local_comps = _cons(i, local_comps)
          end
          @assign tree.localComponents = local_comps
          ()
        end
      end
    end
  end
  return tree
end

"""  
  This function replaces all duplicate elements with the element that is
  kept, such that lookup in the extends nodes will find the correct node. 
"""
function replaceDuplicates(tree::ClassTree)::ClassTree
  () = begin
    @match tree begin
      CLASS_TREE_INSTANTIATED_TREE(__) where {(!DuplicateTree.isEmpty(tree.duplicates))} => begin
        @assign tree.duplicates =
          DuplicateTree.map(tree.duplicates, (name, entry) -> replaceDuplicates2(name, entry, tree))
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return tree
end

function mapRedeclareChains(tree::ClassTree, func::FuncT)
  return @assign () = begin
    @match tree begin
      CLASS_TREE_INSTANTIATED_TREE(__) where {(!DuplicateTree.isEmpty(tree.duplicates))} => begin
        DuplicateTree.map(
          tree.duplicates,
          (name, entry) -> mapRedeclareChain(name, entry, func, tree),
        )
        ()
      end
      _ => begin
        ()
      end
    end
  end
end

function clone(tree::ClassTree)::ClassTree
  local outTree::ClassTree

  @assign outTree = begin
    local clss::Array{InstNode}
    @match tree begin
      CLASS_TREE_EXPANDED_TREE(__) => begin
        @assign clss = arrayCopy(tree.classes)
        @assign clss = ArrayUtil.mapNoCopy(clss, clone)
        CLASS_TREE_EXPANDED_TREE(
          tree.tree,
          clss,
          tree.components,
          tree.exts,
          tree.imports,
          tree.duplicates,
        )
      end

      _ => begin
        tree
      end
    end
  end
  return outTree
end

function fromRecordConstructor(fields::List{<:InstNode}, out::InstNode)::ClassTree
  local tree::ClassTree = EMPTY_CLASS_TREE

  local ltree::LookupTree.Tree = LookupTree.new()
  local i::Int = 1
  local comps::Array{InstNode}

  @assign comps = arrayCreateNoInit(listLength(fields) + 1, EMPTY_NODE())
  for ci in fields
    @assign comps[i] = ci
    @assign ltree =
      addLocalElement(name(ci), LookupTree.COMPONENT(i), tree, ltree)
    @assign i = i + 1
  end
  @assign comps[i] = out
  @assign ltree =
    addLocalElement(name(out), LookupTree.COMPONENT(i), tree, ltree)
  @assign tree =
    CLASS_TREE_FLAT_TREE(ltree, listArray(nil), comps, listArray(nil), DuplicateTree.new())
  return tree
end

""" #= This function instantiates an expanded tree. clsNode is the class to
       be instantiated, while instance is the instance the clsNode belongs to.
       instance is usually the component which has the class as its type. In
       some cases the class itself is the instance, like for the top-level
       model that's being instantiated or packages used for lookup. Because the
       actual instance of clsNode will then be the cloned clsNode created by
       this function it's not possible to send in the correct instance in that
       case, so setting the instance to an empty node is interpreted by this
       function to mean that the instance should be set to the cloned clsNode. =#"""
function instantiate(
  clsNode::InstNode,
  instance::InstNode = EMPTY_NODE(),
  scope::InstNode = EMPTY_NODE(),
)::Tuple{InstNode, InstNode, Int, Int}
  local compCount::Int = 0
  local classCount::Int = 0
  local cls::Class
  local tree::ClassTree
  local ext_tree::ClassTree
  local ltree::LookupTree.Tree
  local exts::Array{InstNode}
  local old_clss::Array{InstNode}
  local old_comps::Array{InstNode}
  local imps::Array{Import}
  local clss::Array{Pointer{InstNode}}
  local comps::Array{Pointer{InstNode}}
  local ext_clss::Array{Pointer{InstNode}}
  local local_comps::List{Int} = nil
  local cls_idx::Int = 1
  local comp_idx::Int = 1
  local cls_count::Int
  local comp_count::Int
  local node::InstNode
  local parent_scope::InstNode
  local inner_node::InstNode
  local inst_scope::InstNode
  local dups::DuplicateTree.Tree
  local comp::Component
  local ext_def::SCode.Element
  local is_typish::Bool
@debug "Instantiating in class tree"
  #=  TODO: If we don't have any extends we could probably generate a flat
  =#
  #=  tree directly and skip a lot of this.
  =#
  #=  Clone the class node by replacing the class in the node with itself.
  =#
  cls = getClass(clsNode)
  clsNode = replaceClass(cls, clsNode)
  () = begin
    @match cls begin
      EXPANDED_CLASS(elements = CLASS_TREE_INSTANTIATED_TREE(__)) => begin
        ()
      end
      EXPANDED_CLASS(__) => begin
        #=  If the instance is an empty node, use the cloned clsNode as the instance.
        =#
        if isEmpty(instance)
          @assign instance = clsNode
          @assign parent_scope = parent(clsNode)
        else
          @assign parent_scope = instance
          @assign inst_scope = scope
        end
        @assign inst_scope = if isEmpty(scope)
          instance
        else
          scope
        end
        #=  Fetch the elements from the class tree.
        =#
        @match CLASS_TREE_EXPANDED_TREE(ltree, old_clss, old_comps, exts, imps, dups) = cls.elements
        #=  Count the number of local classes and components we have.
        =#
        @assign classCount = arrayLength(old_clss)
        #=  The component array contains placeholders for extends, so the length of the
        =#
        #=  extends array needs to be subtracted here to get the number of components.
        =#
        @assign compCount = arrayLength(old_comps) - arrayLength(exts)
        #=  Make a new extends array, and recursively instantiate the extends nodes.
        =#
        @assign exts = arrayCopy(exts)
        for i = 1:arrayLength(exts)
          @assign node = exts[i]
          @match BASE_CLASS(definition = ext_def) = nodeType(node)
          @assign node =
            setNodeType(BASE_CLASS(instance, ext_def), node)
          @assign (node, _, cls_count, comp_count) =
            instantiate(node, EMPTY_NODE(), inst_scope)
          @assign exts[i] = node
          @assign classCount = cls_count + classCount
          @assign compCount = comp_count + compCount
        end
        #=  Update the parent of the extends to be the new instance.
        =#
        #=  Instantiate the class tree of the extends.
        =#
        #=  Add the inherited elements to the class/component counts.
        =#
        #=  Create new arrays that can hold both local and inherited elements.
        =#
        @assign comps =
          arrayCreateNoInit(compCount, P_Pointer.create(EMPTY_NODE()))
        #= /*dummy*/ =#
        @assign clss =
          arrayCreateNoInit(classCount, P_Pointer.create(EMPTY_NODE()))
        #= /*dummy*/ =#
        #=  Copy the local classes into the new class array, and set the
        =#
        #=  class we're instantiating to be their parent.
        =#
        @assign is_typish =
          isType(cls.restriction) ||
          isOperatorRecord(cls.restriction) ||
          isOperator(cls.restriction)
        for c in old_clss
          if is_typish
            @assign c = setParent(clsNode, c)
          else
            @assign c = clone(c)
            @assign c = setParent(instance, c)
          end
          if isOuter(c)
            checkOuterClass(c)
            @assign c = linkInnerOuter(c, parent_scope)
          end
          arrayUpdateNoBoundsChecking(clss, cls_idx, P_Pointer.create(c))
          @assign cls_idx = cls_idx + 1
        end
        for ext in exts
          @assign () = begin
            @match classTree(getClass(ext)) begin
              CLASS_TREE_INSTANTIATED_TREE(classes = ext_clss) => begin
                @assign cls_count = arrayLength(ext_clss)
                if cls_count > 0
                  ArrayUtil.copyRange(ext_clss, clss, 1, cls_count, cls_idx)
                  @assign cls_idx = cls_idx + cls_count
                end
                ()
              end

              _ => begin
                ()
              end
            end
          end
        end
        #=  Copy both local and inherited components into the new array.
        =#
        for c in old_comps
          @assign () = begin
            @match c begin
              COMPONENT_NODE(__) => begin
                #=  Set the component's parent and create a unique instance for it. =#
                @assign node = setParent(instance, c)
                @assign comp = component(node)
                @assign node = replaceComponent(comp, node)
                #=  If the component is outer, link it with the corresponding
                =#
                #=  inner component.
                =#
                if isOuter(comp)
                  @assign node = linkInnerOuter(node, inst_scope)
                end
                #=  Add the node to the component array.
                =#
                arrayUpdateNoBoundsChecking(comps, comp_idx, P_Pointer.create(node))
                @assign local_comps = _cons(comp_idx, local_comps)
                @assign comp_idx = comp_idx + 1
                ()
              end
              REF_NODE(__) => begin
                @assign comp_idx = instExtendsComps(exts[c.index], comps, comp_idx)
                ()
              end
            end
          end
        end
        #=  Sanity check.
        =#
        if comp_idx != compCount + 1
          Error.assertion(
            false,
            getInstanceName() + " miscounted components in " + name(clsNode),
            sourceInfo(),
          )
        end
        if cls_idx != classCount + 1
          Error.assertion(
            false,
            getInstanceName() + " miscounted classes in " + name(clsNode),
            sourceInfo(),
          )
        end
        #=  Create a new class tree and update the class in the node.
        =#
        @assign cls.elements =
          CLASS_TREE_INSTANTIATED_TREE(ltree, clss, comps, local_comps, exts, imps, dups)
          ()
      end
      EXPANDED_DERIVED(baseClass = node) => begin
        @assign node = setNodeType(
          BASE_CLASS(clsNode, definition(node)),
          node,
        )
        @assign (node, instance, classCount, compCount) =
          instantiate(node, instance, scope)
        @assign cls.baseClass = node
        ()
      end
      PARTIAL_BUILTIN(elements = tree && CLASS_TREE_FLAT_TREE(components = old_comps)) =>
        begin
          @assign instance = if isEmpty(instance)
            clsNode
          else
            instance
          end
          @assign old_comps = arrayCopy(old_comps)
          for i = 1:arrayLength(old_comps)
            @assign node = old_comps[i]
            @assign node = setParent(instance, node)
            @assign old_comps[i] =
              replaceComponent(component(node), node)
          end
          @assign tree.components = old_comps
          @assign cls.elements = tree
          @assign compCount = arrayLength(old_comps)
          ()
        end
      PARTIAL_BUILTIN(__) => begin
        ()
      end
      _ => begin
        ###Error.assertion(false, getInstanceName() + " got invalid class", sourceInfo())
        str = clsNode.name
        @error "Got an invalid class $cls for $(str)"
        fail()
      end
    end
end
  updateClass(cls, clsNode)
  return (clsNode, instance, classCount, compCount)
end

""" #= This function adds all local and inherited class and component names to
       the lookup tree. Note that only their names are added, the elements
       themselves are added to their respective arrays by the instantiation
       function below. =#"""
function expand(tree::ClassTree)::ClassTree
  local ltree::LookupTree.Tree
  local lentry::LookupTree.Entry
  local exts::Array{InstNode}
  local clss::Array{InstNode}
  local comps::Array{InstNode}
  local imps::Array{Import}
  local ext_idxs::List{Tuple{Int, Int}} = nil
  local ccount::Int
  local cls_idx::Int
  local comp_idx::Int = 1
  local dups::DuplicateTree.Tree
  local dups_ptr::Pointer

  @match CLASS_TREE_PARTIAL_TREE(ltree, clss, comps, exts, imps, dups) = tree
  @assign cls_idx = arrayLength(clss) + 1
  #=  Since we now know the names of both local and inherited components we
  =#
  #=  can add them to the lookup tree. First we add the local components'
  =#
  #=  names, to be able to catch duplicate local elements easier.
  =#
  for c in comps
    @assign () = begin
      @match c begin
        COMPONENT_NODE(__) => begin
          #=  A component. Add its name to the lookup tree.
          =#
          @assign lentry = LookupTree.COMPONENT(comp_idx)
          @assign ltree = addLocalElement(name(c), lentry, tree, ltree)
          #=  If the component is an element redeclare, add an entry in the duplicate
          =#
          #=  tree so we can check later that it actually redeclares something.
          =#
          if isRedeclare(c)
            @assign dups =
              DuplicateTree.add(dups, c.name, DuplicateTree.newRedeclare(lentry))
          end
          @assign comp_idx = comp_idx + 1
          ()
        end

        REF_NODE(__) => begin
          @assign ext_idxs = _cons((cls_idx - 1, comp_idx - 1), ext_idxs)
          @assign (cls_idx, comp_idx) =
            countInheritedElements(exts[c.index], cls_idx, comp_idx)
          ()
        end
        _ => begin
          Error.assertion(false, getInstanceName() + " got invalid component", sourceInfo())
          fail()
        end
      end
    end
  end
  @assign dups_ptr = P_Pointer.create(dups)
  #=  Add the names of inherited components and classes to the lookup tree. =#
  if !listEmpty(ext_idxs)
    @assign ext_idxs = listReverseInPlace(ext_idxs)
    for ext in exts
      @match _cons((cls_idx, comp_idx), ext_idxs) = ext_idxs
      @assign ltree = expandExtends(ext, ltree, cls_idx, comp_idx, dups_ptr)
    end
  end
  @assign tree = CLASS_TREE_EXPANDED_TREE(ltree, clss, comps, exts, imps, P_Pointer.access(dups_ptr))
  return tree
end

""" #= Adds a list of class and/or component nodes as elements to a flat class
       tree, in the same order as they are listed. Name conflicts will result in
       an duplicate element error, and trying to add nodes that are not pure
       class or component nodes will result in undefined behaviour. =#"""
function addElementsToFlatTree(elements::List{<:InstNode}, tree::ClassTree)::ClassTree
  local ltree::LookupTree.Tree
  local cls_arr::Array{InstNode}
  local comp_arr::Array{InstNode}
  local cls_lst::List{InstNode} = nil
  local comp_lst::List{InstNode} = nil
  local imports::Array{Import}
  local duplicates::DuplicateTree.Tree
  local cls_idx::Int
  local comp_idx::Int
  local lentry::LookupTree.Entry
  @match CLASS_TREE_FLAT_TREE(ltree, cls_arr, comp_arr, imports, duplicates) = tree
  @assign cls_idx = arrayLength(cls_arr)
  @assign comp_idx = arrayLength(comp_arr)
  for e in elements
    if isComponent(e)
      @assign comp_idx = comp_idx + 1
      @assign lentry = LookupTree.COMPONENT(comp_idx)
      @assign comp_lst = _cons(e, comp_lst)
    else
      @assign cls_idx = cls_idx + 1
      @assign lentry = LookupTree.CLASS(cls_idx)
      @assign cls_lst = _cons(e, cls_lst)
    end
    @assign ltree = addLocalElement(name(e), lentry, tree, ltree)
  end
  @assign cls_arr = ArrayUtil.appendList(cls_arr, listReverseInPlace(cls_lst))
  @assign comp_arr = ArrayUtil.appendList(comp_arr, listReverseInPlace(comp_lst))
  @assign tree = CLASS_TREE_FLAT_TREE(ltree, cls_arr, comp_arr, imports, duplicates)
  return tree
end

""" #= Creates a class tree for an enumeration type. =#"""
function fromEnumeration(
  literals::List{<:SCode.Enum},
  enumType::M_Type,
  enumClass::InstNode,
)::ClassTree #= The InstNode of the enumeration type =#
  local tree::ClassTree

  local comps::Array{InstNode}
  local attr_count::Int = 5
  local i::Int = 0
  local comp::InstNode
  local ltree::LookupTree.Tree
  local name::String

  @assign comps =
    arrayCreateNoInit(listLength(literals) + attr_count, EMPTY_NODE())
  @assign ltree = NFBuiltin.ENUM_LOOKUP_TREE
  arrayUpdateNoBoundsChecking(
    comps,
    1,
    fromComponent(
      "quantity",
      TYPE_ATTRIBUTE(TYPE_STRING(), MODIFIER_NOMOD()),
      enumClass,
    ),
  )
  arrayUpdateNoBoundsChecking(
    comps,
    2,
    fromComponent(
      "min",
      TYPE_ATTRIBUTE(enumType, MODIFIER_NOMOD()),
      enumClass,
    ),
  )
  arrayUpdateNoBoundsChecking(
    comps,
    3,
    fromComponent(
      "max",
      TYPE_ATTRIBUTE(enumType, MODIFIER_NOMOD()),
      enumClass,
    ),
  )
  arrayUpdateNoBoundsChecking(
    comps,
    4,
    fromComponent(
      "start",
      TYPE_ATTRIBUTE(enumType, MODIFIER_NOMOD()),
      enumClass,
    ),
  )
  arrayUpdateNoBoundsChecking(
    comps,
    5,
    fromComponent(
      "fixed",
      TYPE_ATTRIBUTE(TYPE_BOOLEAN(), MODIFIER_NOMOD()),
      enumClass,
    ),
  )
  for l in literals
    @assign name = l.literal
    @assign i = i + 1
    @assign comp =
      fromComponent(name, P_Component.newEnum(enumType, name, i), enumClass)
    arrayUpdateNoBoundsChecking(comps, i + attr_count, comp)
    @assign ltree = LookupTree.add(
      ltree,
      name,
      LookupTree.COMPONENT(i + attr_count),
      (comp) -> addEnumConflict(literal = comp),
    )
  end
  #=  Make a new component node for the literal and add it to the lookup tree.
  =#
  #=  Enumerations can't contain extends, so we can go directly to a flat tree here.
  =#
  @assign tree =
    CLASS_TREE_FLAT_TREE(ltree, listArray(nil), comps, listArray(nil), DuplicateTree.EMPTY())
  return tree
end

""" #= Creates a new class tree from a list of SCode elements. =#"""
function fromSCode(
  elements::List{<:SCode.Element},
  isClassExtends::Bool,
  parent::InstNode,
)::ClassTree
  local tree::ClassTree

  local ltree::LookupTree.Tree
  local lentry::LookupTree.Entry
  local clsc::Int
  local compc::Int
  local extc::Int
  local i::Int
  local clss::Array{InstNode}
  local comps::Array{InstNode}
  local exts::Array{InstNode}
  local cls_idx::Int = 0
  local ext_idx::Int = 0
  local comp_idx::Int = 0
  local dups::DuplicateTree.Tree
  local imps::List{Import} = nil
  local imps_arr::Array{Import}
  local info::SourceInfo

  @assign ltree = LookupTree.new()
  #=  Count the different types of elements.
  =#
  @assign (clsc, compc, extc) = countElements(elements)
  #=  If the class is a class extends, reserve space for the extends.
  =#
  if isClassExtends
    @assign extc = extc + 1
  end
  #=  Preallocate arrays for the elements. We can't do this for imports
  =#
  #=  though, since an import clause might import multiple elements.
  =#
  @assign clss = arrayCreate(clsc, EMPTY_NODE())
  @assign comps = arrayCreate(compc + extc, EMPTY_NODE())
  @assign exts = arrayCreate(extc, EMPTY_NODE())
  @assign dups = DuplicateTree.new()
  #=  Make a temporary class tree so we can do lookup for error reporting.
  =#
  @assign tree = CLASS_TREE_PARTIAL_TREE(ltree, clss, comps, exts, listArray(nil), dups)
  #=  If the class is a class extends, fill in the first extends with an
  =#
  #=  empty node so we don't have unassigned memory after this step.
  =#
  if isClassExtends
    @assign exts[1] = EMPTY_NODE()
    @assign comps[1] = REF_NODE(1)
    @assign ext_idx = ext_idx + 1
    @assign comp_idx = comp_idx + 1
  end
  for e in elements
    @assign () = begin
      @match e begin
        SCode.CLASS(__) => begin
          #=  A class, add it to the class array and add an entry in the lookup tree.
          =#
          @assign cls_idx = cls_idx + 1
          arrayUpdate(clss, cls_idx, newClass(e, parent))
          @assign lentry = LookupTree.CLASS(cls_idx)
          @assign ltree = addLocalElement(e.name, lentry, tree, ltree)
          #=  If the class is an element redeclare, add an entry in the duplicate
          =#
          #=  tree so we can check later that it actually redeclares something.
          =#
          if SCodeUtil.isElementRedeclare(e) || SCodeUtil.isClassExtends(e)
            @assign dups =
              DuplicateTree.add(dups, e.name, DuplicateTree.newRedeclare(lentry))
          end
          ()
        end

        SCode.COMPONENT(__) => begin
          #=  A component, add it to the component array but don't add an entry
          =#
          #=  in the lookup tree. We need to preserve the components' order, but
          =#
          #=  won't know their actual indices until we've expanded the extends.
          =#
          #=  We don't really need to be able to look up components until after
          =#
          #=  that happens, so we add them to the lookup tree later instead.
          =#
          @assign comp_idx = comp_idx + 1
          arrayUpdate(comps, comp_idx, newComponent(e))
          ()
        end

        SCode.EXTENDS(__) => begin
          #=  An extends clause, add it to the list of extends, and also add a
          =#
          #=  reference in the component array so we can preserve the order of
          =#
          #=  components.
          =#
          @assign ext_idx = ext_idx + 1
          arrayUpdate(exts, ext_idx, newExtends(e, parent))
          @assign comp_idx = comp_idx + 1
          arrayUpdate(comps, comp_idx, REF_NODE(ext_idx))
          ()
        end

        SCode.IMPORT(imp = Absyn.P_Import.Import.UNQUAL_IMPORT(__), info = info) => begin
          #=  An unqualified import clause. We need to know which names are
          =#
          #=  imported by the clause, so it needs to be instantiated.
          =#
          @assign imps = P_Import.Import.instUnqualified(e.imp, parent, info, imps)
          ()
        end

        SCode.IMPORT(__) => begin
          #=  A qualified import clause. Since the import itself gives the name
          =#
          #=  of the imported element we can delay resolving the path until we
          =#
          #=  need it (i.e. when the name is used). Doing so avoids some
          =#
          #=  dependency issues, like when a package is imported into one of it's
          =#
          #=  enclosing scopes.
          =#
          @assign imps =
            _cons(P_Import.Import.UNRESOLVED_IMPORT(e.imp, parent, e.info), imps)
          ()
        end
      end
    end
  end
  #= else
  =#
  #=   algorithm
  =#
  #=     print(getInstanceName() + \" skipping:\\n\" +
  =#
  #=       SCodeDump.unparseElementStr(e) + \"\\n\");
  =#
  #=   then
  =#
  #=     ();
  =#
  #=  Add all the imported names to the lookup tree.
  =#
  @assign imps_arr = listArray(imps)
  @assign i = 1
  for e in imps
    @assign ltree = addImport(e, i, ltree, imps_arr)
    @assign i = i + 1
  end
  @assign tree = CLASS_TREE_PARTIAL_TREE(ltree, clss, comps, exts, imps_arr, dups)
  return tree
end

""" #= Checks that a class used as outer is valid, i.e. is a short class
       definition with no modifier. =#"""
function checkOuterClass(outerCls::InstNode)
  local def::SCode.ClassDef

  return if isOnlyOuter(outerCls)
    @assign def = SCodeUtil.getClassDef(definition(outerCls))
    @assign () = begin
      @match def begin
        SCode.ClassDef.DERIVED(modifications = SCode.Mod.NOMOD(__)) => begin
          ()
        end

        SCode.ClassDef.DERIVED(__) => begin
          #=  Outer short class definition without mod is ok.
          =#
          #=  Outer short class definition with mod is an error.
          =#
          Error.addSourceMessage(
            Error.OUTER_ELEMENT_MOD,
            list(SCodeDump.printModStr(def.modifications), name(outerCls)),
            info(outerCls),
          )
          fail()
        end

        _ => begin
          #=  Outer long class definition is an error.
          =#
          Error.addSourceMessage(
            Error.OUTER_LONG_CLASS,
            list(name(outerCls)),
            info(outerCls),
          )
          fail()
        end
      end
    end
  end
end

""" #= Looks up the corresponding inner node for the given outer node,
       and returns an INNER_OUTER_NODE containing them both. =#"""
function linkInnerOuter(outerNode::InstNode, scope::InstNode)::InstNode
  local innerOuterNode::InstNode

  local inner_node::InstNode

  @assign inner_node = Lookup.lookupInner(outerNode, scope)
  #=  Make sure we found a node of the same kind.
  =#
  if valueConstructor(outerNode) != valueConstructor(inner_node)
    Error.addMultiSourceMessage(
      Error.FOUND_WRONG_INNER_ELEMENT,
      list(
        typeName(inner_node),
        name(outerNode),
        typeName(outerNode),
      ),
      list(info(outerNode), info(inner_node)),
    )
    fail()
  end
  @assign innerOuterNode = INNER_OUTER_NODE(inner_node, outerNode)
  return innerOuterNode
end

function replaceDuplicates4(
  entry::DuplicateTree.Entry,
  node::InstNode,
)::DuplicateTree.Entry

  @assign entry.node = SOME(node)
  @assign entry.children = list(replaceDuplicates4(c, node) for c in entry.children)
  return entry
end

function replaceDuplicates3(
  entry::DuplicateTree.Entry,
  kept::InstNode,
  tree::ClassTree,
)::DuplicateTree.Entry
  local node_ptr::Pointer{InstNode}
  local node::InstNode
  @assign node_ptr = resolveEntryPtr(entry.entry, tree)
  @assign node = P_Pointer.access(node_ptr)
  @assign entry.node = SOME(node)
  P_Pointer.update(node_ptr, kept)
  @assign entry.children = list(replaceDuplicates3(c, kept, tree) for c in entry.children)
  return entry
end

function replaceDuplicates2(
  name::String,
  entry::DuplicateTree.Entry,
  tree::ClassTree,
)::DuplicateTree.Entry

  local kept::InstNode
  local node_ptr::Pointer{InstNode}
  local children::List{DuplicateTree.Entry}
  local kept_entry::DuplicateTree.Entry

  @assign node_ptr = resolveEntryPtr(entry.entry, tree)
  @assign () = begin
    @match entry.ty begin
      DuplicateTree.EntryType.REDECLARE => begin
        @assign kept = P_Pointer.access(resolveEntryPtr(entry.entry, tree))
        @assign entry = replaceDuplicates4(entry, kept)
        ()
      end

      DuplicateTree.EntryType.DUPLICATE => begin
        @assign kept = P_Pointer.access(node_ptr)
        @assign entry.node = SOME(kept)
        @assign entry.children =
          list(replaceDuplicates3(c, kept, tree) for c in entry.children)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return entry
end

function getRedeclareChain(
  entry::DuplicateTree.Entry,
  tree::ClassTree,
  chain::List{<:Pointer{<:InstNode}} = nil,
)::List{Pointer{InstNode}}

  @assign chain = begin
    local node_ptr::Pointer{InstNode}
    local node::InstNode
    @match entry.ty begin
      DuplicateTree.EntryType.REDECLARE => begin
        @assign node_ptr = resolveEntryPtr(entry.entry, tree)
        if listEmpty(entry.children)
          @assign node = P_Pointer.access(node_ptr)
          if SCodeUtil.isClassExtends(definition(node))
            Error.addSourceMessage(
              Error.CLASS_EXTENDS_TARGET_NOT_FOUND,
              list(name(node)),
              info(node),
            )
          else
            Error.addSourceMessage(
              Error.REDECLARE_NONEXISTING_ELEMENT,
              list(name(node)),
              info(node),
            )
          end
          fail()
        end
        getRedeclareChain(listHead(entry.children), tree, _cons(node_ptr, chain))
      end

      DuplicateTree.EntryType.ENTRY => begin
        @assign node_ptr = resolveEntryPtr(entry.entry, tree)
        _cons(node_ptr, chain)
      end

      _ => begin
        chain
      end
    end
  end
  return chain
end

function mapRedeclareChain(
  name::String,
  entry::DuplicateTree.Entry,
  func::FuncT,
  tree::ClassTree,
)::DuplicateTree.Entry

  local chain::List{Pointer{InstNode}}

  @assign chain = getRedeclareChain(entry, tree)
  if !listEmpty(chain)
    func(chain)
  end
  return entry
end

function enumerateDuplicates4(
  entry::LookupTree.Entry,
  classes::List{<:Int},
  components::List{<:Int},
)::Tuple{List{Int}, List{Int}}

  @assign () = begin
    @match entry begin
      LookupTree.CLASS(__) => begin
        #= classes := entry.index :: classes;
        =#
        ()
      end

      LookupTree.COMPONENT(__) => begin
        @assign components = _cons(entry.index, components)
        ()
      end
    end
  end
  return (classes, components)
end

function enumerateDuplicates3(
  entry::DuplicateTree.Entry,
  classes::List{<:Int},
  components::List{<:Int},
)::Tuple{List{Int}, List{Int}}

  @assign (classes, components) = enumerateDuplicates4(entry.entry, classes, components)
  for c in entry.children
    @assign (classes, components) = enumerateDuplicates3(c, classes, components)
  end
  return (classes, components)
end

function enumerateDuplicates2(
  name::String,
  entry::DuplicateTree.Entry,
  classes::List{<:Int},
  components::List{<:Int},
)::Tuple{List{Int}, List{Int}}

  for c in entry.children
    @assign (classes, components) = enumerateDuplicates3(c, classes, components)
  end
  return (classes, components)
end

""" #= Returns the indices of the duplicate classes and components,
       not including the ones that should be kept. =#"""
function enumerateDuplicates(
  duplicates::DuplicateTree.Tree,
)::Tuple{List{Int}, List{Int}}
  local components::List{Int}
  local classes::List{Int}

  if DuplicateTree.isEmpty(duplicates)
    @assign classes = nil
    @assign components = nil
  else
    @assign (classes, components) =
      DuplicateTree.fold_2(duplicates, enumerateDuplicates2, nil, nil)
    @assign classes = ListUtil.sort(classes, intGt)
    @assign components = ListUtil.sort(components, intGt)
  end
  return (classes, components)
end

""" #= Joins two duplicate tree entries together. =#"""
function joinDuplicates(
  newEntry::DuplicateTree.Entry,
  oldEntry::DuplicateTree.Entry,
  name::String,
)::DuplicateTree.Entry
  local entry::DuplicateTree.Entry = oldEntry

  #=  Add the new entry as a child of the old entry.
  =#
  @assign entry.children = _cons(newEntry, entry.children)
  return entry
end

function offsetDuplicate(
  entry::LookupTree.Entry,
  classOffset::Int,
  componentOffset::Int,
)::LookupTree.Entry
  local offsetEntry::LookupTree.Entry

  @assign offsetEntry = begin
    @match entry begin
      LookupTree.CLASS(__) => begin
        LookupTree.CLASS(entry.index + classOffset)
      end

      LookupTree.COMPONENT(__) => begin
        LookupTree.COMPONENT(entry.index + componentOffset)
      end
    end
  end
  return offsetEntry
end

""" #= Offsets all values in the given entry so that they become valid for the
       inheriting class. =#"""
function offsetDuplicates(
  name::String,
  entry::DuplicateTree.Entry,
  classOffset::Int,
  componentOffset::Int,
)::DuplicateTree.Entry
  local offsetEntry::DuplicateTree.Entry

  local parent::LookupTree.Entry
  local children::List{DuplicateTree.Entry}

  @assign parent = offsetDuplicate(entry.entry, classOffset, componentOffset)
  @assign children =
    list(offsetDuplicates(name, c, classOffset, componentOffset) for c in entry.children)
  @assign offsetEntry = DuplicateTree.ENTRY(parent, NONE(), children, entry.ty)
  return offsetEntry
end

""" #= Conflict handler for addInheritedComponent. =#"""
function addInheritedElementConflict(
  newEntry::LookupTree.Entry,
  oldEntry::LookupTree.Entry,
  name::String,
  duplicates::Pointer{<:DuplicateTree.Tree},
  extDuplicates::DuplicateTree.Tree,
)::LookupTree.Entry
  local entry::LookupTree.Entry

  local dups::DuplicateTree.Tree
  local opt_dup_entry::Option{DuplicateTree.Entry}
  local dup_entry::DuplicateTree.Entry
  local new_id::Int = LookupTree.index(newEntry)
  local old_id::Int = LookupTree.index(oldEntry)
  local ty::DuplicateTree.EntryType

  #=  Overwrite the existing entry if it's an import. This happens when a
  =#
  #=  class both imports and inherits the same name.
  =#
  if LookupTree.isImport(oldEntry)
    @assign entry = newEntry
    return entry
  end
  @assign dups = P_Pointer.access(duplicates)
  @assign opt_dup_entry = DuplicateTree.getOpt(dups, name)
  if isNone(opt_dup_entry)
    if new_id < old_id
      @assign entry = newEntry
      @assign dup_entry = DuplicateTree.newDuplicate(newEntry, oldEntry)
    else
      @assign entry = oldEntry
      @assign dup_entry = DuplicateTree.newDuplicate(oldEntry, newEntry)
    end
    @assign dups = DuplicateTree.add(dups, name, dup_entry)
    P_Pointer.update(duplicates, dups)
  else
    @match SOME(dup_entry) = opt_dup_entry
    @assign ty = dup_entry.ty
    if !DuplicateTree.idExistsInEntry(newEntry, dup_entry)
      if ty == DuplicateTree.EntryType.REDECLARE
        @assign entry = newEntry
        @assign dup_entry.children =
          _cons(DuplicateTree.newEntry(newEntry), dup_entry.children)
      else
        if new_id < old_id
          @assign entry = newEntry
          @assign dup_entry = DuplicateTree.Entry.ENTRY(
            newEntry,
            NONE(),
            _cons(DuplicateTree.newEntry(oldEntry), dup_entry.children),
            dup_entry.ty,
          )
        else
          @assign entry = oldEntry
          @assign dup_entry.children =
            _cons(DuplicateTree.newEntry(newEntry), dup_entry.children)
        end
      end
      @assign dups = DuplicateTree.update(dups, name, dup_entry)
      P_Pointer.update(duplicates, dups)
    elseif !DuplicateTree.idExistsInEntry(oldEntry, dup_entry)
      if ty == DuplicateTree.EntryType.REDECLARE || new_id < old_id
        @assign entry = newEntry
        @assign dup_entry.children =
          _cons(DuplicateTree.newEntry(oldEntry), dup_entry.children)
      else
        @assign entry = newEntry
        @assign dup_entry = DuplicateTree.Entry.ENTRY(
          newEntry,
          NONE(),
          _cons(DuplicateTree.newEntry(oldEntry), dup_entry.children),
          dup_entry.ty,
        )
      end
      @assign dups = DuplicateTree.update(dups, name, dup_entry)
      P_Pointer.update(duplicates, dups)
    else
      @assign entry = if new_id < old_id
        newEntry
      else
        oldEntry
      end
    end
  end
  #=  If no duplicate entry yet exists, add a new one.
  =#
  #=  Here it's possible for either the new or the old entry to not exist in the duplicate entry.
  =#
  #=  The new might not exist simply because it hasn't been added yet, while the old might not
  =#
  #=  exist because it wasn't a duplicate in its own scope. At least one of them must exist though,
  =#
  #=  since duplicate entries are added for any name occurring more than once.
  =#
  #=  If the existing entry is for a redeclare, then the position of the element
  =#
  #=  doesn't matter and the new entry should be added as a child to the redeclare.
  =#
  #=  Otherwise we need to keep the 'first' element as the parent.
  =#
  #=  Note that this only actually works for components, since we don't
  =#
  #=  preserve the order for classes. But which class we choose shouldn't
  =#
  #=  matter since they should be identical. We might also compare e.g. a
  =#
  #=  component to a class here, but that will be caught in checkDuplicates.
  =#
  #=  Same as above but we add the old entry instead.
  =#
  #=  If both the old and the new entry already exists, which can happen if the
  =#
  #=  new entry was added by expandExtents, then we don't need to add anything.
  =#
  return entry
end

function addInheritedElement(
  name::String,
  entry::LookupTree.Entry,
  classOffset::Int,
  componentOffset::Int,
  conflictFunc::LookupTree.ConflictFunc,
  tree::LookupTree.Tree,
)::LookupTree.Tree

  @assign () = begin
    @match entry begin
      LookupTree.CLASS(__) => begin
        @assign entry.index = entry.index + classOffset
        @assign tree = LookupTree.add(tree, name, entry, conflictFunc)
        ()
      end

      LookupTree.COMPONENT(__) => begin
        @assign entry.index = entry.index + componentOffset
        @assign tree = LookupTree.add(tree, name, entry, conflictFunc)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  #=  Ignore IMPORT, since imports aren't inherited.
  =#
  return tree
end

function expandExtends(
  extendsNode::InstNode,
  tree::LookupTree.Tree,
  classOffset::Int,
  componentOffset::Int,
  duplicates::Pointer{<:DuplicateTree.Tree},
)::LookupTree.Tree #= Duplicate elements info. =#

  local cls_tree::ClassTree
  local ext_tree::LookupTree.Tree
  local ext_dups::DuplicateTree.Tree
  local dups::DuplicateTree.Tree
  local conf_func::LookupTree.ConflictFunc

  #=  The extends node's lookup tree should at this point contain all the
  =#
  #=  entries we need, so we don't need to recursively traverse its
  =#
  #=  elements. Instead we can just take each entry in the extends node's
  =#
  #=  lookup tree, add the class or component index as an offset, and then
  =#
  #=  add the entry to the given lookup tree.
  =#
  @assign cls_tree = classTree(getClass(extendsNode))
  @assign (ext_tree, ext_dups) = begin
    @match cls_tree begin
      CLASS_TREE_EXPANDED_TREE(__) => begin
        (cls_tree.tree, cls_tree.duplicates)
      end

      CLASS_TREE_FLAT_TREE(__) => begin
        (cls_tree.tree, cls_tree.duplicates)
      end

      _ => begin
        return
        (tree, DuplicateTree.new())
      end
    end
  end
  #=  Copy entries from the extends node's duplicate tree if there are any.
  =#
  if !DuplicateTree.isEmpty(ext_dups)
    @assign dups = DuplicateTree.map(
      ext_dups,
      (classOffset, componentOffset) ->
        offsetDuplicates(classOffset = classOffset, componentOffset = componentOffset),
    )
    @assign dups = DuplicateTree.join(P_Pointer.access(duplicates), dups, joinDuplicates)
    P_Pointer.update(duplicates, dups)
  end
  #=  Offset the entries so they're correct for the inheriting class tree.
  =#
  #=  Join the two duplicate trees together.
  =#
  conf_func =
    (newEntry, oldEntry, name) ->
      addInheritedElementConflict(newEntry, oldEntry, name, duplicates, ext_dups)
  #=  Copy entries from the extends node's lookup tree.
  =#
  foldFunc = (nameX, entryX, treeX) -> addInheritedElement(
      nameX,
      entryX,
      classOffset,
      componentOffset,
      conf_func,
      treeX,
    )
  tree = LookupTree.fold(
    ext_tree,
    foldFunc,
    tree,
  )
  return tree #= The lookup tree to add names to =#
end

function countInheritedElements(
  extendsNode::InstNode,
  classCount::Int = 0,
  componentCount::Int = 0,
)::Tuple{Int, Int}

  local clss::Array{InstNode}
  local comps::Array{InstNode}
  local exts::Array{InstNode}

  @assign () = begin
    @match classTree(getClass(extendsNode)) begin
      CLASS_TREE_EXPANDED_TREE(classes = clss, components = comps, exts = exts) => begin
        #=  The component array contains placeholders for extends, which need to be
        =#
        #=  subtracted to get the proper component count.
        =#
        @assign componentCount = componentCount + arrayLength(comps) - arrayLength(exts)
        @assign classCount = classCount + arrayLength(clss)
        for ext in exts
          @assign (classCount, componentCount) =
            countInheritedElements(ext, classCount, componentCount)
        end
        ()
      end

      CLASS_TREE_FLAT_TREE(classes = clss, components = comps) => begin
        @assign componentCount = componentCount + arrayLength(comps)
        @assign classCount = classCount + arrayLength(clss)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return (classCount, componentCount)
end

""" #= Counts the number of classes, components and extends clauses in a list of
       SCode elements. =#"""
function countElements(elements::List{<:SCode.Element})::Tuple{Int, Int, Int}
  local extCount::Int = 0
  local compCount::Int = 0
  local classCount::Int = 0

  for e in elements
    @assign () = begin
      @match e begin
        SCode.CLASS(__) => begin
          @assign classCount = classCount + 1
          ()
        end

        SCode.COMPONENT(__) => begin
          @assign compCount = compCount + 1
          ()
        end

        SCode.EXTENDS(__) => begin
          @assign extCount = extCount + 1
          ()
        end

        _ => begin
          ()
        end
      end
    end
  end
  return (classCount, compCount, extCount)
end

function resolveImport(index::Int, tree::ClassTree)::InstNode
  local element::InstNode

  local imports::Array{Import}
  local imp::Import
  local changed::Bool

  @assign imports = begin
    @match tree begin
      CLASS_TREE_PARTIAL_TREE(__) => begin
        tree.imports
      end

      CLASS_TREE_EXPANDED_TREE(__) => begin
        tree.imports
      end

      CLASS_TREE_INSTANTIATED_TREE(__) => begin
        tree.imports
      end

      CLASS_TREE_FLAT_TREE(__) => begin
        tree.imports
      end
    end
  end
  #=  Imports are resolved on demand, i.e. here.
  =#
  @assign (element, changed, imp) = P_Import.Import.resolve(imports[index])
  #=  Save the import if it wasn't already resolved.
  =#
  if changed
    arrayUpdate(imports, index, imp)
  end
  return element
end

function resolveComponent(index::Int, tree::ClassTree)::InstNode
  local element::InstNode

  @assign element = begin
    @match tree begin
      CLASS_TREE_INSTANTIATED_TREE(__) => begin
        P_Pointer.access(arrayGet(tree.components, index))
      end

      CLASS_TREE_FLAT_TREE(__) => begin
        arrayGet(tree.components, index)
      end
    end
  end
  return element
end

function resolveClass(index::Int, tree::ClassTree)::InstNode
  local element::InstNode
  @assign element = begin
    @match tree begin
      CLASS_TREE_PARTIAL_TREE(__) => begin
        arrayGet(tree.classes, index)
      end

      CLASS_TREE_EXPANDED_TREE(__) => begin
        arrayGet(tree.classes, index)
      end

      CLASS_TREE_INSTANTIATED_TREE(__) => begin
        P_Pointer.access(arrayGet(tree.classes, index))
      end

      CLASS_TREE_FLAT_TREE(__) => begin
        arrayGet(tree.classes, index)
      end
    end
  end
  @debug "Returning element in resolveClass"
  return element
end

function resolveDuplicateEntriesPtr(
  entry::DuplicateTree.Entry,
  tree::ClassTree,
  elements::List{<:Pointer{<:InstNode}} = nil,
)::List{Pointer{InstNode}}

  local node_ptr::Pointer{InstNode}

  @assign node_ptr = resolveEntryPtr(entry.entry, tree)
  @assign elements = _cons(node_ptr, elements)
  for child in entry.children
    @assign elements = resolveDuplicateEntriesPtr(child, tree, elements)
  end
  return elements
end

function resolveEntryPtr(entry::LookupTree.Entry, tree::ClassTree)::Pointer{InstNode}
  local element::Pointer{InstNode}

  local elems::Array{Pointer{InstNode}}

  @assign element = begin
    @match entry begin
      LookupTree.CLASS(__) => begin
        @match CLASS_TREE_INSTANTIATED_TREE(classes = elems) = tree
        arrayGet(elems, entry.index)
      end

      LookupTree.COMPONENT(__) => begin
        @match CLASS_TREE_INSTANTIATED_TREE(components = elems) = tree
        arrayGet(elems, entry.index)
      end
    end
  end
  return element
end

""" #= Resolves a lookup tree entry to an inst node. =#"""
function resolveEntry(entry::LookupTree.Entry, tree::ClassTree)::Tuple{InstNode, Bool}
  local isImport::Bool
  local element::InstNode

  @assign (element, isImport) = begin
    @match entry begin
      LookupTree.CLASS(__) => begin
        (resolveClass(entry.index, tree), false)
      end

      LookupTree.COMPONENT(__) => begin
        (resolveComponent(entry.index, tree), false)
      end

      LookupTree.IMPORT(__) => begin
        (resolveImport(entry.index, tree), true)
      end
    end
  end
  return (element, isImport)
end

function addDuplicateConflict(
  newEntry::DuplicateTree.Entry,
  oldEntry::DuplicateTree.Entry,
  name::String,
)::DuplicateTree.Entry
  local entry::DuplicateTree.Entry

  #=  The previously kept entry should be either kept or dup, since it's the
  =#
  #=  one found during lookup. So we can ignore it here.
  =#
  @assign entry = DuplicateTree.ENTRY(
    newEntry.entry,
    NONE(),
    _cons(listHead(newEntry.children), oldEntry.children),
    DuplicateTree.EntryType.DUPLICATE,
  )
  return entry
end

""" #= Adds an entry to the duplicates tree. =#"""
function addDuplicate(
  name::String,
  duplicateEntry::LookupTree.Entry,
  keptEntry::LookupTree.Entry,
  duplicates::Pointer{<:DuplicateTree.Tree},
)::Pointer{DuplicateTree.Tree}

  P_Pointer.update(
    duplicates,
    DuplicateTree.add(
      P_Pointer.access(duplicates),
      name,
      DuplicateTree.newDuplicate(keptEntry, duplicateEntry),
      addDuplicateConflict,
    ),
  )
  return duplicates
end

function addImportConflict(
  newEntry::LookupTree.Entry,
  oldEntry::LookupTree.Entry,
  name::String,
  imports::Array{<:Import},
)::LookupTree.Entry
  local entry::LookupTree.Entry

  @assign entry = begin
    local imp1::Import
    local imp2::Import
    @match (newEntry, oldEntry) begin
      (LookupTree.IMPORT(__), LookupTree.IMPORT(__)) => begin
        @assign imp1 = imports[newEntry.index]
        @assign imp2 = imports[oldEntry.index]
        #=  Check what kind of imports we have. In case of an error we replace the import
        =#
        #=  with the error information, and only print the error if the name is looked up.
        =#
        @assign entry = begin
          @match (imp1, imp2) begin
            (P_Import.Import.UNRESOLVED_IMPORT(__), P_Import.Import.UNRESOLVED_IMPORT(__)) => begin
              #=  Two qualified imports of the same name gives an error.
              =#
              arrayUpdate(
                imports,
                oldEntry.index,
                P_Import.Import.CONFLICTING_IMPORT(imp1, imp2),
              )
              oldEntry
            end

            (P_Import.Import.RESOLVED_IMPORT(__), P_Import.Import.RESOLVED_IMPORT(__)) => begin
              #=  A name imported from several unqualified imports gives an error.
              =#
              arrayUpdate(
                imports,
                oldEntry.index,
                P_Import.Import.CONFLICTING_IMPORT(imp1, imp2),
              )
              oldEntry
            end

            (P_Import.Import.UNRESOLVED_IMPORT(__), _) => begin
              newEntry
            end

            _ => begin
              oldEntry
            end
          end
        end
        #=  Qualified import overwrites an unqualified.
        =#
        #=  oldEntry is either qualified or a delayed error, keep it.
        =#
        entry
      end

      _ => begin
        oldEntry
      end
    end
  end
  #=  Other elements overwrite an imported name.
  =#
  return entry
end

function addImport(
  imp::Import,
  index::Int,
  tree::LookupTree.Tree,
  imports::Array{<:Import},
)::LookupTree.Tree

  @assign tree = LookupTree.add(
    tree,
    P_Import.Import.name(imp),
    LookupTree.IMPORT(index),
    (imports) -> addImportConflict(imports = imports),
  )
  return tree
end

""" #= Conflict handler for fromEnumeration. =#"""
function addEnumConflict(
  newEntry::LookupTree.Entry,
  oldEntry::LookupTree.Entry,
  name::String,
  literal::InstNode,
)::LookupTree.Entry
  local entry::LookupTree.Entry

  Error.addSourceMessage(
    Error.DOUBLE_DECLARATION_OF_ELEMENTS,
    list(name(literal)),
    info(literal),
  )
  fail()
  return entry
end

""" #= Helper function to addLocalElementConflict. Looks up an entry in a
       partial class tree. =#"""
function findLocalConflictElement(entry::LookupTree.Entry, classTree::ClassTree)::InstNode
  local node::InstNode

  @assign node = begin
    local comps::Array{InstNode}
    local exts::Array{InstNode}
    local i::Int
    #=  For classes we can just use the normal resolveClass function.
    =#
    @match entry begin
      LookupTree.CLASS(__) => begin
        resolveClass(entry.index, classTree)
      end

      LookupTree.COMPONENT(__) => begin
        #=  Components are more complicated, since they are given indices based
        =#
        #=  on where they will end up once inherited elements have been inserted
        =#
        #=  into the component array. We therefore just count components until we
        =#
        #=  get to the given index. Not very efficient, but it doesn't really
        =#
        #=  matter at this point since we're just going to show an error and fail.
        =#
        @assign i = 0
        @match CLASS_TREE_PARTIAL_TREE(components = comps, exts = exts) = classTree
        for c in comps
          @assign i = begin
            @match c begin
              COMPONENT_NODE(__) => begin
                i + 1
              end

              REF_NODE(__) => begin
                @assign (_, i) = countInheritedElements(exts[c.index], 0, i)
                i
              end
            end
          end
          if i == entry.index
            @assign node = c
            break
          end
        end
        #=  Make extra sure that we actually found the component.
        =#
        Error.assertion(
          i == entry.index,
          getInstanceName() + " got invalid entry index",
          sourceInfo(),
        )
        node
      end

      _ => begin
        Error.assertion(false, getInstanceName() + " got invalid entry", sourceInfo())
        fail()
      end
    end
  end
  return node
end

function addLocalElementConflict(
  newEntry::LookupTree.Entry,
  oldEntry::LookupTree.Entry,
  name::String,
  classTree::ClassTree,
)::LookupTree.Entry
  local entry::LookupTree.Entry
  local n1::InstNode
  local n2::InstNode
  @assign entry = begin
    @match (newEntry, oldEntry) begin
      (_, LookupTree.IMPORT(__)) => begin
        newEntry
      end
      _ => begin
        #=  Local elements overwrite imported elements with same name.
        =#
        #=  Otherwise we have two local elements with the same name, which is an error.
        =#
        @assign n1 = findLocalConflictElement(newEntry, classTree)
        @assign n2 = findLocalConflictElement(oldEntry, classTree)
        Error.addMultiSourceMessage(
          Error.DOUBLE_DECLARATION_OF_ELEMENTS,
          list(name),
          list(info(n2), info(n1)),
        )
        fail()
      end
    end
  end
  return entry
end

function addLocalElement(
  name::String,
  entry::LookupTree.Entry,
  classTree::ClassTree,
  tree::LookupTree.Tree,
)::LookupTree.Tree
  @assign tree = LookupTree.add(
    tree,
    name,
    entry,
    (classTree) -> addLocalElementConflict(classTree = classTree),
  )
  return tree
end

function lookupTree(ctree::ClassTree)::LookupTree.Tree
  local ltree::LookupTree.Tree
  @assign ltree = begin
    @match ctree begin
      CLASS_TREE_PARTIAL_TREE(__) => begin
        ctree.tree
      end
      CLASS_TREE_EXPANDED_TREE(__) => begin
        ctree.tree
      end
      CLASS_TREE_INSTANTIATED_TREE(__) => begin
        ctree.tree
      end
      CLASS_TREE_FLAT_TREE(__) => begin
        ctree.tree
      end
    end
  end
  return ltree
end

function getDuplicates(tree::ClassTree)::DuplicateTree.Tree
  local duplicates::DuplicateTree.Tree

  @assign duplicates = begin
    @match tree begin
      CLASS_TREE_PARTIAL_TREE(__) => begin
        tree.duplicates
      end

      CLASS_TREE_EXPANDED_TREE(__) => begin
        tree.duplicates
      end

      CLASS_TREE_INSTANTIATED_TREE(__) => begin
        tree.duplicates
      end

      CLASS_TREE_FLAT_TREE(__) => begin
        tree.duplicates
      end
    end
  end
  return duplicates
end

function instExtendsComps(
  extNode::InstNode,
  comps::Array{<:Pointer{<:InstNode}},
  index::Int,
)::Int #= The first free index in comps =#
  local ext_comps_ptrs::Array{Pointer{InstNode}}
  local ext_comps::Array{InstNode}
  local comp_count::Int
  local ext_comp::InstNode
  @assign () = begin
    @match classTree(getClass(extNode)) begin
      CLASS_TREE_INSTANTIATED_TREE(components = ext_comps_ptrs) => begin
        @assign comp_count = arrayLength(ext_comps_ptrs)
        if comp_count > 0
          ArrayUtil.copyRange(ext_comps_ptrs, comps, 1, comp_count, index)
          @assign index = index + comp_count
        end
        ()
      end
      CLASS_TREE_FLAT_TREE(components = ext_comps) => begin
        @assign comp_count = arrayLength(ext_comps)
        if comp_count > 0
          for i = index:(index + comp_count - 1)
            arrayUpdate(comps, i, P_Pointer.create(ext_comps[i]))
          end
          @assign index = index + comp_count
        end
        ()
      end
      _ => begin
        ()
      end
    end
  end
  return index #= The first free index in comps =#
end
