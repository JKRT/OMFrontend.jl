@UniontypeDecl Tree
valueStr = Function
ConflictFunc = Function
EachFunc = Function
FoldFunc = Function
MapFunc = Function
#=  TODO: We should have an Any type =#
#= The binary tree data structure. =#
@Uniontype Tree begin
  @Record NODE begin
    key::Key #= The key of the node. =#
    value::Value
    height::Integer #= Height of tree, used for balancing =#
    left::Tree #= Left subtree. =#
    right::Tree #= Right subtree. =#
  end
  @Record LEAF begin
    key::Key #= The key of the node. =#
    value::Value
  end
  @Record EMPTY begin
  end
end

""" #= Return an empty tree =#"""
function new()::Tree
  local outTree::Tree = EMPTY()
  return outTree
end

""" #= Gets a value from the tree given a key. =#"""
function hasKey(inTree::Tree, inKey::Key)::Bool
  local comp::Bool = false

  local key::Key
  local key_comp::Integer
  local tree::Tree

  @assign key = begin
    @match inTree begin
      NODE(__) => begin
        inTree.key
      end

      LEAF(__) => begin
        inTree.key
      end

      EMPTY(__) => begin
        return
        fail()
      end
    end
  end
  @assign key_comp = keyCompare(inKey, key)
  @assign comp = begin
    @match (key_comp, inTree) begin
      (0, _) => begin
        true
      end

      (1, NODE(right = tree)) => begin
        hasKey(tree, inKey)
      end

      (-1, NODE(left = tree)) => begin
        hasKey(tree, inKey)
      end

      _ => begin
        false
      end
    end
  end
  return comp
end

function isEmpty(tree::Tree)::Bool
  local isEmpty::Bool
  @assign isEmpty = begin
    @match tree begin
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

""" #= Converts the tree to a flat list of keys (in order). =#"""
function listKeys(inTree::Tree, lst::List{<:Key} = nil)::List{Key}

  @assign lst = begin
    @match inTree begin
      LEAF(__) => begin
        _cons(inTree.key, lst)
      end

      NODE(__) => begin
        lst = listKeys(inTree.right, lst)
        lst = _cons(inTree.key, lst)
        lst = listKeys(inTree.left, lst)
        lst
      end

      _ => begin
        lst
      end
    end
  end
  return lst
end

""" #= Converts the tree to a flat list of keys (in order). =#"""
function listKeysReverse(inTree::Tree, lst::List{<:Key} = nil)::List{Key}

  @assign lst = begin
    @match inTree begin
      LEAF(__) => begin
        _cons(inTree.key, lst)
      end

      NODE(__) => begin
        lst = listKeysReverse(inTree.left, lst)
        lst = _cons(inTree.key, lst)
        lst = listKeysReverse(inTree.right, lst)
        lst
      end

      _ => begin
        lst
      end
    end
  end
  return lst
end


""" #= Prints the tree to a string using UTF-8 box-drawing characters to construct a
   graphical view of the tree. =#"""
function printTreeStr(inTree::Tree)::String
  local outString::String
  local left::Tree
  local right::Tree
  outString = begin
    @match inTree begin
      EMPTY(__) => begin
        "EMPTY()"
      end
      LEAF(__) => begin
        printNodeStr(inTree)
      end
      NODE(left = left, right = right) => begin
        printTreeStr2(left, true, "") +
        printNodeStr(inTree) +
        "
        " +
        printTreeStr2(right, false, "")
      end
    end
  end
  return outString
end

function referenceEqOrEmpty(t1::Tree, t2::Tree)::Bool
  local b::Bool

  @assign b = begin
    @match (t1, t2) begin
      (EMPTY(__), EMPTY(__)) => begin
        true
      end

      _ => begin
        referenceEq(t1, t2)
      end
    end
  end
  return b
end

""" #= Balances a Tree =#"""
function balance(inTree::Tree)
  local outTree::Tree = inTree
  outTree = begin
    local lh::Integer
    local rh::Integer
    local diff::Integer
    local child::Tree
    local balanced_tree::Tree
    @match outTree begin
      LEAF(__) => begin
        inTree
      end
      NODE(__) => begin
        lh = height(outTree.left)
        rh = height(outTree.right)
        diff = lh - rh
        if diff < (-1)
          balanced_tree = if calculateBalance(outTree.right) > 0
            rotateLeft(setTreeLeftRight(
              outTree,
              left = outTree.left,
              right = rotateRight(outTree.right),
            ))
          else
            rotateLeft(outTree)
          end
        elseif diff > 1
          balanced_tree = if calculateBalance(outTree.left) < 0
            rotateRight(setTreeLeftRight(
              outTree,
              left = rotateLeft(outTree.left),
              right = outTree.right,
            ))
          else
            rotateRight(outTree)
          end
        elseif outTree.height != max(lh, rh) + 1
          @assign outTree.height = max(lh, rh) + 1
          balanced_tree = outTree
        else
          balanced_tree = outTree
        end
        balanced_tree
      end
    end
  end
  return outTree
end

function height(inNode::Tree)::Integer
  local outHeight::Integer
  @assign outHeight = begin
    @match inNode begin
      NODE(__) => begin
        inNode.height
      end
      LEAF(__) => begin
        1
      end
      _ => begin
        0
      end
    end
  end
  return outHeight
end

function calculateBalance(inNode::Tree)::Integer
  local outBalance::Integer

  outBalance = begin
    @match inNode begin
      NODE(__) => begin
        height(inNode.left) - height(inNode.right)
      end

      LEAF(__) => begin
        0
      end

      _ => begin
        0
      end
    end
  end
  return outBalance
end

""" #= Performs an AVL left rotation on the given tree. =#"""
function rotateLeft(inNode::Tree)::Tree
  local outNode::Tree = inNode

  @assign outNode = begin
    local node::Tree
    local child::Tree
    @match outNode begin
      NODE(right = child && NODE(__)) => begin
        node = setTreeLeftRight(outNode, outNode.left, child.left)
        setTreeLeftRight(child, node, child.right)
      end

      NODE(right = child && LEAF(__)) => begin
        node = setTreeLeftRight(outNode, outNode.left, EMPTY())
        setTreeLeftRight(child, node, EMPTY())
      end

      _ => begin
        inNode
      end
    end
  end
  return outNode
end

""" #= Performs an AVL right rotation on the given tree. =#"""
function rotateRight(inNode::Tree)::Tree
  local outNode::Tree = inNode

  @assign outNode = begin
    local node::Tree
    local child::Tree
    @match outNode begin
      NODE(left = child && NODE(__)) => begin
        node = setTreeLeftRight(outNode, left = child.right, right = outNode.right)
        setTreeLeftRight(child, right = node, left = child.left)
      end

      NODE(left = child && LEAF(__)) => begin
        node = setTreeLeftRight(outNode, left = EMPTY(), right = outNode.right)
        setTreeLeftRight(child, right = node, left = EMPTY())
      end
      _ => begin
        inNode
      end
    end
  end
  return outNode
end

""" #= Helper function to printTreeStr. =#"""
function printTreeStr2(inTree::Tree, isLeft::Bool, inIndent::String)::String
  local outString::String
  local val_node::Option{ValueNode}
  local left::Option{Tree}
  local right::Option{Tree}
  local left_str::String
  local right_str::String
  outString = begin
    @match inTree begin
      NODE(__) => begin
        printTreeStr2(inTree.left, true, inIndent + (
          if isLeft
            "     "
          else
            " │   "
          end
        )) +
        inIndent +
        (
          if isLeft
            " ┌"
          else
            " └"
          end
        ) +
        "────" +
        printNodeStr(inTree) +
        "
        " +
        printTreeStr2(inTree.right, false, inIndent + (
          if isLeft
            " │   "
          else
            "     "
          end
        ))
      end

      _ => begin
        ""
      end
    end
  end
  return outString
end


#===#
function printNodeStr(inNode::Tree)::String
  local outString::String
  @assign outString = begin
    @match inNode begin
      NODE(__) => begin
        "(" + keyStr(inNode.key)
        +", " + valueStr(inNode.value) + ")"
      end

      LEAF(__) => begin
        "(" + keyStr(inNode.key)
        +", " + valueStr(inNode.value) + ")"
      end
    end
  end
  return outString
end


#= Default conflict resolving function for add. =#
 """ #= Conflict resolving function for add which fails on conflict. =#"""
function addConflictFail(newValue::Value, oldValue::Value, key::Key)::Value
  local value::Value
  fail()
  return value
end
addConflictDefault = addConflictFail

""" #= Conflict resolving function for add which replaces the old value with the new. =#"""
function addConflictReplace(newValue::Value, oldValue::Value, key::Key)::Value
  local value::Value = newValue
  return value
end

""" #= Conflict resolving function for add which keeps the old value. =#"""
function addConflictKeep(newValue::Value, oldValue::Value, key::Key)::Value
  local value::Value = oldValue
  return value
end

""" #= Inserts a new node in the tree. =#"""
function add(
  inTree::Tree,
  inKey::Key,
  inValue::Value,
  conflictFunc::ConflictFunc = addConflictDefault,
)::Tree #= Used to resolve conflicts. =#
  local tree::Tree = inTree

  @assign tree = begin
    local key::Key
    local value::Value
    local key_comp::Integer
    local outTree::Tree
    #=  Empty tree.
    =#
    @match tree begin
      EMPTY(__) => begin
        LEAF(inKey, inValue)
      end

      NODE(key = key) => begin
        @assign key_comp = keyCompare(inKey, key)
        if key_comp == (-1)
          @assign tree.left = add(tree.left, inKey, inValue, conflictFunc)
        elseif key_comp == 1
          @assign tree.right = add(tree.right, inKey, inValue, conflictFunc)
        else
          @assign value = conflictFunc(inValue, tree.value, key)
          if !referenceEq(tree.value, value)
            @assign tree.value = value
          end
        end
        #=  Replace left branch.
        =#
        #=  Replace right branch.
        =#
        #=  Use the given function to resolve the conflict.
        =#
        if key_comp == 0
          tree
        else
          balance(tree)
        end
      end
      LEAF(key = key) => begin
        @assign key_comp = keyCompare(inKey, key)
        if key_comp == (-1)
          @assign outTree = NODE(tree.key, tree.value, 2, LEAF(inKey, inValue), EMPTY())
        elseif key_comp == 1
          @assign outTree = NODE(tree.key, tree.value, 2, EMPTY(), LEAF(inKey, inValue))
        else
          @assign value = conflictFunc(inValue, tree.value, key)
          if !referenceEq(tree.value, value)
            @assign tree.value = value
          end
          @assign outTree = tree
        end
        if key_comp == 0
          outTree
        else
          balance(outTree)
        end
      end
    end
  end
  return tree
end

""" #= Adds a list of key-value pairs to the tree. =#"""
function addList(
  tree::Tree,
  inValues::List{<:Tuple{<:Key, Value}},
  conflictFunc::ConflictFunc = addConflictDefault,
)::Tree #= Used to resolve conflicts. =#

  local key::Key
  local value::Value

  for t in inValues
    @assign (key, value) = t
    @assign tree = add(tree, key, value, conflictFunc)
  end
  return tree
end

""" #= Alias for add that replaces the node in case of conflict. =#"""
function update(tree::Tree, key::Key, value::Value)::Tree
  local outTree::Tree = add(tree, key, value, addConflictReplace)
  return outTree
end

""" #= Fetches a value from the tree given a key, or fails if no value is associated
   with the key. =#"""
function get(tree::Tree, key::Key)::Value
  local value::Value
  local k::Key
  @assign k = begin
    @match tree begin
      NODE(__) => begin
        tree.key
      end
      LEAF(__) => begin
        tree.key
      end
    end
  end
  @assign value = begin
    @match (keyCompare(key, k), tree) begin
      (0, LEAF(__)) => begin
        tree.value
      end

      (0, NODE(__)) => begin
        tree.value
      end

      (1, NODE(__)) => begin
        get(tree.right, key)
      end

      (-1, NODE(__)) => begin
        get(tree.left, key)
      end
    end
  end
  return value
end

""" #= Fetches a value from the tree given a key, or returns NONE if no value is
   associated with the key. =#"""
function getOpt(tree::Tree, key::Key)::Option{Value}
  local value::Option{Value}

  local k::Key

  @assign k = begin
    @match tree begin
      NODE(__) => begin
        tree.key
      end

      LEAF(__) => begin
        tree.key
      end

      _ => begin
        key
      end
    end
  end
  @assign value = begin
    @match (keyCompare(key, k), tree) begin
      (0, LEAF(__)) => begin
        SOME(tree.value)
      end

      (0, NODE(__)) => begin
        SOME(tree.value)
      end

      (1, NODE(__)) => begin
        getOpt(tree.right, key)
      end

      (-1, NODE(__)) => begin
        getOpt(tree.left, key)
      end

      _ => begin
        NONE()
      end
    end
  end
  return value
end

""" #= Creates a new tree from a list of key-value pairs. =#"""
function fromList(
  inValues::List{<:Tuple{<:Key, Value}},
  conflictFunc::ConflictFunc = addConflictDefault,
)::Tree #= Used to resolve conflicts. =#
  local tree::Tree = EMPTY()

  local key::Key
  local value::Value

  for t in inValues
    @assign (key, value) = t
    @assign tree = add(tree, key, value, conflictFunc)
  end
  return tree
end

""" #= Converts the tree to a flat list of key-value tuples. =#"""
function toList(
  inTree::Tree,
  lst::List{<:Tuple{<:Key, Value}} = nil,
)::List{Tuple{Key, Value}}

  @assign lst = begin
    local key::Key
    local value::Value
    @match inTree begin
      NODE(key = key, value = value) => begin
        @assign lst = toList(inTree.right, lst)
        @assign lst = _cons((key, value), lst)
        @assign lst = toList(inTree.left, lst)
        lst
      end

      LEAF(key = key, value = value) => begin
        _cons((key, value), lst)
      end

      _ => begin
        lst
      end
    end
  end
  return lst
end

""" #= Constructs a list of all the values in the tree. =#"""
function listValues(tree::Tree, lst::List{<:Value} = nil) where {T <: Value}
  @assign lst = begin
    local value::Value
    @match tree begin
      NODE(value = value) => begin
        lst = listValues(tree.right, lst)
        lst = _cons(value, lst)
        lst = listValues(tree.left, lst)
        lst
      end
      LEAF(value = value) => begin
        _cons(value, lst)
      end
      _ => begin
        lst
      end
    end
  end
  return lst
end

""" #= Joins two trees by adding the second one to the first. =#"""
function join(
  tree::Tree,
  treeToJoin::Tree,
  conflictFunc::ConflictFunc = addConflictDefault,
)::Tree #= Used to resolve conflicts. =#

  @assign tree = begin
    @match treeToJoin begin
      EMPTY(__) => begin
        tree
      end

      NODE(__) => begin
        @assign tree = add(tree, treeToJoin.key, treeToJoin.value, conflictFunc)
        @assign tree = join(tree, treeToJoin.left, conflictFunc)
        @assign tree = join(tree, treeToJoin.right, conflictFunc)
        tree
      end

      LEAF(__) => begin
        add(tree, treeToJoin.key, treeToJoin.value, conflictFunc)
      end
    end
  end
  return tree
end

""" #= Traverses the tree in depth-first pre-order and applies the given function to
   each node, but without constructing a new tree like with map. =#"""
function forEach(tree::Tree, func::EachFunc)
  return @assign _ = begin
    @match tree begin
      NODE(__) => begin
        forEach(tree.left, func)
        func(tree.key, tree.value)
        forEach(tree.right, func)
        ()
      end

      LEAF(__) => begin
        func(tree.key, tree.value)
        ()
      end

      EMPTY(__) => begin
        ()
      end
    end
  end
end

function intersection()
  return fail()
end

""" #= Traverses the tree in depth-first pre-order and applies the given function to
   each node, constructing a new tree with the resulting nodes. =#"""
function map(inTree::Tree, inFunc::MapFunc)::Tree
  local outTree::Tree = inTree
  @assign outTree = begin
    local key::Key
    local value::Value
    local new_value::Value
    local branch::Tree
    local new_branch::Tree
    @match outTree begin
      NODE(key = key, value = value) => begin
        @assign new_branch = map(outTree.left, inFunc)
        if !referenceEq(new_branch, outTree.left)
          @assign outTree.left = new_branch
        end
        @assign new_value = inFunc(key, value)
        if !referenceEq(value, new_value)
          @assign outTree.value = new_value
        end
        @assign new_branch = map(outTree.right, inFunc)
        if !referenceEq(new_branch, outTree.right)
          @assign outTree.right = new_branch
        end
        outTree
      end
      LEAF(key = key, value = value) => begin
        @assign new_value = inFunc(key, value)
        if !referenceEq(value, new_value)
          @assign outTree.value = new_value
        end
        outTree
      end
      _ => begin
        inTree
      end
    end
  end
  return outTree
end

""" #= Traverses the tree in depth-first pre-order and applies the given function to
   each node, in the process updating the given argument. =#"""
function fold(inTree::Tree, inFunc::FoldFunc, inStartValue::FT) where {FT}
  local outResult = inStartValue
  outResult = begin
    local key::Key
    local value::Value
    @match inTree begin
      NODE(key = key, value = value) => begin
        outResult = fold(inTree.left, inFunc, outResult)
        outResult = inFunc(key, value, outResult)
        outResult = fold(inTree.right, inFunc, outResult)
        outResult
      end
      LEAF(key = key, value = value) => begin
        outResult = inFunc(key, value, outResult)
        outResult
      end 
      _ => begin
        outResult
      end
    end
  end
  return outResult
end

""" #= Like fold, but takes two fold arguments. =#"""
function fold_2(
  tree::Tree,
  foldFunc::FoldFunc,
  foldArg1::FT1,
  foldArg2::FT2,
) where {FT1, FT2}

  @assign () = begin
    @match tree begin
      NODE(__) => begin
        @assign (foldArg1, foldArg2) = fold_2(tree.left, foldFunc, foldArg1, foldArg2)
        @assign (foldArg1, foldArg2) = foldFunc(tree.key, tree.value, foldArg1, foldArg2)
        @assign (foldArg1, foldArg2) = fold_2(tree.right, foldFunc, foldArg1, foldArg2)
        ()
      end

      LEAF(__) => begin
        @assign (foldArg1, foldArg2) = foldFunc(tree.key, tree.value, foldArg1, foldArg2)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return (foldArg1, foldArg2)
end

""" #= Like fold, but if the fold function returns false it will not continue down
   into the tree (but will still continue with other branches). =#"""
function foldCond(tree::Tree, foldFunc::FoldFunc, value::FT) where {FT}

  @assign value = begin
    local c::Bool
    @match tree begin
      NODE(__) => begin
        @assign (value, c) = foldFunc(tree.key, tree.value, value)
        if c
          @assign value = foldCond(tree.left, foldFunc, value)
          @assign value = foldCond(tree.right, foldFunc, value)
        end
        value
      end

      LEAF(__) => begin
        @assign (value, c) = foldFunc(tree.key, tree.value, value)
        value
      end

      _ => begin
        value
      end
    end
  end
  return value
end

""" #= Traverses the tree in depth-first pre-order and applies the given function to
   each node, constructing a new tree with the resulting nodes. mapFold also
   takes an extra argument which is updated on each call to the given function. =#"""
function mapFold(inTree::Tree, inFunc::MapFunc, inStartValue::FT) where {FT}
  local outResult::FT = inStartValue
  local outTree::Tree = inTree

  @assign outTree = begin
    local key::Key
    local value::Value
    local new_value::Value
    local branch::Tree
    local new_branch::Tree
    @match outTree begin
      NODE(key = key, value = value) => begin
        @assign (new_branch, outResult) = mapFold(outTree.left, inFunc, outResult)
        if !referenceEq(new_branch, outTree.left)
          @assign outTree.left = new_branch
        end
        @assign (new_value, outResult) = inFunc(key, value, outResult)
        if !referenceEq(value, new_value)
          @assign outTree.value = new_value
        end
        @assign (new_branch, outResult) = mapFold(outTree.right, inFunc, outResult)
        if !referenceEq(new_branch, outTree.right)
          @assign outTree.right = new_branch
        end
        outTree
      end

      LEAF(key = key, value = value) => begin
        @assign (new_value, outResult) = inFunc(key, value, outResult)
        if !referenceEq(value, new_value)
          @assign outTree.value = new_value
        end
        outTree
      end

      _ => begin
        inTree
      end
    end
  end
  return (outTree, outResult)
end

function setTreeLeftRight(orig::Tree, left::Tree, right::Tree)::Tree
  setTreeLeftRight(orig, left = left, right = right)
end

function setTreeLeftRight(orig::Tree; left::Tree = EMPTY(), right::Tree = EMPTY())::Tree
  local res::Tree

  @assign res = begin
    @match (orig, left, right) begin
      (NODE(__), EMPTY(__), EMPTY(__)) => begin
        LEAF(orig.key, orig.value)
      end

      (LEAF(__), EMPTY(__), EMPTY(__)) => begin
        orig
      end

      (NODE(__), _, _) => begin
        if referenceEqOrEmpty(orig.left, left) && referenceEqOrEmpty(orig.right, right)
          orig
        else
          NODE(orig.key, orig.value, max(height(left), height(right)) + 1, left, right)
        end
      end

      (LEAF(__), _, _) => begin
        NODE(orig.key, orig.value, max(height(left), height(right)) + 1, left, right)
      end
    end
  end
  return res
end
