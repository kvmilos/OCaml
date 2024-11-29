open Bstree

module IntOrder = struct
  type t = int
  let compare = compare
end

module StringOrder = struct
  type t = string
  let compare = compare
end

module IntTree = BST(IntOrder)

module StringTree = BST(StringOrder)

let sort_int l =
  IntTree.to_list (IntTree.from_list l)

let sort_string l =
  StringTree.to_list (StringTree.from_list l)
