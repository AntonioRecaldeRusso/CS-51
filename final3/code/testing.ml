open Core.Std
open Order
open Rbt

module RB = RBT (IntCompare)
let a = RB.empty
let b = RB.insert 1 a
let c = RB.insert 2 b
let d = RB.insert 3 c
let e = RB.insert 4 d
let f = RB.insert 5 e
let g = RB.insert 6 f
let h = RB.insert 7 g



let aa = RB.delete 1 b
let bb = RB.delete 2 c
let dd = RB.delete 4 e
let emp = RB.delete 1 (RB.delete 2 (RB.delete 3 (RB.delete 4 (RB.delete 5 (RB.delete 6 (RB.delete 7 h))))))

let _ = assert (a = aa)
let _ = assert (b = bb)
let _ = assert (d = dd)
let _ = assert (a = emp)
let _ = assert (RB.search 1 b)
let _ = assert (RB.search 6 h)
let _ = assert (not (RB.search 2 a))





