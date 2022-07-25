namespace Okasaki.C2Persistence

/// Figure 2.1 Example of a 'Stack' signature but with list
/// nomenclature. 
type LStack<'a> = {
   empty: LStack<'a>
   isEmpty: LStack<'a> -> bool
   cons: 'a * LStack<'a> -> LStack<'a>
   head: LStack<'a> -> 'a
   tail: LStack<'a> -> 'a
}
/// Figure 2.2 List stack implemented using F# built-in list type
/// instead of user defined stack signature.
[<RequireQualifiedAccess>]
module ListStack =
   type ListStack<'a> =
       'a list
       
   let Empty: ListStack<_> = []
   
   let isEmpty listStack =
       List.isEmpty listStack
       
   let cons x xs:ListStack<_> = x::xs
   
   let head s = List.head s
   
   let tail s = List.tail s
   
/// Figure 2.3 Custom stack with DU 'Empty' for NIL value.
[<RequireQualifiedAccess>]
module CustomStack =
    type CStack<'a> =
        | Empty
        | Cons of 'a * CStack<'a>
        
    let empty = Empty
    
    let isEmpty = function
        | Empty -> true
        | _ -> false
        
    let cons x xs = Cons(x, xs)
    
    let head = function
        | Empty -> failwith "empty"
        | Cons(x, _) -> x
        
    let tail = function
        | Empty -> failwith "empty"
        | Cons(_, xs) -> xs
        
    /// Append a CStack list to a existing one in a imperative way.
    /// This 'destroys' the previous list because of mutating the previous representation.
    let rec (++) x xs =
        match x with
        | Empty -> xs
        | Cons(h, t) -> Cons(h, t ++ xs)
        
    /// Figure 2.4 Append a CStack list to a existing one in a declarative way,
    /// preserving the previous representation by copying from the affected node. 
    let rec update = function
        | (Empty, _, _) -> failwith "subscript is invalid"
        | (Cons(_, xs), 0, x) -> Cons(x, xs)
        | (Cons(_, xs), i, x) when i > 0 -> update (xs, i - 1, x)
    
   
   
   
   

