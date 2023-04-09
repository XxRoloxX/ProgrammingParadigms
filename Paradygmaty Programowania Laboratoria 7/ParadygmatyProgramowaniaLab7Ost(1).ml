module type Point_type = sig

  type precision = Int of int | Float of float


  type 'a point
  val distance : (precision point* precision point) -> float
  val createPoint: (precision*precision*precision)->precision point

  val compare: precision point->precision point->bool
end;;

module type Point_Type_Generic = sig
  type precision

  type  point
  val createPoint: (precision*precision*precision)->point


end;;

module Point_Generic:Point_Type_Generic = struct

  type precision = Int of int | Float of float


  type point = {
    x:precision; 
    y:precision;
    z:precision;
    }


  let createPoint(x1,y1,z1)={x=x1;y=y1;z=z1};;

  let distance (point1, point2) = match (point1.x, point1.y, point1.z ,point2.x, point2.y,point2.z) with
  (Int x1,Int y1,Int z1,Int x2,Int y2,Int z2 ) -> sqrt(Int.to_float((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2)))
  |(Float x1,Float y1,Float z1,Float x2,Float y2,Float z2 ) -> sqrt((x1-.x2)*.(x1-.x2) +. (y1-.y2)*.(y1-.y2) +. (z1-.z2)*.(z1-.z2))
  | _ -> -1.0;;

  let compare(point1)(point2)= match (point1.x,point2.x) with 
  |(Int(x1),Int(x2))-> distance(point1,{x=Int(0);y=Int(0);z=Int(0)}) < distance(point2,{x=Int(0);y=Int(0);z=Int(0)})
  |(Float(x1),Float(x2))-> distance(point1,{x=Float(0.0);y=Float(0.0);z=Float(0.0)}) < distance(point2,{x=Float(0.0);y=Float(0.0);z=Float(0.0)})
  | _ -> failwith("Compare type mismatch, both points should be einter float or int");;


end;;


module Point: Point_type = struct

  type precision = Int of int | Float of float


  type 'a point = {
    x:'a; 
    y:'a;
    z:'a;
    }


  let createPoint(x1,y1,z1)={x=x1;y=y1;z=z1};;

  let distance (point1, point2) = match (point1.x, point1.y, point1.z ,point2.x, point2.y,point2.z) with
  (Int x1,Int y1,Int z1,Int x2,Int y2,Int z2 ) -> sqrt(Int.to_float((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2)))
  |(Float x1,Float y1,Float z1,Float x2,Float y2,Float z2 ) -> sqrt((x1-.x2)*.(x1-.x2) +. (y1-.y2)*.(y1-.y2) +. (z1-.z2)*.(z1-.z2))
  | _ -> -1.0;;

  let compare(point1)(point2)= match (point1.x,point2.x) with 
  |(Int(x1),Int(x2))-> distance(point1,{x=Int(0);y=Int(0);z=Int(0)}) < distance(point2,{x=Int(0);y=Int(0);z=Int(0)})
  |(Float(x1),Float(x2))-> distance(point1,{x=Float(0.0);y=Float(0.0);z=Float(0.0)}) < distance(point2,{x=Float(0.0);y=Float(0.0);z=Float(0.0)})
  | _ -> failwith("Compare type mismatch, both points should be einter float or int");;


end;;


module type Length_type = sig

  type 'a segment
  val distance : Point.precision segment -> float
  val createSegment: (Point.precision Point.point)*(Point.precision Point.point) -> Point.precision segment
end;;


module type Segment_Type_Generic = sig
  type precision
  type 'a segment
  val createSegment: (Point_Generic.point)*(Point_Generic.point) -> Point.precision segment

end

module Length: Length_type = struct

  type precision = Int of int | Float of float


  type 'a segment = {
    point1:'a Point.point;
    point2:'a Point.point;
    }

  let createSegment (point_new_1,point_new_2) = {point1=point_new_1; point2=point_new_2}

  let distance (given_segment) = Point.distance(given_segment.point1, given_segment.point2)

end;;

(*
  (*ALTERNATYWNE ROZWIĄZANIE*)

   module type Point_type = sig

  type 'a point
  type t
  val distance : (t point*t point) -> float
  val createPoint: (t*t*t)->t point

  val compare: t point->t point->bool
end

module type Basic_point_type = sig

  type 'a point
  type t
  val createPoint: (t*t*t)->t point
end


module Point_Int: Point_type = struct

  type t=int
  type 'a point = {
    x:'a; 
    y:'a;
    z:'a;
  }


  let createPoint(x1,y1,z1)={x=x1;y=y1;z=z1};;

  let distance (point1, point2) =  sqrt(Int.to_float((point1.x - point2.x)*(point1.x - point2.x) +(point1.y - point2.y)*(point1.y - point2.y) +(point1.z - point2.z)*(point1.z - point2.z) ))
  

  let compare(point1)(point2)= distance(point1,{x=0;y=0;z=0}) < distance(point2,{x=0;y=0;z=0})
  


end
module Point_Float: Point_type = struct


  type t=float
  type 'a point = {
    x:'a; 
    y:'a;
    z:'a;
  }


  let createPoint(x1,y1,z1)={x=x1;y=y1;z=z1};;

  let distance (point1, point2) =  sqrt((point1.x -. point2.x)*.(point1.x -. point2.x) +.(point1.y -. point2.y)*.(point1.y -. point2.y) +.(point1.z -. point2.z)*.(point1.z -. point2.z))
  

  let compare(point1)(point2)= distance(point1,{x=0.0;y=0.0;z=0.0}) < distance(point2,{x=0.0;y=0.0;z=0.0})
  


end




module type Length_type = sig

  type t

  type 'a point = {
    x:'a; 
    y:'a;
    z:'a;
  }

  type segment = {
    point1:t point ;
    point2:t point ;
    }
  val distance : segment -> float
  val createSegment: (t point)*(t point) -> segment
end

module Length_Int: Length_type = struct

  type t=int

  type 'a point = {
    x:'a; 
    y:'a;
    z:'a;
  }

  type segment = {
    point1:t point;
    point2:t point;
    }

  let createSegment (point_new_1,point_new_2) = {point1=point_new_1; point2=point_new_2}

  let distance (given_segment) = let (point1,point2) = (given_segment.point1,given_segment.point2) in sqrt(Int.to_float((point1.x - point2.x)*(point1.x - point2.x) +(point1.y - point2.y)*(point1.y - point2.y) +(point1.z - point2.z)*(point1.z - point2.z) ))

end

module Length_Float: Length_type = struct

  type t=float

  type 'a point = {
    x:'a; 
    y:'a;
    z:'a;
  }

  type segment = {
    point1:t point;
    point2:t point;
    }

  let createSegment (point_new_1,point_new_2) = {point1=point_new_1; point2=point_new_2}

  let distance (given_segment) = let (point1,point2) = (given_segment.point1,given_segment.point2) in sqrt((point1.x -. point2.x)*.(point1.x -. point2.x) +.(point1.y -. point2.y)*.(point1.y -. point2.y) +.(point1.z -. point2.z)*.(point1.z -. point2.z))

end





*)

module type BinaryTree_type = sig

  type key

  type bt 

  val addNodeUnit: key-> bt ref -> unit;;
  val deleteNodeUnit:  key-> bt ref -> unit;;

  val createTree: key->   bt ref;;

  val preOrder: bt ref-> key list;;

  val inorder: bt ref-> key list;;

  val postorder: bt ref-> key list;;

  val getLeafs:  bt ref-> key list;;

end

module type KeyOrder = sig
  type key
  type order = LES | EQ | GT
  val compare: key->key -> order

end

module BinaryTree(Ord:KeyOrder): BinaryTree_type with type key = Ord.key = struct

  type key=Ord.key

  type bt = Nil | Cons of key*(unit-> bt)*(unit->bt)

  exception DuplicatedKey of key
  exception EmptyTree of string


  let rec addNode(node,tree) = match (node,tree) with 
  (val1, Cons(val2,l2,r2)) -> (
    match (Ord.compare val1 val2) with
      LES-> (match l2() with
         Nil -> Cons(val2,(fun() ->Cons(node,(fun ()->Nil),(fun()->Nil))),r2)
        |Cons(_,_,_) -> Cons(val2,(fun()->addNode(node,l2())),r2))
      |GT-> (match r2() with
        Nil -> Cons(val2,l2,(fun() ->Cons(node,(fun()->Nil),(fun()->Nil))))
        |Cons(_,_,_) -> Cons(val2,l2,(fun()->addNode(node,r2()))))
      |EQ -> tree)
  | _ -> failwith "Tree and Node should have values"
  

  let rec getMostRightChild tree = match tree with
    Cons(val1,left,right) ->(match right() with
      Nil->val1
      |Cons(_,_,_)->getMostRightChild(right()))
    |Nil-> raise (EmptyTree "tree is empty")


   let rec deleteNode (node,tree) = match (node,tree) with 
   (val1, Cons(val2,l2,r2)) -> (
      match Ord.compare val1 val2 with 
        LES->(match l2() with
          Nil -> Cons(val2,(fun ()->Nil),r2)
          |Cons(_,_,_) ->  Cons(val2,(fun()->deleteNode(node,l2())),r2))
        |GT -> (match r2() with
          Nil -> Cons(val2,l2,(fun()->Nil))
          |Cons(_,_,_) -> Cons(val2,l2,(fun()->deleteNode(node,r2()))))
        |EQ -> match (l2(),r2()) with
          (Nil,Nil) ->Nil
          |(Cons(_,_,_),Nil)->l2()
          |(Nil,Cons(_,_,_))->r2()
          |(Cons(_,_,_),Cons(_,_,_))-> let mostRightChild = getMostRightChild (l2()) in Cons(mostRightChild,(fun()->deleteNode (mostRightChild,(l2()))),r2)
        )
    | _ -> failwith "Tree and Node should have values"



   let addNodeUnit (node) (tree) = tree := addNode(node,!tree);; 
   let deleteNodeUnit (node)(tree) = tree := deleteNode(node,!tree);; 

  let createTree(root_value) = ref(Cons(root_value,(fun()->Nil),(fun()->Nil))) ;;

 
    let rec getLeafs tree = 
      let rec getLeafsAccumulator tree accum =
        match !tree with
          Nil -> accum
          |Cons(val1,left,right) -> 
            match (left(),right()) with
              (Cons(_,_,_),Cons(_,_,_)) -> getLeafsAccumulator(ref(left()))(getLeafsAccumulator(ref(right()))(accum))
              |(Cons(_,_,_),Nil) -> getLeafsAccumulator(ref(left()))(accum)
              |(Nil,Cons(_,_,_)) -> getLeafsAccumulator(ref(right()))(accum)
              |(Nil,Nil) -> val1::accum
    in getLeafsAccumulator tree [];;


    (*
   let default_tree = addNode(Cons(1,(fun()->Nil),(fun()->Nil)),!default_tree);;
   let default_tree = addNode(Cons(2,(fun()->Nil),(fun()->Nil)),default_tree);;

   let default_tree = addNode(Cons(3,(fun()->Nil),(fun()->Nil)),!default_tree);;

   let default_tree = addNode(Cons(5,(fun()->Nil),(fun()->Nil)),default_tree);;

   let default_tree = addNode(Cons(7,(fun()->Nil),(fun()->Nil)),default_tree);;

   let default_tree = addNode(Cons(6,(fun()->Nil),(fun()->Nil)),default_tree);;

   let default_tree = addNode(Cons(-1,(fun()->Nil),(fun()->Nil)),default_tree);;
   let default_tree = addNode(Cons(-2,(fun()->Nil),(fun()->Nil)),default_tree);;

*)


   let preOrder startNode = 
    let rec preOrderHelp node accum = match node with
    Nil -> accum
   | Cons(v,l,r)->v::preOrderHelp(l()) (preOrderHelp(r()) accum)
  in preOrderHelp !startNode [];;

  let rec inorder tree = match !tree with
  Cons(v,l,r) ->(inorder (ref(l()))) @ v::(inorder (ref(r())))
| Nil -> [];;

let rec postorder tree = match !tree with
Cons(v,l,r) ->(postorder (ref(l()))) @ (postorder (ref(r()))) @ [v]
|Nil -> [];;

(*inorder default_tree;;*)

(*getLeafs default_tree;;*)
  

end;;


module BT_INT = BinaryTree(
  struct 
    type key=int;;
    type order = LES | EQ | GT;;
    let compare val1 val2 = if val1<val2 then LES else if val1>val2 then GT else EQ;;
  end);;


let testTree = BT_INT.createTree 5;;
BT_INT.addNodeUnit 3 testTree;; 
BT_INT.addNodeUnit 6 testTree;; 
BT_INT.addNodeUnit 2 testTree;; 
BT_INT.addNodeUnit 7 testTree;; 
BT_INT.addNodeUnit 4 testTree;; 
BT_INT.addNodeUnit 1 testTree;; 
BT_INT.addNodeUnit 10 testTree;; 
BT_INT.addNodeUnit 9 testTree;; 

BT_INT.inorder testTree;;

BT_INT.getLeafs testTree;;

BT_INT.deleteNodeUnit 5 testTree;;
BT_INT.deleteNodeUnit 9 testTree;;

let p1  = Point.createPoint(Int(1),Int(2),Int(3));;
let p2 = Point.createPoint(Int(3),Int(3),Int(3));;

let dist = Point.distance(p1,p2);;


let seg1 = Length.createSegment(p1,p2);;

let dist2 = Length.distance seg1;;

module Make_Point(Type: sig type t end):Point_Type_Generic with type precision =Type.t = struct

  type precision= Type.t

  type point = {
    x:precision;
    y:precision;
    z:precision;
    }


  let createPoint(x1,y1,z1)={x=x1;y=y1;z=z1};;


end

module Make_Point_Int = Make_Point(struct type t=int end);;

let p = Make_Point_Int.createPoint(1,2,3);;

module type Segment_Functors = sig
  type t;; 
  type point ={x:t;y:t;z:t};; 
  val point1:point;; 
  val point2:point 

end;;

module type Segment_Result = sig
  type t

  type point ={x:t;y:t;z:t};; 



  type segment_type;;

  val segment:segment_type;;
  
end



module Make_Segment(SegmentArg:Segment_Functors):Segment_Result with type t=SegmentArg.t = struct

  type t=SegmentArg.t;;

  type point ={x:t;y:t;z:t};; 



  type segment_type = {
    point1: SegmentArg.point;
    point2: SegmentArg.point;
    }

  let segment = {point1=SegmentArg.point1;point2=SegmentArg.point2}

end

module type Translation_type = sig

  type t
  type translation_type={x:t;y:t;z:t}
   val translation:translation_type
  
end

module type Translation_Int_Type = sig

  type t=int
  type translation_type={x:t;y:t;z:t}
   val translation:translation_type
  
end

module type Translation_Float_Type = sig

  type t=float
  type translation_type={x:t;y:t;z:t}
   val translation:translation_type
  
end

module type Point_Functors_Int = sig
  type t;;
  type point ={x:int;y:int;z:int};; 
   val point1:point 
   val createPoint: int*int*int->point
end
 
module type Point_Functors_Float = sig
  type t;;
  type point ={x:float;y:float;z:float};; 
  
   val point1:point 

   val createPoint: float*float*float->point
end

module type Segment_Functors_Int = sig
  type t;; 
  type point ={x:int;y:int;z:int};; 
  type segment_type ={point1:point;point2:point};;
  val segment:segment_type
  val createSegment: point*point->segment_type;;
end
 
module type Segment_Functors_Float =sig 
  type t;; 
  type point ={x:float;y:float;z:float};; 
  type segment_type ={point1:point;point2:point};;
  val segment:segment_type
  val createSegment: point*point->segment_type;;

end



module Translate_Point_Int(Translate_Point_Arg: Point_Functors_Int)(TranslateInt:Translation_Int_Type):Point_Functors_Int = struct 

  type t = Translate_Point_Arg.t

  type point ={x:int;y:int;z:int}
  
  let point1={x=((TranslateInt.translation.x)+Translate_Point_Arg.point1.x);y=TranslateInt.translation.y+Translate_Point_Arg.point1.y;z=TranslateInt.translation.z+Translate_Point_Arg.point1.z}

  let createPoint (x1, y1, z1)= {x=(TranslateInt.translation.x+x1);y =(TranslateInt.translation.y+y1);z=(TranslateInt.translation.z+z1)}

end

module Translate_Point_Float(Translate_Point_Arg: Point_Functors_Float)(TranslateFloat:Translation_Float_Type):Point_Functors_Float = struct 

  type t = Translate_Point_Arg.t

  type point ={x:float;y:float;z:float}
  
  let point1={x=((TranslateFloat.translation.x)+.Translate_Point_Arg.point1.x);y=TranslateFloat.translation.y+.Translate_Point_Arg.point1.y;z=TranslateFloat.translation.z+.Translate_Point_Arg.point1.z}

  let createPoint (x1, y1, z1)= {x=(TranslateFloat.translation.x+.x1);y =(TranslateFloat.translation.y+.y1);z=(TranslateFloat.translation.z+.z1)}
end


module Translate_Segment_Int(Translate_Segment_Arg: Segment_Functors_Int)(TranslateInt:Translation_Int_Type):Segment_Functors_Int = struct 

  type t = Translate_Segment_Arg.t

  type point ={x:int;y:int;z:int}

  type segment_type ={point1:point;point2:point};;

  let segment:segment_type = {
    point1 = {
      x=Translate_Segment_Arg.segment.point1.x + TranslateInt.translation.x;
      y =Translate_Segment_Arg.segment.point1.y + TranslateInt.translation.y;
      z = Translate_Segment_Arg.segment.point1.z + TranslateInt.translation.z };
    point2= {
      x=Translate_Segment_Arg.segment.point2.x + TranslateInt.translation.x;
      y =Translate_Segment_Arg.segment.point2.y + TranslateInt.translation.y;
      z = Translate_Segment_Arg.segment.point2.z + TranslateInt.translation.z}
      };;

  let createSegment (point1New, point2New) = {
    point1 = {
      x=point1New.x + TranslateInt.translation.x;
      y =point1New.y + TranslateInt.translation.y;
      z = point1New.z + TranslateInt.translation.z };
    point2= {
      x=point2New.x + TranslateInt.translation.x;
      y =point2New.y + TranslateInt.translation.y;
      z =point2New.z + TranslateInt.translation.z}
      };;


end

module Translate_Segment_Float(Translate_Segment_Arg: Segment_Functors_Float)(TranslateFloat:Translation_Float_Type):Segment_Functors_Float = struct 

  type t = Translate_Segment_Arg.t;;

  type point ={x:float;y:float;z:float};;

  type segment_type ={point1:point;point2:point};;


  let segment:segment_type = {
     point1 = {x=(Translate_Segment_Arg.segment.point1.x +. TranslateFloat.translation.x);
              y =(Translate_Segment_Arg.segment.point1.y +. TranslateFloat.translation.y);
              z = (Translate_Segment_Arg.segment.point1.z +. TranslateFloat.translation.z)};
     point2= {x=(Translate_Segment_Arg.segment.point2.x +. TranslateFloat.translation.x);
              y =(Translate_Segment_Arg.segment.point2.y +. TranslateFloat.translation.y);
              z = (Translate_Segment_Arg.segment.point2.z +. TranslateFloat.translation.z)}
      };;

   let createSegment(point1New,point2New):segment_type= {
    point1 = {
      x=(point1New.x +. TranslateFloat.translation.x);
      y =(point1New.y +. TranslateFloat.translation.y);
      z = (point1New.z +. TranslateFloat.translation.z) };
    point2= {
      x=(point2New.x +. TranslateFloat.translation.x);
      y =(point2New.y +. TranslateFloat.translation.y);
      z =(point2New.z +. TranslateFloat.translation.z)};
   };;

end



(*
  (*ALTERNATYWNIE ,ale raczej gorzej*)  
  module Make_Point(PointArg:(sig type t;; val x:t;;val y:t;;val z:t;; end)) = struct

  type t=PointArg.t

  type point_type = {
    x:t; 
    y:t;
    z:t;
  }

  let point = {x=PointArg.x;y=PointArg.y;z=PointArg.z}


end

module Make_Segment(SegmentArg:(sig type t;; type point ={x:t;y:t;z:t};; val point1:point;; val point2:point end)) = struct

  type t=SegmentArg.t


  type segment_type = {
    point1: SegmentArg.point;
    point2: SegmentArg.point;
    }

  let segment = {point1=SegmentArg.point1;point2=SegmentArg.point2}

end

module type Translation_type = sig

  type t
  type translation_type={x:t;y:t;z:t}
   val translation:translation_type
  
end

module TranslateIntOne= struct

  type t=int
  type translation_type={x:int;y:int;z:int}
  let translation= {x=1;y=1;z=1}
  
end

module TranslateFloatOne= struct

  type t=float
  type translation_type={x:float;y:float;z:float}
  let translation= {x=1.0;y=1.0;z=1.0}
  
end



module Translate_Point_Int(Translate_Point_Arg: (sig type t;; type point ={x:int;y:int;z:int};; val point1:point end)) = struct 

  type t = Translate_Point_Arg.t

  type point ={x:int;y:int;z:int}
  
  let point1={x=((TranslateIntOne.translation.x)+Translate_Point_Arg.point1.x);y=TranslateIntOne.translation.y+Translate_Point_Arg.point1.y;z=TranslateIntOne.translation.z+Translate_Point_Arg.point1.z}

end

module Translate_Point_Float(Translate_Point_Arg: (sig type t;; type point ={x:float;y:float;z:float};; val point1:point end)) = struct 

  type t = Translate_Point_Arg.t

  type point ={x:float;y:float;z:float}
  
  let point1={x=((TranslateFloatOne.translation.x)+.Translate_Point_Arg.point1.x);y=TranslateFloatOne.translation.y+.Translate_Point_Arg.point1.y;z=TranslateFloatOne.translation.z+.Translate_Point_Arg.point1.z}

end


module Translate_Segment_Int(Translate_Segment_Arg: (sig type t;; type point ={x:int;y:int;z:int};; type segment_type ={point1:point;point2:point};;val segment:segment_type end)) = struct 

  type t = Translate_Segment_Arg.t

  type point ={x:int;y:int;z:int}

  let segment:Translate_Segment_Arg.segment_type = {point1 = {x=Translate_Segment_Arg.segment.point1.x + TranslateIntOne.translation.x;y =Translate_Segment_Arg.segment.point1.y + TranslateIntOne.translation.y;z = Translate_Segment_Arg.segment.point1.z + TranslateIntOne.translation.z };
  point2= {x=Translate_Segment_Arg.segment.point2.x + TranslateIntOne.translation.x;y =Translate_Segment_Arg.segment.point2.y + TranslateIntOne.translation.y;z = Translate_Segment_Arg.segment.point2.z + TranslateIntOne.translation.z}}

end

module Translate_Segment_Float(Translate_Segment_Arg: (sig type t;; type point ={x:float;y:float;z:float};; type segment_type ={point1:point;point2:point};;val segment:segment_type end)) = struct 

  type t = Translate_Segment_Arg.t

  type point ={x:int;y:int;z:int}

  let segment:Translate_Segment_Arg.segment_type = {
    point1 = {x=Translate_Segment_Arg.segment.point1.x +. TranslateFloatOne.translation.x;
              y =Translate_Segment_Arg.segment.point1.y +. TranslateFloatOne.translation.y;
              z = Translate_Segment_Arg.segment.point1.z +. TranslateFloatOne.translation.z };
     point2= {x=Translate_Segment_Arg.segment.point2.x +. TranslateFloatOne.translation.x;
              y =Translate_Segment_Arg.segment.point2.y +. TranslateFloatOne.translation.y;
              z = Translate_Segment_Arg.segment.point2.z +. TranslateFloatOne.translation.z}};

end 


*)
