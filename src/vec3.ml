type vec3 = { x: float;
              y: float;
              z: float }

let of_floats (e1, e2, e3) =
  {x = e1; y = e2; z = e3}

let add v w =
  {x = v.x +. w.x; 
   y = v.y +. w.y; 
   z = v.z +. w.z}

let sub v w =
  {x = v.x -. w.x;
   y = v.y -. w.y; 
   z = v.z -. w.z}

let dot v w =
  v.x*.w.x +. v.y*.w.y +.v.z*.w.z

let cross v w =
  {x = v.y*.w.z -. v.z*.w.y;
   y = v.z*.w.x -. v.x*.w.z;
   z = v.x*.w.y -. v.y*.w.x}

let length v =
  sqrt (dot v v)

let unit_vector v =
  let l = length v in
  { x = v.x /. l;
    y = v.y /. l;
    z = v.z /. l }

let mul t v =
  { x = t *. v.x;
    y = t *. v.y;
    z = t *. v.z }

(* pairwise multiplication *)
let pmul v w =
  { x = v.x *. w.x;
    y = v.y *. w.y;
    z = v.z *. w.z; }
