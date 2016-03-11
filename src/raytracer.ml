open Core.Std
open Printf
open Vec3
open Ray

type sphere = { center: Vec3.vec3;
                radius: float }

type hitable = Sphere of sphere
             | World of hitable list

type hit_rec = { t : float;
                 p: Vec3.vec3;
                 normal: Vec3.vec3; }

type hit = hit_rec option


let hit_sphere sphere ray (tmin, tmax) =
  let oc = sub ray.origin sphere.center in
  let a = dot ray.dir ray.dir in
  let b = (dot oc ray.dir) in
  let c = (dot oc oc) -. (sphere.radius *. sphere.radius) in
  let discriminant = b*.b -. a*.c in

  if (discriminant > 0.0)
  then
    let t = (-.b +. (sqrt discriminant)) /. a in

    if (t < tmax && t > tmin)
    then 
      let p = Ray.point_at_parameter ray t in
      Some { t = t;
             p = p;
             normal = unit_vector (sub p sphere.center); }
    else
      let t = (-.b -. (sqrt discriminant)) /. a in
      if (t < tmax && t > tmin)
      then 
        let p = Ray.point_at_parameter ray t in
        Some { t = t;
               p = p;
               normal = unit_vector (sub p sphere.center); }
      else None
  else None

let rec hit_world world ray (tmin, tmax) =
  List.fold world
    ~init: None
    ~f: (fun acc h ->
        let prev_rec = match acc with
            None -> { t = tmax;
                      p = Vec3.of_floats (-1., -1., -1.);
                      normal = Vec3.of_floats (-1., -1., -1.); }
          | Some r -> r in
        match (hit h ray (tmin, prev_rec.t)) with
          Some r -> Some r
        | None -> acc)

and hit h ray (tmin, tmax) =
  match h with
    Sphere s -> hit_sphere s ray (tmin, tmax)
  | World w -> hit_world w ray (tmin, tmax)


let get_color world ray =
  match (hit world ray (0., Float.infinity)) with
  Some hit_result ->
    let t = hit_result.t in
    if (t > 0.0)
    then let n = hit_result.normal in
      mul 0.5 (Vec3.of_floats (n.x +. 1., n.y +. 1., n.z +. 1.))
    else let unit_direction = unit_vector ray.dir in
      let t = 0.5 *. (unit_direction.y +. 1.0) in
      add (mul (1.0 -. t) {x= 1.0; y=1.0; z= 1.0}) (mul t {x= 0.5; y= 0.7; z= 1.0})
  | None ->
    let unit_direction = unit_vector ray.dir in
    let t = 0.5 *. (unit_direction.y +. 1.0) in
    add (mul (1.0 -. t) {x= 1.0; y=1.0; z= 1.0}) (mul t {x= 0.5; y= 0.7; z= 1.0})


let write_to_file filename =
  Random.self_init ();

  let sphere1 = Sphere {center = Vec3.of_floats (0., 0., -1.);
                        radius = 0.5 } in
  let sphere2 = Sphere {center = Vec3.of_floats (0., -100.5, -1.);
                        radius = 100.0 } in
  let sphere3 = Sphere {center = Vec3.of_floats (-1.0, -0.75, -2.);
                        radius = 0.25 } in
  let world = World [sphere2; sphere1; sphere3] in
  
  let nx = 200 in
  let ny = 100 in
  let ns = 100 in
  let oc = Out_channel.create filename in
  fprintf oc "P3\n";
  fprintf oc "%d\n" nx;
  fprintf oc "%d\n"  ny;
  fprintf oc "\n255\n";
  let lower_left_corner = Vec3.of_floats (-2., -1., -1.) in
  let horizontal = Vec3.of_floats (4., 0., 0.) in
  let vertical = Vec3.of_floats (0., 2., 0.) in
  let origin = Vec3.of_floats (0., 0., 0.) in

  for j = ny downto 1 do
    for i = 0 to nx-1 do
      let color = ref {x=0.; y=0.; z=0.} in
      for s = 0 to ns-1 do
        (* NOTE: Random.float is bounds __inclusive__ *)
        let u = (Float.of_int i +. (Random.float 1.0)) /. (Float.of_int nx) in
        let v = (Float.of_int j +. (Random.float 1.0)) /. (Float.of_int ny) in

        let r = { origin = origin;
                  dir = Vec3.add lower_left_corner (Vec3.add (Vec3.mul u horizontal) (Vec3.mul v vertical)) } in
        color := Vec3.add !color (get_color world r);

      done;

      color := Vec3.mul (1. /. (Float.of_int ns)) !color ;
      let {x=r; y=g; z=b} = !color in
      let (ir, ig, ib) = (Int.of_float (r*.255.99),
                          Int.of_float (g*.255.99),
                          Int.of_float (b*.255.99)) in
      fprintf oc "%d " ir;
      fprintf oc "%d " ig;
      fprintf oc "%d \n" ib;
    done
  done;
  Out_channel.close oc;
;;
