open Core.Std
open Printf
open Vec3
open Ray

type sphere = { center: Vec3.vec3;
                radius: float }

type hitable = Sphere of sphere
             | World of hitable list

type hit = { t : float;
             p: Vec3.vec3;
             normal: Vec3.vec3;
             hit: bool }

let hit_sphere sphere ray (tmin, tmax) =
  let oc = sub ray.origin sphere.center in
  let a = dot ray.dir ray.dir in
  let b = (dot oc ray.dir) in
  let c = (dot oc oc) -. (sphere.radius *. sphere.radius) in
  let discriminant = b*.b -. a*.c in

  let no_hit = { t = -1.0;
                 p = Vec3.of_floats (Float.infinity, Float.infinity, Float.infinity);
                 normal = Vec3.of_floats (0., 0., 0.);
                 hit = false } in

  if (discriminant > 0.0)
  then
    let t = (-.b +. (sqrt discriminant)) /. a in

    if (t < tmax && t > tmin)
    then 
      let p = Ray.point_at_parameter ray t in
      { t = t;
        p = p;
        normal = unit_vector (sub p sphere.center);
        hit = true }
    else
      let t = (-.b -. (sqrt discriminant)) /. a in
      if (t < tmax && t > tmin)
      then 
        let p = Ray.point_at_parameter ray t in
        { t = t;
          p = p;
          normal = unit_vector (sub p sphere.center);
          hit = true }
      else no_hit
  else no_hit

let rec hit_world world ray (tmin, tmax) =
  let init_rec = { t = tmax;
                   p = Vec3.of_floats (-1., -1., -1.);
                   normal = Vec3.of_floats (-1., -1., -1.);
                   hit = false; } in
  List.fold world
    ~init: init_rec
    ~f: (fun acc h -> 
        let hit_rec = (hit h ray (tmin, acc.t)) in
        if (hit_rec.hit) 
        then hit_rec
        else acc)
and hit h ray (tmin, tmax) =
  match h with
    Sphere s -> hit_sphere s ray (tmin, tmax)
  | World w -> hit_world w ray (tmin, tmax)


let get_color ray =
  let sphere1 = Sphere {center = Vec3.of_floats (0., 0., -1.);
                        radius = 0.5 } in
  let sphere2 = Sphere {center = Vec3.of_floats (0., -100.5, -1.);
                        radius = 100.0 } in
  let world = World [sphere1; sphere2] in
  let hit_result = (hit world ray (0., Float.infinity)) in
  let t = hit_result.t in
  if (hit_result.hit && (t > 0.0))
  then let n = hit_result.normal in
    mul 0.5 (Vec3.of_floats (n.x +. 1., n.y +. 1., n.z +. 1.))
  else let unit_direction = unit_vector ray.dir in
    let t = 0.5 *. (unit_direction.y +. 1.0) in
    add (mul (1.0 -. t) {x= 1.0; y=1.0; z= 1.0}) (mul t {x= 0.5; y= 0.7; z= 1.0})


let write_to_file filename =
  let nx = 200 in
  let ny = 100 in
  let oc = Out_channel.create filename in
  fprintf oc "P3\n";
  fprintf oc "%d\n" nx;
  fprintf oc "%d\n"  ny;
  fprintf oc "\n255\n";
  let lower_left_corner = Vec3.of_floats (-2., -1., -1.) in
  let horizontal = Vec3.of_floats (4., 0., 0.) in
  let vertical = Vec3.of_floats (0., 2., 0.) in
  let origin = Vec3.of_floats (0., 0., 0.) in
  let color = ref {x=0.; y=0.; z=0.} in
  for j = ny downto 1 do
    for i = 0 to nx-1 do
      let u = (Float.of_int i) /. (Float.of_int nx) in
      let v = (Float.of_int j) /. (Float.of_int ny) in

      let r = { origin = origin;
                dir = Vec3.add lower_left_corner (Vec3.add (Vec3.mul u horizontal) (Vec3.mul v vertical)) } in
      color := get_color r;

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


let main() =
  let nx = 200 in
  let ny = 100 in
  print_string "P3\n";
  printf "%d\n" nx;
  printf "%d\n"  ny;
  print_string "\n255\n";

  let lower_left_corner = Vec3.of_floats (-2., -1., -1.) in
  let horizontal = Vec3.of_floats (4., 0., 0.) in
  let vertical = Vec3.of_floats (0., 2., 0.) in
  let origin = Vec3.of_floats (0., 0., 0.) in
  let color = ref {x=0.; y=0.; z=0.} in
  for j = ny downto 1 do
    for i = 0 to nx-1 do
      let u = (Float.of_int i) /. (Float.of_int nx) in
      let v = (Float.of_int j) /. (Float.of_int ny) in

      let r = { origin = origin;
                dir = Vec3.add lower_left_corner (Vec3.add (Vec3.mul u horizontal) (Vec3.mul v vertical)) } in
      color := get_color r;

      let {x=r; y=g; z=b} = !color in
      let (ir, ig, ib) = (Int.of_float (r*.255.99),
                          Int.of_float (g*.255.99),
                          Int.of_float (b*.255.99)) in
      printf "%d " ir;
      printf "%d " ig;
      printf "%d " ib;
      print_newline();
    done
  done
;;

(* main() *)
