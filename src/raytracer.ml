open Core.Std
open Printf
open Vec3
    
type sphere = { center: Vec3.vec3;
                radius: float }
type ray = { origin: Vec3.vec3;
             dir: Vec3.vec3 }

let point_at_parameter r t =
  add r.origin (mul t r.dir)

let hit_sphere sphere ray =
  let oc = sub ray.origin sphere.center in
  let a = dot ray.dir ray.dir in
  let b = (dot oc ray.dir) in
  let c = (dot oc oc) -. (sphere.radius *. sphere.radius) in
  let discriminant = b*.b -. a*.c in

  if (discriminant > 0.0)
  then (-.b +. (sqrt discriminant)) /. a
  else -1.0

let get_color ray =
  let sphere = {center = Vec3.of_floats (0., 0., -1.);
                radius = 0.5 } in
  let t = (hit_sphere sphere ray) in
  if (t > 0.0)
  then let n = unit_vector (sub (point_at_parameter ray t) sphere.center) in
    mul 0.5 (Vec3.of_floats (n.x +. 1., n.y +. 1., n.z +. 1.))
  else let unit_direction = unit_vector ray.dir in
    let t = 0.5 *. (unit_direction.y +. 1.0) in
    add (mul (1.0 -. t) {x= 1.0; y=1.0; z= 1.0}) (mul t {x= 0.5; y= 0.7; z= 1.0})
          

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

main()
