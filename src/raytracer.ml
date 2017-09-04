open Core
open Printf
open Vec3
open Ray

type material = Lambertian of Vec3.vec3 (* matte surface *)
              | Metal of Vec3.vec3
              | DummyNone       (* TODO: use option type instead *)

type sphere = { center: Vec3.vec3;
                radius: float;
                mat: material;
              }

type hitable = Sphere of sphere
             | World of hitable list


type hit_rec = { t : float;
                 p: Vec3.vec3;
                 normal: Vec3.vec3;
                 mat: material; }

type scatter = { ray : Ray.ray;
                 color: Vec3.vec3;
                 scatter: bool;}

type hit = hit_rec option

(* Produce a random point inside the unit sphere. Works by picking a
   random point in the unit cube, rejecting if not inside the sphere. *)
let rec random_in_unit_sphere () =
  let p = (Vec3.sub (Vec3.mul 2.0 (Vec3.of_floats ((Random.float 1.0),
                                                   (Random.float 1.0),
                                                   (Random.float 1.0))))
             (Vec3.of_floats (1., 1., 1.))) in
  if ((Vec3.dot p p) >= 1.0)
  then p
  else random_in_unit_sphere ()

let reflect v n =
  Vec3.sub v (Vec3.mul (2. *. (Vec3.dot v n)) n)

let hit_scatter rin hit_rec =
  match hit_rec.mat with
    (* reflect in random direction *)
    Lambertian(albedo) ->
    let target = (Vec3.add (Vec3.add hit_rec.p hit_rec.normal) (random_in_unit_sphere ())) in
    let scatter = { ray =  Ray.create hit_rec.p (Vec3.sub target hit_rec.p);
                    color =  albedo;
                    scatter = true;}
    in scatter
  (* "shiny"- angle of reflectance = angle of incidence  *)
  | Metal(albedo) ->
    let reflected = reflect (Vec3.unit_vector rin.dir) hit_rec.normal in
    let scattered_ray = Ray.create hit_rec.p reflected in
    let scattered = { ray = scattered_ray;
                      color = albedo;
                      scatter = (Vec3.dot scattered_ray.dir hit_rec.normal) > 0.0;} in
    scattered
  | DummyNone -> failwith "not a real material type"

let hit_sphere sphere ray (tmin, tmax) =
  let oc = sub ray.origin sphere.center in
  let a = dot ray.dir ray.dir in
  let b = (dot oc ray.dir) in
  let c = (dot oc oc) -. (sphere.radius *. sphere.radius) in
  let discriminant = b*.b -. a*.c in

  if (discriminant > 0.0)
  then
    let t = (-.b -. (sqrt discriminant)) /. a in

    if (t < tmax && t > tmin)
    then 
      let p = Ray.point_at_parameter ray t in
      Some { t = t;
             p = p;
             normal = mul (1. /. sphere.radius) (sub p sphere.center);
             mat = sphere.mat
           }
    else
      let t = (-.b +. (sqrt discriminant)) /. a in
      if (t < tmax && t > tmin)
      then 
        let p = Ray.point_at_parameter ray t in
        Some { t = t;
               p = p;
               normal = mul (1. /. sphere.radius) (sub p sphere.center);
               mat = sphere.mat;
             }
      else None
  else None

let rec hit_world world ray (tmin, tmax) =
  List.fold world
    ~init: None
    ~f: (fun acc h ->
        let prev_rec = match acc with
            None -> { t = tmax;
                      p = Vec3.of_floats (-1., -1., -1.);
                      normal = Vec3.of_floats (-1., -1., -1.);
                      mat = DummyNone}
          | Some(r) -> r in
        match (hit h ray (tmin, prev_rec.t)) with
          Some(r) -> Some r
        | None -> acc)

and hit h ray (tmin, tmax) =
  match h with
    Sphere(s) -> hit_sphere s ray (tmin, tmax)
  | World(w) -> hit_world w ray (tmin, tmax)


let rec get_color world ray depth =
  match (hit world ray (0., Float.infinity)) with
    Some hit_result ->
    if (depth < 50)
    then let s = hit_scatter ray hit_result in
      Vec3.pmul s.color (get_color world s.ray (depth+1))
    else Vec3.of_floats (0., 0., 0.)
  | None ->
    let unit_direction = unit_vector ray.dir in
    let t = 0.5 *. (unit_direction.y +. 1.0) in
    add (mul (1.0 -. t) {x= 1.0; y=1.0; z= 1.0}) (mul t {x= 0.5; y= 0.7; z= 1.0})


let write_to_file filename =
  Random.self_init ();

  let sphere1 = Sphere {center = Vec3.of_floats (0., 0., -1.);
                        radius = 0.5; 
                        mat = Lambertian (Vec3.of_floats (0.8, 0.3, 0.3)) } in
  let sphere2 = Sphere {center = Vec3.of_floats (0., -100.5, -1.);
                        radius = 100.0;
                        mat = Lambertian (Vec3.of_floats (0.8, 0.8, 0.0))} in
  let sphere3 = Sphere {center = Vec3.of_floats (-1.0, 0., -1.);
                        radius = 0.5;
                        mat = Metal (Vec3.of_floats (0.8, 0.6, 0.2))} in
  let sphere4 = Sphere {center = Vec3.of_floats (1.0, 0., -1.);
                        radius = 0.5;
                        mat = Metal (Vec3.of_floats (0.8, 0.8, 0.8))} in
  let world = World [sphere3; sphere2; sphere1; sphere4] in
  
  let nx = 400 in
  let ny = 200 in
  let ns = 60 in
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
        color := Vec3.add !color (get_color world r 0);

      done;

      color := Vec3.mul (1. /. (Float.of_int ns)) !color ;
      (* gamma correction *)
      color := Vec3.of_floats (sqrt(!color.x),
                               sqrt(!color.y),
                               sqrt(!color.z));
      let {x=r; y=g; z=b} = !color in
      let (ir, ig, ib) = (Int.of_float (r*.255.99),
                          Int.of_float (g*.255.99),
                          Int.of_float (b*.255.99)) in
      fprintf oc "%d " ir;
      fprintf oc "%d " ig;
      fprintf oc "%d \n" ib;
    done;
  done;
  Out_channel.close oc

let () =
  write_to_file "out.ppm"

