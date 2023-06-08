open Vec3
open Ray
open Moonpool

let ( let@ ) = ( @@ )
let fpf = Printf.fprintf
let pf = Printf.printf

type material =
  | Lambertian of Vec3.vec3 (* albedo *)
  | Metal of Vec3.vec3 * float (* albedo, fuzz *)
  | Dielectric of float (* refractive index *)

type sphere = {
  center: Vec3.vec3;
  radius: float;
  mat: material;
}

type hitable =
  | Sphere of sphere
  | World of hitable list

type hit_rec = {
  t: float;
  p: Vec3.vec3;
  normal: Vec3.vec3;
  mat: material option;
}

type scatter = {
  ray: Ray.ray;
  color: Vec3.vec3;
  scatter: bool;
}

type hit = hit_rec option

(* Produce a random point inside the unit sphere. Works by picking a
   random point in the unit cube, rejecting if not inside the sphere. *)
let rec random_in_unit_sphere () =
  let p =
    Vec3.sub
      (Vec3.mul 2.0
         (Vec3.of_floats (Random.float 1.0, Random.float 1.0, Random.float 1.0)))
      (Vec3.of_floats (1., 1., 1.))
  in
  if Vec3.dot p p >= 1.0 then
    p
  else
    random_in_unit_sphere ()

let reflect v n = Vec3.sub v (Vec3.mul (2. *. Vec3.dot v n) n)

let refract v n ni_over_nt =
  let uv = Vec3.unit_vector v in
  let dt = Vec3.dot uv n in
  let discriminant = 1.0 -. (ni_over_nt *. ni_over_nt *. (1.0 -. (dt *. dt))) in
  if discriminant > 0.0 then (
    let refracted =
      Vec3.sub
        (Vec3.mul ni_over_nt (Vec3.sub v (Vec3.mul dt n)))
        (Vec3.mul (sqrt discriminant) n)
    in
    Some refracted
  ) else
    None

let hit_scatter r_in hit_rec =
  match hit_rec.mat with
  (* reflect in random direction *)
  | Some (Lambertian albedo) ->
    let target =
      Vec3.add (Vec3.add hit_rec.p hit_rec.normal) (random_in_unit_sphere ())
    in
    let scatter =
      {
        ray = Ray.create hit_rec.p (Vec3.sub target hit_rec.p);
        color = albedo;
        scatter = true;
      }
    in
    scatter
  (* "shiny"- angle of reflectance = angle of incidence  *)
  | Some (Metal (albedo, fuzz)) ->
    let reflected = reflect (Vec3.unit_vector r_in.dir) hit_rec.normal in
    let scattered_ray =
      Ray.create hit_rec.p
        (Vec3.add reflected (Vec3.mul fuzz (random_in_unit_sphere ())))
    in
    let scattered =
      {
        ray = scattered_ray;
        color = albedo;
        scatter = Vec3.dot scattered_ray.dir hit_rec.normal > 0.0;
      }
    in
    scattered
  | Some (Dielectric ref_idx) ->
    let reflected = reflect (Vec3.unit_vector r_in.dir) hit_rec.normal in
    let attenuation = Vec3.of_floats (1.0, 1.0, 1.0) in
    let outward_normal, ni_over_nt =
      if Vec3.dot r_in.dir hit_rec.normal > 0.0 then
        Vec3.neg hit_rec.normal, ref_idx
      else
        hit_rec.normal, 1.0 /. ref_idx
    in
    let scattered_ray =
      match refract r_in.dir outward_normal ni_over_nt with
      | Some refracted -> Ray.create hit_rec.p refracted
      | None -> Ray.create hit_rec.p reflected
    in
    let scattered =
      { ray = scattered_ray; color = attenuation; scatter = false }
    in
    scattered
  | None -> failwith "not a real material type"

let hit_sphere sphere ray (tmin, tmax) : hit =
  let oc = sub ray.origin sphere.center in
  let a = dot ray.dir ray.dir in
  let b = dot oc ray.dir in
  let c = dot oc oc -. (sphere.radius *. sphere.radius) in
  let discriminant = (b *. b) -. (a *. c) in

  if discriminant > 0.0 then (
    let t = (-.b -. sqrt discriminant) /. a in

    if t < tmax && t > tmin then (
      let p = Ray.point_at_parameter ray t in
      Some
        {
          t;
          p;
          normal = mul (1. /. sphere.radius) (sub p sphere.center);
          mat = Some sphere.mat;
        }
    ) else (
      let t = (-.b +. sqrt discriminant) /. a in
      if t < tmax && t > tmin then (
        let p = Ray.point_at_parameter ray t in
        Some
          {
            t;
            p;
            normal = mul (1. /. sphere.radius) (sub p sphere.center);
            mat = Some sphere.mat;
          }
      ) else
        None
    )
  ) else
    None

let rec hit_world (world : hitable list) ray (tmin, tmax) : hit =
  List.fold_left
    (fun acc h ->
      let prev_rec =
        match acc with
        | None ->
          {
            t = tmax;
            p = Vec3.of_floats (-1., -1., -1.);
            normal = Vec3.of_floats (-1., -1., -1.);
            mat = None;
          }
        | Some r -> r
      in
      match hit h ray (tmin, prev_rec.t) with
      | Some r -> Some r
      | None -> acc)
    None world

and hit h ray (tmin, tmax) : hit =
  match h with
  | Sphere s -> hit_sphere s ray (tmin, tmax)
  | World w -> hit_world w ray (tmin, tmax)

let rec get_color world ray depth : vec3 =
  match hit world ray (0., Float.infinity) with
  | Some hit_result ->
    if depth < 50 then (
      let s = hit_scatter ray hit_result in
      Vec3.pmul s.color (get_color world s.ray (depth + 1))
    ) else
      Vec3.of_floats (0., 0., 0.)
  | None ->
    let unit_direction = unit_vector ray.dir in
    let t = 0.5 *. (unit_direction.y +. 1.0) in
    add
      (mul (1.0 -. t) { x = 1.0; y = 1.0; z = 1.0 })
      (mul t { x = 0.5; y = 0.7; z = 1.0 })

let mk_world () =
  let sphere1 =
    Sphere
      {
        center = Vec3.of_floats (0., 0., -1.);
        radius = 0.5;
        mat = Lambertian (Vec3.of_floats (0.8, 0.3, 0.3));
      }
  in
  let sphere2 =
    Sphere
      {
        center = Vec3.of_floats (0., -100.5, -1.);
        radius = 100.0;
        mat = Lambertian (Vec3.of_floats (0.8, 0.8, 0.0));
      }
  in
  let sphere3 =
    Sphere
      {
        center = Vec3.of_floats (-1.0, 0., -1.);
        radius = 0.5;
        mat = Metal (Vec3.of_floats (0.8, 0.6, 0.2), 0.4);
      }
  in
  (* let sphere4 = Sphere {center = Vec3.of_floats (1.0, 0., -1.); *)
  (*                       radius = 0.5; *)
  (*                       mat = Metal ((Vec3.of_floats (0.8, 0.8, 0.8)), 0.1)} in *)
  let sphere4 =
    Sphere
      {
        center = Vec3.of_floats (1.0, 0.0, -1.);
        radius = 0.5;
        mat = Dielectric 1.5;
      }
  in
  World [ sphere3; sphere2; sphere1; sphere4 ]

type config = {
  nx: int;
  ny: int;
  ns: int;
  j: int;
  out: string;
}

let run (config : config) =
  Random.self_init ();
  let world = mk_world () in
  let nx = 400 in
  let ny = 200 in
  let ns = 150 in
  (* samples per pixel *)
  let oc = open_out config.out in
  let@ () =
    Fun.protect ~finally:(fun () ->
        flush oc;
        close_out oc)
  in
  fpf oc "P3\n";
  fpf oc "%d\n" nx;
  fpf oc "%d\n" ny;
  fpf oc "\n255\n";
  let lower_left_corner = Vec3.of_floats (-2., -1., -1.) in
  let horizontal = Vec3.of_floats (4., 0., 0.) in
  let vertical = Vec3.of_floats (0., 2., 0.) in
  let origin = Vec3.of_floats (0., 0., 0.) in

  for j = ny downto 1 do
    for i = 0 to nx - 1 do
      let color = ref { x = 0.; y = 0.; z = 0. } in
      for _step = 0 to ns - 1 do
        (* NOTE: Random.float is bounds __inclusive__ *)
        let u = (Float.of_int i +. Random.float 1.0) /. Float.of_int nx in
        let v = (Float.of_int j +. Random.float 1.0) /. Float.of_int ny in

        let r =
          {
            origin;
            dir =
              Vec3.add lower_left_corner
                (Vec3.add (Vec3.mul u horizontal) (Vec3.mul v vertical));
          }
        in
        color := Vec3.add !color (get_color world r 0)
      done;

      color := Vec3.mul (1. /. Float.of_int ns) !color;
      (* gamma correction *)
      color := Vec3.of_floats (sqrt !color.x, sqrt !color.y, sqrt !color.z);
      let { x = r; y = g; z = b } = !color in
      let ir, ig, ib =
        ( Int.of_float (r *. 255.99),
          Int.of_float (g *. 255.99),
          Int.of_float (b *. 255.99) )
      in
      fpf oc "%d " ir;
      fpf oc "%d " ig;
      fpf oc "%d \n" ib
    done
  done

let () =
  let nx = ref 400 in
  let ny = ref 200 in
  let ns = ref 150 in
  let j = ref 4 in
  let out = ref "out.ppm" in
  let progress = ref false in
  let opts =
    [
      "-j", Arg.Set_int j, " set minimum number of threads";
      "-nx", Arg.Set_int nx, " pixels in x axis";
      "-ny", Arg.Set_int ny, " pixels in y axis";
      "-ns", Arg.Set_int ns, " number of samples per pixel";
      "-o", Arg.Set_string out, " output file";
      "-p", Arg.Set progress, " progress bar";
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "";

  let config = { nx = !nx; ny = !ny; ns = !ns; out = !out; j = !j } in

  let t = Unix.gettimeofday () in
  run config;
  let elapsed = Unix.gettimeofday () -. t in
  pf "done in %.4fs\n%!" elapsed;

  ()
