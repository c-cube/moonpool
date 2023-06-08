type ray = {
  origin: Vec3.vec3;
  dir: Vec3.vec3;
}

let point_at_parameter r t = Vec3.add r.origin (Vec3.mul t r.dir)
let create o d = { origin = o; dir = d }
