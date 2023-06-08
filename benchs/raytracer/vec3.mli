type vec3 = {
  x: float;
  y: float;
  z: float;
}

val of_floats : float * float * float -> vec3
val add : vec3 -> vec3 -> vec3
val sub : vec3 -> vec3 -> vec3
val neg : vec3 -> vec3
val dot : vec3 -> vec3 -> float
val cross : vec3 -> vec3 -> vec3
val mul : float -> vec3 -> vec3
val unit_vector : vec3 -> vec3
val pmul : vec3 -> vec3 -> vec3
