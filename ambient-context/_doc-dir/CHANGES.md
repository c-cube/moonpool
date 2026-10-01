
# 0.2.0

**Breaking changes:**

- Simplified API with renamed functions:
  - `create_key()` → `new_key()`
  - `with_binding` → `with_key_bound_to`
  - `without_binding` → `with_key_unbound`
  - `set_storage_provider` → `set_current_storage`

- Storage type changed from first-class module to record:
  - Old: `type storage = (module STORAGE)`
  - New: `Storage.t` record type

- Key type simplified:
  - Old: `type 'a key = int * 'a Hmap.key`
  - New: `type 'a key = 'a Hmap.key`

**Migration guide:**

```ocaml
(* Old code *)
let k = Ctx.create_key () in
Ctx.set_storage_provider (Ambient_context_lwt.storage ()) ;
Ctx.with_binding k "value" @@ fun () -> ...

(* New code *)
let k = Ctx.new_key () in
Ctx.set_current_storage Ambient_context_lwt.storage ;
Ctx.with_key_bound_to k "value" @@ fun () -> ...
```

# 0.1.1

re-release after github broke the release archive, this time using dune-release to avoid it in the future
