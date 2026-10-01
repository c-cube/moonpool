ocaml-ambient-context
=====================

This OCaml module provides a API that is type- and dependency-abstracted across thread-local storage, Lwt's [sequence-associated storage](https://github.com/ocsigen/lwt/blob/cc05e2bda6c34126a3fd8d150ee7cddb3b8a440b/src/core/lwt.ml#L727-L751 "Internal documentation for Lwt's SAS mechanism"), and Eio's [fiber-local storage](https://github.com/ocaml-multicore/eio/pull/256 "ocaml-multicore/eio#256, adding a fibre-local storage API") — all different approaches to "throwing" information "across the stack" at runtime, without modifying interleaving methods/components/dependencies.

As a library author, depending on `ambient-context` allows you to

1. abstract some sort of application-provided information into something like "thread-local storage",
2. while still being compatible with dependants who are calling into you from an asynchronous context (like Lwt or Eio),
3. *without* functorizing your interface over (or even depending on!) specifically Lwt or Eio, or preventing non-Lwt/Eio users from consuming your API.

Simply put, `ambient-context` allows you to communicate with your dependants in situations where you cannot control intermediate dependencies, and cannot modify the API of your own library to accept a new parameter.

> [!WARNING]
> Ambient context like this — effectively, implicit global state — is usually frowned upon for most uses, and with good reason. This module is *intended* to be used as a last resort, and exclusively for debugging, tracing, and reporting purposes, if at all possible.

Installation and usage ...
--------------------------

The intended usage of this library is in two collaborating components:

1. that of a "deep in the dependency-tree" library (e.g. `foo-deep-lib`),
2. and a top-of-the-dependency-tree (e.g. `widget-app`).

The former needs to be able to obtain information from the latter, *without* changing the API presented to intermediate dependencies (e.g. `bar-intermediary-lib`) — and equivalently, without changing the function-signatures of intermediate wrappers/callers.

<a name="as-a-top-level-application"></a>

### ... as a top-level application

If a library you depend on (let's pretend it's `foo-deep-lib`) uses `ambient-context`, they're effectively deferring an important decision about _how_ their library communicates with you.

_This means you must to choose, and configure, a storage-mechanism relevant to the callsite(s) in your own application._

Your choice will vary depending on *from where*, in your own code, you're calling into a library that uses `ambient-context` — that is, whether an asynchronous event-loop (such as Lwt or Eio) exists 'above' your calls on the stack. Having determined whether you'll be calling your dependancy (e.g. `foo-deep-lib`) from such an asynchronous context , you'll then need to install the relevant storage-provider at runtime with an appropriately-placed call to `Ambient_context.set_current_storage`.

> **Example:** if you're writing an Lwt-enabled application, and you'll be calling `bar-intermediary-lib` below the `Lwt_main.run` event-loop on the stack, you'll need to install the `ambient-context-lwt` "storage provider" ...
>
> ```diff
>  ; dune-project
>   (depends
>    (ocaml
>     (>= 4.08))
> +  ambient-context.unix
> +  ambient-context-lwt
>    bar-intermediary-lib
>    (alcotest :with-test)
>    (ocaml-lsp-server :with-dev-setup)
> ```
>
> ```diff
>  ; src/dune
>  (executable
>   (name widget_app)
> - (libraries bar-intermediary-lib))
> + (libraries ambient-context.unix ambient-context-lwt bar-intermediary-lib))
> ```
>
> ... and at runtime, your application will need to dictate the relevant storage backend (TLS, Lwt, or Eio) for a delineated section of the stack — usually, this involves wrapping the invocation of your asynchronous thread-scheduler's runloop — in this example, `Lwt_main.run`:
>
> ```ocaml
> (* src/widget_app.ml *)
> module Ctx = Ambient_context
>
> let () =
>    let sock = create_socket () in
>    let serve = create_server sock in
>
>    (* add this line before [Lwt_main.run]: *)
>    Ctx.set_current_storage Ambient_context_lwt.storage ;
>    Lwt_main.run @@ serve ()
> ```

Once your application has configured the appropriate runtime context-storage, you'll presumably need to actually _use_ the ambient context in your calls to `foo-deep-lib`.

To communicate with transitive dependencies, you need an opaque `key` — these are usually created and exposed by your transitive dependency; see its documentation.

You can provide ambient values to the transitive dependency via calls to `Ambient_context.with_key_bound_to`; which takes that opaque `key`, the new `value` you want to set, and then a callback.

> **Example:** In our Lwt-enabled application, assuming `foo-deep-lib` takes advantage of the ambient context to communicate about a header it wants to add to HTTP requests, we can set that header in our application's top-level context, and it will be available to `foo-deep-lib`'s calls to `Curl`:
>
> ```ocaml
> (* src/widget_app.ml *)
> module Ctx = Ambient_context
>
> let () =
>    let sock = create_socket () in
>    let serve = create_server sock in
>
>    Ctx.set_current_storage Ambient_context_lwt.storage ;
>    Lwt_main.run @@ fun () ->
>    Ctx.with_key_bound_to Foo_deep.header_context_key "my header value" @@ fun () ->
>       (* This empty [bind] may be necessary; see
>          {!Ambient_context_lwt.with_key_bound_to}. *)
>       Lwt.bind (serve ()) (fun () -> ())
> ```

Refer to your dependency's documentation for specific instructions on how to provide the ambient context they expect.

<!-- FIXME: v3.ocaml.org links for the backends' documentation -->
> [!NOTE]
> The precise semantics of `with_key_bound_to` depend on the chosen storage-backend; refer to your chosen backend's documentation.

### ... as a library

This library allows you to avoid depending on, or functorizing over, `Lwt.t`. In the most basic usage, you simply provide a `Ambient_context.key`, direct your consumers to the [above documentation](#as-a-top-level-application) and then anywhere in your API, you can pull the value 'currently' assigned to your key out of the ambient-context.

You need depend only on `ambient-context` itself, *not* `ambient-context-lwt`, or even `lwt` itself:

```diff
 ; dune-project
  (depends
   (ocaml
    (>= 4.08))
+  ambient-context
   (alcotest :with-test)
   (ocaml-lsp-server :with-dev-setup)
```

```diff
 ; lib/dune
 (library
  (name foo-deep-lib)
- (libraries curl pcre)
+ (libraries ambient-context curl pcre))
```

Use `Ambient_context.new_key` to create an opaque key for the value, and expose that to your user:

```ocaml
(* lib/foo_deep.ml *)
module Ctx = Ambient_context

let header_context_key : string Ctx.key = Ctx.new_key ()

(* ... *)
```

Then, anywhere you like, you should be able to obtain the value assigned to `Foo_deep.header_context_key` by the consuming application up-stack from you:

```ocaml
(* lib/foo_deep.ml *)
module Ctx = Ambient_context

(* ... *)

let http_request ?headers ?body action url =
   let open Curl in
   let headers =
      match Ctx.get Foo_deep.header_context_key with
      | None -> headers
      | Some header ->
         let header = "x-foo-deep: " ^ header in
         Some (header :: Option.default [] headers)
   in
   (* ... *)
```

Contributing
------------

1. Create an opam switch and install the dependencies (opam >= 2.2 is recommended for the `--with-dev-setup` flag):

   ```console
   $ opam switch create . ocaml.4.14.2 --deps-only --no-install
   $ eval $(opam env --switch=.)

   $ opam install ./ambient-context.opam \
      --deps-only --with-test --with-dev-setup
   ```

2. [Install pre-commit][], and then configure your checkout:

   ```console
   $ pre-commit install
   pre-commit installed at .git/hooks/pre-commit
   ```

[Install pre-commit]: <https://pre-commit.com/index.html#install> "Installation instructions for th pre-commit tool"

3. Build and run the tests:

   ```console
   $ dune runtest -f --only-packages=ambient-context --display short
   ```

License
-------

Copyright © 2023 ELLIOTTCABLE

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the “Software”), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE, AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES, OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT, OR OTHERWISE, ARISING FROM, OUT OF, OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
