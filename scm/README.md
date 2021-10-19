# dune -> obazl

## naming executables

We have a 'name' field; we may also have 'public_name', the name of
the build output, the package name (from `dune-project` file.

The 'name' field must correspond to a source file. So we'll have a
module of that name, and must ensure that the two rules have different
names. For the module source we'll follow the convention of using the
normalized module name. For the executable, we'll use the public name
if we have one, otherwise we have to check if the name is the same as
the module file name; in that case we need to add something to
uniquify it, e.g. ".exe".

Then we have to worry about references to the executable. In some
cases (e.g. `libraries` field) referrence will use the pubic name. But
variables could also refer to it, as shown below.

## installs, cross-refs, etc.

`src/proto_008_PtEdo2Zk/lib_protocol/dune`:

```
(rule
 (targets "dune.inc.gen")
 (deps TEZOS_PROTOCOL (glob_files *.ml) (glob_files *.mli))
 (action
  (run
    %{libexec:tezos-protocol-compiler:replace}
    %{libexec:tezos-protocol-compiler:dune_protocol.template}
    "dune.inc.gen"
    %{libexec:tezos-protocol-compiler:final_protocol_versions})))
```

The `libexec:tezos-protocol-compiler:foo` stuff:

In `src/lib_protocol_compiler/dune`:

```
(install
 (section libexec)
 (files (replace.exe as replace)
        dune_protocol
        dune_protocol.template
        final_protocol_versions))
```

In `src/lib_protocol_compiler/dune-project`:

```
(lang dune 2.0)
(formatting (enabled_for ocaml))
(name tezos-protocol-compiler)
```

Doc for `install` says that the `package` field is optional if your
"project contains a single package.

Doc for `dune-package` says: "used to mark the root of projects as
well as define project-wide parameters."

So evidently, the `install` stanza above (from the `dune` file) puts
files into the `libexc` section of the package that is specified in
the `dune-project` file.  That makes them accessible by variable, e.g.
`%{libexec:tezos-protocol-compiler:final_protocol_versions}`.

To translate this to Bazel, we need to map expressions like
`%{libexec:tezos-protocol-compiler:final_protocol_versions}` to a
Bazel target. In this case `install` lists a built executable and
three source files. For the sources, we can use `exports_files` or
create a filegroup. For the executable we just need to derive its
Bazel label.  Giving:

* `%{libexec:tezos-protocol-compiler:replace}`
      => `//src/lib_protocol_compiler:replace`

* `%{libexec:tezos-protocol-compiler:dune_protocol.template}`
     =>  `exports_files("dune_protocol.template")`

* `%{libexec:tezos-protocol-compiler:final_protocol_versions}`
     =>  `exports_files("final_protocol_versions")`

Then the action:

```
 (action
  (run
    %{libexec:tezos-protocol-compiler:replace}
    %{libexec:tezos-protocol-compiler:dune_protocol.template}
    "dune.inc.gen"
    %{libexec:tezos-protocol-compiler:final_protocol_versions})))
```

becomes something like:

```
genrule(
    name = "...",
    srcs = ["dune_protocol.template", "final_protocol_versions"]
    outs = ["dune.inc.gen"],
    tools = ["//src/lib_protocol_compiler:replace"],
    cmd   = "(location //src/lib_protocol_compiler:replace) ...etc..."
)
```

To get to this we need to:

* maintain an install lookup table, so we can resolve dune vars to
  Bazel labels, e.g. `%{libexec:tezos-protocol-compiler:replace}`

* BUT: how do we know which elements of a `run` expr are srcs, outs,
  etc. Well, output will be in a `target` field of the rule.
