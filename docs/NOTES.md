# ocaml dependency resolution

## naming

### aggregator/component name clashses

Aggregator names may clash with component names. For example:
  `src/lib_protocol_environment` builds library
  `tezos_protocol_environment` (pubic_name:
  `tezos-protocol-environment`), which contains module
  `Tezos_protocol_environment`.  Note that this is an unwrapped library.

Dune stanzas that need this depend on the public name
`tezos-protocol-environment`. Meaning, the dependency is on a
"package" rather than a module.

Currently camlark writes such deps using the `name` field rather than
the `public_name` field. The name field is normalized to a module
name, so in this case the dependency name would be
`Tezos_protocol_environment`, which clashes with the component module
of that name.

Policy: leave the module names as-is: the module name matches the file
name. Where this clashes with the containing aggregator, change the
aggregator name.

The path of least resistence is to use the public_name unchanged as
the target name.
