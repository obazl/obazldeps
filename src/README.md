# notes

most of this C code has been replaced by Scheme code.

Original C code:

* `tools_obazl/src/lib/obazl_codept` crawl the fs to get src files,
emit codept.args, run codept (codept.c:run_codept), and partially
parse the output. Uses sfsexp to parse sexps. Puts everything into C
structs (utarray, uthash). Also includes (outdated) Lua binding.

Second version used the s7 api to do some of that stuff in C.

Current version uses s7 Scheme code for (almost?) everything. The only
C code is for the repl/runner and configuration (setting paths,
initializing s7, etc.). Maybe use the C `run_codept` instead of
`(system "codept ...")`.
