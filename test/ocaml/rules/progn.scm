(define proj-root (string-append (getenv "HOME") "/ocaml/ocaml/"))

;; ocaml/runtime/dune

(define libocamlrun
  '((:pkg-path "runtime")
    (:rule
     (rule
      (targets libcamlrun.a)
      (mode    fallback)
      (deps
       ../Makefile.config
       ../Makefile.build_config
       ../Makefile.config_if_required
       ../Makefile.common Makefile
       (glob_files caml/*.h)
       ;; matches the line structure of files in Makefile/BYTECODE_C_SOURCES
       interp.c misc.c stacks.c fix_code.c startup_aux.c startup_byt.c freelist.c
       major_gc.c
       minor_gc.c memory.c alloc.c roots_byt.c globroots.c fail_byt.c signals.c
       signals_byt.c printexc.c backtrace_byt.c backtrace.c compare.c ints.c
       eventlog.c
       floats.c str.c array.c io.c extern.c intern.c hash.c sys.c meta.c parsing.c
       gc_ctrl.c  md5.c obj.c
       lexing.c callback.c debugger.c weak.c compact.c finalise.c custom.c dynlink.c
       afl.c unix.c win32.c bigarray.c main.c memprof.c domain.c
       skiplist.c codefrag.c
       )
      (action
       (progn
        (bash "touch .depend") ; hack.
        (run make %{targets} COMPUTE_DEPS=false)
        (bash "rm .depend")))))))

