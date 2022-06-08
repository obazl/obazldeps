#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <glob.h>
#include <libgen.h>
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>             /* PATH_MAX */
#endif
#include <pwd.h>
#include <spawn.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

#include "ini.h"
#include "log.h"
#if EXPORT_INTERFACE
#include "s7.h"
#include "utarray.h"
#include "utstring.h"
#endif

#include "s7_config.h"

#if INTERFACE
#define TO_STR(x) s7_object_to_c_string(s7, x)
#endif

bool debug;
bool verbose;

s7_scheme *s7;                  /* GLOBAL s7 */
s7_pointer old_port;
/* LOCAL s7_pointer result; */
int gc_loc = -1;

int rc;

s7_pointer s7_error_handler(s7_scheme *sc, s7_pointer args)
{
    log_error("error: %s\n", s7_string(s7_car(args)));
    fprintf(stdout, "error: %s\n", s7_string(s7_car(args)));
    return(s7_f(sc));
}

#if INTERFACE
#define DUNE_CRAWL_HELP "(dune-crawl rootdir pathdir) rootdir is relative to $HOME; pathdir is relative to rootdir.  Change dir to rootdir and crawl pathdir, creating pkg-tbl"

/* NB: we need to escape #\" in C... */
#define DUNE_CRAWL_FORMAL_PARAMS "s"
#endif

/* s7_pointer pkg_tbl; */

#define PKG_CT 50

EXPORT void s7_initialize(void)
{
    s7 = s7_init();

    /* trap error messages */
    /* old_port = s7_set_current_error_port(s7, s7_open_output_string(s7)); */
    /* if (old_port != s7_nil(s7)) */
    /*     gc_loc = s7_gc_protect(s7, old_port); */

    s7_define_function(s7, "error-handler",
                       s7_error_handler, 1, 0, false,
                       "our error handler");

    /* s7_define_safe_function(s7, "dune-crawl", g_dune_crawl, */
    /*                         0, 2, 0, */
    /*                              /\* DUNE_CRAWL_FORMAL_PARAMS, *\/ */
    /*                         DUNE_CRAWL_HELP); */

    set_load_path(callback_script_file);

    s7_repl(s7);

    s7_pointer lf;
    /* log_info("loading default script: %s", callback_script_file); */
    lf =  s7_load(s7, callback_script_file);

    lf =  s7_load(s7, "alist.scm");

    // pkg-tbl
    if (debug)
        log_debug("making pkg-tbl");
    s7_pointer pkg_tbl = s7_make_hash_table(s7, PKG_CT);
    s7_define_variable(s7, "pkg-tbl", pkg_tbl);
}

EXPORT void s7_shutdown(void)
{
    s7_close_output_port(s7, s7_current_error_port(s7));
    s7_set_current_error_port(s7, old_port);
    if (gc_loc != -1)
        s7_gc_unprotect_at(s7, gc_loc);
    s7_quit(s7);
}
