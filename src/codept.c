#include <assert.h>
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>              /* open() */
#include <libgen.h>             /* for basename() */
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>
#endif
#include <spawn.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/socket.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>

#include "log.h"
#include "s7.h"
#include "utarray.h"
#include "uthash.h"
#include "utstring.h"

#include "codept.h"

int errnum;
int rc;

/* do not use, use the c fn */
EXPORT void gen_codept_argsfile(char *_rootdir, UT_array *_subdirs)
{
    log_debug("running gen_codept_argsfile script on %s", _rootdir);

    s7_pointer rootdir = s7_make_string(s7, _rootdir);

    /* convert subdirs to scheme list */
    s7_pointer subdirs = s7_make_list(s7, utarray_len(_subdirs), s7_nil(s7));
    s7_pointer subdir;
    char **dir = NULL;
    int i = 0;
    while ( (dir=(char**)utarray_next(_subdirs,dir))) {
        /* log_debug("subdir: %s",*dir); */
        subdir = s7_make_string(s7, *dir);
        s7_list_set(s7, subdirs, i, subdir);
        i++;
    }
    s7_pointer args =  s7_list(s7, 2, rootdir, subdirs);

    s7_pointer result = s7_call(s7,
                                s7_name_to_value(s7, "gen-codept-argsfile"),
                                args);
    errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
    if ((errmsg) && (*errmsg)) {
        log_error("[%s\n]", errmsg);
        s7_quit(s7);
        exit(EXIT_FAILURE);
    }
    log_debug("result: %s", s7_object_to_c_string(s7, result));
}

EXPORT void run_ocamlark_handler(char *_depsfile)
{
    log_debug("running handle_deps_file script on %s", _depsfile);

    s7_pointer depsfile = s7_make_string(s7, _depsfile);
    log_debug("depsfile str: %s", s7_object_to_c_string(s7, depsfile));

    s7_pointer args =  s7_list(s7, 1, depsfile);

    s7_pointer result = s7_call(s7,
                                s7_name_to_value(s7, "camlark-handler"),
                                args);
    errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
    if ((errmsg) && (*errmsg)) {
        log_error("[%s\n]", errmsg);
        s7_quit(s7);
        exit(EXIT_FAILURE);
    }
    log_debug("result: %s", s7_object_to_c_string(s7, result));
}

/* do not use - we already have c code for this */
EXPORT void run_codept_s7(char *depsfile)
{
    log_debug("running user s7 script on %s", depsfile);
    s7_pointer lp = s7_load_path(s7);
    log_debug("s7 load path: %s", s7_object_to_c_string(s7, lp));

    /* s7_pointer lf; */
    /* log_debug("loading default script: %s", callback_script_file); */
    /* lf =  s7_load(s7, callback_script_file); */

    log_debug("calling s7 routine");
    /* now call routine */
    s7_pointer s = s7_make_string(s7, depsfile);
    log_debug("depsfile str: %s", s7_object_to_c_string(s7, s));

    s7_pointer args =  s7_list(s7, 1, s);
    log_debug("args: %s", s7_object_to_c_string(s7, args));

    /* log_debug("calling ast_handler"); */
    s7_pointer result = s7_call(s7,
                                s7_name_to_value(s7, "run-codept"),
                                args);
    errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
    if ((errmsg) && (*errmsg)) {
        log_error("[%s\n]", errmsg);
        s7_quit(s7);
        exit(EXIT_FAILURE);
    }

    /* result of call is last form evaluated */
    log_debug("result: %s", s7_object_to_c_string(s7, result));
}
