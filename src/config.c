#include <ctype.h>
#include <errno.h>
#include <libgen.h>
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>             /* PATH_MAX */
#endif
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

#include "ini.h"
#include "log.h"
#include "s7.h"
#include "utarray.h"
#include "utstring.h"

#include "config.h"

s7_scheme *s7;                  /* GLOBAL s7 */
s7_pointer old_port;
LOCAL s7_pointer result;
int gc_loc = -1;
const char *errmsg = NULL;

LOCAL UT_string *opam_switch;
LOCAL UT_string *opam_bin;
LOCAL UT_string *opam_lib;

UT_string *exec_root;
UT_string *runfiles_root;
UT_string *proj_root;
UT_string *obazl_d;

UT_string *runtime_data_dir;

bool ini_error = false;
UT_string *obazl_ini_path;
const char *obazl_ini_file = ".obazlrc";

UT_string *codept_args_file;
const char *codept_args_filename = "codept.args";

/* static const char codept_depends[] = ".obazl.d/codept.depends"; */
UT_string *codept_deps_file;
const char *codept_deps_filename = "codept.deps";

#if EXPORT_INTERFACE
struct configuration_s {
    char *obazl_version;
    int libct;
    UT_array *src_dirs;         /* string list; used by fileseq to get src_files */
    UT_array *watch_dirs;       /* string list */
    /* struct lib_s *ocamllibs[10]; /\* is 10 enough? *\/ */
    /* struct lib_s *coqlibs[10]; /\* is 10 enough? *\/ */
};
#endif

#define OBAZL_VERSION "0.1.0"

struct configuration_s obazl_config = {.obazl_version = OBAZL_VERSION, .libct = 0};

UT_array *src_files;            /* FIXME: put this in configuration_s? */

EXPORT int obazl_configure(char *_exec_root)
{
    log_debug("obazl_configure");
    char *_proj_root = getenv("BUILD_WORKSPACE_DIRECTORY");
    if (_proj_root == NULL) {
        log_error("Env var 'BUILD_WORKSPACE_DIRECTORY' not found. This program must be run in a Bazel project.");
        exit(EXIT_FAILURE);
    }
    log_debug("BUILD_WORKSPACE_DIRECTORY: %s", _proj_root);

    utstring_new(exec_root);
    utstring_printf(exec_root, "%s", _exec_root);
    log_debug("exec_root: %s", utstring_body(exec_root));

    utstring_new(runfiles_root);
    utstring_printf(runfiles_root, "%s", getcwd(NULL, 0));
    log_debug("runfiles_root: %s", utstring_body(runfiles_root));

    utstring_new(proj_root);
    utstring_printf(proj_root, "%s", _proj_root);
    log_debug("proj_root: %s", utstring_body(proj_root));

    /* .obazl.d hidden directory */
    utstring_new(obazl_d);
    utstring_printf(obazl_d, "%s/%s", utstring_body(proj_root), ".obazl.d");
    log_debug("obazl_d: %s", utstring_body(obazl_d));
    log_debug("mkdir %s", utstring_body(obazl_d));
    int rc = mkdir(utstring_body(obazl_d), S_IRWXU | S_IRGRP | S_IWGRP);
    if (rc != 0) {
        if (errno != EEXIST) {
            perror(utstring_body(obazl_d));
            log_error("mkdir error");
        }
    }

    /* .obazlrc config file */
    utstring_new(obazl_ini_path);
    utstring_printf(obazl_ini_path, "%s/%s", utstring_body(proj_root), obazl_ini_file);

    utarray_new(src_files,&ut_str_icd);

    utstring_new(codept_args_file);
    utstring_printf(codept_args_file, "%s/%s", utstring_body(obazl_d), codept_args_filename);
    /* log_debug("codept_args_file: %s", utstring_body(codept_args_file)); */

    utstring_new(codept_deps_file);
    utstring_printf(codept_deps_file, "%s/%s", utstring_body(obazl_d), codept_deps_filename);
    /* log_debug("codept_deps_file: %s", utstring_body(codept_deps_file)); */

    _s7_init();
}

void config_opam(char *_opam_switch)
{
    /*
      1. discover switch
         a. check env var OPAMSWITCH
         b. use -s option
         c. run 'opam var switch'
      2. discover lib dir: 'opam var lib'
     */

    utstring_new(opam_switch);
    utstring_new(opam_bin);
    utstring_new(opam_lib);

    /* FIXME: handle switch arg */
    char *cmd, *result;
    if (_opam_switch == NULL) {
        /* log_info("opam: using current switch"); */
        cmd = "opam var switch";

        result = run_cmd(cmd);
        if (result == NULL) {
            fprintf(stderr, "FAIL: run_cmd(%s)\n", cmd);
        } else {
            utstring_printf(opam_switch, "%s", result);
        }
    }

    cmd = "opam var bin";
    result = NULL;
    result = run_cmd(cmd);
    if (result == NULL) {
        log_fatal("FAIL: run_cmd(%s)\n", cmd);
        exit(EXIT_FAILURE);
    } else
        utstring_printf(opam_bin, "%s", result);

    cmd = "opam var lib";
    result = NULL;
    result = run_cmd(cmd);
    if (result == NULL) {
        log_fatal("FAIL: run_cmd(%s)\n", cmd);
        exit(EXIT_FAILURE);
    } else
        utstring_printf(opam_lib, "%s", result);

}

EXPORT UT_array *inventory_opam(void)
{
    config_opam(NULL);
    log_debug("opam switch: %s", utstring_body(opam_switch));
    log_debug("opam bin: %s", utstring_body(opam_bin));
    log_debug("opam lib: %s", utstring_body(opam_lib));

    // FIXME: make re-entrant
    UT_array *opam_dirs;             /* string list */
    utarray_new(opam_dirs, &ut_str_icd);

    // FIXME: add support for exclusions list
    int rc = dirseq(utstring_body(opam_lib), opam_dirs);

    return opam_dirs;
}

s7_pointer s7_error_handler(s7_scheme *sc, s7_pointer args)
{
  fprintf(stdout, "error: %s\n", s7_string(s7_car(args)));
  return(s7_f(sc));
}

LOCAL void _s7_init(void)
{
    s7 = s7_init();

    /* trap error messages */
    old_port = s7_set_current_error_port(s7, s7_open_output_string(s7));
    if (old_port != s7_nil(s7))
        gc_loc = s7_gc_protect(s7, old_port);

    s7_define_function(s7, "error-handler",
                       s7_error_handler, 1, 0, false,
                       "our error handler");

    //FIXME: error handling
    /* look for error messages */
    errmsg = s7_get_output_string(s7, s7_current_error_port(s7));

    /* if we got something, wrap it in "[]" */
    if ((errmsg) && (*errmsg)) {
        log_error("[%s\n]", errmsg);
        s7_quit(s7);
        exit(EXIT_FAILURE);
    }
    /* return s7; */
}

EXPORT void obazl_shutdown(void)
{
    s7_close_output_port(s7, s7_current_error_port(s7));
    s7_set_current_error_port(s7, old_port);
    if (gc_loc != -1)
        s7_gc_unprotect_at(s7, gc_loc);
    s7_quit(s7);
}
