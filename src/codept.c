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

EXPORT void run_codept(char *codept_args_file, char *codept_deps_file)
{
    log_debug("running codept");

    rc = chdir(utstring_body(proj_root));
    if (rc) {
        perror("chdir");
        log_fatal("Unable to chdir to projroot %", utstring_body(proj_root));
        exit(EXIT_FAILURE);
    }

    char cmd[PATH_MAX];
    sprintf(cmd, "codept -verbosity info -k -args %s 2> /dev/null", codept_args_file);

    bool keep = true;        /* ??? */

    /* FILE *deps_fp; */
    /* if (keep) { */
    /*     printf("keeping %s\n", codept_deps_file); */
    /*     deps_fp = fopen(codept_deps_file, "w"); */
    /*     if (deps_fp == NULL) { */
    /*         perror(codept_deps_file); */
    /*         log_fatal("FAIL: run_codept fopen(%s, 'w')", codept_deps_file); */
    /*         exit(EXIT_FAILURE); */
    /*     } */
    /* } */
    /* log_debug("opened codept_deps_file %s for writing", codept_deps_file); */

    /* log_debug("codept_args_file: %s", codept_args_file); */
    /* log_debug("codept_deps_file: %s", codept_deps_file); */

    pid_t pid;
    int rc;
    char *argv[] = {
        "codept",
        "-args", codept_args_file,
        NULL};
    extern char **environ;

    /* FIXME: write stderr to log instead of dev/null? */
    int DEVNULL_FILENO = open("/dev/null", O_WRONLY);
    posix_spawn_file_actions_t action;
    posix_spawn_file_actions_init(&action);
    posix_spawn_file_actions_addopen (&action, STDOUT_FILENO, codept_deps_file,
                                      O_WRONLY | O_CREAT | O_TRUNC,
                                      S_IRUSR | S_IWUSR | S_IRGRP );
    if (rc = posix_spawn_file_actions_adddup2(&action,
                                              DEVNULL_FILENO,
                                              STDERR_FILENO)) {
        perror("posix_spawn_file_actions_adddup2");
        posix_spawn_file_actions_destroy(&action);
        exit(rc);
    }

    // FIXME: get absolute path of codept
    // FIXME: restrict environ

    char *codept_cmd = "/Users/gar/.opam/4.10/bin/codept";
    log_debug("spawning %s", codept_cmd);
    rc = posix_spawn(&pid, codept_cmd, &action, NULL, argv, environ);

    if (rc == 0) {
        /* log_debug("posix_spawn child pid: %i\n", pid); */
        if (waitpid(pid, &rc, 0) != -1) {
            if (rc) {
                log_error("codept rc: %d", rc);
                posix_spawn_file_actions_destroy(&action);
            } else {
                return;         /* success */
            }
        } else {
            perror("waitpid");
            log_error("run_codept posix_spawn");
        }
    } else {
        /* does not set errno */
        log_fatal("run_codept posix_spawn error rc: %d, %s", rc, strerror(rc));
    }
    posix_spawn_file_actions_destroy(&action);
    exit(EXIT_FAILURE);
}

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
                                s7_name_to_value(s7, "ocamlark-handler"),
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
