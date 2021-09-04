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
#include "utarray.h"
#include "uthash.h"
#include "utstring.h"

#include "codept_emit.h"

int errnum;
int rc;

/* BUFSIZE = 1024; */

/* char basedir[PATH_MAX]; */
/* char coqlib[PATH_MAX]; */

EXPORT void run_codept(char *codept_args_file, char *codept_deps_file)
{
    log_debug("running codept");

    /* char cwd[PATH_MAX]; */
    /* if (getcwd(cwd, sizeof(cwd)) != NULL) { */
    /*     log_debug("Current working dir: %s", cwd); */
    /* } */
    /* log_debug("changing dir to %s", utstring_body(proj_root)); */
    rc = chdir(utstring_body(proj_root));
    if (rc) {
        perror("chdir");
        log_fatal("Unable to chdir to projroot %", utstring_body(proj_root));
        exit(EXIT_FAILURE);
    }

    char cmd[PATH_MAX];
    sprintf(cmd, "codept -k -args %s 2> /dev/null", codept_args_file);

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

    posix_spawn_file_actions_t action;
    posix_spawn_file_actions_init(&action);
    posix_spawn_file_actions_addopen (&action, STDOUT_FILENO, codept_deps_file,
                                      O_WRONLY | O_CREAT | O_TRUNC,
                                      S_IRUSR | S_IWUSR | S_IRGRP );

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
                exit(EXIT_FAILURE);
            }
        } else {
            perror("waitpid");
            log_error("run_codept posix_spawn");
        }
    } else {
        /* does not set errno */
        log_fatal("run_codept posix_spawn error rc: %d, %s", rc, strerror(rc));
        exit(EXIT_FAILURE);
    }

    /* FILE *fp; */
    /* if ((fp = popen(cmd, "r+")) == NULL) { */
    /*     printf("Error opening pipe for codept!\n"); */
    /*     exit(-1); */
    /* } */
    /* log_debug("action: %s", cmd); */
    /* char buf[1024]; */
    /* while (fgets(buf, sizeof(buf), fp) != NULL) { */
    /*     if (keep) */
    /*         fputs(buf, deps_fp); */
    /*     process_codept_line(buf); */
    /* } */
    /* rc = pclose(fp); */
    /* if (rc) { */
    /*     exit(-1); */
    /* } */

    /* if (keep) { */
    /*     fflush(deps_fp); */
    /*     fclose(deps_fp); */
    /* } */
}

LOCAL void emit_codept_src_files(FILE *fp, UT_array *src_files)
{
    log_debug("emit_codept_src_files");

    /* fputs("-one-line\n", fp); */
    fputs("-sexp\n", fp);

    char **p = NULL;
    while ( (p=(char**)utarray_next(src_files, p))) {
            /* log_debug("writing codept arg: %s", *p); */

            rc = fputs(*p, fp);
            if ( rc < 1 ) { // success == non-neg int
                errnum = errno;
                perror("emit_codept_src_files");
                /* log_error("fputs fail putting %s to %s", *p, codept_args_file); */
                /* log_error("rc: %d, errno: %d", rc, errno); */
                /* log_error("Error putting %s: %s", *p, strerror( errnum )); */
                fclose(fp);
                exit(1);
            }
            rc = fputs("\n", fp);
            if ( rc < 1) {
                errnum = errno;
                perror("emit_codept_src_files 2");
                /* log_error("fputs fail putting newline to %s", codept_args_file); */
                /* log_error("Value of errno: %d", errno); */
                /* log_error("Error putting newline: %s", strerror( errnum )); */
                fclose(fp);
                exit(1);
            }
    }

    fputs("\n", fp);
}

LOCAL void emit_codept_opam_dirs(FILE *fp, UT_array *opam_dirs)
{
    log_debug("emit_codept_opam_dirs");

    char **p = NULL;
    while ( (p=(char**)utarray_next(opam_dirs, p))) {
        /* log_debug("writing codept arg: %s", *p); */

        /* Warning! '-L' is just for .cmi files, it will miss pkgs
           like Zarith, which does not have zarith.cmi. We need both
           -I and -L. */
        rc = fputs("-L\n", fp);
        rc = fputs(*p, fp);
        /* rc = fputs("\n-I\n", fp); */
        /* rc = fputs(*p, fp); */
        if ( rc < 1 ) { // success == non-neg int
            errnum = errno;
            perror("emit_codept_opam_dirs");
            log_error("emit_codept_opam_dirs 1 fputs");
            fclose(fp);
            exit(1);
        }
        rc = fputs("\n", fp);
        if ( rc < 1) {
            errnum = errno;
            perror("emit_codept_opam_dirs 2");
            log_error("emit_codept_opam_dirs 2 fputs");
            fclose(fp);
            exit(1);
        }
    }

    fputs("\n", fp);
}

EXPORT void emit_codept_args(UT_string *_codept_args_file, UT_array *opam_dirs, UT_array *src_files)
{
    log_debug("emit_codept_args: %s",
              utstring_body(_codept_args_file));

    char *codept_args_file = utstring_body(_codept_args_file);

    FILE *fp;
    int rc;

    truncate(codept_args_file, 0);
    fp = fopen(codept_args_file, "w");
    if (fp == NULL) {
        errnum = errno;
        perror(codept_args_file);
        log_error("fopen(%s): %s", codept_args_file, strerror( errnum ));
        exit(1);
    }

    emit_codept_src_files(fp, src_files);

    emit_codept_opam_dirs(fp, opam_dirs);

    // PREPROCS: .mll, .mly, .mlg, etc. - files that require preprocessing
    /* struct preproc *pp, *pptmp; */
    /* HASH_ITER(hh, preprocs, pp, pptmp) { */
    /*     /\* printf("putting %s to %s", cd->dir, codept_args_file); *\/ */
    /*     //FIXME: macro to handle error checking */
    /*     if (pp->mlout) { */
    /*         /\* printf("codept preproc: %s", pp->mlout); *\/ */
    /*         rc = fputs(pp->mlout, fp); */
    /*     } */
    /*     if ( rc < 1 ) { // success == non-neg int */
    /*         errnum = errno; */
    /*         log_error("fputs fail putting %s to %s", pp->name, codept_args_file); */
    /*         log_error("rc: %d, errno: %d", rc, errno); */
    /*         log_error("Error putting %s: %s", pp->name, strerror( errnum )); */
    /*         fclose(fp); */
    /*         exit(1); */
    /*     } */
    /*     rc = fputs("\n", fp); */
    /*     if ( rc < 1) { */
    /*         errnum = errno; */
    /*         log_error("fputs fail putting newline to %s", codept_args_file); */
    /*         log_error("Value of errno: %d", errno); */
    /*         log_error("Error putting newline: %s", strerror( errnum )); */
    /*         fclose(fp); */
    /*         exit(1); */
    /*     } */
    /* } */

    /* now write out -I params for lib deps if enabled */
    /* emit_codept_lib_deps(); */

    /* if (coq) { */
    /*     emit_coq_codept_args_file(fp); */
    /* } */

    fflush(fp);

    rc = fclose(fp);
    if ( rc != EXIT_SUCCESS ) {
        errnum = errno;
        log_error("fclose fail on %s, rc: %d", codept_args_file, rc);
        log_error("Value of errno: %d", errno);
        log_error("Error fclosing %s: %s", codept_args_file, strerror( errnum ));
        exit(1);
    }
}
