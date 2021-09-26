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

LOCAL void emit_codept_src_files(FILE *fp, UT_array *src_files)
{
    log_debug("emit_codept_src_files");

    /* fputs("-one-line\n", fp); */
    fputs("-sexp\n-k\n", fp);

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

    /* emit_codept_opam_dirs(fp, opam_dirs); */

    emit_codept_src_files(fp, src_files);

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
