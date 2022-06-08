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

#include "cmd_runner.h"

int errnum;
int rc;

/* see also tools_opam/bootstrap/opam_deps.c */
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
    sprintf(cmd, "codept -verbosity info -sexp -k -args %s 2> /dev/null", codept_args_file);

    /* bool keep = true;        /\* ??? *\/ */

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
    if ((rc = posix_spawn_file_actions_adddup2(&action,
                                              DEVNULL_FILENO,
                                               STDERR_FILENO))) {
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

EXPORT char * run_cmd(char *executable, char **argv)
{
    log_debug("run_cmd %s", argv[0]);

    pid_t pid;
    /* char *argv[] = { */
    /*     "codept", */
    /*     "-args", codept_args_file, */
    /*     NULL}; */
    int rc;

    extern char **environ;

    /* FIXME: write stderr to log instead of dev/null? */
    int DEVNULL_FILENO = open("/dev/null", O_WRONLY);

    int cout_pipe[2];
    int cerr_pipe[2];

    if(pipe(cout_pipe) || pipe(cerr_pipe)) {
        log_error("pipe returned an error.");
        exit(EXIT_FAILURE);
    }

    posix_spawn_file_actions_t action;
    posix_spawn_file_actions_init(&action);

    /* child inherits open FDs, so: */
    /* close read end of pipes on child */
    posix_spawn_file_actions_addclose(&action, cout_pipe[0]);
    posix_spawn_file_actions_addclose(&action, cerr_pipe[0]);

    /* dup write-ends on child-side, connect stdout/stderr */
    posix_spawn_file_actions_adddup2(&action, cout_pipe[1],
                                     STDOUT_FILENO);
    posix_spawn_file_actions_adddup2(&action, cerr_pipe[1],
                                     STDERR_FILENO);

    /* close write end on child side */
    posix_spawn_file_actions_addclose(&action, cout_pipe[1]);
    posix_spawn_file_actions_addclose(&action, cerr_pipe[1]);

    /* now child will not inherit open pipes, but its stdout/stderr
       FDs will be connected to the write ends of the pipe.
     */

    /* posix_spawn_file_actions_addopen(&action, */
    /*                                  STDOUT_FILENO, */
    /*                                  codept_deps_file, */
    /*                                   O_WRONLY | O_CREAT | O_TRUNC, */
    /*                                   S_IRUSR | S_IWUSR | S_IRGRP ); */

    /* if ((rc = posix_spawn_file_actions_adddup2(&action, */
    /*                                            DEVNULL_FILENO, */
    /*                                            STDERR_FILENO))) { */
    /*     perror("posix_spawn_file_actions_adddup2"); */
    /*     posix_spawn_file_actions_destroy(&action); */
    /*     exit(rc); */
    /* } */

    // FIXME: get absolute path of codept
    // FIXME: restrict environ

    /* log_debug("spawning %s", executable); */
    rc = posix_spawnp(&pid, executable, &action, NULL, argv, environ);

    if (rc != 0) {
        /* does not set errno */
        log_fatal("run_command posix_spawn error rc: %d, %s",
                  rc, strerror(rc));
        exit(EXIT_FAILURE);
    }

    /* now close the write end on parent side */
    close(cout_pipe[1]);
    close(cerr_pipe[1]);

    /* https://github.com/pixley/InvestigativeProgramming/blob/114b698339fb0243f50cf5bfbe5d5a701733a125/test_spawn_pipe.cpp */

    // Read from pipes
    static char buffer[1024] = "";
    struct timespec timeout = {5, 0};

    fd_set read_set;
    memset(&read_set, 0, sizeof(read_set));
    FD_SET(cout_pipe[0], &read_set);
    FD_SET(cerr_pipe[0], &read_set);

    int larger_fd = (cout_pipe[0] > cerr_pipe[0])
        ? cout_pipe[0]
        : cerr_pipe[0];

    rc = pselect(larger_fd + 1, &read_set, NULL, NULL, &timeout, NULL);
    //thread blocks until either packet is received or the timeout goes through
    if (rc == 0) {
        fprintf(stderr, "pselect timed out.\n");
        /* return 1; */
        exit(EXIT_FAILURE);
    }

    int bytes_read = read(cerr_pipe[0], &buffer[0], 1024);
    if (bytes_read > 0) {
        /* fprintf(stdout, "Read message: %s", buffer); */
    }

    bytes_read = read(cout_pipe[0], &buffer[0], 1024);
    if (bytes_read > 0){
        //std::cout << "Read in " << bytes_read << " bytes from cout_pipe." << std::endl;
        buffer[bytes_read] = '\0';
        fprintf(stdout, "%s\n",
                //bytes_read,
                buffer);
    }

    waitpid(pid, &rc, 0);
    if (rc) {
        log_error("run_command rc: %d", rc);
        posix_spawn_file_actions_destroy(&action);
        exit(EXIT_FAILURE);
    }

    /* fprintf(stdout,  "exit code: %d\n", rc); */

    posix_spawn_file_actions_destroy(&action);
    return buffer;
}
