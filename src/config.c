#include <ctype.h>
#include <errno.h>
#include <libgen.h>
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>             /* PATH_MAX */
#endif
#include <pwd.h>
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
#if EXPORT_INTERFACE
#include "utarray.h"
#include "utstring.h"
#endif

#include "config.h"

bool debug;
bool verbose;

s7_scheme *s7;                  /* GLOBAL s7 */
s7_pointer old_port;
/* LOCAL s7_pointer result; */
int gc_loc = -1;
const char *errmsg = NULL;

int rc;

char *callback_script_file = "camlark.scm"; // passed in 'data' attrib
char *callback = "camlark_handler"; /* fn in callback_script_file  */

/* load-path script directories: sys, user, proj, in order
   obazl (sys) scripts:
       run under `bazel run`: dir in runfiles containing callback script
           @camlark//scm/s7, @camlark//scm
       run directly: XDG_DATA_DIRS default: /usr/local/share
           XDG_DATA_DIRS/s7
           XDG_DATA_DIRS/obazl/scm
   user scripts:
       ($HOME)/.obazl.d/scm
       $XDG_DATA_HOME default: $HOME/.local/share
           XDG_DATA_HOME/s7
           XDG_DATA_HOME/obazl/scm
   proj scripts:
       .obazl.d

 */

char *bazel_script_dir = NULL;

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

EXPORT int config_handler(void* config, const char* section, const char* name, const char* value)
{
    /* log_debug("config_handler section %s: %s=%s", section, name, value); */
    struct configuration_s *pconfig = (struct configuration_s*)config;

    #define MATCH(s, n) strcmp(section, s) == 0 && strcmp(name, n) == 0

    if (MATCH("obazl", "version")) {
        if (verbose)
            log_debug("obazl version: %s", value);
        return 1;
    }

    if (MATCH("srcs", "dirs")) {
        /* log_debug("section: srcs; entry: dirs"); */
        /* log_debug("\t%s", value); */
        char *token, *sep = " ,\t";
        token = strtok((char*)value, sep);
        while( token != NULL ) {
            /* if (token[0] == '/') { */
            /*     log_error("Ini file: 'dir' values in section 'srcs' must be relative paths: %s", token); */
            /*     ini_error = true; */
            /*     return 0; */
            /* } else { */
                /* log_debug("pushing src dir: %s", token); */
                utarray_push_back(pconfig->src_dirs, &token);
                token = strtok(NULL, sep);
            /* } */
        }
        return 1;
    }

    if (MATCH("watch", "dirs")) {
        /* log_debug("section: watch; entry: dirs"); */
        /* log_debug("\t%s", value); */
        char *token, *sep = " ,\t";
        token = strtok((char*)value, sep);
        while( token != NULL ) {
            if (token[0] == '/') {
                log_error("Ini file: 'dir' values in section 'watch' must be relative paths: %s", token);
                ini_error = true;
                return 0;
            } else {
                /* log_debug("pushing watch dir: %s", token); */
                utarray_push_back(pconfig->watch_dirs, &token);
                token = strtok(NULL, sep);
            }
        }
        return 1;
    }

    /* if (MATCH("obazl", "repos")) { */
    /*     resolve_repos((char*)value); */
    /* } */

    /* if (MATCH("repos", "coq")) { */
    /*     resolve_coq_repos((char*)value); */
    /* } */

    /* if (MATCH("repos", "ocaml")) { */
    /*     resolve_ocaml_repos((char*)value); */
    /* } */

    /* if ( strncmp(section, "repo:", 5) == 0 ) { */
    /*     /\* printf("REPO section: %s (%s = %s)\n", section, name, value); *\/ */
    /*     char *the_repo = &section[5]; */

    /*     char *repo_dir = get_workspace_dir(the_repo); */
    /*     printf("repo: %s -> %s\n", the_repo, repo_dir); */

    /*     /\* tmp_repo = NULL; *\/ */
    /*     /\* HASH_FIND_STR(repo_map, the_repo, tmp_repo);  /\\* already in the hash? *\\/ *\/ */
    /*     /\* if (tmp_repo) { *\/ */
    /*     /\*     printf("%s -> %s\n", tmp_repo->name, tmp_repo->base_path); *\/ */
    /*     /\* } else { *\/ */
    /*     /\*     fprintf(stderr, "No WS repo found for '%s' listed in .obazlrc\n", the_repo); *\/ */
    /*     /\*     exit(EXIT_FAILURE); *\/ */
    /*     /\* } *\/ */
    /* } */

    /* if ( strcmp(section, "coqlibs") == 0 ) { */
    /*     struct lib_s *cl = calloc(1, sizeof *cl); */
    /*     cl->name = strdup(name); */
    /*     cl->path = strdup(value); */
    /*     pconfig->coqlibs[pconfig->libct] = cl; */
    /*     /\* printf("loaded lib %d (%p): %s -> %s\n", *\/ */
    /*     /\*        pconfig->libct, *\/ */
    /*     /\*        pconfig->coqlibs[pconfig->libct], *\/ */
    /*     /\*        pconfig->coqlibs[pconfig->libct]->name, *\/ */
    /*     /\*        pconfig->coqlibs[pconfig->libct]->path); *\/ */
    /*     pconfig->libct++; */
    /* } */
    return 1;
}

EXPORT void obazl_configure(char *_exec_root)
{
    /* fprintf(stdout, "MAIN\n"); */
    /* char *new_argv[] = { */
    /*     "opam", */
    /*     "var", */
    /*     "lib", */
    /*     NULL}; */

    /* run_opam_cmd("opam", new_argv); */
    /* log_debug("BEGIN"); */
    /* for (int i = 0; i < 1000; i++) */
    /*     access("/Users/gar/.opam/4.10/lib/yojson", F_OK); */
    /* log_debug("END"); */

    /* log_debug("obazl_configure"); */
    utstring_new(exec_root);
    utstring_printf(exec_root, "%s", _exec_root);
    if (debug)
        log_debug("EXEC_ROOT: %s", utstring_body(exec_root));

    utstring_new(runfiles_root);
    utstring_printf(runfiles_root, "%s", getcwd(NULL, 0));
    if (debug)
        log_debug("runfiles_root: %s", utstring_body(runfiles_root));

    char *_proj_root = getenv("BUILD_WORKSPACE_DIRECTORY");
    if (_proj_root == NULL) {
        if (debug)
            log_debug("BUILD_WORKSPACE_DIRECTORY: null");
    } else {
        if (debug)
            log_debug("BUILD_WORKSPACE_DIRECTORY: %s", _proj_root);
    }

    char *_wd = getenv("BUILD_WORKING_DIRECTORY");
    if (_wd == NULL) {
        if (debug)
            log_debug("BUILD_WORKING_DIRECTORY: null");
    } else {
        if (debug)
            log_debug("BUILD_WORKING_DIRECTORY: %s", _wd);
    }

    utstring_new(proj_root);
    if (_proj_root == NULL)
        utstring_printf(proj_root, "%s", getcwd(NULL, 0));
    else
        utstring_printf(proj_root, "%s", _proj_root);

    /* .obazlrc config file */
    utstring_new(obazl_ini_path);
    utstring_printf(obazl_ini_path, "%s/%s", utstring_body(proj_root), obazl_ini_file);

    rc = access(utstring_body(obazl_ini_path), R_OK);
    if (rc) {
        if (verbose || debug)
            log_warn("Config file %s not found.", utstring_body(obazl_ini_path));
    } else {
        ini_error = false;
        utarray_new(obazl_config.src_dirs, &ut_str_icd);
        utarray_new(obazl_config.watch_dirs, &ut_str_icd);
        rc = ini_parse(utstring_body(obazl_ini_path), config_handler, &obazl_config);
        if (rc < 0) {
            //FIXME: deal with missing .obazl
            perror("ini_parse");
            log_fatal("Can't load/parse ini file: %s", utstring_body(obazl_ini_path));
            exit(EXIT_FAILURE);
        }
        if (ini_error) {
            log_error("Error parsing ini file");
            exit(EXIT_FAILURE);
        /* } else { */
        /*     log_debug("Config loaded from %s", utstring_body(obazl_ini_path)); */
        }
    }

    utarray_new(src_files,&ut_str_icd);

    _s7_init();

    chdir(_proj_root);
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
    /* FIXME: argv */
    char *exe = NULL, *result = NULL;
    if (_opam_switch == NULL) {
        /* log_info("opam: using current switch"); */

        exe = "opam";
        char *argv[] = {"opam", "var", "switch",NULL};

        result = run_cmd(exe, argv);
        if (result == NULL) {
            fprintf(stderr, "FAIL: run_cmd 'opam var switch'\n");
        } else {
            utstring_printf(opam_switch, "%s", result);
        }
    } // else??

    /* cmd = "opam var bin"; */
    char *argv1[] = {"opam", "var", "bin", NULL};
    result = NULL;
    result = run_cmd(exe, argv1);
    if (result == NULL) {
        log_fatal("FAIL: run_cmd 'opam var bin'\n");
        exit(EXIT_FAILURE);
    } else
        utstring_printf(opam_bin, "%s", result);

    /* cmd = "opam var lib"; */
    char *argv2[] = {"opam", "var", "lib", NULL};
    result = NULL;
    result = run_cmd(exe, argv2);
    if (result == NULL) {
        log_fatal("FAIL: run_cmd 'opam var lib'\n");
        exit(EXIT_FAILURE);
    } else
        utstring_printf(opam_lib, "%s", result);

}

/* EXPORT UT_array *inventory_opam(void) */
/* { */
/*     config_opam(NULL); */
/*     log_debug("opam switch: %s", utstring_body(opam_switch)); */
/*     log_debug("opam bin: %s", utstring_body(opam_bin)); */
/*     log_debug("opam lib: %s", utstring_body(opam_lib)); */

/*     // FIXME: make re-entrant */
/*     UT_array *opam_dirs;             /\* string list *\/ */
/*     utarray_new(opam_dirs, &ut_str_icd); */

/*     // FIXME: add support for exclusions list */
/*     //int rc = */
/*     dirseq(utstring_body(opam_lib), opam_dirs); */

/*     return opam_dirs; */
/* } */

s7_pointer s7_error_handler(s7_scheme *sc, s7_pointer args)
{
    log_error("error: %s\n", s7_string(s7_car(args)));
    fprintf(stdout, "error: %s\n", s7_string(s7_car(args)));
    return(s7_f(sc));
}

LOCAL s7_pointer _dirname(s7_scheme *s7, s7_pointer args)
{
    /* log_debug("_dirname %s", s7_object_to_c_string(s7, args)); */
    if ( !s7_is_null(s7, args) ) {
        char *d = dirname(s7_object_to_c_string(s7, s7_car(args)));
        return s7_make_string(s7, d);
    } else
        return s7_nil(s7);
}

LOCAL s7_pointer _basename(s7_scheme *s7, s7_pointer args)
{
    /* log_debug("_basename %s", s7_object_to_c_string(s7, args)); */
    if ( !s7_is_null(s7, args) ) {
        char *bn = basename(s7_object_to_c_string(s7, s7_car(args)));
       /* s7_make_string copies bn into scheme? */
        return s7_make_string(s7, bn);
    } else
        return s7_nil(s7);
}

LOCAL void _s7_init(void)
{
    s7 = s7_init();

    /* trap error messages */
    /* old_port = s7_set_current_error_port(s7, s7_open_output_string(s7)); */
    /* if (old_port != s7_nil(s7)) */
    /*     gc_loc = s7_gc_protect(s7, old_port); */

    s7_define_function(s7, "error-handler",
                       s7_error_handler, 1, 0, false,
                       "our error handler");

    s7_define_safe_function(s7, "dirname", _dirname,
                            1, 0, false,
                            "dirname: return directory part of file path");
    s7_define_safe_function(s7, "basename", _basename,
                            1, 0, false,
                            "basename: return basename part of file path");

    set_load_path(callback_script_file);

    s7_pointer lf;
    /* log_info("loading default script: %s", callback_script_file); */
    lf =  s7_load(s7, callback_script_file);
}

/*
  sets bazel_script_dir to dir containing scriptfile, which must be
  passed in 'data' attrib of cc_binary rule
 */
LOCAL void _config_bazel_load_path(char *scriptfile, UT_string *manifest)
{
    if (verbose)
        log_info("Configuring for `bazel run`");
    FILE * fp;
    char * line = NULL;
    size_t len = 0;
    ssize_t read;

    /* if running under bazel .obazl.d must exist, for codept output */
    utstring_new(obazl_d);
    utstring_printf(obazl_d, "%s/%s", utstring_body(proj_root), ".obazl.d");
    rc = access(utstring_body(obazl_d), R_OK);
    if (rc) {
        if (verbose)
            log_info("Creating project obazl workdir: %s", utstring_body(obazl_ini_path));
        rc = mkdir(utstring_body(obazl_d), S_IRWXU | S_IRGRP | S_IWGRP);
        if (rc != 0) {
            if (errno != EEXIST) {
                perror(utstring_body(obazl_d));
                log_error("mkdir error");
            }
        }
    }

    utstring_new(codept_args_file);
    utstring_printf(codept_args_file, "%s/%s", utstring_body(obazl_d), codept_args_filename);

    utstring_new(codept_deps_file);
    utstring_printf(codept_deps_file, "%s/%s", utstring_body(obazl_d), codept_deps_filename);

    /* bazel (sys) script dir */
    fp = fopen(utstring_body(manifest), "r");
    if (fp == NULL) {
        log_error("fopen failure %s", utstring_body(manifest));
        /* exit(EXIT_FAILURE); */
    }

    while ((read = getline(&line, &len, fp)) != -1) {
        /* log_debug("Retrieved line of length %zu:", read); */
        /* log_debug("\n%s", line); */
        bool hit = false;
        /* two tokens per line */
        while ((bazel_script_dir = strsep(&line, " ")) != NULL) {
            if (hit) {
                bazel_script_dir = dirname(bazel_script_dir);
                /* log_debug("bazel script dir: %s", bazel_script_dir); */
                UT_string *s7_script_dir = NULL;
                utstring_new(s7_script_dir);
                utstring_printf(s7_script_dir, "%s/s7", bazel_script_dir);
                if (verbose)
                    log_debug("adding to *load-path*: %s",
                              utstring_body(s7_script_dir));
                s7_add_to_load_path(s7, utstring_body(s7_script_dir));
                utstring_free(s7_script_dir);
                if (verbose)
                    log_debug("adding to *load-path*: %s", bazel_script_dir);
                s7_add_to_load_path(s7, bazel_script_dir);
                fclose(fp);
                return;
            } else {
                char *dot = strrchr(bazel_script_dir, '/');
                if (dot && !strcmp(dot+1, scriptfile))
                    hit = true;
            }
        }
    }
    fclose(fp);
    log_error("script %s not found; did you add it to the 'data' attribute of the build rule?", scriptfile);
    exit(EXIT_FAILURE);
}

LOCAL void _config_project_load_path(void)
{
    char *project_script_dir = ".obazl.d/scm";

    UT_string *proj_script_dir;
    utstring_new(proj_script_dir);
    utstring_printf(proj_script_dir, "%s/%s",
                    utstring_body(proj_root), project_script_dir);
    rc = access(utstring_body(proj_script_dir), R_OK);
    if (rc) {
        if (verbose || debug)
            log_debug("project script dir %s not found",
                     utstring_body(proj_script_dir));
    } else {
        if (verbose)
            log_debug("adding to *load-path*: %s",
                     utstring_body(proj_script_dir));
        s7_add_to_load_path(s7, utstring_body(proj_script_dir));
    }

    /* private project script dir */
    UT_string *private_script_dir;
    utstring_new(private_script_dir);
    utstring_printf(private_script_dir, "%s/.private/scm",
                    utstring_body(proj_root));
    rc = access(utstring_body(private_script_dir), R_OK);
    if (rc) {
        if (verbose || debug)
            log_warn("private script dir %s not found",
                     utstring_body(private_script_dir));
    } else {
        if (verbose)
            log_debug("adding to *load-path*: %s",
                     utstring_body(private_script_dir));
        s7_add_to_load_path(s7, utstring_body(private_script_dir));
    }
}

LOCAL void _config_user_load_path(void)
{
    char *_home_dir = getenv("HOME");
    char *_user_script_dir = ".obazl.d/scm";
    UT_string *user_script_dir;

    utstring_new(user_script_dir);
    utstring_printf(user_script_dir, "%s/%s",
                    _home_dir, _user_script_dir);

    rc = access(utstring_body(user_script_dir), R_OK);
    if (rc) {
        if (verbose || debug)
            log_info("Not found: user script dir: %s.",
                     utstring_body(user_script_dir));
    } else {
        if (verbose)
            log_debug("adding to *load-path*: %s",
                     utstring_body(user_script_dir));
        s7_add_to_load_path(s7, utstring_body(user_script_dir));
    }
}

LOCAL void _config_xdg_load_path(void)
{
    UT_string *xdg_bazel_script_dir;
    UT_string *xdg_user_script_dir;

    /* system obazl script dirs:
       $XDG_DATA_DIRS/s7, $XDG_DATA_DIRS/obazl/scm
    */
    char *xdg_data_dirs = getenv("XDG_DATA_DIRS");
    if (xdg_data_dirs == NULL) {
        xdg_data_dirs = "/usr/local/share";
    }
    utstring_new(xdg_bazel_script_dir);
    utstring_printf(xdg_bazel_script_dir, "%s/%s",
                    xdg_data_dirs, "obazl/scm");
    /* log_debug("xdg_bazel_script_dir: %s", */
    /*           utstring_body(xdg_bazel_script_dir)); */

    rc = access(utstring_body(xdg_bazel_script_dir), R_OK);
    if (rc) {
        if (verbose || debug)
            log_info("Not found: obazl sys script dir: %s.",
                     utstring_body(xdg_bazel_script_dir));
    } else {
        s7_add_to_load_path(s7, utstring_body(xdg_bazel_script_dir));
    }

    utstring_renew(xdg_bazel_script_dir);
    utstring_printf(xdg_bazel_script_dir, "%s/%s",
                    xdg_data_dirs, "s7");
    /* log_debug("s7 xdg_bazel_script_dir: %s", */
    /*           utstring_body(xdg_bazel_script_dir)); */

    rc = access(utstring_body(xdg_bazel_script_dir), R_OK);
    if (rc) {
        if (verbose || debug)
            log_info("Not found: obazl s7 sys script dir: %s",
                     utstring_body(xdg_bazel_script_dir));
    } else {
        if (verbose)
            log_debug("adding to *load-path*: %s",
                     utstring_body(xdg_bazel_script_dir));
        s7_add_to_load_path(s7, utstring_body(xdg_bazel_script_dir));
    }

    /* user obazl script dir: $XDG_DATA_HOME/obazl/scm */
    struct passwd *pw = getpwuid(getuid());
    const char *homedir = pw->pw_dir;
    /* log_debug("HOME DIR: %s", homedir); */

    utstring_new(xdg_user_script_dir);
    char *xdg_data_home = getenv("XDG_DATA_HOME");
    /* s7 first */
    if (xdg_data_home == NULL) {
        utstring_printf(xdg_user_script_dir, "%s/%s",
                        homedir, ".local/share/s7");
    } else {
        utstring_printf(xdg_user_script_dir, "%s/%s",
                        xdg_data_home, "s7");
    }
    rc = access(utstring_body(xdg_user_script_dir), R_OK);

    if (rc) {
        if (verbose || debug)
            log_info("Not found: user xdg s7 script dir: %s.",
                     utstring_body(xdg_user_script_dir));
    } else {
        if (verbose)
            log_debug("adding to *load-path*: %s",
                     utstring_body(xdg_user_script_dir));
        s7_add_to_load_path(s7, utstring_body(xdg_user_script_dir));
    }
    /* user obazl/scm */
    utstring_renew(xdg_user_script_dir);
    if (xdg_data_home == NULL) {
        utstring_printf(xdg_user_script_dir, "%s/%s",
                        homedir, ".local/share/obazl/scm");
    } else {
        utstring_printf(xdg_user_script_dir, "%s/%s",
                        xdg_data_home, "obazl/scm");
    }
    /* log_debug("s7 xdg_user_script_dir: %s", */
    /*           utstring_body(xdg_user_script_dir)); */
    rc = access(utstring_body(xdg_user_script_dir), R_OK);

    if (rc) {
        if (verbose || debug)
            log_info("Not found: user xdg obazl script dir: %s.",
                     utstring_body(xdg_user_script_dir));
    } else {
        if (verbose)
            log_debug("adding to *load-path*: %s",
                     utstring_body(xdg_user_script_dir));
        s7_add_to_load_path(s7, utstring_body(xdg_user_script_dir));
    }
}

EXPORT void set_load_path(char *scriptfile)
{
    /* log_debug("set_load_path for %s", scriptfile); */
    char *_wd = getcwd(NULL, 0);
    if (debug)
        log_debug("CURRENT WORKING DIRECTORY: %s", _wd);

    /* FIXME: reliable way to detect if we're run by bazel */

    /* https://docs.bazel.build/versions/main/user-manual.html#run
bazel run is similar, but not identical, to directly invoking the binary built by Bazel and its behavior is different depending on whether the binary to be invoked is a test or not. When the binary is not a test, the current working directory will be the runfiles tree of the binary. When the binary is a test, the current working directory will be the exec root and a good-faith attempt is made to replicate the environment tests are usually run in.
  */

    /* so if we find MANIFEST, we're running a test? */
    char *mdir = dirname(_wd);
    /* log_debug("MANIFEST DIR: %s", mdir); */

    UT_string *manifest;
    utstring_new(manifest);
    utstring_printf(manifest, "%s%s", mdir, "/MANIFEST");
    /* log_debug("MANIFEST: %s", utstring_body(manifest)); */

    rc = access(utstring_body(manifest), R_OK);

    if (rc) {
        if (verbose)
            log_info("Configuring for non-bazel run");
        _config_xdg_load_path();
        _config_user_load_path();
    } else {
        _config_bazel_load_path(scriptfile, manifest);
    }
    _config_project_load_path();
    if (verbose || debug) {
        s7_pointer lp = s7_load_path(s7);
        log_info("load path: %s", s7_object_to_c_string(s7, lp));
    }
}

EXPORT void obazl_shutdown(void)
{
    s7_close_output_port(s7, s7_current_error_port(s7));
    s7_set_current_error_port(s7, old_port);
    if (gc_loc != -1)
        s7_gc_unprotect_at(s7, gc_loc);
    s7_quit(s7);
}
