#include <errno.h>
#include <fcntl.h>
/* #include <getopt.h> */
#include <libgen.h>

#if INTERFACE
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>
#endif
#endif

#include <unistd.h>             /* getopt, close, getcwd */

#include "ini.h"
#include "log.h"
#include "utarray.h"
#include "uthash.h"
#include "utstring.h"

#include "obazldeps.h"
#include "runner.h"

UT_array *opam_dirs;             /* string list */
UT_array *subdirs;
UT_array *files;

int rc;
/* these externs are defined by //src:codept */
extern UT_string *exec_root;
extern UT_string *runfiles_root;
extern UT_string *proj_root;
extern UT_string *obazl_d;

extern bool ini_error;
extern UT_string *obazl_ini_path;
extern struct configuration_s obazl_config;
extern UT_array *src_files;
extern UT_string *codept_args_file;
extern UT_string *codept_deps_file;
extern struct filedeps_s *codept_filedeps;

int config_handler(void* config, const char* section, const char* name, const char* value)
{
    /* log_debug("config_handler section %s: %s=%s", section, name, value); */
    struct configuration_s *pconfig = (struct configuration_s*)config;

    #define MATCH(s, n) strcmp(section, s) == 0 && strcmp(name, n) == 0

    if (MATCH("obazl", "version")) {
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
                log_debug("pushing src dir: %s", token);
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

int main(int argc, char *argv[])
{
    int opt;

    while ((opt = getopt(argc, argv, "h")) != -1) {
        switch (opt) {
        case 1:
            break;
        case 'h':
            log_info("Help: ");
            exit(EXIT_SUCCESS);
        default:
            log_error("Usage: %s ... [TODO]", argv[0]);
            /* exit(EXIT_FAILURE); */
        }
    }

    obazl_configure(getcwd(NULL, 0));

    rc = access(utstring_body(obazl_ini_path), R_OK);
    if (rc) {
        log_warn("Config file %s not found.", utstring_body(obazl_ini_path));
    } else {
        ini_error = false;
        utarray_new(obazl_config.src_dirs, &ut_str_icd);
        utarray_new(obazl_config.watch_dirs, &ut_str_icd);
        rc = ini_parse(utstring_body(obazl_ini_path), config_handler, &obazl_config);
        log_debug("ini_parse rc: %d", rc);
        if (rc < 0) {
            //FIXME: deal with missing .obazl
            log_fatal("Can't load ini file: %s", utstring_body(obazl_ini_path));
            return -1;
        }
        if (ini_error) {
            log_error("Error parsing ini file");
            exit(EXIT_FAILURE);
        /* } else { */
        /*     log_debug("Config loaded from %s", utstring_body(obazl_ini_path)); */
        }
    }

    /* list opam dirs, to parameterize codept */
    opam_dirs = inventory_opam();
    log_debug("OPAM dir ct: %d", utarray_len(opam_dirs));
    char **dir = NULL;
    /* while ( (dir=(char**)utarray_next(opam_dirs,dir))) { */
    /*     log_debug("%s",*dir); */
    /* } */

    dir = NULL;
    while ( (dir=(char**)utarray_next(obazl_config.src_dirs,dir))) {
        log_debug("src dir: %s",*dir);
    }

    /* list source files, to parameterize codept */
    fileseq(utstring_body(proj_root), /* input */
            obazl_config.src_dirs, /* input (from ini file) */
            src_files);            /* output */
    dir = NULL;
    while ( (dir=(char**)utarray_next(src_files,dir))) {
        log_debug("src file: %s",*dir);
    }
    emit_codept_args(codept_args_file, opam_dirs, src_files);

    run_codept(utstring_body(codept_args_file), utstring_body(codept_deps_file));

    obazl_deps_parse_file(utstring_body(codept_deps_file));

    /* struct module_s *module, *tmp; */
    /* HASH_ITER(hh, codept_modules, module, tmp) { */
    /*     log_debug(""); */
    /*     log_debug("module->name: %s", module->name); */
    /*     log_debug("module->type: %d", module->type); */
    /*     if (module->type == M_LOCAL) { */
    /*         log_debug("module->structfile: %s", module->structfile); */
    /*         log_debug("module->sigfile: %s", module->sigfile); */
    /*     } else { */
    /*         log_debug("module->lib: %s", module->lib); */
    /*     } */
    /* } */

    /* struct filedeps_s *fdeps, *tmpfdeps; */
    /* char **p = NULL; */
    /* HASH_ITER(hh, codept_filedeps, fdeps, tmpfdeps) { */
    /*     log_debug(""); */
    /*     log_debug("fdeps->name: %s", fdeps->name); */
    /*     /\* log_debug("fdeps->type: %d", fdeps->type); *\/ */
    /*     log_debug("fdeps->deps:"); */
    /*     while ( (p=(char**)utarray_next(fdeps->deps, p))) { */
    /*         log_debug("\t%s",*p); */
    /*     } */
    /* } */

    obazl_shutdown();

    return 0;
}
