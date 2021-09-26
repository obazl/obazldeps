#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "ini.h"
#include "log.h"

#include "linenoise.h"
#include "s7.h"

#include "utarray.h"
#include "utstring.h"

#include "ocamlark.h"
#include "repl.h"

extern s7_scheme *s7;

char *history = ".ocamlark.history.txt";

extern bool debug;
extern bool verbose;
extern bool ini_error;
extern UT_string *obazl_ini_path;
extern struct configuration_s obazl_config;

UT_array *opam_dirs;             /* string list */
extern UT_string *codept_args_file;

void completion(const char *buf, linenoiseCompletions *lc) {
    if (buf[0] == 'h') {
        linenoiseAddCompletion(lc,"hello");
        linenoiseAddCompletion(lc,"hello there");
    }
}

char *hints(const char *buf, int *color, int *bold) {
    if (!strcasecmp(buf,"hello")) {
        *color = 35;
        *bold = 0;
        return " World";
    }
    return NULL;
}

void print_usage(void)
{
    printf("Usage: ocamlark [-m | -k | -v | -h ]\n");
}

int main(int argc, char **argv) {

    int rc;
    char response[1024];        /* result of evaluating input */

    char *line;
    char *prgname = argv[0];

    /* log_set_level(LOG_INFO); */

    /* Parse options, with --multiline we enable multi line editing. */
    int opt;
    while ((opt = getopt(argc, argv, "dmkhv")) != -1) {
        switch (opt) {
        case 'd':
            debug = true;
            break;
        case 'm':
            linenoiseSetMultiLine(1);
            printf("Multi-line mode enabled.\n");
            break;
        case 'k':
            linenoisePrintKeyCodes();
            exit(0);
            break;
        case 'h':
            print_usage();
            exit(EXIT_SUCCESS);
            break;
        case 'v':
            verbose =true;
            break;
        default:
            print_usage();
            exit(EXIT_FAILURE);
        }
    }

    //s7 = s7_init();                 /* initialize the interpreter */
    /* s7 = sunlark_init(); */
    obazl_configure(getcwd(NULL, 0));

    rc = access(utstring_body(obazl_ini_path), R_OK);
    if (rc) {
        log_warn("Config file %s not found.", utstring_body(obazl_ini_path));
    } else {
        ini_error = false;
        utarray_new(obazl_config.src_dirs, &ut_str_icd);
        utarray_new(obazl_config.watch_dirs, &ut_str_icd);
        rc = ini_parse(utstring_body(obazl_ini_path), config_handler, &obazl_config);
        /* log_debug("ini_parse rc: %d", rc); */
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

    const char *errmsg = NULL;

    /* list opam dirs, to parameterize codept */
    /* opam_dirs = inventory_opam(); */
    /* log_debug("OPAM dir ct: %d", utarray_len(opam_dirs)); */
    /* char **dir = NULL; */
    /* while ( (dir=(char**)utarray_next(opam_dirs,dir))) { */
    /*     log_debug("%s",*dir); */
    /* } */

    /* dir = NULL; */
    /* while ( (dir=(char**)utarray_next(obazl_config.src_dirs,dir))) { */
    /*     log_debug("src dir: %s",*dir); */
    /* } */

    /* log_debug("linenoise config"); */
    linenoiseSetMultiLine(1);   /* always support multiline */

    /* Set the completion callback. This will be called every time the
     * user uses the <tab> key. */
    linenoiseSetCompletionCallback(completion);
    linenoiseSetHintsCallback(hints);

    /* Load history from file. The history file is just a plain text file
     * where entries are separated by newlines. */
    linenoiseHistoryLoad(history); /* Load the history at startup */

    /* Now this is the main loop of the typical linenoise-based application.
     * The call to linenoise() will block as long as the user types something
     * and presses enter.
     *
     * The typed string is returned as a malloc() allocated string by
     * linenoise, so the user needs to free() it. */

    while((line = linenoise("s7> ")) != NULL) {

        if (line[0] != '\0' && line[0] != '/') {
            snprintf(response, 1024, "(write %s)", line);
            s7_eval_c_string(s7, response);
            printf("%s", "\n");

            linenoiseHistoryAdd(line); /* Add to the history. */
            linenoiseHistorySave(history); /* Save the history on disk. */
        } else if (!strncmp(line,"/historylen",11)) {
            /* The "/historylen" command will change the history len. */
            int len = atoi(line+11);
            linenoiseHistorySetMaxLen(len);
        } else if (!strncmp(line, "/mask", 5)) {
            linenoiseMaskModeEnable();
        } else if (!strncmp(line, "/unmask", 7)) {
            linenoiseMaskModeDisable();
        } else if (line[0] == '/') {
            printf("Unreconized command: %s\n", line);
        }
        free(line);
    }
    return 0;
}
