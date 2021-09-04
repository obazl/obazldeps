#include <dirent.h>
#include <errno.h>
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>             /* PATH_MAX */
#endif
#include <stdio.h>
#ifndef WIN32
# include <unistd.h>
#else
# define ssize_t int
# include <io.h>
# include <sys/types.h>
#endif
#include <fcntl.h>
#include <stdlib.h>

#include <string.h>

#include "s7.h"
#include "utarray.h"
#include "uthash.h"
#include "utstring.h"

#include "log.h"

#include "codept_parser.h"

EXPORT struct obazl_deps_s *obazl_deps_parse_file(char *fname)
{
    /* FIXME: handle corrupted input */
    log_debug("obazl_deps_parse_file: %s", fname);
    s7_pointer port = s7_open_input_file(s7, fname, "r");
    s7_pointer fn = s7_name_to_value(s7, "read");
    s7_pointer obj = s7_read(s7, port);
    errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
    /* if we got something, wrap it in "[]" */
    if ((errmsg) && (*errmsg)) {
        log_error("[%s\n]", errmsg);
        s7_quit(s7);
        exit(EXIT_FAILURE);
    }
    log_debug("TEST %s", s7_object_to_c_string(s7, obj));
    /* obj = s7_read(s7, port); */
    /* errmsg = s7_get_output_string(s7, s7_current_error_port(s7)); */
    /* /\* if we got something, wrap it in "[]" *\/ */
    /* if ((errmsg) && (*errmsg)) { */
    /*     log_error("[%s\n]", errmsg); */
    /*     s7_quit(s7); */
    /*     exit(EXIT_FAILURE); */
    /* } */
    /* log_debug("TEST2 %s", s7_object_to_c_string(s7, obj)); */

    log_debug("len %d", s7_list_length(s7, obj));

    s7_pointer version = s7_car(obj);
    log_debug("version %s", s7_object_to_c_string(s7, version));

    s7_pointer deps = s7_cadr(obj);
    log_debug("deps %s", s7_object_to_c_string(s7, deps));

    s7_pointer locals = s7_caddr(obj);
    log_debug("locals %s", s7_object_to_c_string(s7, locals));

    s7_pointer libs = s7_cadddr(obj);
    log_debug("libs %s", s7_object_to_c_string(s7, libs));

    s7_pointer missing = s7_car(s7_cdr(s7_cdddr(obj)));
    log_debug("missing %s", s7_object_to_c_string(s7, missing));

    return NULL;
}
