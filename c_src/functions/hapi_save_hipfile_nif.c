/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_function_nif.c.template
/// This file corresponds to HAPI_SaveHIPFile function from HAPI.h or HAPI_Common.h

#include "../hapi_private_nif.h"
#include "../hapi_enums_nif.h"
#include "../hapi_records_nif.h"


ERL_NIF_TERM
hapi_save_hipfile_schedule(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* Input parameters. */
    char* param_file_path = NULL;
    /* No output parameters. */

    //return hapi_make_atom_ok(env);
    return enif_make_badarg(env);
}


ERL_NIF_TERM
hapi_save_hipfile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_schedule_nif(env, "hapi_save_hipfile_schedule", 0, hapi_save_hipfile_schedule, argc, argv);
}
