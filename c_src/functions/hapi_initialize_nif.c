/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_function_nif.c.template
/// This file corresponds to HAPI_Initialize function from HAPI.h or HAPI_Common.h

#include "../hapi_private_nif.h"
#include "../hapi_enums_nif.h"
#include "../hapi_records_nif.h"


ERL_NIF_TERM
hapi_initialize_schedule(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* Input parameters. */
    char* param_otl_search_path = NULL;
    char* param_dso_search_path = NULL;
    HAPI_CookOptions param_cook_options;
    HAPI_Bool param_use_cooking_thread = false;
    int32_t param_cooking_thread_stack_size = 0;
    /* No output parameters. */

    //return hapi_make_atom_ok(env);
    return enif_make_badarg(env);
}


ERL_NIF_TERM
hapi_initialize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_schedule_nif(env, "hapi_initialize_schedule", 0, hapi_initialize_schedule, argc, argv);
}
