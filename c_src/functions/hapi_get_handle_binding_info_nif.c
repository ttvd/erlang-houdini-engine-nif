/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_function_nif.c.template
/// This file corresponds to HAPI_GetHandleBindingInfo function from HAPI.h or HAPI_Common.h

#include "../hapi_private_nif.h"
#include "../hapi_enums_nif.h"
#include "../hapi_records_nif.h"
#include "../hapi_types_nif.h"


ERL_NIF_TERM
hapi_get_handle_binding_info_schedule(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int32_t param_handle_index = 0;
    HAPI_HandleBindingInfo param_handle_infos;
    int32_t param_start = 0;
    int32_t param_length = 0;

    //%{HAPI_FUNCTION_OUTPUT_VARS}%

    //%{HAPI_FUNCTION_CLEANUP}%

    //return hapi_make_atom_ok(env);
    return enif_make_badarg(env);
}


ERL_NIF_TERM
hapi_get_handle_binding_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_schedule_nif(env, "hapi_get_handle_binding_info_schedule", 0, hapi_get_handle_binding_info_schedule, argc, argv);
}
