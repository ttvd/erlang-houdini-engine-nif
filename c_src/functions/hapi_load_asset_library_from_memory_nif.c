/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_function_nif.c.template
/// This file corresponds to HAPI_LoadAssetLibraryFromMemory function from HAPI.h or HAPI_Common.h

#include "../hapi_private_nif.h"
#include "../hapi_enums_nif.h"
#include "../hapi_records_nif.h"
#include "../hapi_types_nif.h"


ERL_NIF_TERM
hapi_load_asset_library_from_memory_schedule(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int32_t param_library_buffer_size = 0;
    bool param_allow_overwrite = false;

    //%{HAPI_FUNCTION_OUTPUT_VARS}%

    //%{HAPI_FUNCTION_CLEANUP}%

    //return hapi_make_atom_ok(env);
    return enif_make_badarg(env);
}


ERL_NIF_TERM
hapi_load_asset_library_from_memory(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_schedule_nif(env, "hapi_load_asset_library_from_memory_schedule", 0, hapi_load_asset_library_from_memory_schedule, argc, argv);
}
