/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_function_nif.c.template
/// This file corresponds to HAPI_DestroyAsset function from HAPI.h or HAPI_Common.h

#include "../hapi_private_nif.h"
#include "../hapi_enums_nif.h"
#include "../hapi_records_nif.h"


ERL_NIF_TERM
hapi_destroy_asset_schedule(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* Input parameters. */
    HAPI_AssetId param_asset_id = -1;
    /* No output parameters. */

    //return hapi_make_atom_ok(env);
    return enif_make_badarg(env);
}


ERL_NIF_TERM
hapi_destroy_asset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_schedule_nif(env, "hapi_destroy_asset_schedule", 0, hapi_destroy_asset_schedule, argc, argv);
}
