/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_function_nif.c.template
/// This file corresponds to HAPI_SaveGeoToFile function from HAPI.h or HAPI_Common.h

#include "../hapi_private_nif.h"
#include "../hapi_enums_nif.h"
#include "../hapi_records_nif.h"


ERL_NIF_TERM
hapi_save_geo_to_file_schedule(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    //%{HAPI_FUNCTION_VARS}%

    //return hapi_make_atom_ok(env);
    return enif_make_badarg(env);
}


ERL_NIF_TERM
hapi_save_geo_to_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_schedule_nif(env, "hapi_save_geo_to_file_schedule", 0, hapi_save_geo_to_file_schedule, argc, argv);
}
