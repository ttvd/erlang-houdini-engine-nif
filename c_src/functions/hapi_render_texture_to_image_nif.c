/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_function_nif.c.template
/// This file corresponds to HAPI_RenderTextureToImage function from HAPI.h or HAPI_Common.h

#include "../hapi_private_nif.h"
#include "../hapi_enums_nif.h"
#include "../hapi_records_nif.h"


ERL_NIF_TERM
hapi_render_texture_to_image_schedule(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    

    //%{HAPI_FUNCTION_OUTPUT_VARS}%

    //%{HAPI_FUNCTION_CLEANUP}%

    //return hapi_make_atom_ok(env);
    return enif_make_badarg(env);
}


ERL_NIF_TERM
hapi_render_texture_to_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_schedule_nif(env, "hapi_render_texture_to_image_schedule", 0, hapi_render_texture_to_image_schedule, argc, argv);
}
