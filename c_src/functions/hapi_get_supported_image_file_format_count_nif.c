/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_function_nif.c.template
/// This file corresponds to HAPI_GetSupportedImageFileFormatCount function from HAPI.h or HAPI_Common.h

#include "../hapi_private_nif.h"
#include "../hapi_enums_nif.h"
#include "../hapi_records_nif.h"


ERL_NIF_TERM
hapi_get_supported_image_file_format_count_schedule(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* No input parameters. */
    /* No output parameters. */

    //return hapi_make_atom_ok(env);
    return enif_make_badarg(env);
}


ERL_NIF_TERM
hapi_get_supported_image_file_format_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_schedule_nif(env, "hapi_get_supported_image_file_format_count_schedule", 0, hapi_get_supported_image_file_format_count_schedule, argc, argv);
}
