/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_function_nif.c.template
/// This file corresponds to HAPI_SetAttributeIntData function from HAPI.h or HAPI_Common.h

#include "../hapi_private_nif.h"
#include "../hapi_enums_nif.h"
#include "../hapi_records_nif.h"


ERL_NIF_TERM
hapi_set_attribute_int_data_schedule(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* Input parameters. */
    HAPI_AssetId param_asset_id = -1;
    HAPI_ObjectId param_object_id = -1;
    HAPI_GeoId param_geo_id = -1;
    char* param_name = NULL;
    HAPI_AttributeInfo param_attr_info;
    int32_t param_data = 0;
    int32_t param_start = 0;
    int32_t param_length = 0;
    /* No output parameters. */

    //return hapi_make_atom_ok(env);
    return enif_make_badarg(env);
}


ERL_NIF_TERM
hapi_set_attribute_int_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_schedule_nif(env, "hapi_set_attribute_int_data_schedule", 0, hapi_set_attribute_int_data_schedule, argc, argv);
}
