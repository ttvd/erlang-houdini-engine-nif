/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_function_nif.c.template
/// This file corresponds to HAPI_SetAttributeFloatData function from HAPI.h or HAPI_Common.h

#include "../hapi_private_nif.h"
#include "../hapi_enums_nif.h"
#include "../hapi_records_nif.h"
#include "../hapi_types_nif.h"


ERL_NIF_TERM
hapi_set_attribute_float_data_schedule(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    HAPI_AttributeInfo param_attr_info;
    float param_data = 0.0f;
    int32_t param_start = 0;
    int32_t param_length = 0;

    //%{HAPI_FUNCTION_OUTPUT_VARS}%

    //%{HAPI_FUNCTION_CLEANUP}%

    //return hapi_make_atom_ok(env);
    return enif_make_badarg(env);
}


ERL_NIF_TERM
hapi_set_attribute_float_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_schedule_nif(env, "hapi_set_attribute_float_data_schedule", 0, hapi_set_attribute_float_data_schedule, argc, argv);
}
