/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_enum_nif.c.template
/// This file corresponds to HAPI_Transform struct from HAPI_Common.h

#include "../hapi_private_nif.h"
#include "../hapi_records_nif.h"
#include <string.h>


ERL_NIF_TERM
hapi_make_hapi_transform(ErlNifEnv* env, const HAPI_Transform* hapi_struct)
{
    return enif_make_tuple(env, 5,
        hapi_make_atom(env, "hapi_transform"),
        hapi_make_list_float(env, 3, hapi_struct->position),
        hapi_make_list_float(env, 4, hapi_struct->rotationQuaternion),
        hapi_make_list_float(env, 3, hapi_struct->scale),
        enif_make_int(env, (int32_t) hapi_struct->rstOrder));
}


bool
hapi_get_hapi_transform(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_Transform* hapi_struct)
{
    int32_t tuple_size = 0;
    const ERL_NIF_TERM* tuple_record = NULL;
    bool atom_name_match = false;

    float record_position[3];
    float record_rotation_quaternion[4];
    float record_scale[3];
    int32_t record_rst_order = 0;

    if(!enif_get_tuple(env, term, &tuple_size, &tuple_record) ||
        (tuple_size != 5) ||
        !hapi_check_atom(env, tuple_record[0], "hapi_transform", &atom_name_match) ||
        !atom_name_match ||
        !hapi_get_list_float(env, tuple_record[1], 3, &record_position[0]) ||
        !hapi_get_list_float(env, tuple_record[2], 4, &record_rotation_quaternion[0]) ||
        !hapi_get_list_float(env, tuple_record[3], 3, &record_scale[0]) ||
        !enif_get_int(env, tuple_record[4], &record_rst_order))
    {
        return false;
    }

    memcpy(&hapi_struct->position, &record_position[0], 3 * sizeof(float));
    memcpy(&hapi_struct->rotationQuaternion, &record_rotation_quaternion[0], 4 * sizeof(float));
    memcpy(&hapi_struct->scale, &record_scale[0], 3 * sizeof(float));
    hapi_struct->rstOrder = (HAPI_RSTOrder) record_rst_order;

    return true;
}
