/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_enum_nif.c.template
/// This file corresponds to HAPI_TransformEuler struct from HAPI_Common.h

#include "../hapi_private_nif.h"
#include "../hapi_records_nif.h"
#include "../hapi_enums_nif.h"
#include <string.h>


ERL_NIF_TERM
hapi_make_hapi_transform_euler(ErlNifEnv* env, const HAPI_TransformEuler* hapi_struct)
{
    return enif_make_tuple(env, 6,
        hapi_make_atom(env, "hapi_transform_euler"),
        hapi_make_list_float(env, 3, hapi_struct->position),
        hapi_make_list_float(env, 3, hapi_struct->rotationEuler),
        hapi_make_list_float(env, 3, hapi_struct->scale),
        hapi_make_hapi_xyzorder(env, hapi_struct->rotationOrder),
        hapi_make_hapi_rstorder(env, hapi_struct->rstOrder));
}


bool
hapi_get_hapi_transform_euler(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_TransformEuler* hapi_struct)
{
    int32_t tuple_size = 0;
    const ERL_NIF_TERM* tuple_record = NULL;
    bool atom_name_match = false;

    float record_position[3];
    float record_rotation_euler[3];
    float record_scale[3];
    HAPI_XYZOrder record_rotation_order;
    HAPI_RSTOrder record_rst_order;

    if(!enif_get_tuple(env, term, &tuple_size, &tuple_record) ||
        (tuple_size != 6) ||
        !hapi_check_atom(env, tuple_record[0], "hapi_transform_euler", &atom_name_match) ||
        !atom_name_match ||
        !hapi_get_list_float(env, tuple_record[1], 3, &record_position[0]) ||
        !hapi_get_list_float(env, tuple_record[2], 3, &record_rotation_euler[0]) ||
        !hapi_get_list_float(env, tuple_record[3], 3, &record_scale[0]) ||
        !hapi_get_hapi_xyzorder(env, tuple_record[4], &record_rotation_order) ||
        !hapi_get_hapi_rstorder(env, tuple_record[5], &record_rst_order))
    {
        return false;
    }

    memcpy(&hapi_struct->position, &record_position[0], 3 * sizeof(float));
    memcpy(&hapi_struct->rotationEuler, &record_rotation_euler[0], 3 * sizeof(float));
    memcpy(&hapi_struct->scale, &record_scale[0], 3 * sizeof(float));
    hapi_struct->rotationOrder = record_rotation_order;
    hapi_struct->rstOrder = record_rst_order;

    return true;
}


ERL_NIF_TERM
hapi_make_hapi_transform_euler_list(ErlNifEnv* env, const HAPI_TransformEuler* hapi_structs, int32_t list_size)
{
    ERL_NIF_TERM list = enif_make_list(env, 0);

    for(int32_t idx = list_size - 1; idx >= 0; idx--)
    {
        const HAPI_TransformEuler* hapi_struct = hapi_structs + idx;
        list = enif_make_list_cell(env, hapi_make_hapi_transform_euler(env, hapi_struct), list);
    }

    return list;
}


bool
hapi_get_hapi_transform_euler_list(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_TransformEuler* hapi_structs, int32_t list_size)
{
    uint32_t read_list_size = 0;
    ERL_NIF_TERM head, tail;

    if(enif_get_list_length(env, term, &read_list_size) && (list_size == read_list_size))
    {
        ERL_NIF_TERM list = term;
        int32_t index = 0;

        while(enif_get_list_cell(env, list, &head, &tail))
        {
            HAPI_TransformEuler* hapi_struct = hapi_structs + index;

            if(!hapi_get_hapi_transform_euler(env, head, hapi_struct))
            {
                return false;
            }

            index++;
            list = tail;
        }

        return true;
    }

    return false;
}
