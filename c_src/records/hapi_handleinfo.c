/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_enum_nif.c.template
/// This file corresponds to HAPI_HandleInfo struct from HAPI_Common.h

#include "../hapi_private_nif.h"
#include "../hapi_records_nif.h"
#include <string.h>


ERL_NIF_TERM
hapi_make_hapi_handleinfo(ErlNifEnv* env, const HAPI_HandleInfo* hapi_struct)
{
    return enif_make_tuple(env, 4,
        hapi_make_atom(env, "hapi_handleinfo"),
        enif_make_int(env, (int32_t) hapi_struct->nameSH),
        enif_make_int(env, (int32_t) hapi_struct->typeNameSH),
        enif_make_int(env, hapi_struct->bindingsCount));
}


bool
hapi_get_hapi_handleinfo(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_HandleInfo* hapi_struct)
{
    int32_t tuple_size = 0;
    const ERL_NIF_TERM* tuple_record = NULL;
    bool atom_name_match = false;

    int32_t record_name_sh = 0;
    int32_t record_type_name_sh = 0;
    int32_t record_bindings_count = 0;

    if(!enif_get_tuple(env, term, &tuple_size, &tuple_record) ||
        (tuple_size != 4) ||
        !hapi_check_atom(env, tuple_record[0], "hapi_handleinfo", &atom_name_match) ||
        !atom_name_match ||
        !enif_get_int(env, tuple_record[1], &record_name_sh) ||
        !enif_get_int(env, tuple_record[2], &record_type_name_sh) ||
        !enif_get_int(env, tuple_record[3], &record_bindings_count))
    {
        return false;
    }

    hapi_struct->nameSH = (HAPI_StringHandle) record_name_sh;
    hapi_struct->typeNameSH = (HAPI_StringHandle) record_type_name_sh;
    hapi_struct->bindingsCount = record_bindings_count;

    return true;
}
