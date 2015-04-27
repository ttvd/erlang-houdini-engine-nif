/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_enum_nif.c.template
/// This file corresponds to HAPI_MaterialInfo struct from HAPI_Common.h

#include "../hapi_private_nif.h"
#include "../hapi_records_nif.h"
#include <string.h>


ERL_NIF_TERM
hapi_make_hapi_materialinfo(ErlNifEnv* env, const HAPI_MaterialInfo* hapi_struct)
{
    return enif_make_tuple(env, 6,
        hapi_make_atom(env, "hapi_materialinfo"),
        enif_make_int(env, (int32_t) hapi_struct->id),
        enif_make_int(env, (int32_t) hapi_struct->assetId),
        enif_make_int(env, (int32_t) hapi_struct->nodeId),
        hapi_make_atom_bool(env, (bool) hapi_struct->exists),
        hapi_make_atom_bool(env, (bool) hapi_struct->hasChanged));
}


bool
hapi_get_hapi_materialinfo(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_MaterialInfo* hapi_struct)
{
    int32_t tuple_size = 0;
    const ERL_NIF_TERM* tuple_record = NULL;
    bool atom_name_match = false;

    int32_t record_id = 0;
    int32_t record_asset_id = 0;
    int32_t record_node_id = 0;
    bool record_exists = false;
    bool record_has_changed = false;

    if(!enif_get_tuple(env, term, &tuple_size, &tuple_record) ||
        (tuple_size != 6) ||
        !hapi_check_atom(env, tuple_record[0], "hapi_materialinfo", &atom_name_match) ||
        !atom_name_match ||
        !enif_get_int(env, tuple_record[1], &record_id) ||
        !enif_get_int(env, tuple_record[2], &record_asset_id) ||
        !enif_get_int(env, tuple_record[3], &record_node_id) ||
        !hapi_get_atom_bool(env, tuple_record[4], &record_exists) ||
        !hapi_get_atom_bool(env, tuple_record[5], &record_has_changed))
    {
        return false;
    }

    hapi_struct->id = (HAPI_MaterialId) record_id;
    hapi_struct->assetId = (HAPI_AssetId) record_asset_id;
    hapi_struct->nodeId = (HAPI_NodeId) record_node_id;
    hapi_struct->exists = (HAPI_Bool) record_exists;
    hapi_struct->hasChanged = (HAPI_Bool) record_has_changed;

    return true;
}
