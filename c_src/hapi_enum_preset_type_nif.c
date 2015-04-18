#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <string.h>


bool hapi_enum_preset_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_PresetType* preset_type)
{
    bool nif_success = true;

    uint32_t atom_hash = 0;
    int32_t tuple_size = 0;
    const ERL_NIF_TERM* hash_tuple = NULL;

    char* atom_value = NULL;

    if(enif_is_tuple(env, term) && enif_get_tuple(env, term, &tuple_size, &hash_tuple) && (2 == tuple_size))
    {
        if(!enif_get_uint(env, hash_tuple[1], &atom_hash))
        {
            nif_success = false;
            goto label_cleanup;
        }
    }
    else if(enif_is_atom(env, term))
    {
        uint32_t atom_len = 0;

        if(!enif_get_atom_length(env, term, &atom_len, ERL_NIF_LATIN1))
        {
            nif_success = false;
            goto label_cleanup;
        }

        atom_value = malloc(atom_len + 1);
        memset(atom_value, 0, atom_len + 1);

        if(!enif_get_atom(env, term, atom_value, atom_len + 1, ERL_NIF_LATIN1))
        {
            nif_success = false;
            goto label_cleanup;
        }

        atom_hash = XXH32(atom_value, strlen(atom_value), 0);
    }
    else if(!enif_get_uint(env, term, &atom_hash))
    {
        nif_success = false;
        goto label_cleanup;
    }

    switch(atom_hash)
    {
        // "hapi_presettype_invalid"
        case 3370572381:
        {
            *preset_type = HAPI_PRESETTYPE_INVALID;
            break;
        }

        // "hapi_presettype_binary"
        case 1629204808:
        {
            *preset_type = HAPI_PRESETTYPE_BINARY;
            break;
        }

        // "hapi_presettype_idx"
        case 2476503831:
        {
            *preset_type = HAPI_PRESETTYPE_IDX;
            break;
        }

        // "hapi_presettype_max"
        case 1183497328:
        {
            *preset_type = HAPI_PRESETTYPE_MAX;
            break;
        }

        default:
        {
            nif_success = false;
            break;
        }
    }

label_cleanup:

    if(atom_value)
    {
        free(atom_value);
    }

    return nif_success;
}


ERL_NIF_TERM hapi_enum_preset_type_c_to_erl(ErlNifEnv* env, HAPI_PresetType preset_type)
{
    switch(preset_type)
    {
        /*
        case HAPI_PRESETTYPE_INVALID:
        {
            return hapi_private_make_hash_tuple(env, "hapi_presettype_invalid");
        }
        */

        case HAPI_PRESETTYPE_BINARY:
        {
            return hapi_private_make_hash_tuple(env, "hapi_presettype_binary");
        }

        case HAPI_PRESETTYPE_IDX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_presettype_idx");
        }

        case HAPI_PRESETTYPE_MAX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_presettype_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
