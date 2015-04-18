#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <stdio.h>


bool hapi_enum_attribute_owner_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_AttributeOwner* attribute_owner)
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
        // "hapi_attrowner_invalid"
        case 679692306:
        {
            *attribute_owner = HAPI_ATTROWNER_INVALID;
            break;
        }

        // "hapi_attrowner_vertex"
        case 3318397195:
        {
            *attribute_owner = HAPI_ATTROWNER_VERTEX;
            break;
        }

        // "hapi_attrowner_point"
        case 902726316:
        {
            *attribute_owner = HAPI_ATTROWNER_POINT;
            break;
        }

        // "hapi_attrowner_prim"
        case 3208328655:
        {
            *attribute_owner = HAPI_ATTROWNER_PRIM;
            break;
        }

        // "hapi_attrowner_detail"
        case 1131849991:
        {
            *attribute_owner = HAPI_ATTROWNER_DETAIL;
            break;
        }

        // "hapi_attrowner_max"
        case 4198344184:
        {
            *attribute_owner = HAPI_ATTROWNER_MAX;
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


ERL_NIF_TERM hapi_enum_attribute_owner_c_to_erl(ErlNifEnv* env, HAPI_AttributeOwner attribute_owner)
{
    switch(attribute_owner)
    {
        /*
        case HAPI_ATTROWNER_INVALID:
        {
            return hapi_private_make_hash_tuple(env, "hapi_attrowner_invalid");
        }
        */

        case HAPI_ATTROWNER_VERTEX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_attrowner_vertex");
        }

        case HAPI_ATTROWNER_POINT:
        {
            return hapi_private_make_hash_tuple(env, "hapi_attrowner_point");
        }

        case HAPI_ATTROWNER_PRIM:
        {
            return hapi_private_make_hash_tuple(env, "hapi_attrowner_prim");
        }

        case HAPI_ATTROWNER_DETAIL:
        {
            return hapi_private_make_hash_tuple(env, "hapi_attrowner_detail");
        }

        case HAPI_ATTROWNER_MAX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_attrowner_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
