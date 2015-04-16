#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_attribute_owner_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_AttributeOwner* attribute_owner)
{
    bool nif_success = true;

    uint32_t atom_hash = 0;
    int32_t tuple_size = 0;
    const ERL_NIF_TERM* hash_tuple = NULL;

    if(enif_is_tuple(env, term) && enif_get_tuple(env, term, &tuple_size, &hash_tuple) && (2 == tuple_size))
    {
        if(!enif_get_uint(env, hash_tuple[1], &atom_hash))
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
            }

            // "hapi_attrowner_vertex"
            case 3318397195:
            {
                *attribute_owner = HAPI_ATTROWNER_VERTEX;
            }

            // "hapi_attrowner_point"
            case 902726316:
            {
                *attribute_owner = HAPI_ATTROWNER_POINT;
            }

            // "hapi_attrowner_prim"
            case 3208328655:
            {
                *attribute_owner = HAPI_ATTROWNER_PRIM;
            }

            // "hapi_attrowner_detail"
            case 1131849991:
            {
                *attribute_owner = HAPI_ATTROWNER_DETAIL;
            }

            // "hapi_attrowner_max"
            case 4198344184:
            {
                *attribute_owner = HAPI_ATTROWNER_MAX;
            }

            default:
            {
                break;
            }
        }
    }

label_cleanup:

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
