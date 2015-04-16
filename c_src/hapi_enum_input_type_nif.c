#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_input_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_InputType* input_type)
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
            // "hapi_input_invalid"
            case 831007676:
            {
                *input_type = HAPI_INPUT_INVALID;
            }

            // "hapi_input_transform"
            case 1095021568:
            {
                *input_type = HAPI_INPUT_TRANSFORM;
            }

            // "hapi_input_geometry"
            case 2583127688:
            {
                *input_type = HAPI_INPUT_GEOMETRY;
            }

            // "hapi_input_max"
            case 4193495948:
            {
                *input_type = HAPI_INPUT_MAX;
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


ERL_NIF_TERM hapi_enum_input_type_c_to_erl(ErlNifEnv* env, HAPI_InputType input_type)
{
    switch(input_type)
    {
        /*
        case HAPI_INPUT_INVALID:
        {
            return hapi_private_make_hash_tuple(env, "hapi_input_invalid");
        }
        */

        case HAPI_INPUT_TRANSFORM:
        {
            return hapi_private_make_hash_tuple(env, "hapi_input_transform");
        }

        case HAPI_INPUT_GEOMETRY:
        {
            return hapi_private_make_hash_tuple(env, "hapi_input_geometry");
        }

        case HAPI_INPUT_MAX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_input_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
