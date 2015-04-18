#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <stdio.h>


bool hapi_enum_ramp_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_RampType* ramp_type)
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
        // "hapi_ramptype_invalid"
        case 1007057020:
        {
            *ramp_type = HAPI_RAMPTYPE_INVALID;
            break;
        }

        // "hapi_ramptype_float"
        case 1236730525:
        {
            *ramp_type = HAPI_RAMPTYPE_FLOAT;
            break;
        }

        // "hapi_ramptype_color"
        case 784329284:
        {
            *ramp_type = HAPI_RAMPTYPE_COLOR;
            break;
        }

        // "hapi_ramptype_max"
        case 1883049949:
        {
            *ramp_type = HAPI_RAMPTYPE_MAX;
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


ERL_NIF_TERM hapi_enum_ramp_type_c_to_erl(ErlNifEnv* env, HAPI_RampType ramp_type)
{
    switch(ramp_type)
    {
        /*
        case HAPI_RAMPTYPE_INVALID:
        {
            return hapi_private_make_hash_tuple(env, "hapi_ramptype_invalid");
        }
        */

        case HAPI_RAMPTYPE_FLOAT:
        {
            return hapi_private_make_hash_tuple(env, "hapi_ramptype_float");
        }

        case HAPI_RAMPTYPE_COLOR:
        {
            return hapi_private_make_hash_tuple(env, "hapi_ramptype_color");
        }

        case HAPI_RAMPTYPE_MAX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_ramptype_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
