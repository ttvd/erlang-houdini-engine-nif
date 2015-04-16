#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_ramp_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_RampType* ramp_type)
{
    bool nif_success = true;

    uint32_t atom_len = 0;
    char* atom_value = NULL;

    if(enif_is_atom(env, term))
    {
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

        if(!strcmp(atom_value, "hapi_ramptype_invalid"))
        {
            *ramp_type = HAPI_RAMPTYPE_INVALID;
        }
        else if(!strcmp(atom_value, "hapi_ramptype_float"))
        {
            *ramp_type = HAPI_RAMPTYPE_FLOAT;
        }
        else if(!strcmp(atom_value, "hapi_ramptype_color"))
        {
            *ramp_type = HAPI_RAMPTYPE_COLOR;
        }
        else if(!strcmp(atom_value, "hapi_ramptype_max"))
        {
            *ramp_type = HAPI_RAMPTYPE_MAX;
        }
        else
        {
            nif_success = false;
        }
    }
    else
    {
        nif_success = false;
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
            return hapi_private_make_atom(env, "hapi_ramptype_invalid");
        }
        */

        case HAPI_RAMPTYPE_FLOAT:
        {
            return hapi_private_make_atom(env, "hapi_ramptype_float");
        }

        case HAPI_RAMPTYPE_COLOR:
        {
            return hapi_private_make_atom(env, "hapi_ramptype_color");
        }

        case HAPI_RAMPTYPE_MAX:
        {
            return hapi_private_make_atom(env, "hapi_ramptype_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
