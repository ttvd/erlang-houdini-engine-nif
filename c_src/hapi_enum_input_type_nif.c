#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_input_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_InputType* input_type)
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

        if(!strcmp(atom_value, "hapi_input_invalid"))
        {
            *input_type = HAPI_INPUT_INVALID;
        }
        else if(!strcmp(atom_value, "hapi_input_transform"))
        {
            *input_type = HAPI_INPUT_TRANSFORM;
        }
        else if(!strcmp(atom_value, "hapi_input_geometry"))
        {
            *input_type = HAPI_INPUT_GEOMETRY;
        }
        else if(!strcmp(atom_value, "hapi_input_max"))
        {
            *input_type = HAPI_INPUT_MAX;
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


ERL_NIF_TERM hapi_enum_input_type_c_to_erl(ErlNifEnv* env, HAPI_InputType input_type)
{
    switch(input_type)
    {
        /*
        case HAPI_INPUT_INVALID:
        {
            return hapi_private_make_atom(env, "hapi_input_invalid");
        }
        */

        case HAPI_INPUT_TRANSFORM:
        {
            return hapi_private_make_atom(env, "hapi_input_transform");
        }

        case HAPI_INPUT_GEOMETRY:
        {
            return hapi_private_make_atom(env, "hapi_input_geometry");
        }

        case HAPI_INPUT_MAX:
        {
            return hapi_private_make_atom(env, "hapi_input_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
