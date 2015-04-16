#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_preset_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_PresetType* preset_type)
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

        if(!strcmp(atom_value, "hapi_presettype_invalid"))
        {
            *preset_type = HAPI_PRESETTYPE_INVALID;
        }
        else if(!strcmp(atom_value, "hapi_presettype_binary"))
        {
            *preset_type = HAPI_PRESETTYPE_BINARY;
        }
        else if(!strcmp(atom_value, "hapi_presettype_idx"))
        {
            *preset_type = HAPI_PRESETTYPE_IDX;
        }
        else if(!strcmp(atom_value, "hapi_presettype_max"))
        {
            *preset_type = HAPI_PRESETTYPE_MAX;
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


ERL_NIF_TERM hapi_enum_preset_type_c_to_erl(ErlNifEnv* env, HAPI_PresetType preset_type)
{
    switch(preset_type)
    {
        /*
        case HAPI_PRESETTYPE_INVALID:
        {
            return hapi_private_make_atom(env, "hapi_presettype_invalid");
        }
        */

        case HAPI_PRESETTYPE_BINARY:
        {
            return hapi_private_make_atom(env, "hapi_presettype_binary");
        }

        case HAPI_PRESETTYPE_IDX:
        {
            return hapi_private_make_atom(env, "hapi_presettype_idx");
        }

        case HAPI_PRESETTYPE_MAX:
        {
            return hapi_private_make_atom(env, "hapi_presettype_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
