#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_asset_sub_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_AssetSubType* asset_sub_type)
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

        if(!strcmp(atom_value, "hapi_assetsubtype_invalid"))
        {
            *asset_sub_type = HAPI_ASSETSUBTYPE_INVALID;
        }
        else if(!strcmp(atom_value, "hapi_assetsubtype_default"))
        {
            *asset_sub_type = HAPI_ASSETSUBTYPE_DEFAULT;
        }
        else if(!strcmp(atom_value, "hapi_assetsubtype_curve"))
        {
            *asset_sub_type = HAPI_ASSETSUBTYPE_CURVE;
        }
        else if(!strcmp(atom_value, "hapi_assetsubtype_input"))
        {
            *asset_sub_type = HAPI_ASSETSUBTYPE_INPUT;
        }
        else if(!strcmp(atom_value, "hapi_assetsubtype_max"))
        {
            *asset_sub_type = HAPI_ASSETSUBTYPE_MAX;
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


ERL_NIF_TERM hapi_enum_asset_sub_type_c_to_erl(ErlNifEnv* env, HAPI_AssetSubType asset_sub_type)
{
    switch(asset_sub_type)
    {
        /*
        case HAPI_ASSETSUBTYPE_INVALID:
        {
            return hapi_private_make_atom(env, "hapi_assetsubtype_invalid");
        }
        */

        case HAPI_ASSETSUBTYPE_DEFAULT:
        {
            return hapi_private_make_atom(env, "hapi_assetsubtype_default");
        }

        case HAPI_ASSETSUBTYPE_CURVE:
        {
            return hapi_private_make_atom(env, "hapi_assetsubtype_curve");
        }

        case HAPI_ASSETSUBTYPE_INPUT:
        {
            return hapi_private_make_atom(env, "hapi_assetsubtype_input");
        }

        case HAPI_ASSETSUBTYPE_MAX:
        {
            return hapi_private_make_atom(env, "hapi_assetsubtype_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
