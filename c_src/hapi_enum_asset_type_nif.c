#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_asset_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_AssetType* asset_type)
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

        if(!strcmp(atom_value, "hapi_assettype_invalid"))
        {
            *asset_type = HAPI_ASSETTYPE_INVALID;
        }
        else if(!strcmp(atom_value, "hapi_assettype_obj"))
        {
            *asset_type = HAPI_ASSETTYPE_OBJ;
        }
        else if(!strcmp(atom_value, "hapi_assettype_sop"))
        {
            *asset_type = HAPI_ASSETTYPE_SOP;
        }
        else if(!strcmp(atom_value, "hapi_assettype_popnet"))
        {
            *asset_type = HAPI_ASSETTYPE_POPNET;
        }
        else if(!strcmp(atom_value, "hapi_assettype_pop"))
        {
            *asset_type = HAPI_ASSETTYPE_POP;
        }
        else if(!strcmp(atom_value, "hapi_assettype_chopnet"))
        {
            *asset_type = HAPI_ASSETTYPE_CHOPNET;
        }
        else if(!strcmp(atom_value, "hapi_assettype_chop"))
        {
            *asset_type = HAPI_ASSETTYPE_CHOP;
        }
        else if(!strcmp(atom_value, "hapi_assettype_rop"))
        {
            *asset_type = HAPI_ASSETTYPE_ROP;
        }
        else if(!strcmp(atom_value, "hapi_assettype_shop"))
        {
            *asset_type = HAPI_ASSETTYPE_SHOP;
        }
        else if(!strcmp(atom_value, "hapi_assettype_cop2"))
        {
            *asset_type = HAPI_ASSETTYPE_COP2;
        }
        else if(!strcmp(atom_value, "hapi_assettype_copnet"))
        {
            *asset_type = HAPI_ASSETTYPE_COPNET;
        }
        else if(!strcmp(atom_value, "hapi_assettype_vop"))
        {
            *asset_type = HAPI_ASSETTYPE_VOP;
        }
        else if(!strcmp(atom_value, "hapi_assettype_vopnet"))
        {
            *asset_type = HAPI_ASSETTYPE_VOPNET;
        }
        else if(!strcmp(atom_value, "hapi_assettype_dop"))
        {
            *asset_type = HAPI_ASSETTYPE_DOP;
        }
        else if(!strcmp(atom_value, "hapi_assettype_mgr"))
        {
            *asset_type = HAPI_ASSETTYPE_MGR;
        }
        else if(!strcmp(atom_value, "hapi_assettype_dir"))
        {
            *asset_type = HAPI_ASSETTYPE_DIR;
        }
        else if(!strcmp(atom_value, "hapi_assettype_max"))
        {
            *asset_type = HAPI_ASSETTYPE_MAX;
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


ERL_NIF_TERM hapi_enum_asset_type_c_to_erl(ErlNifEnv* env, HAPI_AssetType asset_type)
{
    switch(asset_type)
    {
        /*
        case HAPI_ASSETTYPE_INVALID:
        {
            return hapi_private_make_atom(env, "hapi_assettype_invalid");
        }
        */

        case HAPI_ASSETTYPE_OBJ:
        {
            return hapi_private_make_atom(env, "hapi_assettype_obj");
        }

        case HAPI_ASSETTYPE_SOP:
        {
            return hapi_private_make_atom(env, "hapi_assettype_sop");
        }

        case HAPI_ASSETTYPE_POPNET:
        {
            return hapi_private_make_atom(env, "hapi_assettype_popnet");
        }

        case HAPI_ASSETTYPE_POP:
        {
            return hapi_private_make_atom(env, "hapi_assettype_pop");
        }

        case HAPI_ASSETTYPE_CHOPNET:
        {
            return hapi_private_make_atom(env, "hapi_assettype_chopnet");
        }

        case HAPI_ASSETTYPE_CHOP:
        {
            return hapi_private_make_atom(env, "hapi_assettype_chop");
        }

        case HAPI_ASSETTYPE_ROP:
        {
            return hapi_private_make_atom(env, "hapi_assettype_rop");
        }

        case HAPI_ASSETTYPE_SHOP:
        {
            return hapi_private_make_atom(env, "hapi_assettype_shop");
        }

        case HAPI_ASSETTYPE_COP2:
        {
            return hapi_private_make_atom(env, "hapi_assettype_cop2");
        }

        case HAPI_ASSETTYPE_COPNET:
        {
            return hapi_private_make_atom(env, "hapi_assettype_copnet");
        }

        case HAPI_ASSETTYPE_VOP:
        {
            return hapi_private_make_atom(env, "hapi_assettype_vop");
        }

        case HAPI_ASSETTYPE_VOPNET:
        {
            return hapi_private_make_atom(env, "hapi_assettype_vopnet");
        }

        case HAPI_ASSETTYPE_DOP:
        {
            return hapi_private_make_atom(env, "hapi_assettype_dop");
        }

        case HAPI_ASSETTYPE_MGR:
        {
            return hapi_private_make_atom(env, "hapi_assettype_mgr");
        }

        case HAPI_ASSETTYPE_DIR:
        {
            return hapi_private_make_atom(env, "hapi_assettype_dir");
        }

        case HAPI_ASSETTYPE_MAX:
        {
            return hapi_private_make_atom(env, "hapi_assettype_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
