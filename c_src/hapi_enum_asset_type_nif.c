#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <string.h>


bool hapi_enum_asset_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_AssetType* asset_type)
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
        // "hapi_assettype_invalid"
        case 3266553345:
        {
            *asset_type = HAPI_ASSETTYPE_INVALID;
            break;
        }

        // "hapi_assettype_obj"
        case 3893167597:
        {
            *asset_type = HAPI_ASSETTYPE_OBJ;
            break;
        }

        // "hapi_assettype_sop"
        case 1581980326:
        {
            *asset_type = HAPI_ASSETTYPE_SOP;
            break;
        }

        // "hapi_assettype_popnet"
        case 870436306:
        {
            *asset_type = HAPI_ASSETTYPE_POPNET;
            break;
        }

        // "hapi_assettype_pop"
        case 3433094276:
        {
            *asset_type = HAPI_ASSETTYPE_POP;
            break;
        }

        // "hapi_assettype_chopnet"
        case 1965316406:
        {
            *asset_type = HAPI_ASSETTYPE_CHOPNET;
            break;
        }

        // "hapi_assettype_chop"
        case 2466487462:
        {
            *asset_type = HAPI_ASSETTYPE_CHOP;
            break;
        }

        // "hapi_assettype_rop"
        case 523518409:
        {
            *asset_type = HAPI_ASSETTYPE_ROP;
            break;
        }

        // "hapi_assettype_shop"
        case 3062776791:
        {
            *asset_type = HAPI_ASSETTYPE_SHOP;
            break;
        }

        // "hapi_assettype_cop2"
        case 3777195680:
        {
            *asset_type = HAPI_ASSETTYPE_COP2;
            break;
        }

        // "hapi_assettype_copnet"
        case 3179476083:
        {
            *asset_type = HAPI_ASSETTYPE_COPNET;
            break;
        }

        // "hapi_assettype_vop"
        case 3357604912:
        {
            *asset_type = HAPI_ASSETTYPE_VOP;
            break;
        }

        // "hapi_assettype_vopnet"
        case 3064806235:
        {
            *asset_type = HAPI_ASSETTYPE_VOPNET;
            break;
        }

        // "hapi_assettype_dop"
        case 4248199486:
        {
            *asset_type = HAPI_ASSETTYPE_DOP;
            break;
        }

        // "hapi_assettype_mgr"
        case 1777286259:
        {
            *asset_type = HAPI_ASSETTYPE_MGR;
            break;
        }

        // "hapi_assettype_dir"
        case 3193664342:
        {
            *asset_type = HAPI_ASSETTYPE_DIR;
            break;
        }

        // "hapi_assettype_max"
        case 483989670:
        {
            *asset_type = HAPI_ASSETTYPE_MAX;
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


ERL_NIF_TERM hapi_enum_asset_type_c_to_erl(ErlNifEnv* env, HAPI_AssetType asset_type)
{
    switch(asset_type)
    {
        /*
        case HAPI_ASSETTYPE_INVALID:
        {
            return hapi_private_make_hash_tuple(env, "hapi_assettype_invalid");
        }
        */

        case HAPI_ASSETTYPE_OBJ:
        {
            return hapi_private_make_hash_tuple(env, "hapi_assettype_obj");
        }

        case HAPI_ASSETTYPE_SOP:
        {
            return hapi_private_make_hash_tuple(env, "hapi_assettype_sop");
        }

        case HAPI_ASSETTYPE_POPNET:
        {
            return hapi_private_make_hash_tuple(env, "hapi_assettype_popnet");
        }

        case HAPI_ASSETTYPE_POP:
        {
            return hapi_private_make_hash_tuple(env, "hapi_assettype_pop");
        }

        case HAPI_ASSETTYPE_CHOPNET:
        {
            return hapi_private_make_hash_tuple(env, "hapi_assettype_chopnet");
        }

        case HAPI_ASSETTYPE_CHOP:
        {
            return hapi_private_make_hash_tuple(env, "hapi_assettype_chop");
        }

        case HAPI_ASSETTYPE_ROP:
        {
            return hapi_private_make_hash_tuple(env, "hapi_assettype_rop");
        }

        case HAPI_ASSETTYPE_SHOP:
        {
            return hapi_private_make_hash_tuple(env, "hapi_assettype_shop");
        }

        case HAPI_ASSETTYPE_COP2:
        {
            return hapi_private_make_hash_tuple(env, "hapi_assettype_cop2");
        }

        case HAPI_ASSETTYPE_COPNET:
        {
            return hapi_private_make_hash_tuple(env, "hapi_assettype_copnet");
        }

        case HAPI_ASSETTYPE_VOP:
        {
            return hapi_private_make_hash_tuple(env, "hapi_assettype_vop");
        }

        case HAPI_ASSETTYPE_VOPNET:
        {
            return hapi_private_make_hash_tuple(env, "hapi_assettype_vopnet");
        }

        case HAPI_ASSETTYPE_DOP:
        {
            return hapi_private_make_hash_tuple(env, "hapi_assettype_dop");
        }

        case HAPI_ASSETTYPE_MGR:
        {
            return hapi_private_make_hash_tuple(env, "hapi_assettype_mgr");
        }

        case HAPI_ASSETTYPE_DIR:
        {
            return hapi_private_make_hash_tuple(env, "hapi_assettype_dir");
        }

        case HAPI_ASSETTYPE_MAX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_assettype_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
