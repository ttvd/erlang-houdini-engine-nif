#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_transform_component_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_TransformComponent* transform_component)
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

        if(!strcmp(atom_value, "hapi_transform_tx"))
        {
            *transform_component = HAPI_TRANSFORM_TX;
        }
        else if(!strcmp(atom_value, "hapi_transform_ty"))
        {
            *transform_component = HAPI_TRANSFORM_TY;
        }
        else if(!strcmp(atom_value, "hapi_transform_tz"))
        {
            *transform_component = HAPI_TRANSFORM_TZ;
        }
        else if(!strcmp(atom_value, "hapi_transform_rx"))
        {
            *transform_component = HAPI_TRANSFORM_RX;
        }
        else if(!strcmp(atom_value, "hapi_transform_ry"))
        {
            *transform_component = HAPI_TRANSFORM_RY;
        }
        else if(!strcmp(atom_value, "hapi_transform_rz"))
        {
            *transform_component = HAPI_TRANSFORM_RZ;
        }
        else if(!strcmp(atom_value, "hapi_transform_qx"))
        {
            *transform_component = HAPI_TRANSFORM_QX;
        }
        else if(!strcmp(atom_value, "hapi_transform_qy"))
        {
            *transform_component = HAPI_TRANSFORM_QY;
        }
        else if(!strcmp(atom_value, "hapi_transform_qz"))
        {
            *transform_component = HAPI_TRANSFORM_QZ;
        }
        else if(!strcmp(atom_value, "hapi_transform_qw"))
        {
            *transform_component = HAPI_TRANSFORM_QW;
        }
        else if(!strcmp(atom_value, "hapi_transform_sx"))
        {
            *transform_component = HAPI_TRANSFORM_SX;
        }
        else if(!strcmp(atom_value, "hapi_transform_sy"))
        {
            *transform_component = HAPI_TRANSFORM_SY;
        }
        else if(!strcmp(atom_value, "hapi_transform_sz"))
        {
            *transform_component = HAPI_TRANSFORM_SZ;
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


ERL_NIF_TERM hapi_enum_transform_component_c_to_erl(ErlNifEnv* env, HAPI_TransformComponent transform_component)
{
    switch(transform_component)
    {
        case HAPI_TRANSFORM_TX:
        {
            return hapi_private_make_atom(env, "hapi_transform_tx");
        }

        case HAPI_TRANSFORM_TY:
        {
            return hapi_private_make_atom(env, "hapi_transform_ty");
        }

        case HAPI_TRANSFORM_TZ:
        {
            return hapi_private_make_atom(env, "hapi_transform_tz");
        }

        case HAPI_TRANSFORM_RX:
        {
            return hapi_private_make_atom(env, "hapi_transform_rx");
        }

        case HAPI_TRANSFORM_RY:
        {
            return hapi_private_make_atom(env, "hapi_transform_ry");
        }

        case HAPI_TRANSFORM_RZ:
        {
            return hapi_private_make_atom(env, "hapi_transform_rz");
        }

        case HAPI_TRANSFORM_QX:
        {
            return hapi_private_make_atom(env, "hapi_transform_qx");
        }

        case HAPI_TRANSFORM_QY:
        {
            return hapi_private_make_atom(env, "hapi_transform_qy");
        }

        case HAPI_TRANSFORM_QZ:
        {
            return hapi_private_make_atom(env, "hapi_transform_qz");
        }

        case HAPI_TRANSFORM_QW:
        {
            return hapi_private_make_atom(env, "hapi_transform_qw");
        }

        case HAPI_TRANSFORM_SX:
        {
            return hapi_private_make_atom(env, "hapi_transform_sx");
        }

        case HAPI_TRANSFORM_SY:
        {
            return hapi_private_make_atom(env, "hapi_transform_sy");
        }

        case HAPI_TRANSFORM_SZ:
        {
            return hapi_private_make_atom(env, "hapi_transform_sz");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
