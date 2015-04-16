#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_x_y_z_order_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_XYZOrder* x_y_z_order)
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

        if(!strcmp(atom_value, "hapi_xyz"))
        {
            *x_y_z_order = HAPI_XYZ;
        }
        else if(!strcmp(atom_value, "hapi_xzy"))
        {
            *x_y_z_order = HAPI_XZY;
        }
        else if(!strcmp(atom_value, "hapi_yxz"))
        {
            *x_y_z_order = HAPI_YXZ;
        }
        else if(!strcmp(atom_value, "hapi_yzx"))
        {
            *x_y_z_order = HAPI_YZX;
        }
        else if(!strcmp(atom_value, "hapi_zxy"))
        {
            *x_y_z_order = HAPI_ZXY;
        }
        else if(!strcmp(atom_value, "hapi_zyx"))
        {
            *x_y_z_order = HAPI_ZYX;
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


ERL_NIF_TERM hapi_enum_x_y_z_order_c_to_erl(ErlNifEnv* env, HAPI_XYZOrder x_y_z_order)
{
    switch(x_y_z_order)
    {
        case HAPI_XYZ:
        {
            return hapi_private_make_atom(env, "hapi_xyz");
        }

        case HAPI_XZY:
        {
            return hapi_private_make_atom(env, "hapi_xzy");
        }

        case HAPI_YXZ:
        {
            return hapi_private_make_atom(env, "hapi_yxz");
        }

        case HAPI_YZX:
        {
            return hapi_private_make_atom(env, "hapi_yzx");
        }

        case HAPI_ZXY:
        {
            return hapi_private_make_atom(env, "hapi_zxy");
        }

        case HAPI_ZYX:
        {
            return hapi_private_make_atom(env, "hapi_zyx");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
