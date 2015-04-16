#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_license_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_License* license)
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

        if(!strcmp(atom_value, "hapi_license_none"))
        {
            *license = HAPI_LICENSE_NONE;
        }
        else if(!strcmp(atom_value, "hapi_license_houdini_engine"))
        {
            *license = HAPI_LICENSE_HOUDINI_ENGINE;
        }
        else if(!strcmp(atom_value, "hapi_license_houdini"))
        {
            *license = HAPI_LICENSE_HOUDINI;
        }
        else if(!strcmp(atom_value, "hapi_license_houdini_fx"))
        {
            *license = HAPI_LICENSE_HOUDINI_FX;
        }
        else if(!strcmp(atom_value, "hapi_license_houdini_engine_indie"))
        {
            *license = HAPI_LICENSE_HOUDINI_ENGINE_INDIE;
        }
        else if(!strcmp(atom_value, "hapi_license_houdini_indie"))
        {
            *license = HAPI_LICENSE_HOUDINI_INDIE;
        }
        else if(!strcmp(atom_value, "hapi_license_max"))
        {
            *license = HAPI_LICENSE_MAX;
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


ERL_NIF_TERM hapi_enum_license_c_to_erl(ErlNifEnv* env, HAPI_License license)
{
    switch(license)
    {
        case HAPI_LICENSE_NONE:
        {
            return hapi_private_make_atom(env, "hapi_license_none");
        }

        case HAPI_LICENSE_HOUDINI_ENGINE:
        {
            return hapi_private_make_atom(env, "hapi_license_houdini_engine");
        }

        case HAPI_LICENSE_HOUDINI:
        {
            return hapi_private_make_atom(env, "hapi_license_houdini");
        }

        case HAPI_LICENSE_HOUDINI_FX:
        {
            return hapi_private_make_atom(env, "hapi_license_houdini_fx");
        }

        case HAPI_LICENSE_HOUDINI_ENGINE_INDIE:
        {
            return hapi_private_make_atom(env, "hapi_license_houdini_engine_indie");
        }

        case HAPI_LICENSE_HOUDINI_INDIE:
        {
            return hapi_private_make_atom(env, "hapi_license_houdini_indie");
        }

        case HAPI_LICENSE_MAX:
        {
            return hapi_private_make_atom(env, "hapi_license_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
