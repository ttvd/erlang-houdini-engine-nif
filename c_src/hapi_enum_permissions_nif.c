#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_permissions_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_Permissions* permissions)
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

        if(!strcmp(atom_value, "hapi_permissions_non_applicable"))
        {
            *permissions = HAPI_PERMISSIONS_NON_APPLICABLE;
        }
        else if(!strcmp(atom_value, "hapi_permissions_read_write"))
        {
            *permissions = HAPI_PERMISSIONS_READ_WRITE;
        }
        else if(!strcmp(atom_value, "hapi_permissions_read_only"))
        {
            *permissions = HAPI_PERMISSIONS_READ_ONLY;
        }
        else if(!strcmp(atom_value, "hapi_permissions_write_only"))
        {
            *permissions = HAPI_PERMISSIONS_WRITE_ONLY;
        }
        else if(!strcmp(atom_value, "hapi_permissions_max"))
        {
            *permissions = HAPI_PERMISSIONS_MAX;
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


ERL_NIF_TERM hapi_enum_permissions_c_to_erl(ErlNifEnv* env, HAPI_Permissions permissions)
{
    switch(permissions)
    {
        case HAPI_PERMISSIONS_NON_APPLICABLE:
        {
            return hapi_private_make_atom(env, "hapi_permissions_non_applicable");
        }

        case HAPI_PERMISSIONS_READ_WRITE:
        {
            return hapi_private_make_atom(env, "hapi_permissions_read_write");
        }

        case HAPI_PERMISSIONS_READ_ONLY:
        {
            return hapi_private_make_atom(env, "hapi_permissions_read_only");
        }

        case HAPI_PERMISSIONS_WRITE_ONLY:
        {
            return hapi_private_make_atom(env, "hapi_permissions_write_only");
        }

        case HAPI_PERMISSIONS_MAX:
        {
            return hapi_private_make_atom(env, "hapi_permissions_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
