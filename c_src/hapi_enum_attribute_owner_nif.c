#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_attribute_owner_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_AttributeOwner* attribute_owner)
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

        if(!strcmp(atom_value, "hapi_attrowner_invalid"))
        {
            *attribute_owner = HAPI_ATTROWNER_INVALID;
        }
        else if(!strcmp(atom_value, "hapi_attrowner_vertex"))
        {
            *attribute_owner = HAPI_ATTROWNER_VERTEX;
        }
        else if(!strcmp(atom_value, "hapi_attrowner_point"))
        {
            *attribute_owner = HAPI_ATTROWNER_POINT;
        }
        else if(!strcmp(atom_value, "hapi_attrowner_prim"))
        {
            *attribute_owner = HAPI_ATTROWNER_PRIM;
        }
        else if(!strcmp(atom_value, "hapi_attrowner_detail"))
        {
            *attribute_owner = HAPI_ATTROWNER_DETAIL;
        }
        else if(!strcmp(atom_value, "hapi_attrowner_max"))
        {
            *attribute_owner = HAPI_ATTROWNER_MAX;
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


ERL_NIF_TERM hapi_enum_attribute_owner_c_to_erl(ErlNifEnv* env, HAPI_AttributeOwner attribute_owner)
{
    switch(attribute_owner)
    {
        /*
        case HAPI_ATTROWNER_INVALID:
        {
            return hapi_private_make_atom(env, "hapi_attrowner_invalid");
        }
        */

        case HAPI_ATTROWNER_VERTEX:
        {
            return hapi_private_make_atom(env, "hapi_attrowner_vertex");
        }

        case HAPI_ATTROWNER_POINT:
        {
            return hapi_private_make_atom(env, "hapi_attrowner_point");
        }

        case HAPI_ATTROWNER_PRIM:
        {
            return hapi_private_make_atom(env, "hapi_attrowner_prim");
        }

        case HAPI_ATTROWNER_DETAIL:
        {
            return hapi_private_make_atom(env, "hapi_attrowner_detail");
        }

        case HAPI_ATTROWNER_MAX:
        {
            return hapi_private_make_atom(env, "hapi_attrowner_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
