#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_shader_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ShaderType* shader_type)
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

        if(!strcmp(atom_value, "hapi_shader_invalid"))
        {
            *shader_type = HAPI_SHADER_INVALID;
        }
        else if(!strcmp(atom_value, "hapi_shader_opengl"))
        {
            *shader_type = HAPI_SHADER_OPENGL;
        }
        else if(!strcmp(atom_value, "hapi_shader_mantra"))
        {
            *shader_type = HAPI_SHADER_MANTRA;
        }
        else if(!strcmp(atom_value, "hapi_shader_max"))
        {
            *shader_type = HAPI_SHADER_MAX;
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


ERL_NIF_TERM hapi_enum_shader_type_c_to_erl(ErlNifEnv* env, HAPI_ShaderType shader_type)
{
    switch(shader_type)
    {
        /*
        case HAPI_SHADER_INVALID:
        {
            return hapi_private_make_atom(env, "hapi_shader_invalid");
        }
        */

        case HAPI_SHADER_OPENGL:
        {
            return hapi_private_make_atom(env, "hapi_shader_opengl");
        }

        case HAPI_SHADER_MANTRA:
        {
            return hapi_private_make_atom(env, "hapi_shader_mantra");
        }

        case HAPI_SHADER_MAX:
        {
            return hapi_private_make_atom(env, "hapi_shader_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
