#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <stdio.h>


bool hapi_enum_shader_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ShaderType* shader_type)
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
        // "hapi_shader_invalid"
        case 1767885971:
        {
            *shader_type = HAPI_SHADER_INVALID;
            break;
        }

        // "hapi_shader_opengl"
        case 172551974:
        {
            *shader_type = HAPI_SHADER_OPENGL;
            break;
        }

        // "hapi_shader_mantra"
        case 2307354241:
        {
            *shader_type = HAPI_SHADER_MANTRA;
            break;
        }

        // "hapi_shader_max"
        case 2683922648:
        {
            *shader_type = HAPI_SHADER_MAX;
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


ERL_NIF_TERM hapi_enum_shader_type_c_to_erl(ErlNifEnv* env, HAPI_ShaderType shader_type)
{
    switch(shader_type)
    {
        /*
        case HAPI_SHADER_INVALID:
        {
            return hapi_private_make_hash_tuple(env, "hapi_shader_invalid");
        }
        */

        case HAPI_SHADER_OPENGL:
        {
            return hapi_private_make_hash_tuple(env, "hapi_shader_opengl");
        }

        case HAPI_SHADER_MANTRA:
        {
            return hapi_private_make_hash_tuple(env, "hapi_shader_mantra");
        }

        case HAPI_SHADER_MAX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_shader_max");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
