#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_shader_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ShaderType* shader_type)
{
    bool nif_success = true;

    uint32_t atom_hash = 0;
    int32_t tuple_size = 0;
    const ERL_NIF_TERM* hash_tuple = NULL;

    if(enif_is_tuple(env, term) && enif_get_tuple(env, term, &tuple_size, &hash_tuple) && (2 == tuple_size))
    {
        if(!enif_get_uint(env, hash_tuple[1], &atom_hash))
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
            }

            // "hapi_shader_opengl"
            case 172551974:
            {
                *shader_type = HAPI_SHADER_OPENGL;
            }

            // "hapi_shader_mantra"
            case 2307354241:
            {
                *shader_type = HAPI_SHADER_MANTRA;
            }

            // "hapi_shader_max"
            case 2683922648:
            {
                *shader_type = HAPI_SHADER_MAX;
            }

            default:
            {
                break;
            }
        }
    }

label_cleanup:

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
