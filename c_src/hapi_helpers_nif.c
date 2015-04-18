#include "hapi_helpers_nif.h"
#include "hapi_private_nif.h"
#include "hapi_defines_nif.h"
#include "xxhash.h"

#include <string.h>


// Helper function to return hash of a given atom.
ERL_NIF_TERM
hapi_hash_enum_value_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char* atom_value = NULL;
    bool nif_success = true;
    uint32_t atom_hash = 0u;

    if(enif_is_atom(env, argv[0]))
    {
        uint32_t atom_len = 0;

        if(!enif_get_atom_length(env, argv[0], &atom_len, ERL_NIF_LATIN1))
        {
            nif_success = false;
            goto label_cleanup;
        }

        if(atom_len < HAPI_STACK_STRING_SIZE_MAX)
        {
            char atom_buffer[HAPI_STACK_STRING_SIZE_MAX];
            memset(atom_buffer, 0, HAPI_STACK_STRING_SIZE_MAX);

            if(!enif_get_atom(env, argv[0], atom_buffer, atom_len + 1, ERL_NIF_LATIN1))
            {
                nif_success = false;
                goto label_cleanup;
            }

            atom_hash = XXH32(atom_buffer, strlen(atom_buffer), 0);
        }
        else
        {
            atom_value = malloc(atom_len + 1);
            memset(atom_value, 0, atom_len + 1);

            if(!enif_get_atom(env, argv[0], atom_value, atom_len + 1, ERL_NIF_LATIN1))
            {
                nif_success = false;
                goto label_cleanup;
            }

            atom_hash = XXH32(atom_value, strlen(atom_value), 0);
        }
    }

label_cleanup:

    if(atom_value)
    {
        free(atom_value);
    }

    if(nif_success)
    {
        return enif_make_uint(env, atom_hash);
    }

    return enif_make_badarg(env);
}


ERL_NIF_TERM
hapi_check_enum_value_hash_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    uint32_t atom_hash = 0u;

    if(enif_is_atom(env, argv[0]) && enif_get_uint(env, argv[1], &atom_hash))
    {
        ERL_NIF_TERM result = hapi_hash_enum_value_impl(env, argc, argv);

        uint32_t result_atom_hash = 0u;
        if(enif_get_uint(env, result, &result_atom_hash))
        {
            if(atom_hash == result_atom_hash)
            {
                return hapi_private_make_atom(env, "true");
            }
            else
            {
                return hapi_private_make_atom(env, "false");
            }
        }
    }

    return enif_make_badarg(env);
}
