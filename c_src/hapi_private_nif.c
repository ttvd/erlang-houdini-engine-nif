#include "hapi_private_nif.h"
#include "hapi_enums_nif.h"
#include "HAPI.h"

#include <stdbool.h>
#include <string.h>


ERL_NIF_TERM
hapi_private_make_atom(ErlNifEnv* env, const char* atom_name)
{
    ERL_NIF_TERM atom;

    if(enif_make_existing_atom(env, atom_name, &atom, ERL_NIF_LATIN1))
    {
        return atom;
    }

    return enif_make_atom(env, atom_name);
}



ERL_NIF_TERM
hapi_private_make_hash_tuple(ErlNifEnv* env, const char* atom_name)
{
    ERL_NIF_TERM atom = hapi_private_make_atom(env, atom_name);
    uint32_t atom_hash = XXH32(atom_name, strlen(atom_name), 0);

    return enif_make_tuple(env, 2, atom, enif_make_uint(env, atom_hash));
}


ERL_NIF_TERM
hapi_private_make_result_tuple_int(ErlNifEnv* env, HAPI_Result result, int32_t value)
{
    if(HAPI_RESULT_SUCCESS == result)
    {
        return enif_make_tuple(env, 2, hapi_enum_result_c_to_erl(env, result), enif_make_int(env, value));
    }

    return hapi_enum_result_c_to_erl(env, result);
}


bool
hapi_private_check_atom_value(ErlNifEnv* env, const ERL_NIF_TERM term, const char* value, bool* status)
{
    bool nif_success = true;

    uint32_t atom_len = 0;
    char* atom_value = NULL;

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

label_cleanup:

    if(atom_value)
    {
        free(atom_value);
    }

    if(nif_success)
    {
        *status = (bool)(!strcmp(atom_value, value));
    }

    return nif_success;
}
