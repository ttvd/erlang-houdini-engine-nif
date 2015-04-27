/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL

#include "hapi_private_nif.h"
#include <string.h>


ERL_NIF_TERM
hapi_make_atom(ErlNifEnv* env, const char* atom_name)
{
    ERL_NIF_TERM atom;

    if(enif_make_existing_atom(env, atom_name, &atom, ERL_NIF_LATIN1))
    {
        return atom;
    }

    return enif_make_atom(env, atom_name);
}


ERL_NIF_TERM
hapi_make_atom_ok(ErlNifEnv* env)
{
    return hapi_make_atom(env, "ok");
}


ERL_NIF_TERM
hapi_make_atom_bool(ErlNifEnv* env, bool value)
{
    if(value)
    {
        return hapi_make_atom(env, "true");
    }

    return hapi_make_atom(env, "false");
}


ERL_NIF_TERM
hapi_make_list_float(ErlNifEnv* env, uint32_t size, const float* data)
{
    ERL_NIF_TERM list = enif_make_list(env, 0);

    for(int32_t idx = 0; idx < size; ++idx)
    {
        list = enif_make_list_cell(env, enif_make_int(env, (double) *(data + idx)), list);
    }

    return list;
}


#define HAPI_STACK_STRING_SIZE_MAX 64

bool
hapi_check_atom(ErlNifEnv* env, const ERL_NIF_TERM term, const char* value, bool* status)
{
    bool nif_success = true;

    uint32_t atom_len = 0;
    char* atom_value = NULL;

    if(!enif_get_atom_length(env, term, &atom_len, ERL_NIF_LATIN1))
    {
        nif_success = false;
        goto label_cleanup;
    }

    if(atom_len < HAPI_STACK_STRING_SIZE_MAX)
    {
        char atom_buffer[HAPI_STACK_STRING_SIZE_MAX];
        memset(atom_buffer, 0, HAPI_STACK_STRING_SIZE_MAX);

        if(!enif_get_atom(env, term, atom_buffer, atom_len + 1, ERL_NIF_LATIN1))
        {
            nif_success = false;
            goto label_cleanup;
        }

        *status = (bool)(!strcmp(atom_buffer, value));
    }
    else
    {
        atom_value = malloc(atom_len + 1);
        memset(atom_value, 0, atom_len + 1);

        if(!enif_get_atom(env, term, atom_value, atom_len + 1, ERL_NIF_LATIN1))
        {
            nif_success = false;
            goto label_cleanup;
        }

        *status = (bool)(!strcmp(atom_value, value));
    }

label_cleanup:

    if(atom_value)
    {
        free(atom_value);
    }

    return nif_success;
}


bool
hapi_get_atom_bool(ErlNifEnv* env, const ERL_NIF_TERM term, bool* status)
{
    bool nif_success = true;
    uint32_t atom_len = 0;

    if(enif_is_atom(env, term))
    {
        if(!enif_get_atom_length(env, term, &atom_len, ERL_NIF_LATIN1))
        {
            return false;
        }

        if(atom_len > 6)
        {
            return false;
        }

        char atom_buffer[HAPI_STACK_STRING_SIZE_MAX];
        memset(atom_buffer, 0, HAPI_STACK_STRING_SIZE_MAX);

        if(!enif_get_atom(env, term, atom_buffer, atom_len + 1, ERL_NIF_LATIN1))
        {
            return false;
        }

        if(!strcmp(atom_buffer, "true"))
        {
            *status = true;
        }
        else if(!strcmp(atom_buffer, "false"))
        {
            *status = false;
        }
        else
        {
            nif_success = false;
        }
    }

    return nif_success;
}
