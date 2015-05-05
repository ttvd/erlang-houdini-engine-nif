/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL

#include "hapi_private_nif.h"
#include <string.h>


ERL_NIF_TERM
hapi_priv_make_atom(ErlNifEnv* env, const char* atom_name)
{
    ERL_NIF_TERM atom;

    if(enif_make_existing_atom(env, atom_name, &atom, ERL_NIF_LATIN1))
    {
        return atom;
    }

    return enif_make_atom(env, atom_name);
}


ERL_NIF_TERM
hapi_priv_make_atom_ok(ErlNifEnv* env)
{
    return hapi_priv_make_atom(env, "ok");
}


ERL_NIF_TERM
hapi_priv_make_atom_nil(ErlNifEnv* env)
{
    return hapi_priv_make_atom(env, "nil");
}


ERL_NIF_TERM
hapi_priv_make_bool(ErlNifEnv* env, bool value)
{
    if(value)
    {
        return hapi_priv_make_atom(env, "true");
    }

    return hapi_priv_make_atom(env, "false");
}


ERL_NIF_TERM
hapi_priv_make_float_list(ErlNifEnv* env, const float* data, uint32_t size)
{
    ERL_NIF_TERM list = enif_make_list(env, 0);

    for(int32_t idx = 0; idx < size; ++idx)
    {
        list = enif_make_list_cell(env, enif_make_double(env, (double) *(data + idx)), list);
    }

    return list;
}


ERL_NIF_TERM
hapi_priv_make_double_list(ErlNifEnv* env, const double* data, uint32_t size)
{
    ERL_NIF_TERM list = enif_make_list(env, 0);

    for(int32_t idx = 0; idx < size; ++idx)
    {
        list = enif_make_list_cell(env, enif_make_double(env, *(data + idx)), list);
    }

    return list;
}


ERL_NIF_TERM
hapi_priv_make_int_list(ErlNifEnv* env, const int32_t* data, uint32_t size)
{
    ERL_NIF_TERM list = enif_make_list(env, 0);

    for(int32_t idx = 0; idx < size; ++idx)
    {
        list = enif_make_list_cell(env, enif_make_int(env, (double) *(data + idx)), list);
    }

    return list;
}


ERL_NIF_TERM
hapi_priv_make_string(ErlNifEnv* env, const char* string)
{
    return enif_make_string(env, string, ERL_NIF_LATIN1);
}


ERL_NIF_TERM
hapi_priv_make_float(ErlNifEnv* env, float value)
{
    return enif_make_double(env, (double) value);
}


ERL_NIF_TERM
hapi_priv_make_double(ErlNifEnv* env, double value)
{
    return enif_make_double(env, value);
}


ERL_NIF_TERM
hapi_priv_make_int(ErlNifEnv* env, int32_t value)
{
    return enif_make_int(env, value);
}


ERL_NIF_TERM
hapi_priv_make_char(ErlNifEnv* env, char value)
{
    return enif_make_int(env, (int) value);
}


#define HAPI_STACK_STRING_SIZE_MAX 64

bool
hapi_priv_check_atom(ErlNifEnv* env, const ERL_NIF_TERM term, const char* value, bool* status)
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
hapi_priv_get_bool(ErlNifEnv* env, const ERL_NIF_TERM term, bool* status)
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


bool
hapi_priv_get_nil(ErlNifEnv* env, const ERL_NIF_TERM term, bool* status)
{
    return hapi_priv_check_atom(env, term, "nil", status);
}


bool
hapi_priv_get_float(ErlNifEnv* env, const ERL_NIF_TERM term, float* data)
{
    double result = 0.0;

    if(enif_get_double(env, term, &result))
    {
        *data = (float) result;
        return true;
    }

    return false;
}


bool
hapi_priv_get_double(ErlNifEnv* env, const ERL_NIF_TERM term, double* data)
{
    return enif_get_double(env, term, data);
}


bool
hapi_priv_get_int(ErlNifEnv* env, const ERL_NIF_TERM term, int* data)
{
    return enif_get_int(env, term, data);
}


bool
hapi_priv_get_char(ErlNifEnv* env, const ERL_NIF_TERM term, char* data)
{
    int result = 0;

    if(enif_get_int(env, term, &result))
    {
        *data = (char) result;
        return true;
    }

    return false;
}


bool
hapi_priv_get_float_list(ErlNifEnv* env, const ERL_NIF_TERM term, float* data, uint32_t size)
{
    uint32_t list_size = 0;
    ERL_NIF_TERM head, tail;

    if(enif_get_list_length(env, term, &list_size) && (list_size == size))
    {
        ERL_NIF_TERM list = term;
        int32_t index = 0;

        while(enif_get_list_cell(env, list, &head, &tail))
        {
            double param_double = 0.0;
            int param_int = 0;

            if(enif_get_double(env, head, &param_double))
            {
                *(data + index) = (float) param_double;
            }
            else if(enif_get_int(env, head, &param_int))
            {
                *(data + index) = (float) param_int;
            }
            else
            {
                return false;
            }

            index++;
            list = tail;
        }

        return true;
    }

    return false;
}


bool
hapi_priv_get_double_list(ErlNifEnv* env, const ERL_NIF_TERM term, double* data, uint32_t size)
{
    uint32_t list_size = 0;
    ERL_NIF_TERM head, tail;

    if(enif_get_list_length(env, term, &list_size) && (list_size == size))
    {
        ERL_NIF_TERM list = term;
        int32_t index = 0;

        while(enif_get_list_cell(env, list, &head, &tail))
        {
            double param_double = 0.0;
            int param_int = 0;

            if(enif_get_double(env, head, &param_double))
            {
                *(data + index) = param_double;
            }
            else if(enif_get_int(env, head, &param_int))
            {
                *(data + index) = (double) param_int;
            }
            else
            {
                return false;
            }

            index++;
            list = tail;
        }

        return true;
    }

    return false;
}


bool
hapi_priv_get_int_list(ErlNifEnv* env, const ERL_NIF_TERM term, int32_t* data, uint32_t size)
{
    uint32_t list_size = 0;
    ERL_NIF_TERM head, tail;

    if(enif_get_list_length(env, term, &list_size) && (list_size == size))
    {
        ERL_NIF_TERM list = term;
        int32_t index = 0;

        while(enif_get_list_cell(env, list, &head, &tail))
        {
            int32_t param_int = 0.0;

            if(enif_get_int(env, head, &param_int))
            {
                *(data + index) = param_int;
            }
            else
            {
                return false;
            }

            index++;
            list = tail;
        }

        return true;
    }

    return false;
}

/*
bool
hapi_priv_get_string_list(ErlNifEnv* env, const ERL_NIF_TERM term, char** string, uint32_t* string_length)
{
    uint32_t length = 0;
    char* buffer = NULL;

    if(!enif_get_list_length(env, term, &length))
    {
        return false;
    }

    if(length > 0)
    {
        buffer = malloc(length + 1);
        memset(buffer, 0, length + 1);

        if(enif_get_string(env, term, buffer, length + 1, ERL_NIF_LATIN1) < 1)
        {
            free(buffer);
            return false;
        }
    }

    *string_length = length;
    *string = buffer;
    return true;
}
*/
