/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL

#include "hapi_private_nif.h"


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
