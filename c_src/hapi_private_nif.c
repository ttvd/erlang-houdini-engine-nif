/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL

#include "hapi_private_nif.h"

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
hapi_private_make_atom_bool(ErlNifEnv* env, bool value)
{
    if(value)
    {
        return hapi_private_make_atom(env, "true");
    }

    return hapi_private_make_atom(env, "false");
}
