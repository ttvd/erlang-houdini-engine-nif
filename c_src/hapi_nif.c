#include "erl_nif.h"
#include "HAPI.h"

int hapi_private_init(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
void hapi_private_cleanup(ErlNifEnv* env, void* obj);

ERL_NIF_TERM hapi_is_initialized_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hapi_initialize_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hapi_cleanup_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);


static ErlNifFunc nif_funcs[] =
{
    {"hapi_is_initialized", 0, hapi_is_initialized_impl},
    {"hapi_initialize", 5, hapi_initialize_impl},
    {"hapi_cleanup", 0, hapi_cleanup_impl}
};


static ERL_NIF_TERM g_atom_ok;
static ErlNifResourceType* hapi_handle;


static ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* atom_name)
{
    ERL_NIF_TERM atom;

    if(enif_make_existing_atom(env, atom_name, &atom, ERL_NIF_LATIN1))
    {
        return atom;
    }

    return enif_make_atom(env, atom_name);
}

int
hapi_private_init(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    g_atom_ok = make_atom(env, "ok");

    hapi_handle = enif_open_resource_type(env, "hapi", "hapi_handle", &hapi_private_cleanup,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    if(!hapi_handle)
    {
        return -1;
    }

    return 0;
}


ERL_NIF_INIT(hapi, nif_funcs, &hapi_private_init, NULL, NULL, NULL)


ERL_NIF_TERM
hapi_is_initialized_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return g_atom_ok;
}


ERL_NIF_TERM
hapi_initialize_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return g_atom_ok;
}


ERL_NIF_TERM
hapi_cleanup_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return g_atom_ok;
}
