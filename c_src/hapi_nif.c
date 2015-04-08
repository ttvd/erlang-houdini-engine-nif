#include "erl_nif.h"
//#include "HAPI.h"

int hapi_private_init(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
void hapi_private_cleanup(ErlNifEnv* env, void* obj);

static ErlNifFunc nif_funcs[] =
{
    //{"callname1", 2, func1},
    //{"callname2", 1, func2}
};

static ERL_NIF_TERM g_atom_ok;
static ErlNifResourceType* hapi_handle;

int
hapi_private_init(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    g_atom_ok = enif_make_atom(env, "ok");
    hapi_handle = enif_open_resource_type(env, "hapi", "hapi_handle", &hapi_private_cleanup,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    if(!hapi_handle)
    {
        return -1;
    }

    return 0;
}

ERL_NIF_INIT(hapi, nif_funcs, &hapi_private_init, NULL, NULL, NULL)
