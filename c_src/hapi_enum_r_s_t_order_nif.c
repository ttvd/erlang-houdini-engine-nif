#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_r_s_t_order_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_RSTOrder* r_s_t_order)
{
    bool nif_success = true;

    uint32_t atom_len = 0;
    char* atom_value = NULL;

    if(enif_is_atom(env, term))
    {
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

        if(!strcmp(atom_value, "hapi_trs"))
        {
            *r_s_t_order = HAPI_TRS;
        }
        else if(!strcmp(atom_value, "hapi_tsr"))
        {
            *r_s_t_order = HAPI_TSR;
        }
        else if(!strcmp(atom_value, "hapi_rts"))
        {
            *r_s_t_order = HAPI_RTS;
        }
        else if(!strcmp(atom_value, "hapi_rst"))
        {
            *r_s_t_order = HAPI_RST;
        }
        else if(!strcmp(atom_value, "hapi_str"))
        {
            *r_s_t_order = HAPI_STR;
        }
        else if(!strcmp(atom_value, "hapi_srt"))
        {
            *r_s_t_order = HAPI_SRT;
        }
        else
        {
            nif_success = false;
        }
    }
    else
    {
        nif_success = false;
    }

label_cleanup:

    if(atom_value)
    {
        free(atom_value);
    }

    return nif_success;
}


ERL_NIF_TERM hapi_enum_r_s_t_order_c_to_erl(ErlNifEnv* env, HAPI_RSTOrder r_s_t_order)
{
    switch(r_s_t_order)
    {
        case HAPI_TRS:
        {
            return hapi_private_make_atom(env, "hapi_trs");
        }

        case HAPI_TSR:
        {
            return hapi_private_make_atom(env, "hapi_tsr");
        }

        case HAPI_RTS:
        {
            return hapi_private_make_atom(env, "hapi_rts");
        }

        case HAPI_RST:
        {
            return hapi_private_make_atom(env, "hapi_rst");
        }

        case HAPI_STR:
        {
            return hapi_private_make_atom(env, "hapi_str");
        }

        case HAPI_SRT:
        {
            return hapi_private_make_atom(env, "hapi_srt");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
