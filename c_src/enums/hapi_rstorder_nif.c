/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_enum_nif.c.template
/// This file corresponds to HAPI_RSTOrder enum from HAPI_Common.h

#include "../hapi_private_nif.h"
#include <string.h>


bool
hapi_rstorder_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_RSTOrder* enum_result)
{
    bool nif_success = true;
    uint32_t atom_len = 0u;
    uint32_t atom_hash = 0u;
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

    atom_hash = XXH32(atom_value, strlen(atom_value), 0);

    switch(atom_hash)
    {
        /* hapi_trs */
        case 2754525809:
        {
            *enum_result = HAPI_TRS;
            break;
        }

        /* hapi_tsr */
        case 1637507022:
        {
            *enum_result = HAPI_TSR;
            break;
        }

        /* hapi_rts */
        case 813138616:
        {
            *enum_result = HAPI_RTS;
            break;
        }

        /* hapi_rst */
        case 3620151878:
        {
            *enum_result = HAPI_RST;
            break;
        }

        /* hapi_str */
        case 531899175:
        {
            *enum_result = HAPI_STR;
            break;
        }

        /* hapi_srt */
        case 169178964:
        {
            *enum_result = HAPI_SRT;
            break;
        }

        default:
        {
            nif_success = false;
            break;
        }
    }

label_cleanup:

    if(atom_value)
    {
        free(atom_value);
    }

    return nif_success;
}


ERL_NIF_TERM
hapi_rstorder_c_to_erl(ErlNifEnv* env, HAPI_RSTOrder enum_value)
{
    switch(enum_value)
    {
        case HAPI_TRS:
        {
            return hapi_make_atom(env, "hapi_trs");
        }

        case HAPI_TSR:
        {
            return hapi_make_atom(env, "hapi_tsr");
        }

        case HAPI_RTS:
        {
            return hapi_make_atom(env, "hapi_rts");
        }

        case HAPI_RST:
        {
            return hapi_make_atom(env, "hapi_rst");
        }

        case HAPI_STR:
        {
            return hapi_make_atom(env, "hapi_str");
        }

        case HAPI_SRT:
        {
            return hapi_make_atom(env, "hapi_srt");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
