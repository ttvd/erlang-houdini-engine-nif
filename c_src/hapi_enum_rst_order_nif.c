/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// This file is autogenerated from hapi_enum_nif.c.template
/// This file corresponds to HAPI_RSTOrder enum from HAPI_Common.h

#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <string.h>


bool hapi_enum_rst_order_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_RSTOrder* rst_order)
{
    bool nif_success = true;

    uint32_t atom_hash = 0;
    int32_t tuple_size = 0;
    const ERL_NIF_TERM* hash_tuple = NULL;

    char* atom_value = NULL;

    if(enif_is_tuple(env, term) && enif_get_tuple(env, term, &tuple_size, &hash_tuple) && (2 == tuple_size))
    {
        if(!enif_get_uint(env, hash_tuple[1], &atom_hash))
        {
            nif_success = false;
            goto label_cleanup;
        }
    }
    else if(enif_is_atom(env, term))
    {
        uint32_t atom_len = 0;

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
    }
    else if(!enif_get_uint(env, term, &atom_hash))
    {
        nif_success = false;
        goto label_cleanup;
    }

    switch(atom_hash)
    {
        // "hapi_trs"
        case 2754525809:
        {
            *rst_order = HAPI_TRS;
            break;
        }

        // "hapi_tsr"
        case 1637507022:
        {
            *rst_order = HAPI_TSR;
            break;
        }

        // "hapi_rts"
        case 813138616:
        {
            *rst_order = HAPI_RTS;
            break;
        }

        // "hapi_rst"
        case 3620151878:
        {
            *rst_order = HAPI_RST;
            break;
        }

        // "hapi_str"
        case 531899175:
        {
            *rst_order = HAPI_STR;
            break;
        }

        // "hapi_srt"
        case 169178964:
        {
            *rst_order = HAPI_SRT;
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


ERL_NIF_TERM hapi_enum_rst_order_c_to_erl(ErlNifEnv* env, HAPI_RSTOrder rst_order)
{
    switch(rst_order)
    {
        case HAPI_TRS:
        {
            return hapi_private_make_hash_tuple(env, "hapi_trs");
        }

        case HAPI_TSR:
        {
            return hapi_private_make_hash_tuple(env, "hapi_tsr");
        }

        case HAPI_RTS:
        {
            return hapi_private_make_hash_tuple(env, "hapi_rts");
        }

        case HAPI_RST:
        {
            return hapi_private_make_hash_tuple(env, "hapi_rst");
        }

        case HAPI_STR:
        {
            return hapi_private_make_hash_tuple(env, "hapi_str");
        }

        case HAPI_SRT:
        {
            return hapi_private_make_hash_tuple(env, "hapi_srt");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
