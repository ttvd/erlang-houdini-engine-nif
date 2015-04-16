#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_r_s_t_order_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_RSTOrder* r_s_t_order)
{
    bool nif_success = true;

    uint32_t atom_hash = 0;
    int32_t tuple_size = 0;
    const ERL_NIF_TERM* hash_tuple = NULL;

    if(enif_is_tuple(env, term) && enif_get_tuple(env, term, &tuple_size, &hash_tuple) && (2 == tuple_size))
    {
        if(!enif_get_uint(env, hash_tuple[1], &atom_hash))
        {
            nif_success = false;
            goto label_cleanup;
        }

        switch(atom_hash)
        {
            // "hapi_trs"
            case 2754525809:
            {
                *r_s_t_order = HAPI_TRS;
            }

            // "hapi_tsr"
            case 1637507022:
            {
                *r_s_t_order = HAPI_TSR;
            }

            // "hapi_rts"
            case 813138616:
            {
                *r_s_t_order = HAPI_RTS;
            }

            // "hapi_rst"
            case 3620151878:
            {
                *r_s_t_order = HAPI_RST;
            }

            // "hapi_str"
            case 531899175:
            {
                *r_s_t_order = HAPI_STR;
            }

            // "hapi_srt"
            case 169178964:
            {
                *r_s_t_order = HAPI_SRT;
            }

            default:
            {
                break;
            }
        }
    }

label_cleanup:

    return nif_success;
}


ERL_NIF_TERM hapi_enum_r_s_t_order_c_to_erl(ErlNifEnv* env, HAPI_RSTOrder r_s_t_order)
{
    switch(r_s_t_order)
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
