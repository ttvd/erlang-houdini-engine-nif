/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from hapi_enum_nif.c.template
/// This file corresponds to HAPI_CurveOrders enum from HAPI_Common.h

#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <string.h>


bool hapi_enum_curve_orders_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_CurveOrders* curve_orders)
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
        // "hapi_curve_order_varying"
        case 1332844792:
        {
            *curve_orders = HAPI_CURVE_ORDER_VARYING;
            break;
        }

        // "hapi_curve_order_invalid"
        case 3692858437:
        {
            *curve_orders = HAPI_CURVE_ORDER_INVALID;
            break;
        }

        // "hapi_curve_order_linear"
        case 1573102780:
        {
            *curve_orders = HAPI_CURVE_ORDER_LINEAR;
            break;
        }

        // "hapi_curve_order_quadratic"
        case 3031663613:
        {
            *curve_orders = HAPI_CURVE_ORDER_QUADRATIC;
            break;
        }

        // "hapi_curve_order_cubic"
        case 4027595125:
        {
            *curve_orders = HAPI_CURVE_ORDER_CUBIC;
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


ERL_NIF_TERM hapi_enum_curve_orders_c_to_erl(ErlNifEnv* env, HAPI_CurveOrders curve_orders)
{
    switch(curve_orders)
    {
        case HAPI_CURVE_ORDER_VARYING:
        {
            return hapi_private_make_hash_tuple(env, "hapi_curve_order_varying");
        }

        case HAPI_CURVE_ORDER_INVALID:
        {
            return hapi_private_make_hash_tuple(env, "hapi_curve_order_invalid");
        }

        case HAPI_CURVE_ORDER_LINEAR:
        {
            return hapi_private_make_hash_tuple(env, "hapi_curve_order_linear");
        }

        case HAPI_CURVE_ORDER_QUADRATIC:
        {
            return hapi_private_make_hash_tuple(env, "hapi_curve_order_quadratic");
        }

        case HAPI_CURVE_ORDER_CUBIC:
        {
            return hapi_private_make_hash_tuple(env, "hapi_curve_order_cubic");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
