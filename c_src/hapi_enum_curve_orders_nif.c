#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_curve_orders_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_CurveOrders* curve_orders)
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
            // "hapi_curve_order_varying"
            case 1332844792:
            {
                *curve_orders = HAPI_CURVE_ORDER_VARYING;
            }

            // "hapi_curve_order_invalid"
            case 3692858437:
            {
                *curve_orders = HAPI_CURVE_ORDER_INVALID;
            }

            // "hapi_curve_order_linear"
            case 1573102780:
            {
                *curve_orders = HAPI_CURVE_ORDER_LINEAR;
            }

            // "hapi_curve_order_quadratic"
            case 3031663613:
            {
                *curve_orders = HAPI_CURVE_ORDER_QUADRATIC;
            }

            // "hapi_curve_order_cubic"
            case 4027595125:
            {
                *curve_orders = HAPI_CURVE_ORDER_CUBIC;
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
