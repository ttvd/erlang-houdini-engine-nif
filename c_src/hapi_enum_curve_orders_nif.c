#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_curve_orders_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_CurveOrders* curve_orders)
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

        if(!strcmp(atom_value, "hapi_curve_order_varying"))
        {
            *curve_orders = HAPI_CURVE_ORDER_VARYING;
        }
        else if(!strcmp(atom_value, "hapi_curve_order_invalid"))
        {
            *curve_orders = HAPI_CURVE_ORDER_INVALID;
        }
        else if(!strcmp(atom_value, "hapi_curve_order_linear"))
        {
            *curve_orders = HAPI_CURVE_ORDER_LINEAR;
        }
        else if(!strcmp(atom_value, "hapi_curve_order_quadratic"))
        {
            *curve_orders = HAPI_CURVE_ORDER_QUADRATIC;
        }
        else if(!strcmp(atom_value, "hapi_curve_order_cubic"))
        {
            *curve_orders = HAPI_CURVE_ORDER_CUBIC;
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


ERL_NIF_TERM hapi_enum_curve_orders_c_to_erl(ErlNifEnv* env, HAPI_CurveOrders curve_orders)
{
    switch(curve_orders)
    {
        case HAPI_CURVE_ORDER_VARYING:
        {
            return hapi_private_make_atom(env, "hapi_curve_order_varying");
        }

        case HAPI_CURVE_ORDER_INVALID:
        {
            return hapi_private_make_atom(env, "hapi_curve_order_invalid");
        }

        case HAPI_CURVE_ORDER_LINEAR:
        {
            return hapi_private_make_atom(env, "hapi_curve_order_linear");
        }

        case HAPI_CURVE_ORDER_QUADRATIC:
        {
            return hapi_private_make_atom(env, "hapi_curve_order_quadratic");
        }

        case HAPI_CURVE_ORDER_CUBIC:
        {
            return hapi_private_make_atom(env, "hapi_curve_order_cubic");
        }

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
