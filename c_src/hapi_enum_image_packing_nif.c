#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_image_packing_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ImagePacking* image_packing)
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

        if(!strcmp(atom_value, "hapi_image_packing_unknown"))
        {
            *image_packing = HAPI_IMAGE_PACKING_UNKNOWN;
        }
        else if(!strcmp(atom_value, "hapi_image_packing_single"))
        {
            *image_packing = HAPI_IMAGE_PACKING_SINGLE;
        }
        else if(!strcmp(atom_value, "hapi_image_packing_dual"))
        {
            *image_packing = HAPI_IMAGE_PACKING_DUAL;
        }
        else if(!strcmp(atom_value, "hapi_image_packing_rgb"))
        {
            *image_packing = HAPI_IMAGE_PACKING_RGB;
        }
        else if(!strcmp(atom_value, "hapi_image_packing_bgr"))
        {
            *image_packing = HAPI_IMAGE_PACKING_BGR;
        }
        else if(!strcmp(atom_value, "hapi_image_packing_rgba"))
        {
            *image_packing = HAPI_IMAGE_PACKING_RGBA;
        }
        else if(!strcmp(atom_value, "hapi_image_packing_abgr"))
        {
            *image_packing = HAPI_IMAGE_PACKING_ABGR;
        }
        else if(!strcmp(atom_value, "hapi_image_packing_max"))
        {
            *image_packing = HAPI_IMAGE_PACKING_MAX;
        }
        else if(!strcmp(atom_value, "hapi_image_packing_default3"))
        {
            *image_packing = HAPI_IMAGE_PACKING_DEFAULT3;
        }
        else if(!strcmp(atom_value, "hapi_image_packing_default4"))
        {
            *image_packing = HAPI_IMAGE_PACKING_DEFAULT4;
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


ERL_NIF_TERM hapi_enum_image_packing_c_to_erl(ErlNifEnv* env, HAPI_ImagePacking image_packing)
{
    switch(image_packing)
    {
        /*
        case HAPI_IMAGE_PACKING_UNKNOWN:
        {
            return hapi_private_make_atom(env, "hapi_image_packing_unknown");
        }
        */

        case HAPI_IMAGE_PACKING_SINGLE:
        {
            return hapi_private_make_atom(env, "hapi_image_packing_single");
        }

        case HAPI_IMAGE_PACKING_DUAL:
        {
            return hapi_private_make_atom(env, "hapi_image_packing_dual");
        }

        case HAPI_IMAGE_PACKING_RGB:
        {
            return hapi_private_make_atom(env, "hapi_image_packing_rgb");
        }

        case HAPI_IMAGE_PACKING_BGR:
        {
            return hapi_private_make_atom(env, "hapi_image_packing_bgr");
        }

        case HAPI_IMAGE_PACKING_RGBA:
        {
            return hapi_private_make_atom(env, "hapi_image_packing_rgba");
        }

        case HAPI_IMAGE_PACKING_ABGR:
        {
            return hapi_private_make_atom(env, "hapi_image_packing_abgr");
        }

        case HAPI_IMAGE_PACKING_MAX:
        {
            return hapi_private_make_atom(env, "hapi_image_packing_max");
        }

        /*
        case HAPI_IMAGE_PACKING_DEFAULT3:
        {
            return hapi_private_make_atom(env, "hapi_image_packing_default3");
        }
        */

        /*
        case HAPI_IMAGE_PACKING_DEFAULT4:
        {
            return hapi_private_make_atom(env, "hapi_image_packing_default4");
        }
        */

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
