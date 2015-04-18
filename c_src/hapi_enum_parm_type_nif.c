#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"

#include <stdio.h>


bool hapi_enum_parm_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ParmType* parm_type)
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
        // "hapi_parmtype_int"
        case 2938205493:
        {
            *parm_type = HAPI_PARMTYPE_INT;
            break;
        }

        // "hapi_parmtype_multiparmlist"
        case 94525824:
        {
            *parm_type = HAPI_PARMTYPE_MULTIPARMLIST;
            break;
        }

        // "hapi_parmtype_toggle"
        case 3487126731:
        {
            *parm_type = HAPI_PARMTYPE_TOGGLE;
            break;
        }

        // "hapi_parmtype_button"
        case 3222095719:
        {
            *parm_type = HAPI_PARMTYPE_BUTTON;
            break;
        }

        // "hapi_parmtype_float"
        case 3403782401:
        {
            *parm_type = HAPI_PARMTYPE_FLOAT;
            break;
        }

        // "hapi_parmtype_color"
        case 3651575535:
        {
            *parm_type = HAPI_PARMTYPE_COLOR;
            break;
        }

        // "hapi_parmtype_string"
        case 3029524656:
        {
            *parm_type = HAPI_PARMTYPE_STRING;
            break;
        }

        // "hapi_parmtype_path_file"
        case 3946815810:
        {
            *parm_type = HAPI_PARMTYPE_PATH_FILE;
            break;
        }

        // "hapi_parmtype_path_file_geo"
        case 2237470241:
        {
            *parm_type = HAPI_PARMTYPE_PATH_FILE_GEO;
            break;
        }

        // "hapi_parmtype_path_file_image"
        case 3582069620:
        {
            *parm_type = HAPI_PARMTYPE_PATH_FILE_IMAGE;
            break;
        }

        // "hapi_parmtype_path_node"
        case 1563877995:
        {
            *parm_type = HAPI_PARMTYPE_PATH_NODE;
            break;
        }

        // "hapi_parmtype_folderlist"
        case 465413589:
        {
            *parm_type = HAPI_PARMTYPE_FOLDERLIST;
            break;
        }

        // "hapi_parmtype_folder"
        case 1692078012:
        {
            *parm_type = HAPI_PARMTYPE_FOLDER;
            break;
        }

        // "hapi_parmtype_label"
        case 4255272841:
        {
            *parm_type = HAPI_PARMTYPE_LABEL;
            break;
        }

        // "hapi_parmtype_separator"
        case 2360826373:
        {
            *parm_type = HAPI_PARMTYPE_SEPARATOR;
            break;
        }

        // "hapi_parmtype_max"
        case 220098758:
        {
            *parm_type = HAPI_PARMTYPE_MAX;
            break;
        }

        // "hapi_parmtype_int_start"
        case 2793337556:
        {
            *parm_type = HAPI_PARMTYPE_INT_START;
            break;
        }

        // "hapi_parmtype_int_end"
        case 2924084007:
        {
            *parm_type = HAPI_PARMTYPE_INT_END;
            break;
        }

        // "hapi_parmtype_float_start"
        case 3597937322:
        {
            *parm_type = HAPI_PARMTYPE_FLOAT_START;
            break;
        }

        // "hapi_parmtype_float_end"
        case 2584819633:
        {
            *parm_type = HAPI_PARMTYPE_FLOAT_END;
            break;
        }

        // "hapi_parmtype_string_start"
        case 2299523298:
        {
            *parm_type = HAPI_PARMTYPE_STRING_START;
            break;
        }

        // "hapi_parmtype_string_end"
        case 883769106:
        {
            *parm_type = HAPI_PARMTYPE_STRING_END;
            break;
        }

        // "hapi_parmtype_path_start"
        case 1937567703:
        {
            *parm_type = HAPI_PARMTYPE_PATH_START;
            break;
        }

        // "hapi_parmtype_path_end"
        case 233945276:
        {
            *parm_type = HAPI_PARMTYPE_PATH_END;
            break;
        }

        // "hapi_parmtype_path_file_start"
        case 2795114217:
        {
            *parm_type = HAPI_PARMTYPE_PATH_FILE_START;
            break;
        }

        // "hapi_parmtype_path_file_end"
        case 4055995372:
        {
            *parm_type = HAPI_PARMTYPE_PATH_FILE_END;
            break;
        }

        // "hapi_parmtype_path_node_start"
        case 3219598411:
        {
            *parm_type = HAPI_PARMTYPE_PATH_NODE_START;
            break;
        }

        // "hapi_parmtype_path_node_end"
        case 381338848:
        {
            *parm_type = HAPI_PARMTYPE_PATH_NODE_END;
            break;
        }

        // "hapi_parmtype_container_start"
        case 1309661876:
        {
            *parm_type = HAPI_PARMTYPE_CONTAINER_START;
            break;
        }

        // "hapi_parmtype_container_end"
        case 2873311351:
        {
            *parm_type = HAPI_PARMTYPE_CONTAINER_END;
            break;
        }

        // "hapi_parmtype_nonvalue_start"
        case 1808937396:
        {
            *parm_type = HAPI_PARMTYPE_NONVALUE_START;
            break;
        }

        // "hapi_parmtype_nonvalue_end"
        case 920931063:
        {
            *parm_type = HAPI_PARMTYPE_NONVALUE_END;
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


ERL_NIF_TERM hapi_enum_parm_type_c_to_erl(ErlNifEnv* env, HAPI_ParmType parm_type)
{
    switch(parm_type)
    {
        case HAPI_PARMTYPE_INT:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_int");
        }

        case HAPI_PARMTYPE_MULTIPARMLIST:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_multiparmlist");
        }

        case HAPI_PARMTYPE_TOGGLE:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_toggle");
        }

        case HAPI_PARMTYPE_BUTTON:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_button");
        }

        case HAPI_PARMTYPE_FLOAT:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_float");
        }

        case HAPI_PARMTYPE_COLOR:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_color");
        }

        case HAPI_PARMTYPE_STRING:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_string");
        }

        case HAPI_PARMTYPE_PATH_FILE:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_path_file");
        }

        case HAPI_PARMTYPE_PATH_FILE_GEO:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_path_file_geo");
        }

        case HAPI_PARMTYPE_PATH_FILE_IMAGE:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_path_file_image");
        }

        case HAPI_PARMTYPE_PATH_NODE:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_path_node");
        }

        case HAPI_PARMTYPE_FOLDERLIST:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_folderlist");
        }

        case HAPI_PARMTYPE_FOLDER:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_folder");
        }

        case HAPI_PARMTYPE_LABEL:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_label");
        }

        case HAPI_PARMTYPE_SEPARATOR:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_separator");
        }

        case HAPI_PARMTYPE_MAX:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_max");
        }

        /*
        case HAPI_PARMTYPE_INT_START:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_int_start");
        }
        */

        /*
        case HAPI_PARMTYPE_INT_END:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_int_end");
        }
        */

        /*
        case HAPI_PARMTYPE_FLOAT_START:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_float_start");
        }
        */

        /*
        case HAPI_PARMTYPE_FLOAT_END:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_float_end");
        }
        */

        /*
        case HAPI_PARMTYPE_STRING_START:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_string_start");
        }
        */

        /*
        case HAPI_PARMTYPE_STRING_END:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_string_end");
        }
        */

        /*
        case HAPI_PARMTYPE_PATH_START:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_path_start");
        }
        */

        /*
        case HAPI_PARMTYPE_PATH_END:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_path_end");
        }
        */

        /*
        case HAPI_PARMTYPE_PATH_FILE_START:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_path_file_start");
        }
        */

        /*
        case HAPI_PARMTYPE_PATH_FILE_END:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_path_file_end");
        }
        */

        /*
        case HAPI_PARMTYPE_PATH_NODE_START:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_path_node_start");
        }
        */

        /*
        case HAPI_PARMTYPE_PATH_NODE_END:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_path_node_end");
        }
        */

        /*
        case HAPI_PARMTYPE_CONTAINER_START:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_container_start");
        }
        */

        /*
        case HAPI_PARMTYPE_CONTAINER_END:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_container_end");
        }
        */

        /*
        case HAPI_PARMTYPE_NONVALUE_START:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_nonvalue_start");
        }
        */

        /*
        case HAPI_PARMTYPE_NONVALUE_END:
        {
            return hapi_private_make_hash_tuple(env, "hapi_parmtype_nonvalue_end");
        }
        */

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
