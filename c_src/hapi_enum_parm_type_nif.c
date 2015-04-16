#include "hapi_enums_nif.h"
#include "hapi_private_nif.h"


bool hapi_enum_parm_type_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ParmType* parm_type)
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

        if(!strcmp(atom_value, "hapi_parmtype_int"))
        {
            *parm_type = HAPI_PARMTYPE_INT;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_multiparmlist"))
        {
            *parm_type = HAPI_PARMTYPE_MULTIPARMLIST;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_toggle"))
        {
            *parm_type = HAPI_PARMTYPE_TOGGLE;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_button"))
        {
            *parm_type = HAPI_PARMTYPE_BUTTON;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_float"))
        {
            *parm_type = HAPI_PARMTYPE_FLOAT;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_color"))
        {
            *parm_type = HAPI_PARMTYPE_COLOR;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_string"))
        {
            *parm_type = HAPI_PARMTYPE_STRING;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_path_file"))
        {
            *parm_type = HAPI_PARMTYPE_PATH_FILE;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_path_file_geo"))
        {
            *parm_type = HAPI_PARMTYPE_PATH_FILE_GEO;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_path_file_image"))
        {
            *parm_type = HAPI_PARMTYPE_PATH_FILE_IMAGE;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_path_node"))
        {
            *parm_type = HAPI_PARMTYPE_PATH_NODE;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_folderlist"))
        {
            *parm_type = HAPI_PARMTYPE_FOLDERLIST;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_folder"))
        {
            *parm_type = HAPI_PARMTYPE_FOLDER;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_label"))
        {
            *parm_type = HAPI_PARMTYPE_LABEL;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_separator"))
        {
            *parm_type = HAPI_PARMTYPE_SEPARATOR;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_max"))
        {
            *parm_type = HAPI_PARMTYPE_MAX;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_int_start"))
        {
            *parm_type = HAPI_PARMTYPE_INT_START;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_int_end"))
        {
            *parm_type = HAPI_PARMTYPE_INT_END;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_float_start"))
        {
            *parm_type = HAPI_PARMTYPE_FLOAT_START;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_float_end"))
        {
            *parm_type = HAPI_PARMTYPE_FLOAT_END;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_string_start"))
        {
            *parm_type = HAPI_PARMTYPE_STRING_START;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_string_end"))
        {
            *parm_type = HAPI_PARMTYPE_STRING_END;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_path_start"))
        {
            *parm_type = HAPI_PARMTYPE_PATH_START;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_path_end"))
        {
            *parm_type = HAPI_PARMTYPE_PATH_END;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_path_file_start"))
        {
            *parm_type = HAPI_PARMTYPE_PATH_FILE_START;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_path_file_end"))
        {
            *parm_type = HAPI_PARMTYPE_PATH_FILE_END;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_path_node_start"))
        {
            *parm_type = HAPI_PARMTYPE_PATH_NODE_START;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_path_node_end"))
        {
            *parm_type = HAPI_PARMTYPE_PATH_NODE_END;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_container_start"))
        {
            *parm_type = HAPI_PARMTYPE_CONTAINER_START;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_container_end"))
        {
            *parm_type = HAPI_PARMTYPE_CONTAINER_END;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_nonvalue_start"))
        {
            *parm_type = HAPI_PARMTYPE_NONVALUE_START;
        }
        else if(!strcmp(atom_value, "hapi_parmtype_nonvalue_end"))
        {
            *parm_type = HAPI_PARMTYPE_NONVALUE_END;
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


ERL_NIF_TERM hapi_enum_parm_type_c_to_erl(ErlNifEnv* env, HAPI_ParmType parm_type)
{
    switch(parm_type)
    {
        case HAPI_PARMTYPE_INT:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_int");
        }

        case HAPI_PARMTYPE_MULTIPARMLIST:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_multiparmlist");
        }

        case HAPI_PARMTYPE_TOGGLE:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_toggle");
        }

        case HAPI_PARMTYPE_BUTTON:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_button");
        }

        case HAPI_PARMTYPE_FLOAT:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_float");
        }

        case HAPI_PARMTYPE_COLOR:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_color");
        }

        case HAPI_PARMTYPE_STRING:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_string");
        }

        case HAPI_PARMTYPE_PATH_FILE:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_path_file");
        }

        case HAPI_PARMTYPE_PATH_FILE_GEO:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_path_file_geo");
        }

        case HAPI_PARMTYPE_PATH_FILE_IMAGE:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_path_file_image");
        }

        case HAPI_PARMTYPE_PATH_NODE:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_path_node");
        }

        case HAPI_PARMTYPE_FOLDERLIST:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_folderlist");
        }

        case HAPI_PARMTYPE_FOLDER:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_folder");
        }

        case HAPI_PARMTYPE_LABEL:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_label");
        }

        case HAPI_PARMTYPE_SEPARATOR:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_separator");
        }

        case HAPI_PARMTYPE_MAX:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_max");
        }

        /*
        case HAPI_PARMTYPE_INT_START:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_int_start");
        }
        */

        /*
        case HAPI_PARMTYPE_INT_END:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_int_end");
        }
        */

        /*
        case HAPI_PARMTYPE_FLOAT_START:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_float_start");
        }
        */

        /*
        case HAPI_PARMTYPE_FLOAT_END:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_float_end");
        }
        */

        /*
        case HAPI_PARMTYPE_STRING_START:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_string_start");
        }
        */

        /*
        case HAPI_PARMTYPE_STRING_END:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_string_end");
        }
        */

        /*
        case HAPI_PARMTYPE_PATH_START:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_path_start");
        }
        */

        /*
        case HAPI_PARMTYPE_PATH_END:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_path_end");
        }
        */

        /*
        case HAPI_PARMTYPE_PATH_FILE_START:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_path_file_start");
        }
        */

        /*
        case HAPI_PARMTYPE_PATH_FILE_END:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_path_file_end");
        }
        */

        /*
        case HAPI_PARMTYPE_PATH_NODE_START:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_path_node_start");
        }
        */

        /*
        case HAPI_PARMTYPE_PATH_NODE_END:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_path_node_end");
        }
        */

        /*
        case HAPI_PARMTYPE_CONTAINER_START:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_container_start");
        }
        */

        /*
        case HAPI_PARMTYPE_CONTAINER_END:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_container_end");
        }
        */

        /*
        case HAPI_PARMTYPE_NONVALUE_START:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_nonvalue_start");
        }
        */

        /*
        case HAPI_PARMTYPE_NONVALUE_END:
        {
            return hapi_private_make_atom(env, "hapi_parmtype_nonvalue_end");
        }
        */

        default:
        {
            break;
        }
    }

    return enif_make_badarg(env);
}
