/// @author Mykola Konyk <mykola@konyk.org>
///
/// @copyright 2015
/// @license MS-RL
/// This file is autogenerated from util/hapi_enums_nif.h.template

#pragma once

#if !defined(HAPI_ENUMS_NIF_H)
#define HAPI_ENUMS_NIF_H

#include "erl_nif.h"
#include "HAPI.h"


/* Defined in enums/hapi_ramptype_nif.c */
bool hapi_ramptype_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_RampType* enum_result);
ERL_NIF_TERM hapi_ramptype_c_to_erl(ErlNifEnv* env, HAPI_RampType enum_value);

/* Defined in enums/hapi_xyzorder_nif.c */
bool hapi_xyzorder_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_XYZOrder* enum_result);
ERL_NIF_TERM hapi_xyzorder_c_to_erl(ErlNifEnv* env, HAPI_XYZOrder enum_value);

/* Defined in enums/hapi_imagedataformat_nif.c */
bool hapi_imagedataformat_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ImageDataFormat* enum_result);
ERL_NIF_TERM hapi_imagedataformat_c_to_erl(ErlNifEnv* env, HAPI_ImageDataFormat enum_value);

/* Defined in enums/hapi_license_nif.c */
bool hapi_license_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_License* enum_result);
ERL_NIF_TERM hapi_license_c_to_erl(ErlNifEnv* env, HAPI_License enum_value);

/* Defined in enums/hapi_geotype_nif.c */
bool hapi_geotype_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_GeoType* enum_result);
ERL_NIF_TERM hapi_geotype_c_to_erl(ErlNifEnv* env, HAPI_GeoType enum_value);

/* Defined in enums/hapi_inputtype_nif.c */
bool hapi_inputtype_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_InputType* enum_result);
ERL_NIF_TERM hapi_inputtype_c_to_erl(ErlNifEnv* env, HAPI_InputType enum_value);

/* Defined in enums/hapi_assettype_nif.c */
bool hapi_assettype_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_AssetType* enum_result);
ERL_NIF_TERM hapi_assettype_c_to_erl(ErlNifEnv* env, HAPI_AssetType enum_value);

/* Defined in enums/hapi_rstorder_nif.c */
bool hapi_rstorder_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_RSTOrder* enum_result);
ERL_NIF_TERM hapi_rstorder_c_to_erl(ErlNifEnv* env, HAPI_RSTOrder enum_value);

/* Defined in enums/hapi_attributeowner_nif.c */
bool hapi_attributeowner_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_AttributeOwner* enum_result);
ERL_NIF_TERM hapi_attributeowner_c_to_erl(ErlNifEnv* env, HAPI_AttributeOwner enum_value);

/* Defined in enums/hapi_shadertype_nif.c */
bool hapi_shadertype_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ShaderType* enum_result);
ERL_NIF_TERM hapi_shadertype_c_to_erl(ErlNifEnv* env, HAPI_ShaderType enum_value);

/* Defined in enums/hapi_imagepacking_nif.c */
bool hapi_imagepacking_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ImagePacking* enum_result);
ERL_NIF_TERM hapi_imagepacking_c_to_erl(ErlNifEnv* env, HAPI_ImagePacking enum_value);

/* Defined in enums/hapi_result_nif.c */
bool hapi_result_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_Result* enum_result);
ERL_NIF_TERM hapi_result_c_to_erl(ErlNifEnv* env, HAPI_Result enum_value);

/* Defined in enums/hapi_curvetype_nif.c */
bool hapi_curvetype_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_CurveType* enum_result);
ERL_NIF_TERM hapi_curvetype_c_to_erl(ErlNifEnv* env, HAPI_CurveType enum_value);

/* Defined in enums/hapi_statusverbosity_nif.c */
bool hapi_statusverbosity_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_StatusVerbosity* enum_result);
ERL_NIF_TERM hapi_statusverbosity_c_to_erl(ErlNifEnv* env, HAPI_StatusVerbosity enum_value);

/* Defined in enums/hapi_permissions_nif.c */
bool hapi_permissions_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_Permissions* enum_result);
ERL_NIF_TERM hapi_permissions_c_to_erl(ErlNifEnv* env, HAPI_Permissions enum_value);

/* Defined in enums/hapi_envinttype_nif.c */
bool hapi_envinttype_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_EnvIntType* enum_result);
ERL_NIF_TERM hapi_envinttype_c_to_erl(ErlNifEnv* env, HAPI_EnvIntType enum_value);

/* Defined in enums/hapi_assetsubtype_nif.c */
bool hapi_assetsubtype_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_AssetSubType* enum_result);
ERL_NIF_TERM hapi_assetsubtype_c_to_erl(ErlNifEnv* env, HAPI_AssetSubType enum_value);

/* Defined in enums/hapi_grouptype_nif.c */
bool hapi_grouptype_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_GroupType* enum_result);
ERL_NIF_TERM hapi_grouptype_c_to_erl(ErlNifEnv* env, HAPI_GroupType enum_value);

/* Defined in enums/hapi_transformcomponent_nif.c */
bool hapi_transformcomponent_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_TransformComponent* enum_result);
ERL_NIF_TERM hapi_transformcomponent_c_to_erl(ErlNifEnv* env, HAPI_TransformComponent enum_value);

/* Defined in enums/hapi_statustype_nif.c */
bool hapi_statustype_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_StatusType* enum_result);
ERL_NIF_TERM hapi_statustype_c_to_erl(ErlNifEnv* env, HAPI_StatusType enum_value);

/* Defined in enums/hapi_state_nif.c */
bool hapi_state_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_State* enum_result);
ERL_NIF_TERM hapi_state_c_to_erl(ErlNifEnv* env, HAPI_State enum_value);

/* Defined in enums/hapi_curveorders_nif.c */
bool hapi_curveorders_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_CurveOrders* enum_result);
ERL_NIF_TERM hapi_curveorders_c_to_erl(ErlNifEnv* env, HAPI_CurveOrders enum_value);

/* Defined in enums/hapi_presettype_nif.c */
bool hapi_presettype_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_PresetType* enum_result);
ERL_NIF_TERM hapi_presettype_c_to_erl(ErlNifEnv* env, HAPI_PresetType enum_value);

/* Defined in enums/hapi_parmtype_nif.c */
bool hapi_parmtype_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_ParmType* enum_result);
ERL_NIF_TERM hapi_parmtype_c_to_erl(ErlNifEnv* env, HAPI_ParmType enum_value);

/* Defined in enums/hapi_storagetype_nif.c */
bool hapi_storagetype_erl_to_c(ErlNifEnv* env, const ERL_NIF_TERM term, HAPI_StorageType* enum_result);
ERL_NIF_TERM hapi_storagetype_c_to_erl(ErlNifEnv* env, HAPI_StorageType enum_value);


#endif //!defined(HAPI_ENUMS_NIF_H)
