# Erlang NIF bindings for Houdini Engine.


## Goals of this project.

* To be able to run [Houdini Engine](http://www.sidefx.com/index.php?option=com_content&task=blogcategory&id=227&Itemid=381) instance (and be able to perform asset instantiation, cooking, etc) on an [Erlang](http://www.erlang.org/) (as well as [Elixir](http://elixir-lang.org)) node.
* Create an HTTP RESTful service based on this NIF.
* Create a WebGL client to interact with the RESTful service.

## Before building.

* Install latest build of Houdini 14.
  * If you choose to install to a custom location, you may need to patch rebar.config .
  * Install [Erlang](http://www.erlang.org/download.html) (build it yourself or install pre-built).
  * This NIF library relies on enif_schedule_nif, which requires a fairly recent Erlang OTP, for example OTP 17.5 .
  * If you choose to install to a custom location, you may need to patch rebar.config .
* Install [Rebar](https://github.com/rebar/rebar) (for example through [brew](http://brew.sh/)).

## Building.
Use Rebar:

```
rebar get-deps
rebar compile
```

Or use Rake (also run Rake without arguments to see available tasks):

```
$ rake erlang:deps
$ rake erlang:compile
```

## Running.  

```
> code:add_path("ebin").
true

> hapi:is_initialized().
{hapi_result_not_initialized,2669115710}

> hapi:initialize(nil, nil, nil, false, -1).
{hapi_result_success,3713831785}

> hapi:is_initialized().
{hapi_result_success,3713831785}

> hapi:cleanup().
{hapi_result_success,3713831785}

> hapi:is_initialized().
{hapi_result_not_initialized,2669115710}

> hapi:initialize("", "", {hapi_cook_options, false, 3, false, 8.0, false, false}, false, -1).
{hapi_result_success,3713831785}

> hapi:is_initialized().
{hapi_result_success,3713831785}

> % pass an atom as parameter.
> hapi:get_env_int(hapi_envint_version_houdini_major).
{{hapi_result_success,3713831785},14}

> % pass a hash of an atom as parameter.
> hapi:get_env_int(729783216).
{{hapi_result_success,3713831785},14}

> % pass a tuple of an atom and a hash as parameter.
> hapi:get_env_int({hapi_envint_version_houdini_major, 729783216}).
{{hapi_result_success,3713831785},14}

> hapi:get_env_int(hapi_envint_version_houdini_minor).
{{hapi_result_success,3713831785},5}

> hapi:get_env_int(hapi_envint_version_houdini_build).
{{hapi_result_success,3713831785},80}

> hapi:load_asset_library_from_file("/Users/radix/HoudiniAssets/nonexistant.hda",true).
{hapi_result_cant_loadfile,2751182470}

> hapi:load_asset_library_from_file("otls/dummyboxes.otl",true).
{{hapi_result_success,3713831785},0}

> hapi:get_available_asset_count(0).
{{hapi_result_success,3713831785},1}

> hapi:get_available_assets(0, 1).
{{hapi_result_success,3713831785},[2982]}

> hapi:get_string_buf_length(2982).
{{hapi_result_success,3713831785},29}

> hapi:get_string(2982, 29).
{{hapi_result_success,3713831785}, "hapi::Object/dummyboxes::2.0"}

> hapi:instantiate_asset("hapi::Object/dummyboxes::2.0", true).
{{hapi_result_success,3713831785},0}

> hapi:get_asset_info(0).
{{hapi_result_success,3713831785},
 {hapi_asset_info,0,0,0,1409326048,3,4,false,2968,2984,2932,2985,2941,2940,4,1,1,1,false,false}}

> hapi:get_node_info(3).
{{hapi_result_success,3713831785},
 {hapi_node_info,3,0,3616,0,35,3635,84,64,60,21,66}}

> hapi:get_parameters(3, 0, 84).
{{hapi_result_success,3713831785},
 [{{hapi_parm_info,81,-1,4,0,0,1,0,3712,3713,3715,3714,3715,
                  3714,false,false,true,true,0.0,10.0,0.0,10.0,false,false,...},
  hapi_parm_info,82,-1,0,0,0,1,6,3767,3768,3770,3769,3770,
                  3769,false,false,true,true,0.0,1.0,0.0,1.0,false,false,...},
  {hapi_parm_info,83,-1,0,0,0,1,6,3786,3787,3789,3788,3789,
                  3788,false,false,true,true,0.0,1.0,0.0,1.0,false,false,...},
...
  {...}|...]}

> hapi:get_parm_info(3, 0).
{{hapi_result_success,3713831785},
 {hapi_parm_info,0,-1,0,0,0,1,0,3052,3053,3055,3054,3055,
               3054,false,false,true,true,0.0,10.0,0.0,10.0,false,false,
               false,false,false,...}}

> hapi:get_parm_info(3, 83).
{{hapi_result_success,3713831785},
 {hapi_parm_info,83,-1,0,0,0,1,6,3786,3787,3789,3788,3789,
                3788,false,false,true,true,0.0,1.0,0.0,1.0,false,false,
                false,false,true,...}}

> hapi:get_parm_int_value(3, "int_1", 0).
{{hapi_result_success,3713831785},1}

> hapi:set_parm_int_value(3, "int_1", 0, 42).
{hapi_result_success,3713831785}

> hapi:cook_asset(0, nil).
{hapi_result_success,3713831785}

> hapi:get_parm_int_value(3, "int_1", 0).
{{hapi_result_success,3713831785},42}

> hapi:get_parm_float_value(3, "t", 0).
{{hapi_result_success,3713831785},0.0}

> hapi:cook_asset(0, {hapi_cook_options, false, 3, false, 8.0, false, false}).
{hapi_result_success,3713831785}

> hapi:get_asset_transform(0, hapi_trs, hapi_zyx).
{{hapi_result_success,3713831785},
 {hapi_transform,[0,0,0],
                 [0,0,0],
                 [1,1,1],
                 {hapi_zyx,2804301731},
                 {hapi_trs,2754525809}}}

> hapi:get_parm_id_from_name(3, "int_16").
{{hapi_result_success,3713831785},33}

> hapi:get_parm_info(3, 33).
{{hapi_result_success,3713831785},
 {hapi_parm_info,33,23,0,0,0,16,0,3266,3267,3269,3268,3269,
                 3268,false,false,true,true,0.0,10.0,0.0,10.0,false,false,
                 false,false,false,27...}}

> hapi:get_parm_int_values(3, 27, 16).
{{hapi_result_success,3713831785},
 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]}

> hapi:get_parm_id_from_name(3, "t").
 {{hapi_result_success,3713831785},51}

> hapi:get_parm_info(3, 51).
{{hapi_result_success,3713831785},
 {hapi_parm_info,51,3,4,0,0,3,0,3358,3357,3395,3380,3395,
                  3380,false,false,false,false,0.0,10.0,0.0,10.0,false,false,
                  false,false,false,28,...}}

> hapi:get_parm_float_values(3, 28, 3).
{{hapi_result_success,3713831785},[0.0,0.0,0.0]}

> hapi:set_parm_float_values(3, [5, 42.0, 33.5], 28, 3).
{hapi_result_success,3713831785}

> hapi:cook_asset(0, nil).
{hapi_result_success,3713831785}

> hapi:get_parm_float_values(3, 28, 3).
{{hapi_result_success,3713831785},[5.0,42.0,33.5]}

> hapi:get_preset_buf_length(3, hapi_presettype_binary, "preset").
{{hapi_result_success,3713831785},2626}

> hapi:get_preset(3, 2626).
{{hapi_result_success,3713831785},
 <<"#PSI_PRESET\nversion 2.0a\nopvalues\n{\nversion 0.8\nstdswitcher\t[ 0\tlocks=0 ]\t(\t0\t0\t)\nkeeppos\t[ 0\tlocks=0 ]\t(\t\"o"...>>}

> hapi:destroy_asset(0).
{hapi_result_success,3713831785}

> hapi:cleanup().
{hapi_result_success,3713831785}
```
## Supported HAPI calls ( 50 / 135, 37% complete).

* [hapi:initialize/5](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#acd30bbc744cee30749c71b5cd0976de5)
* [hapi:is_initialized/0](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#acf5bd48d9c280385ba33c33d3f462655)
* [hapi:cleanup/0](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a5307521287693aa26a3d852efb3325a1)
* [hapi:get_env_int/1](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a59bd7b7da3763df60062a18dadaefd3c)
* [hapi:get_status/1](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a8564910986b905f0e6bf72e82e569348)
* [hapi:get_status_string_buf_length/2](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#ae697c8549d95a99198b2eb5a08aa4cdc)
* [hapi:get_status_string/1](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a9f1188ab8adea62e85d615b54c3740b0)
* [hapi:get_cooking_total_count/0](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#aa0efa3f279dd85f54fc5ed404fcfc8d1)
* [hapi:get_cooking_current_count/0](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a9182185f65f302082b6f2fecf6b1f036)
* [hapi:python_thread_interpreter_lock/1](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#af7c0e5dcf32115bec5a8840ae8de82af)
* [hapi:get_string_buf_length/1](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#aa68b45ce4dff58b1dab7e84cf59b307a)
* [hapi:get_string/2](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a8290b6a4e94c2ce7719aba089950f952)
* [hapi:get_time/0](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a92c923f2efd403a0e4211d7502490373)
* [hapi:set_time/1](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#aef698f94574edfe0423fd39bad18c053)
* [hapi:get_timeline_options/0](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a30fd246fa3318805532d43b3f21769fe)
* [hapi:set_timeline_options/1](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a0aebc9230aee217266fb1f3d20cbffc7)
* [hapi:is_asset_valid/2](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a6ae6a403e6a5cab5a3216215b16b7317)
* [hapi:load_asset_library_from_file/2](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#acb0c983a1511b1f0283e82eb84609e43)
* [hapi:load_asset_library_from_memory/3](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a6bd2d14464d256ad0b89cfeb51bc126c)
* [hapi:get_available_asset_count/1](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a34b03f464f83f034a5a1ded3b612a6e9)
* [hapi:get_available_assets/2](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a63b356dac571798c706acd2bf3dad20f)
* [hapi:instantiate_asset/2](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#aa0ad52010159f1a9571242532225af33)
* [hapi:destroy_asset/1](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a0fe290a01716de9ea734771de380b17d)
* [hapi:get_asset_info/1](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#abe5d0d885ca3a53137b142c0b672f463)
* [hapi:cook_asset/2](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a60ec3050a540426aea7ce7ef51797ebf)
* [hapi:interrupt/0](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#ab066a3bbaa1e79aaad78e52443605803)
* [hapi:get_asset_transform/3](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#ae1a1aaab1477e7ccb33b3cc2d61cf289)
* [hapi:get_node_info/1](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a5086b707bd8541489b5eb03517610ee4)
* [hapi:get_parameters/3](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#ad21f31f081b745f94fa5a798cf28ef44)
* [hapi:get_parm_info/2](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#afa56beb2a50d7af40cb10d0619d891a3)
* [hapi:get_parm_id_from_name/2](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#ae0632a0616fce5696463936a962409b3)
* [hapi:get_parm_info_from_name/2](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a45863fbcdfc9e56c8a47198233bed531)
* [hapi:get_parm_int_value/3](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a743eb11c638307d36da0ee609a3f1ebc)
* [hapi:get_parm_int_values/3](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#aa55a335e0359ae34278ae68c08dc4d00)
* [hapi:get_parm_float_value/3](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a09465f60a052651e10ea0abfda71141a)
* [hapi:get_parm_float_values/3](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#ababb5d4a50952884002caa46f8160f19)
* [hapi:get_parm_string_value/4](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a3e3f14ed3a120e914bfba13f46ac8cf7)
* [hapi:get_parm_string_values/4](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a561adb791f4c678381af0b973cd050ce)
* [hapi:get_parm_choice_lists/3](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#affc757d3433458e4a3d86ff6b1dce760)
* [hapi:set_parm_int_value/4](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a4c8fa4d73f30acf99c5a8348c648eb22)
* [hapi:set_parm_float_value/4](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#aa519aebc0ff408a2526808c82b45b888)
* [hapi:set_parm_int_values/4](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a483745f85d0e6ac9448c729814b00d9a)
* [hapi:set_parm_float_values/4](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a79c1484bb10e294ee1ec19bcc4235f47)
* [hapi:set_parm_string_value/4](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a62a205e2570451f0bfaeb757cb73a994)
* [hapi:get_handle_info/3](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#aad37a79725a1714523809021d4fb6edd)
* [hapi:get_handle_binding_info/4](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#af3d635abce8787e1e63c687e8d2fd060)
* [hapi:get_preset_buf_length/3](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a392e4fd916f68359f04006cd57a299a2)
* [hapi:get_preset/2](http://www.sidefx.com/docs/hengine1.9/_h_a_p_i_8h.html#a1aaf85e26914888e1fd6c3692d539459)
* **Other HAPI calls are being added.**

## Additional helper calls.

* hapi:hash_enum_value/1
* hapi:check_enum_value_hash/2

## Other notes.

* This is a personal project.
* A lot of code is autogenerated and will be cleaned up.
* Please report bugs.
* Pull requests are awesome.
