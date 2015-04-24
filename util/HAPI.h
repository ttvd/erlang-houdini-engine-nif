/*
 * PROPRIETARY INFORMATION.  This software is proprietary to
 * Side Effects Software Inc., and is not to be reproduced,
 * transmitted, or disclosed in any way without written permission.
 *
 * Produced by:
 *      Side Effects Software Inc
 *      123 Front Street West, Suite 1401
 *      Toronto, Ontario
 *      Canada   M5J 2M2
 *      416-504-9876
 *
 * COMMENTS:
 */

#ifndef __HAPI_h__
#define __HAPI_h__

#include "HAPI_API.h"
#include "HAPI_Common.h"

    // INITIALIZATION / CLEANUP ---------------------------------------------

    /// @brief  Check whether the runtime has been intialized yet using
    ///         ::HAPI_Initialize(). Function will return ::HAPI_RESULT_SUCCESS
    ///         if the runtime has been initialized and ::HAPI_RESULT_FAILURE
    ///         otherwise.
    ///
    HAPI_DECL HAPI_IsInitialized();

    /// @brief  Create the asset manager, set up environment variables, and
    ///         initialize the main Houdini scene. No license checking is 
    ///         during this step. Only when you try to load an asset library
    ///         (OTL) do we actually check for licenses.
    ///
    /// @param[in]      otl_search_path
    ///                 The directory where OTLs are searched for. You can 
    ///                 pass NULL here which will only use the default 
    ///                 Houdini OTL search paths. You can also pass in 
    ///                 multiple paths separated by a ";" on Windows and a ":" 
    ///                 on Linux and Mac. If something other than NULL is 
    ///                 passed the default Houdini search paths will be 
    ///                 appended to the end of the path string.
    ///
    /// @param[in]      dso_search_path
    ///                 The directory where DSOs (custom plugins) are 
    ///                 searched for. You can pass NULL here which will
    ///                 only use the default Houdini DSO search paths. You
    ///                 can also pass in multiple paths separated by a ";"
    ///                 on Windows and a ":" on Linux and Mac. If something 
    ///                 other than NULL is passed the default Houdini search 
    ///                 paths will be appended to the end of the path string.
    ///
    /// @param[in]      cook_options
    ///                 Global cook options used by subsequent default cooks.
    ///                 This can be overwritten by individual cooks but if
    ///                 you choose to instantiate assets with cook_on_load
    ///                 set to true then these cook options will be used.
    ///
    /// @param[in]      use_cooking_thread
    ///                 Use a separate thread for cooking of assets. This
    ///                 allows for asynchronous cooking and larger stack size.
    ///
    /// @param[in]      cooking_thread_stack_size
    ///                 Set the stack size of the cooking thread. Use -1 to
    ///                 set the stack size to the Houdini default. This
    ///                 value is in bytes.
    ///
    /// [HAPI_Initialize]
    HAPI_DECL HAPI_Initialize( const char * otl_search_path,
                               const char * dso_search_path,
                               const HAPI_CookOptions * cook_options,
                               HAPI_Bool use_cooking_thread,
                               int cooking_thread_stack_size );
    /// [HAPI_Initialize]

    /// @brief  Clean up memory. This will unload all assets and you will
    ///         need to call ::HAPI_Initialize() again to be able to use any
    ///         HAPI methods again.
    ///
    ///         @note This does NOT release any licenses.
    ///
    HAPI_DECL HAPI_Cleanup();

    // DIAGNOSTICS ----------------------------------------------------------

    /// @brief  Gives back a certain environment integer like version number.
    ///
    /// @param[in]      int_type
    ///                 One of ::HAPI_EnvIntType.

    ///
    /// @param[out]     value
    ///                 Int value.
    ///
    HAPI_DECL HAPI_GetEnvInt( HAPI_EnvIntType int_type, int * value ); 

    /// @brief  Gives back the status code for a specific status type.
    ///
    /// @param[in]      status_type
    ///                 One of ::HAPI_StatusType.
    ///
    /// @param[out]     status
    ///                 Actual status code for the status type given. That is,
    ///                 if you pass in ::HAPI_STATUS_CALL_RESULT as
    ///                 status_type, you'll get back a ::HAPI_Result for this
    ///                 argument. If you pass in ::HAPI_STATUS_COOK_STATE
    ///                 as status_type, you'll get back a ::HAPI_State enum
    ///                 for this argument.
    ///
    HAPI_DECL HAPI_GetStatus( HAPI_StatusType status_type, int * status );

    /// @brief  Return length of string buffer storing status string message.
    ///
    ///         If called with ::HAPI_STATUS_COOK_RESULT this will actually
    ///         parse the node networks for the previously cooked asset(s)
    ///         and aggregate all node errors, warnings, and messages
    ///         (depending on the @c verbosity level set). Usually this is done
    ///         just for the last cooked single asset but if you load a whole
    ///         Houdini scene using ::HAPI_LoadHIPFile() then you'll have
    ///         multible "previously cooked assets".
    ///
    ///         You MUST call ::HAPI_GetStatusStringBufLength() before calling
    ///         ::HAPI_GetStatusString() because ::HAPI_GetStatusString() will
    ///         not return the real status string and instead return a
    ///         cached version of the string that was created inside
    ///         ::HAPI_GetStatusStringBufLength(). The reason for this is that
    ///         the length of the real status string may change between
    ///         the call to ::HAPI_GetStatusStringBufLength() and the call to
    ///         ::HAPI_GetStatusString().
    ///
    /// @param[in]      status_type
    ///                 One of ::HAPI_StatusType.
    ///
    /// @param[in]      verbosity
    ///                 Preferred verbosity level.
    ///
    /// @param[out]     buffer_size
    ///                 Length of buffer char array ready to be filled.
    ///
    HAPI_DECL HAPI_GetStatusStringBufLength( HAPI_StatusType status_type,
                                             HAPI_StatusVerbosity verbosity,
                                             int * buffer_size );

    /// @brief  Return status string message.
    ///
    ///         You MUST call ::HAPI_GetStatusStringBufLength() before calling
    ///         ::HAPI_GetStatusString() because ::HAPI_GetStatusString() will
    ///         not return the real status string and instead return a
    ///         cached version of the string that was created inside
    ///         ::HAPI_GetStatusStringBufLength(). The reason for this is that
    ///         the length of the real status string may change between
    ///         the call to ::HAPI_GetStatusStringBufLength() and the call to
    ///         ::HAPI_GetStatusString().
    ///
    /// @param[in]      status_type
    ///                 One of ::HAPI_StatusType.
    ///
    /// @param[out]     buffer
    ///                 Buffer char array ready to be filled.
    ///
    HAPI_DECL HAPI_GetStatusString( HAPI_StatusType status_type,
                                    char * buffer );

    /// @brief  Get total number of nodes that need to cook in the current
    ///         session.
    ///
    /// @param[out]     count
    ///                 Total cook count.
    ///
    HAPI_DECL HAPI_GetCookingTotalCount( int * count );

    /// @brief  Get current number of nodes that have already cooked in the
    ///         current session. Note that this is a very crude approximation
    ///         of the cooking progress - it may never make it to 100% or it 
    ///         might spend another hour at 100%. Use ::HAPI_GetStatusString
    ///         to get a better idea of progress if this number gets stuck.
    ///
    /// @param[out]     count
    ///                 Current cook count.
    ///
    HAPI_DECL HAPI_GetCookingCurrentCount( int * count );

    // UTILITY --------------------------------------------------------------

    /// @brief  Converts the transform described by a ::HAPI_TransformEuler 
    ///         struct into a different transform and rotation order.
    ///
    /// @param[in,out]  transform_in_out
    ///                 Used for both input and output.
    ///
    /// @param[in]      rst_order
    ///                 The desired transform order of the output.
    ///
    /// @param[in]      rot_order
    ///                 The desired rotation order of the output.
    ///
    HAPI_DECL HAPI_ConvertTransform( HAPI_TransformEuler * transform_in_out,
                                     HAPI_RSTOrder rst_order,
                                     HAPI_XYZOrder rot_order );

    /// @brief  Converts a 4x4 matrix into its TRS form.
    ///         
    /// @param[in]      mat
    ///                 A 4x4 matrix expressed in a 16 element float array.
    ///
    /// @param[in]      rst_order
    ///                 The desired transform order of the output.
    ///
    /// @param[out]     transform_out
    ///                 Used for the output.
    ///
    HAPI_DECL HAPI_ConvertMatrixToQuat( float * mat,
                                        HAPI_RSTOrder rst_order,
                                        HAPI_Transform * transform_out );

    /// @brief  Converts a 4x4 matrix into its TRS form.
    ///         
    /// @param[in]      mat
    ///                 A 4x4 matrix expressed in a 16 element float array.
    ///
    /// @param[in]      rst_order
    ///                 The desired transform order of the output.
    ///
    /// @param[in]      rot_order
    ///                 The desired rotation order of the output.
    ///
    /// @param[out]     transform_out
    ///                 Used for the output.
    ///
    HAPI_DECL HAPI_ConvertMatrixToEuler( float * mat, 
                                         HAPI_RSTOrder rst_order,
                                         HAPI_XYZOrder rot_order,
                                         HAPI_TransformEuler * transform_out );

    /// @brief  Converts ::HAPI_Transform into a 4x4 transform matrix.
    ///         
    /// @param[in]      transform
    ///                 The ::HAPI_Transform you wish to convert.
    ///
    /// @param[out]     matrix
    ///                 A 16 element float array that will contain the result.
    ///
    HAPI_DECL HAPI_ConvertTransformQuatToMatrix(
                                        const HAPI_Transform * transform,
                                        float * matrix );

    /// @brief  Converts ::HAPI_TransformEuler into a 4x4 transform matrix.
    ///         
    /// @param[in]      transform
    ///                 The ::HAPI_TransformEuler you wish to convert.
    ///
    /// @param[out]     matrix
    ///                 A 16 element float array that will contain the result.
    ///
    HAPI_DECL HAPI_ConvertTransformEulerToMatrix(
                                        const HAPI_TransformEuler * transform,
                                        float * matrix );

    /// @brief  Acquires or releases the Python interpreter lock. This is
    ///         needed if HAPI is called from Python and HAPI is in threaded
    ///         mode (see ::HAPI_Initialize()).
    /// 
    ///         The problem arises when async functions like
    ///         ::HAPI_InstantiateAsset() may start a cooking thread that
    ///         may try to run Python code. That is, we would now have 
    ///         Python running on two different threads - something not
    ///         allowed by Python by default.
    ///
    ///         We need to tell Python to explicitly "pause" the Python state
    ///         on the client thread while we run Python in our cooking thread.
    ///
    ///         You must call this function first with locked == true before
    ///         any async HAPI call. Then, after the async call finished,
    ///         detected via calls to ::HAPI_GetStatus(), call this method
    ///         again to release the lock with locked == false.
    /// 
    /// @param[in]      locked
    ///                 True will acquire the interpreter lock to use it for
    ///                 the HAPI cooking thread. False will release the lock
    ///                 back to the client thread.
    ///
    HAPI_DECL HAPI_PythonThreadInterpreterLock( HAPI_Bool locked );

    // STRINGS --------------------------------------------------------------

    /// @brief  Gives back the string length of the string with the 
    ///         given handle.
    ///
    /// @param[in]      string_handle
    ///                 Handle of the string to query.
    ///
    /// @param[out]     buffer_length
    ///                 Buffer length of the queried string (including NULL
    ///                 terminator).
    ///
    HAPI_DECL HAPI_GetStringBufLength( HAPI_StringHandle string_handle,
                                       int * buffer_length );

    /// @brief  Gives back the string value of the string with the 
    ///         given handle.
    ///
    /// @param[in]      string_handle
    ///                 Handle of the string to query.
    ///
    /// @param[out]     string_value
    ///                 Actual string value (character array).
    ///
    /// @param[in]      buffer_length
    ///                 Length of the string buffer (must match size of 
    ///                 string_value - so including NULL terminator).
    ///
    HAPI_DECL HAPI_GetString( HAPI_StringHandle string_handle,
                              char * string_value, 
                              int buffer_length );

    // TIME -----------------------------------------------------------------

    /// @brief  Gets the global time of the scene. All API calls deal with
    ///         this time to cook.
    ///
    /// @param[out]     time
    ///                 Time as a float in seconds.
    ///
    HAPI_DECL HAPI_GetTime( float * time );

    /// @brief  Sets the global time of the scene. All API calls will deal 
    ///         with this time to cook.
    ///
    /// @param[in]      time
    ///                 Time as a float in seconds.
    ///
    HAPI_DECL HAPI_SetTime( float time );

    /// @brief  Gets the current global timeline options.
    ///
    /// @param[in]      timeline_options
    ///                 The global timeline options struct.
    ///
    HAPI_DECL HAPI_GetTimelineOptions(
                                HAPI_TimelineOptions * timeline_options );

    /// @brief  Sets the global timeline options.
    ///
    /// @param[in]      timeline_options
    ///                 The global timeline options struct.
    ///
    HAPI_DECL HAPI_SetTimelineOptions(
                            const HAPI_TimelineOptions * timeline_options );

    // ASSETS ---------------------------------------------------------------

    /// @brief  Determine if your instance of the asset actually still exists
    ///         inside the Houdini scene. This is what can be used to
    ///         determine when the Houdini scene needs to be re-populated
    ///         using the host application's instances of the assets.
    ///         Note that this function will ALWAYS return
    ///         ::HAPI_RESULT_SUCCESS.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      asset_validation_id
    ///                 The asset validation id that's found in the
    ///                 ::HAPI_AssetInfo struct returned by
    ///                 ::HAPI_GetAssetInfo.
    ///
    /// @param[out]     answer
    ///                 Answer to the question. 1 for valid, 0 for invalid.
    ///
    HAPI_DECL HAPI_IsAssetValid( HAPI_AssetId asset_id, 
                                 int asset_validation_id,
                                 int * answer );

    /// @brief  Loads a Houdini asset library (OTL) from a .otl file.
    ///         It does NOT instantiate anything inside the Houdini scene.
    ///
    ///         @note This is when we actually check for valid licenses.
    ///
    ///         The next step is to call ::HAPI_GetAvailableAssetCount()
    ///         to get the number of assets contained in the library using the
    ///         returned library_id. Then call ::HAPI_GetAvailableAssets()
    ///         to get the list of available assets by name. Use the asset
    ///         names with ::HAPI_InstantiateAsset() to actually instantiate
    ///         one of these assets in the Houdini scene and get back
    ///         an asset_id.
    ///
    ///         @note The HIP file saved using ::HAPI_SaveHIPFile() will only
    ///             have an absolute path reference to the loaded OTL meaning
    ///             that if the OTL is moved or renamed the HIP file won't
    ///             load properly. It also means that if you change the OTL
    ///             using the saved HIP scene the same OTL file will change
    ///             as the one used with Houdini Engine.
    ///             See @ref HAPI_Fundamentals_SavingHIPFile.
    ///
    /// @param[in]      file_path
    ///                 Absolute path to the .otl file.
    ///
    /// @param[in]      allow_overwrite
    ///                 With this true, if the library file being loaded
    ///                 contains asset defintions that have already been
    ///                 loaded they will overwrite the existing definitions.
    ///                 Otherwise, a library containing asset defintions that
    ///                 already exist will fail to load, returning a
    ///                 ::HAPI_Result of
    ///                 ::HAPI_RESULT_ASSET_DEF_ALREADY_LOADED.
    ///
    /// @param[out]     library_id
    ///                 Newly loaded otl id to be used with 
    ///                 ::HAPI_GetAvailableAssetCount() and
    ///                 ::HAPI_GetAvailableAssets().
    ///
    HAPI_DECL HAPI_LoadAssetLibraryFromFile( const char * file_path,
                                             HAPI_Bool allow_overwrite,
                                             HAPI_AssetLibraryId* library_id );

    /// @brief  Loads a Houdini asset library (OTL) from memory.
    ///         It does NOT instantiate anything inside the Houdini scene.
    ///
    ///         @note This is when we actually check for valid licenses.
    ///
    ///         Please note that the performance benefit of loading a library
    ///         from memory are negligible at best. Due to limitations of 
    ///         Houdini's library manager, there is still some disk access
    ///         and file writes because every asset library needs to be
    ///         saved to a real file. Use this function only as a convenience
    ///         if you already have the library file in memory and don't wish
    ///         to have to create your own temporary library file and then
    ///         call ::HAPI_LoadAssetLibraryFromFile().
    ///
    ///         The next step is to call ::HAPI_GetAvailableAssetCount()
    ///         to get the number of assets contained in the library using the
    ///         returned library_id. Then call ::HAPI_GetAvailableAssets()
    ///         to get the list of available assets by name. Use the asset
    ///         names with ::HAPI_InstantiateAsset() to actually instantiate
    ///         one of these assets in the Houdini scene and get back
    ///         an asset_id.
    ///
    ///         @note The saved HIP file using ::HAPI_SaveHIPFile() will
    ///             @a contain the OTL loaded as part of its @b Embedded OTLs.
    ///             This means that you can safely move or rename the original
    ///             OTL file and the HIP will continue to work but if you make
    ///             changes to the OTL while using the saved HIP the changes
    ///             won't be saved to the original OTL.
    ///             See @ref HAPI_Fundamentals_SavingHIPFile.
    ///
    /// @param[in]      library_buffer
    ///                 The memory buffer containing the asset definitions
    ///                 in the same format as a standard Houdini .otl file.
    ///
    /// @param[in]      library_buffer_size
    ///                 The size of the OTL memory buffer.
    ///
    /// @param[in]      allow_overwrite
    ///                 With this true, if the library file being loaded
    ///                 contains asset defintions that have already been
    ///                 loaded they will overwrite the existing definitions.
    ///                 Otherwise, a library containing asset defintions that
    ///                 already exist will fail to load, returning a
    ///                 ::HAPI_Result of
    ///                 ::HAPI_RESULT_ASSET_DEF_ALREADY_LOADED.
    ///
    /// @param[out]     library_id
    ///                 Newly loaded otl id to be used with 
    ///                 ::HAPI_GetAvailableAssetCount() and
    ///                 ::HAPI_GetAvailableAssets().
    ///
    HAPI_DECL HAPI_LoadAssetLibraryFromMemory(
                                        const char * library_buffer,
                                        int library_buffer_size,
                                        HAPI_Bool allow_overwrite,
                                        HAPI_AssetLibraryId * library_id );

    /// @brief  Get the number of assets contained in an asset library.
    ///         You should call ::HAPI_LoadAssetLibraryFromFile() prior to
    ///         get a library_id.
    ///
    /// @param[in]      library_id
    ///                 Returned by ::HAPI_LoadAssetLibraryFromFile().
    ///
    /// @param[out]     asset_count
    ///                 The number of assets contained in this asset library.
    ///
    HAPI_DECL HAPI_GetAvailableAssetCount( HAPI_AssetLibraryId library_id, 
                                           int * asset_count );

    /// @brief  Get the names of the assets contained in an asset library.
    ///
    ///         The asset names will contain additional information about
    ///         the type of asset, namespace, and version, along with the
    ///         actual asset name. For example, if you have an Object type
    ///         asset, in the "hapi" namespace, of version 2.0, named
    ///         "foo", the asset name returned here will be: 
    ///         hapi::Object/foo::2.0
    ///
    ///         However, you should not need to worry about this detail. Just
    ///         pass this string directly to ::HAPI_InstantiateAsset() to 
    ///         instantiate the asset. You can then get the pretty name 
    ///         using ::HAPI_GetAssetInfo().
    ///
    ///         You should call ::HAPI_LoadAssetLibraryFromFile() prior to
    ///         get a library_id. Then, you should call
    ///         ::HAPI_GetAvailableAssetCount() to get the number of assets to
    ///         know how large of a string handles array you need to allocate.
    ///
    /// @param[in]      library_id
    ///                 Returned by ::HAPI_LoadAssetLibraryFromFile().
    ///
    /// @param[out]     asset_names
    ///                 Array of string handles (integers) that should be
    ///                 at least the size of asset_count.
    ///
    /// @param[out]     asset_count
    ///                 Should be the same or less than the value returned by
    ///                 ::HAPI_GetAvailableAssetCount().
    ///
    HAPI_DECL HAPI_GetAvailableAssets( HAPI_AssetLibraryId library_id,
                                       HAPI_StringHandle * asset_names,
                                       int asset_count );

    /// @brief  Instantiate an asset by name. The asset has to have been
    ///         loaded as part of an asset library, using 
    ///         ::HAPI_LoadAssetLibraryFromFile().
    ///
    ///         @note In threaded mode, this is an _async call_!
    ///
    ///         @note This is also when we actually check for valid licenses.
    ///
    ///         This API will invoke the cooking thread if threading is 
    ///         enabled. This means it will return immidiately with a call
    ///         result of ::HAPI_RESULT_SUCCESS, even if fed garbage. Use
    ///         the status and cooking count APIs under DIAGNOSTICS to get
    ///         a sense of the progress. All other API calls will block
    ///         until the instatiation (and, optionally, the first cook)
    ///         has finished.
    ///
    ///         Also note that the cook result won't be of type
    ///         ::HAPI_STATUS_CALL_RESULT like all calls (including this one).
    ///         Whenever the threading cook is done it will fill the
    ///         @a cook result which is queried using
    ///         ::HAPI_STATUS_COOK_RESULT.
    ///
    /// @param[in]      asset_name
    ///                 The name of the asset to load. Use 
    ///                 ::HAPI_GetAvailableAssets() to get the available
    ///                 asset names in a loaded asset library.
    ///
    /// @param[out]     cook_on_load
    ///                 Set to true if you wish the asset to cook as soon
    ///                 as it is instantiated. Otherwise, you will have to
    ///                 call ::HAPI_CookAsset() explicitly after you call
    ///                 this function.
    ///
    ///                 Normally you should set this to true but if you want
    ///                 to change parameters on an asset before the first
    ///                 cook set this to false. You can then use 
    ///                 ::HAPI_GetAssetInfo() to get the node_id of the asset
    ///                 and use the parameter APIs to update the values.
    ///                 Then, call ::HAPI_CookAsset() explicitly. To know
    ///                 whether an asset as cooked at least once there is a
    ///                 flag in ::HAPI_AssetInfo called hasEverCooked.
    ///
    /// @param[out]     asset_id
    ///                 Newly created asset's id. Use ::HAPI_GetAssetInfo()
    ///                 to get more information about the asset.
    ///
    HAPI_DECL HAPI_InstantiateAsset( const char * asset_name,
                                     HAPI_Bool cook_on_load,
                                     HAPI_AssetId * asset_id );

    /// @brief  Creates a special curve asset that can be used as input for
    ///         other assets in the scene.
    ///
    ///         @note In threaded mode, this is an _async call_!
    ///
    ///         Note that when saving the Houdini scene using 
    ///         ::HAPI_SaveHIPFile() the curve nodes created with this
    ///         method will be yellow and will start with the name "curve".
    ///
    /// @param[out]     asset_id
    ///                 Newly created curve's asset id. Use
    ///                 ::HAPI_GetAssetInfo() to get more information
    ///                 about the asset.
    ///
    HAPI_DECL HAPI_CreateCurve( HAPI_AssetId * asset_id );

    /// @brief  Creates a special asset that can accept geometry input.
    ///         This will create a dummy OBJ node with a Null SOP inside that
    ///         you can set the geometry of using the geometry SET APIs.
    ///         You can then connect this asset to any other asset using
    ///         inter-asset connection APIs.
    ///
    ///         @note In threaded mode, this is an _async call_!
    ///
    ///         All you need to remember is that this asset has a single
    ///         object and a single geo which means that for all subsequent
    ///         geometry setter functions just pass in 0 (zero) for the
    ///         object id and 0 (zero) for the geo id.
    ///
    ///         Note that when saving the Houdini scene using 
    ///         ::HAPI_SaveHIPFile() the curve nodes created with this
    ///         method will be green and will start with the name "input".
    ///
    /// @param[out]     asset_id
    ///                 Newly created asset's id. Use ::HAPI_GetAssetInfo()
    ///                 to get more information about the asset.
    ///
    /// @param[in]      name
    ///                 Give this input asset a name for easy debugging.
    ///                 The asset's obj node and the null SOP node will both
    ///                 get this given name with "input_" prepended.
    ///                 You can also pass NULL in which case the name will
    ///                 be "input#" where # is some number.
    ///
    HAPI_DECL HAPI_CreateInputAsset( HAPI_AssetId * asset_id,
                                     const char * name );

    /// @brief  Destroy the asset instance.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    HAPI_DECL HAPI_DestroyAsset( HAPI_AssetId asset_id );

    /// @brief  Fill an asset_info struct.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[out]     asset_info
    ///                 Return value - contains things like asset id.
    ///
    HAPI_DECL HAPI_GetAssetInfo( HAPI_AssetId asset_id, 
                                 HAPI_AssetInfo * asset_info );

    /// @brief  Initiate a cook on this asset. Note that this may trigger
    ///         cooks on other assets if they are connected.
    ///
    ///         @note In threaded mode, this is an _async call_!
    ///
    ///         This API will invoke the cooking thread if threading is 
    ///         enabled. This means it will return immidiately. Use
    ///         the status and cooking count APIs under DIAGNOSTICS to get
    ///         a sense of the progress. All other API calls will block
    ///         until the cook operation has finished.
    ///
    ///         Also note that the cook result won't be of type
    ///         ::HAPI_STATUS_CALL_RESULT like all calls (including this one).
    ///         Whenever the threading cook is done it will fill the
    ///         @a cook result which is queried using
    ///         ::HAPI_STATUS_COOK_RESULT.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      cook_options
    ///                 The cook options. Pass in NULL to use the global
    ///                 cook options that you specified when calling
    ///                 ::HAPI_Initialize().
    ///
    HAPI_DECL HAPI_CookAsset( HAPI_AssetId asset_id,
                              const HAPI_CookOptions * cook_options );

    /// @brief  Interrupt a cook or load operation.
    ///
    HAPI_DECL HAPI_Interrupt();

    /// @brief  Get the transform of an asset to match the transform of the
    ///         asset on the client side.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      rst_order
    ///                 The order of application of translation, rotation and
    ///                 scale.
    ///
    /// @param[in]      rot_order
    ///                 The desired rotation order of the output.
    ///
    /// @param[out]     transform
    ///                 The actual transform struct.
    ///
    HAPI_DECL HAPI_GetAssetTransform( HAPI_AssetId asset_id,
                                      HAPI_RSTOrder rst_order,
                                      HAPI_XYZOrder rot_order,
                                      HAPI_TransformEuler * transform );

    /// @brief  Set the transform of an asset to match the transform of the
    ///         asset on the client side.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      transform
    ///                 The actual transform struct.
    ///
    HAPI_DECL HAPI_SetAssetTransform( HAPI_AssetId asset_id,
                                      HAPI_TransformEuler * transform );

    /// @brief  Get the name of an asset's input. This function will return
    ///         a string handle for the name which will be valid (persist)
    ///         until the next call to this function.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      input_idx
    ///                 Input index of the asset.
    ///
    /// @param[in]      input_type
    ///                 Of type ::HAPI_InputType.
    ///
    /// @param[out]     name
    ///                 Input name string handle return value - valid until
    ///                 the next call to this function.
    ///
    HAPI_DECL HAPI_GetInputName( HAPI_AssetId asset_id, 
                                 int input_idx, int input_type, 
                                 HAPI_StringHandle * name );

    // HIP FILES ------------------------------------------------------------

    /// @brief  Loads a .hip file into the main Houdini scene.
    ///
    ///         @note In threaded mode, this is an _async call_!
    ///
    /// @param[in]      file_name
    ///                 Absolute path to the .hip file to load.
    ///
    /// @param[in]      cook_on_load
    ///                 Set to true if you wish the assets to cook as soon
    ///                 as they are instantiated. Otherwise, you will have to
    ///                 call ::HAPI_CookAsset() explicitly for each after you
    ///                 call this function.
    ///
    HAPI_DECL HAPI_LoadHIPFile( const char * file_name,
                                HAPI_Bool cook_on_load );


    /// @brief  Resyncs a HAPI to the underlying Houdini scene after one more
    ///         more nodes have been created by an asset (say using python)
    ///         at the /OBJ level, or a new scene has been loaded using
    ///         ::HAPI_LoadHIPFile().
    ///         Note that function will always return the same thing until
    ///         you call ::HAPI_GetNewAssetIds() to clear the results.
    ///
    /// @param[out]     new_asset_count
    ///                 A pointer to an int that will receive the asset count.
    ///
    HAPI_DECL HAPI_CheckForNewAssets( int * new_asset_count );

    /// @brief  Retrieves the asset ids from the previous call to 
    ///         ::HAPI_CheckForNewAssets(). 
    ///
    /// @param[out]     asset_ids
    ///                 A buffer of length num_assets as returned
    ///                 by the call to ::HAPI_CheckForNewAssets().
    ///                 When the function returns this buffer 
    ///                 will be filled with the asset ids.
    ///
    HAPI_DECL HAPI_GetNewAssetIds( HAPI_AssetId * asset_ids );

    /// @brief  Saves a .hip file of the current Houdini scene.
    ///
    /// @param[in]      file_path
    ///                 Absolute path to the .hip file to save to.
    ///
    HAPI_DECL HAPI_SaveHIPFile( const char * file_path );

    // NODES ----------------------------------------------------------------

    /// @brief  Fill an ::HAPI_NodeInfo struct.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[out]     node_info
    ///                 Return value - contains things like asset id.
    ///
    HAPI_DECL HAPI_GetNodeInfo( HAPI_NodeId node_id,
                                HAPI_NodeInfo * node_info );

    /// @brief  Fill a ::HAPI_GlobalNodes struct. This contains node ids for
    ///         the default special nodes that are created by HAPI in the
    ///         default Houdini scene underneath. These nodes will be used
    ///         for global asset-independent functionality like rendering
    ///         material to file. Use ::HAPI_GetNodeInfo() to get details
    ///         for each of these special nodes.
    ///
    /// @param[out]     global_nodes
    ///                 The global nodes struct.
    ///
    HAPI_DECL HAPI_GetGlobalNodes( HAPI_GlobalNodes * global_nodes );

    // PARAMETERS -----------------------------------------------------------

    /// @brief  Fill an array of ::HAPI_ParmInfo structs with parameter
    ///         information from the asset instance node.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[out]     parm_infos
    ///                 Array of ::HAPI_ParmInfo at least the size of
    ///                 length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_NodeInfo::parmCount - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 0 and at most 
    ///                 ::HAPI_NodeInfo::parmCount - start.
    ///
    HAPI_DECL HAPI_GetParameters( HAPI_NodeId node_id,
                                  HAPI_ParmInfo * parm_infos,
                                  int start, int length );

    /// @brief  Get the parm info of a parameter by parm id.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[in]      parm_id
    ///                 The parm id.
    ///
    /// @param[out]     parm_info
    ///                 The returned parm info.
    ///
    HAPI_DECL HAPI_GetParmInfo( HAPI_NodeId node_id,
                                HAPI_ParmId parm_id,
                                HAPI_ParmInfo * parm_info );

    /// @brief  All parameter APIs require a ::HAPI_ParmId but if you know the
    ///         parameter you wish to operate on by name than you can use
    ///         this function to get its ::HAPI_ParmId. If the parameter with
    ///         the given name is not found the parameter id returned
    ///         will be -1.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[in]      parm_name
    ///                 The parm name.
    ///
    /// @param[out]     parm_id
    ///                 The return value. The parameter's ::HAPI_ParmId. If
    ///                 the parameter with the given name is not found the
    ///                 parameter id returned will be -1.
    ///
    HAPI_DECL HAPI_GetParmIdFromName( HAPI_NodeId node_id,
                                      const char * parm_name,
                                      HAPI_ParmId * parm_id );

    /// @brief  Get the parm info of a parameter by name.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[in]      parm_name
    ///                 The parm name.
    ///
    /// @param[out]     parm_info
    ///                 The returned parm info.
    ///
    HAPI_DECL HAPI_GetParmInfoFromName( HAPI_NodeId node_id,
                                        const char * parm_name,
                                        HAPI_ParmInfo * parm_info );

    /// @brief  Get single parm int value by name.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[in]      parm_name
    ///                 The parm name.
    ///
    /// @param[in]      index
    ///                 Index within the parameter's values tuple.
    ///
    /// @param[out]     value
    ///                 The returned int value.
    ///
    HAPI_DECL HAPI_GetParmIntValue( HAPI_NodeId node_id,
                                    const char * parm_name,
                                    int index,
                                    int * value );

    /// @brief  Fill an array of parameter int values. This is more efficient
    ///         than calling ::HAPI_GetParmIntValue() individually for each
    ///         parameter value.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[out]     values
    ///                 Array of ints at least the size of length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_NodeInfo::parmIntValueCount - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 0 and at most 
    ///                 ::HAPI_NodeInfo::parmIntValueCount - start.
    ///
    HAPI_DECL HAPI_GetParmIntValues( HAPI_NodeId node_id,
                                     int * values,
                                     int start, int length );

    /// @brief  Get single parm float value by name.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[in]      parm_name
    ///                 The parm name.
    ///
    /// @param[in]      index
    ///                 Index within the parameter's values tuple.
    ///
    /// @param[out]     value
    ///                 The returned float value.
    ///
    HAPI_DECL HAPI_GetParmFloatValue( HAPI_NodeId node_id,
                                      const char * parm_name,
                                      int index,
                                      float * value );

    /// @brief  Fill an array of parameter float values. This is more efficient
    ///         than calling ::HAPI_GetParmFloatValue() individually for each
    ///         parameter value.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[out]     values
    ///                 Array of floats at least the size of length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_NodeInfo::parmFloatValueCount - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 1 and at most 
    ///                 ::HAPI_NodeInfo::parmFloatValueCount - start.
    ///
    HAPI_DECL HAPI_GetParmFloatValues( HAPI_NodeId node_id, float * values,
                                       int start, int length );

    /// @brief  Get single parm string value by name.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[in]      parm_name
    ///                 The name of the parameter.
    ///
    /// @param[in]      index
    ///                 Index within the parameter's values tuple.
    ///
    /// @param[in]      evaluate
    ///                 Whether or not to evaluate the string expression.
    ///                 For example, the string "$F" would evaluate to the
    ///                 current frame number. So, passing in evaluate = false
    ///                 would give you back the string "$F" and passing
    ///                 in evaluate = true would give you back "1" (assuming
    ///                 the current frame is 1).
    ///
    /// @param[out]     value
    ///                 The returned string value.
    ///
    HAPI_DECL HAPI_GetParmStringValue( HAPI_NodeId node_id,
                                       const char * parm_name,
                                       int index,
                                       HAPI_Bool evaluate,
                                       HAPI_StringHandle * value );

    /// @brief  Fill an array of parameter string handles. These handles must
    ///         be used in conjunction with ::HAPI_GetString() to get the
    ///         actual string values. This is more efficient than calling
    ///         ::HAPI_GetParmStringValue() individually for each
    ///         parameter value.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[in]      evaluate
    ///                 Whether or not to evaluate the string expression.
    ///                 For example, the string "$F" would evaluate to the
    ///                 current frame number. So, passing in evaluate = false
    ///                 would give you back the string "$F" and passing
    ///                 in evaluate = true would give you back "1" (assuming
    ///                 the current frame is 1).
    ///
    /// @param[out]     values
    ///                 Array of ints at least the size of length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_NodeInfo::parmStringValueCount - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 1 and at most 
    ///                 ::HAPI_NodeInfo::parmStringValueCount - start.
    ///
    HAPI_DECL HAPI_GetParmStringValues( HAPI_NodeId node_id,
                                        HAPI_Bool evaluate,
                                        HAPI_StringHandle * values,
                                        int start, int length );

    /// @brief  Fill an array of ::HAPI_ParmChoiceInfo structs with parameter
    ///         choice list information from the asset instance node.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[out]     parm_choices
    ///                 Array of ::HAPI_ParmChoiceInfo exactly the size of
    ///                 length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_NodeInfo::parmChoiceCount - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 1 and at most 
    ///                 ::HAPI_NodeInfo::parmChoiceCount - start.
    ///
    HAPI_DECL HAPI_GetParmChoiceLists( HAPI_NodeId node_id,
                                       HAPI_ParmChoiceInfo * parm_choices,
                                       int start, int length );

    /// @brief  Set single parm int value by name.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[in]      parm_name
    ///                 The parm name.
    ///
    /// @param[in]      index
    ///                 Index within the parameter's values tuple.
    ///
    /// @param[in]      value
    ///                 The int value.
    ///
    HAPI_DECL HAPI_SetParmIntValue( HAPI_NodeId node_id,
                                    const char * parm_name,
                                    int index,
                                    int value );

    /// @brief  Set (push) an array of parameter int values.
    /// 
    ///         Note: If threading is enabled, this function will delegate
    ///         its work to the cooking thread but it will wait (block)
    ///         until the work is done and only then return. The only reason
    ///         for going to a separate thread is to be able to use the
    ///         extra stack size of the cooking thread if the host app
    ///         stack size is insufficient for some operations.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[in]      values
    ///                 Array of ints at least the size of length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_NodeInfo::parmIntValueCount - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 1 and at most 
    ///                 ::HAPI_NodeInfo::parmIntValueCount - start.
    ///
    HAPI_DECL HAPI_SetParmIntValues( HAPI_NodeId node_id, const int * values, 
                                     int start, int length );

    /// @brief  Set single parm float value by name.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[in]      parm_name
    ///                 The parm name.
    ///
    /// @param[in]      index
    ///                 Index within the parameter's values tuple.
    ///
    /// @param[in]      value
    ///                 The float value.
    ///
    HAPI_DECL HAPI_SetParmFloatValue( HAPI_NodeId node_id,
                                      const char * parm_name,
                                      int index,
                                      float value );

    /// @brief  Set (push) an array of parameter float values.
    /// 
    ///         Note: If threading is enabled, this function will delegate
    ///         its work to the cooking thread but it will wait (block)
    ///         until the work is done and only then return. The only reason
    ///         for going to a separate thread is to be able to use the
    ///         extra stack size of the cooking thread if the host app
    ///         stack size is insufficient for some operations.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[in]      values
    ///                 Array of floats at least the size of length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_NodeInfo::parmFloatValueCount - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 1 and at most 
    ///                 ::HAPI_NodeInfo::parmFloatValueCount - start.
    ///
    HAPI_DECL HAPI_SetParmFloatValues( HAPI_NodeId node_id,
                                       const float * values,
                                       int start, int length );

    /// @brief  Set (push) a string value. We can only set a single value at
    ///         a time because we want to avoid fixed size string buffers.
    ///
    ///         Note: If threading is enabled, this function will delegate
    ///         its work to the cooking thread but it will wait (block)
    ///         until the work is done and only then return. The only reason
    ///         for going to a separate thread is to be able to use the
    ///         extra stack size of the cooking thread if the host app
    ///         stack size is insufficient for some operations.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[in]      value
    ///                 The string value.
    ///
    /// @param[in]      parm_id
    ///                 Parameter id of the parameter being updated.
    ///
    /// @param[in]      index
    ///                 Index within the parameter's values tuple.
    ///
    HAPI_DECL HAPI_SetParmStringValue( HAPI_NodeId node_id,
                                       const char * value,
                                       HAPI_ParmId parm_id, int index );


    /// @brief Insert an instance of a multiparm before instance_position.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[in]      parm_id
    ///                 A parm id given by a ::HAPI_ParmInfo struct that
    ///                 has type ::HAPI_PARMTYPE_MULTIPARMLIST.
    ///
    /// @param[in]      instance_position
    ///                 The new instance will be inserted at this position
    ///                 index. Do note the multiparms can start at position
    ///                 1 or 0. Use ::HAPI_ParmInfo::instanceStartOffset to
    ///                 distinguish.
    ///
    HAPI_DECL HAPI_InsertMultiparmInstance( HAPI_NodeId node_id,
                                            HAPI_ParmId parm_id,
                                            int instance_position );

    /// @brief Remove the instance of a multiparm given by instance_position.
    ///
    /// @param[in]      node_id
    ///                 The node id.
    ///
    /// @param[in]      parm_id
    ///                 A parm id given by a ::HAPI_ParmInfo struct that
    ///                 has type ::HAPI_PARMTYPE_MULTIPARMLIST.
    ///
    /// @param[in]      instance_position
    ///                 The instance at instance_position will removed.
    ///
    HAPI_DECL HAPI_RemoveMultiparmInstance( HAPI_NodeId node_id,
                                            HAPI_ParmId parm_id,
                                            int instance_position );
    
    // HANDLES --------------------------------------------------------------

    /// @brief  Fill an array of ::HAPI_HandleInfo structs with information
    ///         about every exposed user manipulation handle on the asset.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[out]     handle_infos
    ///                 Array of ::HAPI_HandleInfo at least the size of length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_AssetInfo::handleCount - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 1 and at most 
    ///                 ::HAPI_AssetInfo::handleCount - start.
    ///
    HAPI_DECL HAPI_GetHandleInfo( HAPI_AssetId asset_id,
                                  HAPI_HandleInfo * handle_infos,
                                  int start, int length );

    /// @brief  Fill an array of ::HAPI_HandleInfo structs with information
    ///         about every exposed user manipulation handle on the asset.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      handle_index
    ///                 The index of the handle, from 0 to handleCount - 1
    ///                 from the call to ::HAPI_GetAssetInfo().
    ///
    /// @param[out]     handle_infos
    ///                 Array of ::HAPI_HandleInfo at least the size of length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_HandleInfo::bindingsCount - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 0 and at most 
    ///                 ::HAPI_HandleInfo::bindingsCount - start.
    ///
    HAPI_DECL HAPI_GetHandleBindingInfo( HAPI_AssetId asset_id,
                                         int handle_index,
                                         HAPI_HandleBindingInfo * handle_infos,
                                         int start, int length );

    // PRESETS --------------------------------------------------------------

    /// @brief  Generate a preset blob of the current state of all the
    ///         parameter values, cache it, and return its size in bytes.
    ///
    /// @param[in]      node_id
    ///                 The exposed node id.
    ///
    /// @param[in]      preset_type
    ///                 The preset type.
    ///
    /// @param[in]      preset_name
    ///                 Optional. This is only used if the @p preset_type is
    ///                 ::HAPI_PRESETTYPE_IDX. If NULL is given, the preset
    ///                 name will be the same as the name of the node with
    ///                 the given @p node_id.
    ///
    /// @param[out]     buffer_length
    ///                 Size of the buffer.
    ///
    HAPI_DECL HAPI_GetPresetBufLength( HAPI_NodeId node_id,
                                       HAPI_PresetType preset_type,
                                       const char * preset_name,
                                       int * buffer_length );

    /// @brief  Generates a preset for the given asset.
    ///
    /// @param[in]      node_id
    ///                 The exposed node id.
    ///
    /// @param[out]     buffer
    ///                 Buffer to hold the preset data.
    ///
    /// @param[in]      buffer_length
    ///                 Size of the buffer. Should be the same as the length
    ///                 returned by ::HAPI_GetPresetBufLength().
    ///
    HAPI_DECL HAPI_GetPreset( HAPI_NodeId node_id, char * buffer,
                              int buffer_length );

    /// @brief  Sets a particular asset to a given preset.
    ///
    /// @param[in]      node_id
    ///                 The exposed node id.
    ///
    /// @param[in]      preset_type
    ///                 The preset type.
    ///
    /// @param[in]      preset_name
    ///                 Optional. This is only used if the @p preset_type is
    ///                 ::HAPI_PRESETTYPE_IDX. If NULL is give, the first
    ///                 preset in the IDX file will be chosen.
    ///
    /// @param[in]      buffer
    ///                 Buffer to hold the preset data.
    ///
    /// @param[in]      buffer_length
    ///                 Size of the buffer.
    ///
    HAPI_DECL HAPI_SetPreset( HAPI_NodeId node_id,
                              HAPI_PresetType preset_type,
                              const char * preset_name,
                              const char * buffer,
                              int buffer_length );

    // OBJECTS --------------------------------------------------------------

    /// @brief  Fill an array of ::HAPI_ObjectInfo structs with information
    ///         on each visible object in the scene that has a SOP network
    ///         (is not a sub-network).
    ///         Note that this function will reset all the objects' 
    ///         ::HAPI_ObjectInfo.haveGeosChanged flags to false after 
    ///         it returns the original flag values.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[out]     object_infos
    ///                 Array of ::HAPI_ObjectInfo at least the size of
    ///                 length.
    ///
    /// @param[in]      start
    ///                 First object index to begin fill. Must be at least 
    ///                 0 and at most length - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 0 and at most 
    ///                 ::HAPI_AssetInfo::objectCount  - start.
    ///
    HAPI_DECL HAPI_GetObjects( HAPI_AssetId asset_id, 
                               HAPI_ObjectInfo * object_infos,
                               int start, int length );

    /// @brief  Fill an array of ::HAPI_Transform structs with the transforms
    ///         of each visible object in the scene that has a SOP network
    ///         (is not a sub-network).
    ///         Note that this function will reset all the objects' 
    ///         ::HAPI_ObjectInfo::hasTransformChanged flags to false after 
    ///         it returns the original flag values.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      rst_order
    ///                 The order of application of translation, rotation and
    ///                 scale.
    ///
    /// @param[out]     transforms
    ///                 Array of ::HAPI_Transform at least the size of
    ///                 length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_AssetInfo::objectCount - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 0 and at most 
    ///                 ::HAPI_AssetInfo::objectCount - start.
    ///
    HAPI_DECL HAPI_GetObjectTransforms( HAPI_AssetId asset_id,
                                        HAPI_RSTOrder rst_order,
                                        HAPI_Transform * transforms,
                                        int start, int length );

    /// @brief  Fill an array of ::HAPI_Transform structs with the transforms
    ///         of each instance of this instancer object
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      rst_order
    ///                 The order of application of translation, rotation and
    ///                 scale.
    ///
    /// @param[out]     transforms
    ///                 Array of ::HAPI_Transform at least the size of length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_PartInfo::pointCount - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 0 and at most 
    ///                 ::HAPI_PartInfo::pointCount - @p start.
    ///
    HAPI_DECL HAPI_GetInstanceTransforms( HAPI_AssetId asset_id,
                                          HAPI_ObjectId object_id, 
                                          HAPI_GeoId geo_id,
                                          HAPI_RSTOrder rst_order, 
                                          HAPI_Transform * transforms,
                                          int start, int length );

    /// @brief  Set the transform of an individual object. This is mostly used
    ///         with marshaled geometry objects. Trying to modify the 
    ///         transform of an object belonging to an asset other than
    ///         the special External Input Asset with object id 0 will most
    ///         likely fail, unless the transforms are exposed as editable
    ///         via exposed parameters.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      transform
    ///                 A ::HAPI_TransformEuler that stores the transform.
    ///
    HAPI_DECL HAPI_SetObjectTransform( HAPI_AssetId asset_id,
                                       HAPI_ObjectId object_id,
                                       const HAPI_TransformEuler * transform );

    // GEOMETRY GETTERS -----------------------------------------------------

    /// @brief  Get the main geometry info struct (::HAPI_GeoInfo). Note that
    ///         this function will reset all the geo_infos' 
    ///         ::HAPI_GeoInfo::hasGeoChanged flags to false after it returns 
    ///         the original flag values.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[out]     geo_info
    ///                 ::HAPI_GeoInfo return value.
    ///
    HAPI_DECL HAPI_GetGeoInfo( HAPI_AssetId asset_id,
                               HAPI_ObjectId object_id,
                               HAPI_GeoId geo_id,
                               HAPI_GeoInfo * geo_info );

    /// @brief  Get a particular part info struct.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[out]     part_info
    ///                 ::HAPI_PartInfo return value.
    ///
    HAPI_DECL HAPI_GetPartInfo( HAPI_AssetId asset_id, HAPI_ObjectId object_id,
                                HAPI_GeoId geo_id, HAPI_PartId part_id,
                                HAPI_PartInfo * part_info );

    /// @brief  Get the array of faces where the nth integer in the array is 
    ///         the number of vertices the nth face has.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[out]     face_counts
    ///                 An integer array at least the size of length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_PartInfo::faceCount - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 0 and at most 
    ///                 ::HAPI_PartInfo::faceCount - @p start.
    ///
    HAPI_DECL HAPI_GetFaceCounts( HAPI_AssetId asset_id,
                                  HAPI_ObjectId object_id, 
                                  HAPI_GeoId geo_id,
                                  HAPI_PartId part_id,
                                  int * face_counts,
                                  int start, int length );

    /// @brief  Get array containing the vertex-point associations where the 
    ///         ith element in the array is the point index the ith vertex 
    ///         associates with.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[out]     vertex_list
    ///                 An integer array at least the size of length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_PartInfo::vertexCount - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 0 and at most 
    ///                 ::HAPI_PartInfo::vertexCount - @p start.
    ///
    HAPI_DECL HAPI_GetVertexList( HAPI_AssetId asset_id,
                                  HAPI_ObjectId object_id,
                                  HAPI_GeoId geo_id,
                                  HAPI_PartId part_id,
                                  int * vertex_list,
                                  int start, int length );

    /// @brief  Get the main geometry info struct (::HAPI_GeoInfo).
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[in]      name
    ///                 Attribute name.
    ///
    /// @param[in]      owner
    ///                 Attribute owner.
    ///
    /// @param[out]     attr_info
    ///                 ::HAPI_AttributeInfo to be filled. Check 
    ///                 ::HAPI_AttributeInfo::exists to see if this attribute
    ///                 exists.
    ///
    HAPI_DECL HAPI_GetAttributeInfo( HAPI_AssetId asset_id,
                                     HAPI_ObjectId object_id,
                                     HAPI_GeoId geo_id,
                                     HAPI_PartId part_id,
                                     const char * name,
                                     HAPI_AttributeOwner owner,
                                     HAPI_AttributeInfo * attr_info );

    /// @brief  Get list of attribute names by attribute owner. Note that the
    ///         name string handles are only valid until the next time this
    ///         function is called.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[in]      owner
    ///                 The ::HAPI_AttributeOwner enum value specifying the 
    ///                 owner of the attribute.
    ///
    /// @param[out]     attribute_names
    ///                 Array of ints (string handles) to house the 
    ///                 attribute names. Should be exactly the size of the 
    ///                 appropriate attribute owner type count 
    ///                 in ::HAPI_PartInfo.
    ///
    /// @param[in]      count
    ///                 Sanity check count. Must be equal to the appropriate 
    ///                 attribute owner type count in ::HAPI_PartInfo.
    ///
    HAPI_DECL HAPI_GetAttributeNames( HAPI_AssetId asset_id,
                                      HAPI_ObjectId object_id,
                                      HAPI_GeoId geo_id,
                                      HAPI_PartId part_id,
                                      HAPI_AttributeOwner owner,
                                      HAPI_StringHandle * attribute_names,
                                      int count );

    /// @brief  Get attribute integer data.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[in]      name
    ///                 Attribute name.
    ///
    /// @param[in]      attr_info
    ///                 ::HAPI_AttributeInfo used as input for what tuple size.
    ///                 you want. Also contains some sanity checks like 
    ///                 data type. Generally should be the same struct 
    ///                 returned by ::HAPI_GetAttributeInfo().
    ///
    /// @param[out]     data
    ///                 An integer array at least the size of length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_AttributeInfo::count - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 0 and at most 
    ///                 ::HAPI_AttributeInfo::count - @p start.
    ///
    HAPI_DECL HAPI_GetAttributeIntData( HAPI_AssetId asset_id,
                                        HAPI_ObjectId object_id,
                                        HAPI_GeoId geo_id,
                                        HAPI_PartId part_id,
                                        const char * name,
                                        HAPI_AttributeInfo * attr_info,
                                        int * data,
                                        int start, int length );

    /// @brief  Get attribute float data.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[in]      name
    ///                 Attribute name.
    ///
    /// @param[in]      attr_info
    ///                 ::HAPI_AttributeInfo used as input for what tuple size.
    ///                 you want. Also contains some sanity checks like 
    ///                 data type. Generally should be the same struct 
    ///                 returned by ::HAPI_GetAttributeInfo().
    ///
    /// @param[out]     data
    ///                 An float array at least the size of length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_AttributeInfo::count - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 0 and at most 
    ///                 ::HAPI_AttributeInfo::count - @p start.
    ///
    HAPI_DECL HAPI_GetAttributeFloatData( HAPI_AssetId asset_id,
                                          HAPI_ObjectId object_id, 
                                          HAPI_GeoId geo_id,
                                          HAPI_PartId part_id,
                                          const char * name,
                                          HAPI_AttributeInfo * attr_info,
                                          float * data,
                                          int start, int length );

    /// @brief  Get attribute string data. Note that the string handles 
    ///         returned are only valid until the next time this function
    ///         is called.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[in]      name
    ///                 Attribute name.
    ///
    /// @param[in]      attr_info
    ///                 ::HAPI_AttributeInfo used as input for what tuple size.
    ///                 you want. Also contains some sanity checks like 
    ///                 data type. Generally should be the same struct 
    ///                 returned by ::HAPI_GetAttributeInfo().
    ///
    /// @param[out]     data
    ///                 An int (string handles) array at least the 
    ///                 size of length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_AttributeInfo::count - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 0 and at most 
    ///                 ::HAPI_AttributeInfo::count - @p start.
    ///
    HAPI_DECL HAPI_GetAttributeStringData( HAPI_AssetId asset_id,
                                           HAPI_ObjectId object_id, 
                                           HAPI_GeoId geo_id,
                                           HAPI_PartId part_id,
                                           const char * name,
                                           HAPI_AttributeInfo * attr_info,
                                           int * data,
                                           int start, int length );

    /// @brief  Get group names for an entire geo. Please note that this
    //          function is NOT per-part, but it is per-geo. The companion
    ///         function ::HAPI_GetGroupMembership() IS per-part. Also keep
    ///         in mind that the name string handles are only
    ///         valid until the next time this function is called.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      group_type
    ///                 The group type.
    ///
    /// @param[out]     group_names
    ///                 The array of names to be filled. Should be the size
    ///                 given by ::HAPI_GeoInfo_GetGroupCountByType() with
    ///                 @p group_type and the ::HAPI_GeoInfo of @p geo_id.
    ///                 @note These string handles are only valid until the
    ///                 next call to ::HAPI_GetGroupNames().
    ///
    /// @param[in]      group_count
    ///                 Sanity check. Should be less than or equal to the size
    ///                 of @p group_names.
    ///
    HAPI_DECL HAPI_GetGroupNames( HAPI_AssetId asset_id,
                                  HAPI_ObjectId object_id,
                                  HAPI_GeoId geo_id,
                                  HAPI_GroupType group_type,
                                  HAPI_StringHandle * group_names,
                                  int group_count );

    /// @brief  Get group membership.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[in]      group_type
    ///                 The group type.
    ///
    /// @param[in]      group_name
    ///                 The group name.
    ///
    /// @param[out]     membership
    ///                 Array of ints that represent the membership of this
    ///                 group. Should be the size given by
    ///                 ::HAPI_PartInfo_GetElementCountByGroupType() with
    ///                 @p group_type and the ::HAPI_PartInfo of @p part_id.
    ///
    /// @param[in]      start
    ///                 Start offset into the membership array. Must be
    ///                 less than ::HAPI_PartInfo_GetElementCountByGroupType().
    ///
    /// @param[in]      length
    ///                 Should be less than or equal to the size
    ///                 of @p membership.
    ///
    HAPI_DECL HAPI_GetGroupMembership( HAPI_AssetId asset_id,
                                       HAPI_ObjectId object_id,
                                       HAPI_GeoId geo_id,
                                       HAPI_PartId part_id,
                                       HAPI_GroupType group_type,
                                       const char * group_name,
                                       int * membership,
                                       int start, int length );

    // GEOMETRY SETTERS -----------------------------------------------------

    /// @brief  Set the main geometry info struct (::HAPI_GeoInfo).
    ///         TODO: This function does nothing at the moment. (Placeholder)
    ///    
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      geo_info
    ///                 ::HAPI_GeoInfo value.
    ///
    HAPI_DECL HAPI_SetGeoInfo( HAPI_AssetId asset_id,
                               HAPI_ObjectId object_id,
                               HAPI_GeoId geo_id,
                               HAPI_GeoInfo * geo_info );

    /// @brief  Set the main part info struct (::HAPI_PartInfo).
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_info
    ///                 ::HAPI_PartInfo value that describes the input
    ///                 geometry.
    ///
    HAPI_DECL HAPI_SetPartInfo( HAPI_AssetId asset_id,
                                HAPI_ObjectId object_id,
                                HAPI_GeoId geo_id,
                                const HAPI_PartInfo * part_info );

    /// @brief  Set the array of faces where the nth integer in the array is 
    ///         the number of vertices the nth face has.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      face_counts
    ///                 An integer array at least the size of @p length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_PartInfo::faceCount - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 0 and at most 
    ///                 ::HAPI_PartInfo::faceCount - @p start.
    ///
    HAPI_DECL HAPI_SetFaceCounts( HAPI_AssetId asset_id,
                                  HAPI_ObjectId object_id,
                                  HAPI_GeoId geo_id,
                                  const int * face_counts,
                                  int start, int length );

    /// @brief  Set array containing the vertex-point associations where the 
    ///         ith element in the array is the point index the ith vertex 
    ///         associates with.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      vertex_list
    ///                 An integer array at least the size of length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_PartInfo::vertexCount - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 0 and at most 
    ///                 ::HAPI_PartInfo::vertexCount - @p start.
    ///
    HAPI_DECL HAPI_SetVertexList( HAPI_AssetId asset_id,
                                  HAPI_ObjectId object_id,
                                  HAPI_GeoId geo_id,
                                  const int * vertex_list,
                                  int start, int length );

    /// @brief  Add an attribute.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      name
    ///                 Attribute name.
    ///
    /// @param[in]      attr_info
    ///                 ::HAPI_AttributeInfo stores attribute properties.
    ///
    HAPI_DECL HAPI_AddAttribute( HAPI_AssetId asset_id,
                                 HAPI_ObjectId object_id,
                                 HAPI_GeoId geo_id,
                                 const char * name,
                                 const HAPI_AttributeInfo * attr_info );

    /// @brief  Set attribute integer data.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      name
    ///                 Attribute name.
    ///
    /// @param[in]      attr_info
    ///                 ::HAPI_AttributeInfo used as input for what tuple size.
    ///                 you want. Also contains some sanity checks like 
    ///                 data type. Generally should be the same struct 
    ///                 returned by ::HAPI_GetAttributeInfo().
    ///
    /// @param[in]      data
    ///                 An integer array at least the size of length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_AttributeInfo::count - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 0 and at most 
    ///                 ::HAPI_AttributeInfo::count - @p start.
    ///
    HAPI_DECL HAPI_SetAttributeIntData( HAPI_AssetId asset_id,
                                        HAPI_ObjectId object_id, 
                                        HAPI_GeoId geo_id,
                                        const char * name,
                                        const HAPI_AttributeInfo * attr_info,
                                        const int * data,
                                        int start, int length );

    /// @brief  Set attribute float data.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      name
    ///                 Attribute name.
    ///
    /// @param[in]      attr_info
    ///                 ::HAPI_AttributeInfo used as input for what tuple size.
    ///                 you want. Also contains some sanity checks like 
    ///                 data type. Generally should be the same struct 
    ///                 returned by ::HAPI_GetAttributeInfo().
    ///
    /// @param[in]      data
    ///                 An float array at least the size of length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_AttributeInfo::count - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 0 and at most 
    ///                 ::HAPI_AttributeInfo::count - @p start.
    ///
    HAPI_DECL HAPI_SetAttributeFloatData( HAPI_AssetId asset_id,
                                          HAPI_ObjectId object_id, 
                                          HAPI_GeoId geo_id,
                                          const char * name,
                                          const HAPI_AttributeInfo * attr_info,
                                          const float * data,
                                          int start, int length );

    /// @brief  Set attribute string data.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      name
    ///                 Attribute name.
    ///
    /// @param[in]      attr_info
    ///                 ::HAPI_AttributeInfo used as input for what tuple size.
    ///                 you want. Also contains some sanity checks like 
    ///                 data type. Generally should be the same struct 
    ///                 returned by ::HAPI_GetAttributeInfo().
    ///
    /// @param[in]      data
    ///                 A strings array at least the size of length.
    ///
    /// @param[in]      start
    ///                 First index of range. Must be at least 0 and at 
    ///                 most ::HAPI_AttributeInfo::count - 1.
    ///
    /// @param[in]      length
    ///                 Must be at least 0 and at most 
    ///                 ::HAPI_AttributeInfo::count - @p start.
    ///
    HAPI_DECL HAPI_SetAttributeStringData( HAPI_AssetId asset_id,
                                           HAPI_ObjectId object_id, 
                                           HAPI_GeoId geo_id,
                                           const char * name,
                                           const HAPI_AttributeInfo *attr_info,
                                           const char ** data,
                                           int start, int length );

    /// @brief  Get group names. Note that the name string handles are only
    ///         valid until the next time this function is called.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      group_type
    ///                 The group type.
    ///
    /// @param[out]     group_name
    ///                 Name of new group to be added.
    ///
    HAPI_DECL HAPI_AddGroup( HAPI_AssetId asset_id,
                             HAPI_ObjectId object_id,
                             HAPI_GeoId geo_id,
                             HAPI_GroupType group_type,
                             const char * group_name );

    /// @brief  Set group membership.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      group_type
    ///                 The group type.
    ///
    /// @param[in]      group_name
    ///                 The group name.
    ///
    /// @param[out]     membership
    ///                 Array of ints that represent the membership of this
    ///                 group. Should be the size given by
    ///                 ::HAPI_PartInfo_GetElementCountByGroupType() with
    ///                 @p group_type and the ::HAPI_PartInfo of @p part_id.
    ///
    /// @param[in]      start
    ///                 Start offset into the membership array. Must be
    ///                 less than ::HAPI_PartInfo_GetElementCountByGroupType().
    ///
    /// @param[in]      length
    ///                 Should be less than or equal to the size
    ///                 of @p membership.
    ///
    HAPI_DECL HAPI_SetGroupMembership( HAPI_AssetId asset_id,
                                       HAPI_ObjectId object_id,
                                       HAPI_GeoId geo_id,
                                       HAPI_GroupType group_type,
                                       const char * group_name,
                                       int * membership,
                                       int start, int length );

    /// @brief  Commit the current input geometry to the cook engine. Nodes
    ///         that use this geometry node will re-cook using the input
    ///         geometry given through the geometry setter API calls.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    HAPI_DECL HAPI_CommitGeo( HAPI_AssetId asset_id,
                              HAPI_ObjectId object_id,
                              HAPI_GeoId geo_id );

    /// @brief  Remove all changes that have been committed to this
    ///         geometry.  Only applies to geometry nodes that are
    ///         exposed edit nodes.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    HAPI_DECL HAPI_RevertGeo( HAPI_AssetId asset_id,
                              HAPI_ObjectId object_id,
                              HAPI_GeoId geo_id );

    // INTER-ASSET ----------------------------------------------------------

    /// @brief  Connect the transform of two assets together.
    ///
    /// @param[in]      asset_id_from
    ///                 The asset id of the source asset.
    ///
    /// @param[in]      asset_id_to
    ///                 The asset id of the destination asset.
    ///
    /// @param[in]      input_idx
    ///                 The index on the destination asset where the 
    ///                 connection should be made.
    ///
    HAPI_DECL HAPI_ConnectAssetTransform( HAPI_AssetId asset_id_from, 
                                          HAPI_AssetId asset_id_to, 
                                          int input_idx );

    /// @brief  Break an existing transform connection
    ///
    /// @param[in]      asset_id
    ///                 The asset id of the asset.
    ///
    /// @param[in]      input_idx
    ///                 The index on the asset where the connection
    ///                 should be broken.
    ///
    HAPI_DECL HAPI_DisconnectAssetTransform( HAPI_AssetId asset_id,
                                             int input_idx );

    /// @brief  Connect the geometry of two assets together.  For 
    ///         example we can connect a particular piece of geometry from
    ///         an object level asset to a sop level asset or even another
    ///         object level asset.  This method gives you the fine grained 
    ///         control over the exact piece of geometry to connect by 
    ///         allowing you to specify the exact object and group of the 
    ///         geometry you are trying to connect.
    ///
    /// @param[in]      asset_id_from
    ///                 The asset id of the source asset.
    ///
    /// @param[in]      object_id_from
    ///                 The object within the asset that contains the 
    ///                 geometry to send.
    ///
    /// @param[in]      asset_id_to
    ///                 The asset id of the destination asset.
    ///
    /// @param[in]      input_idx
    ///                 The index on the destination asset where the 
    ///                 connection should be made.
    ///
    HAPI_DECL HAPI_ConnectAssetGeometry( HAPI_AssetId asset_id_from, 
                                         HAPI_ObjectId object_id_from,
                                         HAPI_AssetId asset_id_to, 
                                         int input_idx );

    /// @brief  Break an existing geometry connection
    ///
    /// @param[in]      asset_id
    ///                 The asset id of the asset.
    ///
    /// @param[in]      input_idx
    ///                 The index on the asset where the connection
    ///                 should be broken.
    ///
    HAPI_DECL HAPI_DisconnectAssetGeometry( HAPI_AssetId asset_id,
                                            int input_idx );

    // MATERIALS ------------------------------------------------------------

    /// @brief  Get material ids by face/primitive. The material ids returned
    ///         will be valid as long as the asset is alive. You should query
    ///         this list after every cook to see if the material assignments
    ///         have changed. You should also query each material individually
    ///         using ::HAPI_GetMaterialInfo() to see if it is dirty and needs
    ///         to be re-imported.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[out]     are_all_the_same
    ///                 (optional) If true, all faces on this part have the
    ///                 same material assignment. You can pass NULL here.
    /// 
    /// @param[out]     material_ids
    ///                 An array of ::HAPI_MaterialId at least the size of
    ///                 @p length and at most the size of
    ///                 ::HAPI_PartInfo::faceCount.
    ///
    /// @param[in]      start
    ///                 The starting index into the list of faces from which
    ///                 you wish to get the material ids from. Note that
    ///                 this should be less than ::HAPI_PartInfo::faceCount.
    ///
    /// @param[in]      length
    ///                 The number of material ids you wish to get. Note that
    ///                 this should be at most:
    ///                 ::HAPI_PartInfo::faceCount - @p start.
    ///
    HAPI_DECL HAPI_GetMaterialIdsOnFaces( HAPI_AssetId asset_id,
                                          HAPI_ObjectId object_id,
                                          HAPI_GeoId geo_id,
                                          HAPI_PartId part_id,
                                          HAPI_Bool * are_all_the_same,
                                          HAPI_MaterialId * material_ids,
                                          int start, int length );

    /// @brief  Get the material info.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      material_id
    ///                 The material id as given from
    ///                 ::HAPI_GetMaterialIdsOnFaces().
    ///
    /// @param[out]     material_info
    ///                 The returned material info.
    ///
    HAPI_DECL HAPI_GetMaterialInfo( HAPI_AssetId asset_id,
                                    HAPI_MaterialId material_id,
                                    HAPI_MaterialInfo * material_info );

    /// @brief  Get the material on a part.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[out]     material_info
    ///                 The returned ::HAPI_MaterialInfo. If there is no
    ///                 material on this part the call will still succeed
    ///                 but the ::HAPI_MaterialInfo::exists will be set to
    ///                 false.
    ///
    HAPI_DECL_DEPRECATED_REPLACE( 1.9.16, 14.0.289, HAPI_GetMaterialIdsOnFaces)
    HAPI_GetMaterialOnPart( HAPI_AssetId asset_id,
                            HAPI_ObjectId object_id,
                            HAPI_GeoId geo_id,
                            HAPI_PartId part_id,
                            HAPI_MaterialInfo * material_info );

    /// @brief  Get the material on a group. Use the
    ///         ::HAPI_GetGroupMembership() call to determine where the
    ///         material should be applied.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      group_name
    ///                 The group name.
    ///
    /// @param[out]     material_info
    ///                 The returned ::HAPI_MaterialInfo. If there is no
    ///                 material on this group the call will still succeed
    ///                 but the ::HAPI_MaterialInfo::exists will be set to
    ///                 false.
    ///
    HAPI_DECL_DEPRECATED_REPLACE( 1.9.16, 14.0.289, HAPI_GetMaterialIdsOnFaces)
    HAPI_GetMaterialOnGroup( HAPI_AssetId asset_id,
                             HAPI_ObjectId object_id,
                             HAPI_GeoId geo_id,
                             const char * group_name,
                             HAPI_MaterialInfo * material_info );

    /// @brief  Render the entire material to an image for later extraction.
    ///         This process will use the shader specified to render the
    ///         object assigned with the material you specified (by id) in
    ///         UV space, flattening the material into an image that can be
    ///         later mapped back onto the object.
    ///
    ///         Note that you must call either this method or 
    ///         ::HAPI_RenderTextureToImage() for any of the other material
    ///         APIs to work.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      material_id
    ///                 The material id as given in ::HAPI_MaterialInfo.
    ///
    /// @param[in]      shader_type
    ///                 The shader that will be used to bake this material.
    ///
    HAPI_DECL HAPI_RenderMaterialToImage( HAPI_AssetId asset_id,
                                          HAPI_MaterialId material_id,
                                          HAPI_ShaderType shader_type );

    /// @brief  Render only a single texture to an image for later extraction.
    ///         An example use of this method might be to render the diffuse,
    ///         normal, and bump texture maps of a material to individual
    ///         texture files for use within the client application.
    ///
    ///         Note that you must call either this method or 
    ///         ::HAPI_RenderMaterialToImage() for any of the other material
    ///         APIs to work.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      material_id
    ///                 The material id as given in ::HAPI_MaterialInfo.
    ///
    /// @param[in]      parm_id
    ///                 This is the index in the parameter list of the 
    ///                 material_id's node of the parameter containing the
    ///                 texture map file path.
    ///
    HAPI_DECL HAPI_RenderTextureToImage(  HAPI_AssetId asset_id,
                                          HAPI_MaterialId material_id, 
                                          HAPI_ParmId parm_id );

    /// @brief  Get the number of supported texture file formats.
    ///
    /// @param[out]     file_format_count
    ///                 The number of supported texture file formats.
    ///
    HAPI_DECL HAPI_GetSupportedImageFileFormatCount( int * file_format_count );

    /// @brief  Get a list of support image file formats - their names,
    ///         descriptions and a list of recognized extensions.
    ///
    ///         Note that you MUST call
    ///         ::HAPI_GetSupportedImageFileFormatCount()
    ///         before calling this function for the first time.
    ///
    /// @param[out]     formats
    ///                 The list of ::HAPI_ImageFileFormat structs to
    ///                 be filled.
    ///
    /// @param[in]      file_format_count
    ///                 The number of supported texture file formats. This
    ///                 should be at least as large as the count returned
    ///                 by ::HAPI_GetSupportedImageFileFormatCount().
    ///
    HAPI_DECL HAPI_GetSupportedImageFileFormats( HAPI_ImageFileFormat *formats,
                                                 int file_format_count );

    /// @brief  Get information about the image that was just rendered, like
    ///         resolution and default file format. This information will be
    ///         used when extracting planes to an image.
    ///
    ///         Note that you must call either ::HAPI_RenderMaterialToImage() 
    ///         or ::HAPI_RenderTextureToImage() first for this method call 
    ///         to make sense.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      material_id
    ///                 The material id as given in ::HAPI_MaterialInfo.
    ///
    /// @param[out]     image_info
    ///                 The struct containing the image information.
    ///
    HAPI_DECL HAPI_GetImageInfo( HAPI_AssetId asset_id,
                                 HAPI_MaterialId material_id,
                                 HAPI_ImageInfo * image_info );

    /// @brief  Set image information like resolution and file format.
    ///         This information will be used when extracting planes to
    ///         an image.
    ///
    ///         Note that you must call either ::HAPI_RenderMaterialToImage()
    ///         or ::HAPI_RenderTextureToImage() first for this method call 
    ///         to make sense.
    ///
    ///         You should also first call ::HAPI_GetImageInfo() to get the 
    ///         current Image Info and change only the properties
    ///         you don't like.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      material_id
    ///                 The material id as given in ::HAPI_MaterialInfo.
    ///
    /// @param[in]      image_info
    ///                 The struct containing the new image information.
    ///
    HAPI_DECL HAPI_SetImageInfo( HAPI_AssetId asset_id,
                                 HAPI_MaterialId material_id,
                                 const HAPI_ImageInfo * image_info );

    /// @brief  Get the number of image planes for the just rendered image.
    ///
    ///         Note that you must call either ::HAPI_RenderMaterialToImage()
    ///         or ::HAPI_RenderTextureToImage() first for this method call 
    ///         to make sense.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      material_id
    ///                 The material id as given in ::HAPI_MaterialInfo.
    ///
    /// @param[out]     image_plane_count
    ///                 The number of image planes.
    ///
    HAPI_DECL HAPI_GetImagePlaneCount( HAPI_AssetId asset_id,
                                       HAPI_MaterialId material_id, 
                                       int * image_plane_count );

    /// @brief  Get the names of the image planes of the just rendered image.
    ///
    ///         Note that you must call either ::HAPI_RenderMaterialToImage()
    ///         or ::HAPI_RenderTextureToImage() first for this method call 
    ///         to make sense. 
    ///
    ///         You should also call ::HAPI_GetImagePlaneCount() first to get 
    ///         the total number of image planes so you know how large the 
    ///         image_planes string handle array should be.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      material_id
    ///                 The material id as given in ::HAPI_MaterialInfo.
    ///
    /// @param[out]     image_planes
    ///                 The image plane names.
    ///
    /// @param[in]      image_plane_count
    ///                 The number of image planes to get names for. This 
    ///                 must be less than or equal to the count returned
    ///                 by ::HAPI_GetImagePlaneCount().
    ///
    HAPI_DECL HAPI_GetImagePlanes( HAPI_AssetId asset_id,
                                   HAPI_MaterialId material_id,
                                   HAPI_StringHandle * image_planes,
                                   int image_plane_count );

    /// @brief  Extract a rendered image to a file. 
    ///
    ///         Note that you must call either ::HAPI_RenderMaterialToImage()
    ///         or ::HAPI_RenderTextureToImage() first for this method call 
    ///         to make sense.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      material_id
    ///                 The material id as given in ::HAPI_MaterialInfo.
    ///
    /// @param[in]      image_file_format_name
    ///                 The image file format name you wish the image to be
    ///                 extracted as. You can leave this parameter NULL to
    ///                 get the image in the original format if it comes from
    ///                 another texture file or in the default HAPI format,
    ///                 which is ::HAPI_DEFAULT_IMAGE_FORMAT_NAME, if the image 
    ///                 is generated.
    ///
    ///                 You can get some of the very common standard image
    ///                 file format names from HAPI_Common.h under the 
    ///                 "Defines" section.
    ///
    ///                 You can also get a list of all supported file formats
    ///                 (and the exact names this parameter expects)
    ///                 by using ::HAPI_GetSupportedImageFileFormats(). This
    ///                 list will include custom file formats you created via
    ///                 custom DSOs (see HDK docs about IMG_Format). You will
    ///                 get back a list of ::HAPI_ImageFileFormat. This
    ///                 parameter expects the ::HAPI_ImageFileFormat::nameSH
    ///                 of a given image file format.
    ///
    /// @param[in]      image_planes
    ///                 The image planes you wish to extract into the file.
    ///                 Multiple image planes should be separated by spaces.
    ///
    /// @param[in]      destination_folder_path
    ///                 The folder where the image file should be created.
    ///
    /// @param[in]      destination_file_name
    ///                 Optional parameter to overwrite the name of the
    ///                 extracted texture file. This should NOT include
    ///                 the extension as the file type will be decided
    ///                 by the ::HAPI_ImageInfo you can set using
    ///                 ::HAPI_SetImageInfo(). You still have to use 
    ///                 destination_file_path to get the final file path. 
    ///
    ///                 Pass in NULL to have the file name be automatically
    ///                 generated from the name of the material SHOP node,
    ///                 the name of the texture map parameter if the
    ///                 image was rendered from a texture, and the image
    ///                 plane names specified.
    ///
    /// @param[out]     destination_file_path
    ///                 The full path string handle, including the 
    ///                 destination_folder_path and the texture file name, 
    ///                 to the extracted file. Note that this string handle
    ///                 will only be valid until the next call to 
    ///                 this function.
    ///
    HAPI_DECL HAPI_ExtractImageToFile( HAPI_AssetId asset_id,
                                       HAPI_MaterialId material_id,
                                       const char * image_file_format_name,
                                       const char * image_planes, 
                                       const char * destination_folder_path, 
                                       const char * destination_file_name, 
                                       int * destination_file_path );

    /// @brief  Extract a rendered image to memory. 
    ///
    ///         Note that you must call either ::HAPI_RenderMaterialToImage()
    ///         or ::HAPI_RenderTextureToImage() first for this method call 
    ///         to make sense.
    ///
    ///         Also note that this function will do all the work of 
    ///         extracting and composoting the image into a memory buffer
    ///         but will not return to you that buffer, only its size. Use
    ///         the returned size to allocated a sufficiently large buffer
    ///         and call ::HAPI_GetImageMemoryBuffer() to fill your buffer
    ///         with the just extracted image.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      material_id
    ///                 The material id as given in ::HAPI_MaterialInfo.
    ///
    /// @param[in]      image_file_format_name
    ///                 The image file format name you wish the image to be
    ///                 extracted as. You can leave this parameter NULL to
    ///                 get the image in the original format if it comes from
    ///                 another texture file or in the default HAPI format,
    ///                 which is ::HAPI_DEFAULT_IMAGE_FORMAT_NAME, if the image 
    ///                 is generated.
    ///
    ///                 You can get some of the very common standard image
    ///                 file format names from HAPI_Common.h under the 
    ///                 "Defines" section.
    ///
    ///                 You can also get a list of all supported file formats
    ///                 (and the exact names this parameter expects)
    ///                 by using ::HAPI_GetSupportedImageFileFormats(). This
    ///                 list will include custom file formats you created via
    ///                 custom DSOs (see HDK docs about IMG_Format). You will
    ///                 get back a list of ::HAPI_ImageFileFormat. This
    ///                 parameter expects the ::HAPI_ImageFileFormat::nameSH
    ///                 of a given image file format.
    ///
    /// @param[in]      image_planes
    ///                 The image planes you wish to extract into the file.
    ///                 Multiple image planes should be separated by spaces.
    ///
    /// @param[out]     buffer_size
    ///                 The extraction will be done to an internal buffer
    ///                 who's size you get via this parameter. Use the
    ///                 returned buffer_size when calling 
    ///                 ::HAPI_GetImageMemoryBuffer() to get the image
    ///                 buffer you just extracted.
    ///
    HAPI_DECL HAPI_ExtractImageToMemory( HAPI_AssetId asset_id,
                                         HAPI_MaterialId material_id,
                                         const char * image_file_format_name,
                                         const char * image_planes,
                                         int * buffer_size );

    /// @brief  Fill your allocated buffer with the just extracted 
    ///         image buffer. 
    ///
    ///         Note that you must call either ::HAPI_RenderMaterialToImage()
    ///         or ::HAPI_RenderTextureToImage() first for this method call 
    ///         to make sense.
    ///
    ///         Also note that you must call ::HAPI_ExtractImageToMemory()
    ///         first in order to perform the extraction and get the
    ///         extracted image buffer size that you need to know how much
    ///         memory to allocated to fit your extracted image.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      material_id
    ///                 The material id as given in ::HAPI_MaterialInfo.
    ///
    /// @param[out]     buffer
    ///                 The buffer passed in here will be filled with the
    ///                 image buffer created during the call to
    ///                 ::HAPI_ExtractImageToMemory().
    ///
    /// @param[in]      buffer_size
    ///                 Sanity check. This size should be the same as the
    ///                 size allocated for the buffer passed in and should 
    ///                 be at least as large as the buffer_size returned by
    ///                 the call to ::HAPI_ExtractImageToMemory().
    ///
    HAPI_DECL HAPI_GetImageMemoryBuffer( HAPI_AssetId asset_id,
                                         HAPI_MaterialId material_id,
                                         char * buffer, int buffer_size );

    // SIMULATION/ANIMATION -------------------------------------------------

    /// @brief  Set an animation curve on a parameter of an exposed node.
    ///
    /// @param[in]      node_id
    ///                 The exposed node id.
    ///
    /// @param[in]      parm_id
    ///                 The id of an exposed parameter within the node.
    /// @param[in]      parm_index
    ///                 The index of the parameter, if it is for example
    ///                 a 3 tuple
    ///    
    ///
    /// @param[in]      curve_keyframes
    ///                 An array of ::HAPI_Keyframe structs that describes
    ///                 the keys on this curve.
    ///
    /// @param[in]      keyframe_count
    ///                 The number of keys on the curve.
    ///
    HAPI_DECL HAPI_SetAnimCurve( HAPI_NodeId node_id, HAPI_ParmId parm_id,
                                 int parm_index,
                                 const HAPI_Keyframe * curve_keyframes,
                                 int keyframe_count );

    /// @brief  A specialized convenience function to set the T,R,S values
    ///         on an exposed node.
    ///
    /// @param[in]      node_id
    ///                 The exposed node id.
    ///
    /// @param[in]      trans_comp
    ///                 A value of ::HAPI_TransformComponent that
    ///                 identifies the particular comopnent of the 
    ///                 transform to attach the curve to, for example
    ///                 ::HAPI_TRANSFORM_TX.
    ///
    /// @param[in]      curve_keyframes
    ///                 An array of ::HAPI_Keyframe structs that describes
    ///                 the keys on this curve.
    ///
    /// @param[in]      keyframe_count
    ///                 The number of keys on the curve.
    ///
    HAPI_DECL HAPI_SetTransformAnimCurve( HAPI_NodeId node_id, 
                                          HAPI_TransformComponent trans_comp,
                                          const HAPI_Keyframe *curve_keyframes,
                                          int keyframe_count );

    /// @brief  Resets the simulation cache of the asset.  This is very useful
    ///         for assets that use dynamics, to be called after some
    ///         setup has changed for the asset - for example, asset inputs
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    HAPI_DECL HAPI_ResetSimulation( HAPI_AssetId asset_id );

    // VOLUMES --------------------------------------------------------------

    /// @brief  Retrieve any meta-data about the volume primitive, including 
    ///         its transform, location, scale, taper, resolution.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[out]     volume_info
    ///                 The meta-data associated with the volume on the
    ///                 part specified by the previous parameters.
    ///
    HAPI_DECL HAPI_GetVolumeInfo( HAPI_AssetId asset_id,
                                  HAPI_ObjectId object_id,
                                  HAPI_GeoId geo_id,
                                  HAPI_PartId part_id,
                                  HAPI_VolumeInfo * volume_info );

    /// @brief  Iterate through a volume based on 8x8x8 sections of the volume
    ///         Start iterating through the value of the volume at part_id.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[out]     tile
    ///                 The tile info referring to the first tile in the
    ///                 volume at part_id.
    ///
    HAPI_DECL HAPI_GetFirstVolumeTile( HAPI_AssetId asset_id,
                                       HAPI_ObjectId object_id,
                                       HAPI_GeoId geo_id,
                                       HAPI_PartId part_id,
                                       HAPI_VolumeTileInfo * tile );

    /// @brief  Iterate through a volume based on 8x8x8 sections of the volume
    ///         Continue iterating through the value of the volume at part_id.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[out]     next
    ///                 The tile info referring to the next tile in the
    ///                 set of tiles associated with the volume at this part.
    ///
    HAPI_DECL HAPI_GetNextVolumeTile( HAPI_AssetId asset_id,
                                      HAPI_ObjectId object_id,
                                      HAPI_GeoId geo_id,
                                      HAPI_PartId part_id,
                                      HAPI_VolumeTileInfo * next );

    /// @brief  Retrieve floating point values of the voxels pointed to
    ///         by a tile.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[in]      tile
    ///                 The tile to retrieve.
    ///
    /// @param[out]     values
    ///                 The values of the tile.
    ///
    HAPI_DECL HAPI_GetVolumeTileFloatData( HAPI_AssetId asset_id,
                                           HAPI_ObjectId object_id, 
                                           HAPI_GeoId geo_id,
                                           HAPI_PartId part_id, 
                                           HAPI_VolumeTileInfo * tile,
                                           float * values );

    /// @brief  Retrieve integer values of the voxels pointed to by a tile.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[in]      tile
    ///                 The tile to retrieve.
    ///
    /// @param[out]     values
    ///                 The values of the tile.
    ///
    HAPI_DECL HAPI_GetVolumeTileIntData( HAPI_AssetId asset_id,
                                         HAPI_ObjectId object_id,
                                         HAPI_GeoId geo_id,
                                         HAPI_PartId part_id,
                                         HAPI_VolumeTileInfo * tile,
                                         int * values );

    /// @brief  Set the volume info of a geo on a geo input.
    ///
    /// @param[in]      asset_id
    ///                 An asset that this volume will be input into.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      volume_info
    ///                 All volume information that can be specified per 
    ///                 volume. This includes the position, orientation, scale,
    ///                 data format, tuple size, and taper. The tile size is
    ///                 always 8x8x8.
    ///
    HAPI_DECL HAPI_SetVolumeInfo( HAPI_AssetId asset_id,
                                  HAPI_ObjectId object_id,
                                  HAPI_GeoId geo_id,
                                  const HAPI_VolumeInfo * volume_info );

    /// @brief  Set the values of a float tile: this is an 8x8x8 subsection of
    ///         the volume.
    ///
    /// @param[in]      asset_id
    ///                 The asset that the volume will be input into.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      tile
    ///                 The tile that the volume will be input into.
    ///
    /// @param[in]      values
    ///                 The values of the individual voxel tiles in the 
    ///                 volume. The length of this array should 
    ///                 be 8^3*tupleSize.
    ///
    HAPI_DECL HAPI_SetVolumeTileFloatData( HAPI_AssetId asset_id,
                                           HAPI_ObjectId object_id,
                                           HAPI_GeoId geo_id,
                                           const HAPI_VolumeTileInfo * tile,
                                           const float * values );

    /// @brief  Set the values of an int tile: this is an 8x8x8 subsection of
    ///         the volume.
    ///
    /// @param[in]      asset_id
    ///                 The asset that the volume will be input into.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      tile
    ///                 The tile that the volume will be input into.
    ///
    /// @param[in]      values
    ///                 The values of the individual voxel tiles in the 
    ///                 volume. The length of this array should 
    ///                 be 8^3*tupleSize.
    ///
    HAPI_DECL HAPI_SetVolumeTileIntData( HAPI_AssetId asset_id,
                                         HAPI_ObjectId object_id,
                                         HAPI_GeoId geo_id,
                                         const HAPI_VolumeTileInfo * tile,
                                         const int * values );

    // CURVES ---------------------------------------------------------------

    /// @brief  Retrieve any meta-data about the curves, including the
    ///         curve's type, order, and periodicity.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[out]     info
    ///                 The curve info represents the meta-data about
    ///                 the curves, including the type, order,
    ///                 and periodicity.
    ///
    HAPI_DECL HAPI_GetCurveInfo( HAPI_AssetId asset_id,
                                 HAPI_ObjectId object_id,
                                 HAPI_GeoId geo_id,
                                 HAPI_PartId part_id,
                                 HAPI_CurveInfo * info );

    /// @brief  Retrieve the number of vertices for each curve in the part.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[out]     counts
    ///                 The number of cvs each curve contains
    ///
    /// @param[in]      start
    ///                 The index of the first curve.
    ///
    /// @param[in]      length
    ///                 The number of curves' counts to retrieve.
    ///
    HAPI_DECL HAPI_GetCurveCounts( HAPI_AssetId asset_id,
                                   HAPI_ObjectId object_id,
                                   HAPI_GeoId geo_id,
                                   HAPI_PartId part_id,
                                   int * counts,
                                   int start, int length );

    /// @brief  Retrieve the orders for each curve in the part if the
    ///         curve has varying order.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[out]     orders
    ///                 The order of each curve will be returned in this
    ///                 array.
    ///
    /// @param[in]      start
    ///                 The index of the first curve.
    ///
    /// @param[in]      length
    ///                 The number of curves' orders to retrieve.
    ///
    HAPI_DECL HAPI_GetCurveOrders( HAPI_AssetId asset_id,
                                   HAPI_ObjectId object_id,
                                   HAPI_GeoId geo_id,
                                   HAPI_PartId part_id,
                                   int * orders,
                                   int start, int length );

    /// @brief  Retrieve the knots of the curves in this part.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 The part id.
    ///
    /// @param[out]     knots
    ///                 The knots of each curve will be returned in this
    ///                 array.
    ///
    /// @param[in]      start
    ///                 The index of the first curve.
    ///
    /// @param[in]      length
    ///                 The number of curves' knots to retrieve. The
    ///                 length of all the knots on a single curve is
    ///                 the order of that curve plus the number of
    ///                 vertices (see ::HAPI_GetCurveOrders(),
    ///                 and ::HAPI_GetCurveCounts()).
    ///
    HAPI_DECL HAPI_GetCurveKnots( HAPI_AssetId asset_id,
                                  HAPI_ObjectId object_id, 
                                  HAPI_GeoId geo_id,
                                  HAPI_PartId part_id,
                                  float * knots,
                                  int start, int length );

    /// @brief  Set meta-data for the curve mesh, including the
    ///         curve type, order, and periodicity.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 Currently unused. Input asset geos are assumed
    ///                 to have only one part.
    ///
    /// @param[in]     info
    ///                 The curve info represents the meta-data about
    ///                 the curves, including the type, order,
    ///                 and periodicity.
    ///
    HAPI_DECL HAPI_SetCurveInfo( HAPI_AssetId asset_id,
                                 HAPI_ObjectId object_id,
                                 HAPI_GeoId geo_id,
                                 HAPI_PartId part_id,
                                 const HAPI_CurveInfo * info );

    /// @brief  Set the number of vertices for each curve in the part.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 Currently unused. Input asset geos are assumed
    ///                 to have only one part.
    ///
    /// @param[in]      counts
    ///                 The number of cvs each curve contains.
    ///
    /// @param[in]      start
    ///                 The index of the first curve.
    ///
    /// @param[in]      length
    ///                 The number of curves' counts to set.
    ///
    HAPI_DECL HAPI_SetCurveCounts( HAPI_AssetId asset_id,
                                   HAPI_ObjectId object_id,
                                   HAPI_GeoId geo_id,
                                   HAPI_PartId part_id,
                                   const int * counts,
                                   int start, int length );

    /// @brief  Set the orders for each curve in the part if the
    ///         curve has varying order.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 Currently unused. Input asset geos are assumed
    ///                 to have only one part.
    ///
    /// @param[in]      orders
    ///                 The orders of each curve.
    ///
    /// @param[in]      start
    ///                 The index of the first curve.
    ///
    /// @param[in]      length
    ///                 The number of curves' orders to retrieve.
    ///
    HAPI_DECL HAPI_SetCurveOrders( HAPI_AssetId asset_id,
                                   HAPI_ObjectId object_id,
                                   HAPI_GeoId geo_id,
                                   HAPI_PartId part_id,
                                   const int * orders,
                                   int start, int length );

    /// @brief  Set the knots of the curves in this part.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      part_id
    ///                 Currently unused. Input asset geos are assumed
    ///                 to have only one part.
    ///
    /// @param[in]      knots
    ///                 The knots of each curve.
    ///
    /// @param[in]      start
    ///                 The index of the first curve.
    ///
    /// @param[in]      length
    ///                 The number of curves' knots to set. The
    ///                 length of all the knots on a single curve is
    ///                 the order of that curve plus the number of
    ///                 vertices (see ::HAPI_SetCurveOrders(),
    ///                 and ::HAPI_SetCurveCounts()).
    ///
    HAPI_DECL HAPI_SetCurveKnots( HAPI_AssetId asset_id,
                                  HAPI_ObjectId object_id,
                                  HAPI_GeoId geo_id,
                                  HAPI_PartId part_id,
                                  const float * knots,
                                  int start, int length );

    // CACHING --------------------------------------------------------------

    /// @brief  Saves a geometry to file.  The type of file to save is 
    ///         to be determined by the extension ie. .bgeo, .obj
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      file_name
    ///                 The name of the file to be saved.  The extension
    ///                 of the file determines its type.
    ///    
    HAPI_DECL HAPI_SaveGeoToFile( HAPI_AssetId asset_id,
                                  HAPI_ObjectId object_id,
                                  HAPI_GeoId geo_id,
                                  const char * file_name );

    /// @brief  Loads a geometry file and put its contents onto a SOP
    ///         node.  
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      file_name
    ///                 The name of the file to be loaded
    ///    
    HAPI_DECL HAPI_LoadGeoFromFile( HAPI_AssetId asset_id,
                                    HAPI_ObjectId object_id,
                                    HAPI_GeoId geo_id,
                                    const char * file_name );

    /// @brief  Cache the current state of the geo to memory, given the
    ///         format, and return the size. Use this size with your call
    ///         to ::HAPI_SaveGeoToMemory() to copy the cached geo to your
    ///         buffer. It is guaranteed that the size will not change between
    ///         your call to ::HAPI_GetGeoSize() and ::HAPI_SaveGeoToMemory().
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      format
    ///                 The file format, ie. "obj", "bgeo" etc.
    ///
    /// @param[out]     size
    ///                 The size of the buffer required to hold the output.
    ///
    HAPI_DECL HAPI_GetGeoSize( HAPI_AssetId asset_id,
                               HAPI_ObjectId object_id,
                               HAPI_GeoId geo_id,
                               const char * format, 
                               int * size );

    /// @brief  Saves the cached geometry to your buffer in memory, 
    ///         whose format and required size is identified by the call to
    ///         ::HAPI_GetGeoSize(). The call to ::HAPI_GetGeoSize() is
    ///         required as ::HAPI_GetGeoSize() does the actual saving work.
    ///
    ///         Also note that this call to ::HAPI_SaveGeoToMemory will delete
    ///         the internal geo buffer that was cached in the previous call
    ///         to ::HAPI_GetGeoSize(). This means that you will need to call
    ///         ::HAPI_GetGeoSize() again before you can call this function.
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[out]     buffer
    ///                 The buffer we will write into.
    ///
    /// @param[in]      size
    ///                 The size of the buffer passed in.
    ///
    HAPI_DECL HAPI_SaveGeoToMemory( HAPI_AssetId asset_id,
                                    HAPI_ObjectId object_id,
                                    HAPI_GeoId geo_id,
                                    char * buffer,
                                    int size );

    /// @brief  Loads a geometry from memory and put its 
    ///         contents onto a SOP node.  
    ///
    /// @param[in]      asset_id
    ///                 The asset id.
    ///
    /// @param[in]      object_id
    ///                 The object id.
    ///
    /// @param[in]      geo_id
    ///                 The geometry id.
    ///
    /// @param[in]      format
    ///                 The file format, ie. "obj", "bgeo" etc.
    ///    
    /// @param[in]      buffer
    ///                 The buffer we will read the geomtry from.
    ///
    /// @param[in]      size
    ///                 The size of the buffer passed in.
    ///
    HAPI_DECL HAPI_LoadGeoFromMemory( HAPI_AssetId asset_id,
                                      HAPI_ObjectId object_id,
                                      HAPI_GeoId geo_id,
                                      const char * format,
                                      char * buffer,
                                      int size );

#endif
