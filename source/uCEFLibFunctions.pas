// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2017 Salvador Díaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit uCEFLibFunctions;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

interface

uses
  WinApi.Windows, System.Math,
  uCEFTypes;

const
  LibcefDLL = 'libcef.dll';


// /include/capi/cef_app_capi.h
function  cef_initialize(const args: PCefMainArgs; const settings: PCefSettings; application: PCefApp; windows_sandbox_info: Pointer): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_shutdown; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_execute_process(const args: PCefMainArgs; application: PCefApp; windows_sandbox_info: Pointer): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_do_message_loop_work; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_run_message_loop; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_quit_message_loop; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_set_osmodal_loop(osModalLoop: Integer); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_enable_highdpi_support; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_browser_capi.h
function  cef_browser_host_create_browser(const windowInfo: PCefWindowInfo; client: PCefClient; const url: PCefString; const settings: PCefBrowserSettings; request_context: PCefRequestContext): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_browser_host_create_browser_sync(const windowInfo: PCefWindowInfo; client: PCefClient; const url: PCefString; const settings: PCefBrowserSettings; request_context: PCefRequestContext): PCefBrowser; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_command_line_capi.h
function  cef_command_line_create : PCefCommandLine; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_command_line_get_global : PCefCommandLine; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_cookie_capi.h
function  cef_cookie_manager_get_global_manager(callback: PCefCompletionCallback): PCefCookieManager; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_cookie_manager_create_manager(const path: PCefString; persist_session_cookies: Integer; callback: PCefCompletionCallback): PCefCookieManager; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_crash_util.h
function  cef_crash_reporting_enabled: integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_set_crash_key_value(const key, value : PCefString); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_drag_data_capi.h
function  cef_drag_data_create : PCefDragData; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_file_util_capi.h
function  cef_create_directory(const full_path : PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_get_temp_directory(temp_dir : PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_create_new_temp_directory(const prefix : PCefString; new_temp_path: PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_create_temp_directory_in_directory(const base_dir, prefix : PCefString; new_dir : PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_directory_exists(const path : PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_delete_file(const path : PCefString; recursive : integer): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_zip_directory(const src_dir, dest_file : PCefString; include_hidden_files : integer): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_geolocation_capi.h
function  cef_get_geolocation(callback: PCefGetGeolocationCallback): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_image_capi.h
function  cef_image_create : PCefImage; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_menu_model_capi.h
function  cef_menu_model_create(delegate: PCefMenuModelDelegate): PCefMenuModel; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_origin_whitelist_capi.h
function  cef_add_cross_origin_whitelist_entry(const source_origin, target_protocol, target_domain: PCefString; allow_target_subdomains: Integer): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_remove_cross_origin_whitelist_entry(const source_origin, target_protocol, target_domain: PCefString; allow_target_subdomains: Integer): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_clear_cross_origin_whitelist : Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_parser_capi.h
function  cef_parse_url(const url: PCefString; var parts: TCefUrlParts): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_create_url(parts: PCefUrlParts; url: PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_format_url_for_security_display(const origin_url: PCefString): PCefStringUserFree; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_get_mime_type(const extension: PCefString): PCefStringUserFree; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_get_extensions_for_mime_type(const mime_type: PCefString; extensions: TCefStringList); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_base64encode(const data: Pointer; data_size: NativeUInt): PCefStringUserFree; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_base64decode(const data: PCefString): PCefBinaryValue; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_uriencode(const text: PCefString; use_plus: Integer): PCefStringUserFree; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_uridecode(const text: PCefString; convert_to_utf8: Integer; unescape_rule: TCefUriUnescapeRule): PCefStringUserFree; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_parse_json(const json_string: PCefString; options: TCefJsonParserOptions): PCefValue; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_parse_jsonand_return_error(const json_string: PCefString; options: TCefJsonParserOptions; error_code_out: PCefJsonParserError; error_msg_out: PCefString): PCefValue; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_write_json(node: PCefValue; options: TCefJsonWriterOptions): PCefStringUserFree; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_path_util_capi.h
function  cef_get_path(key: TCefPathKey; path: PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_print_settings_capi.h
function  cef_print_settings_create : PCefPrintSettings; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_process_message_capi.h
function  cef_process_message_create(const name: PCefString): PCefProcessMessage; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_process_util_capi.h
function  cef_launch_process(command_line: PCefCommandLine): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_request_capi.h
function  cef_request_create : PCefRequest; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_post_data_create : PCefPostData; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_post_data_element_create : PCefPostDataElement; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_request_context_capi.h
function  cef_request_context_get_global_context : PCefRequestContext; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_request_context_create_context(const settings: PCefRequestContextSettings; handler: PCefRequestContextHandler): PCefRequestContext; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_create_context_shared(other: PCefRequestContext; handler: PCefRequestContextHandler): PCefRequestContext; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_resource_bundle_capi.h
function  cef_resource_bundle_get_global : PCefResourceBundle; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_response_capi.h
function  cef_response_create : PCefResponse; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_scheme_capi.h
function  cef_register_scheme_handler_factory(const scheme_name, domain_name: PCefString; factory: PCefSchemeHandlerFactory): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_clear_scheme_handler_factories : Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_ssl_info_capi.h
function  cef_is_cert_status_error(status : TCefCertStatus) : integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_is_cert_status_minor_error(status : TCefCertStatus) : integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_stream_capi.h
function  cef_stream_reader_create_for_file(const fileName: PCefString): PCefStreamReader; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_stream_reader_create_for_data(data: Pointer; size: NativeUInt): PCefStreamReader; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_stream_reader_create_for_handler(handler: PCefReadHandler): PCefStreamReader; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_stream_writer_create_for_file(const fileName: PCefString): PCefStreamWriter; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_stream_writer_create_for_handler(handler: PCefWriteHandler): PCefStreamWriter; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_task_capi.h
function  cef_task_runner_get_for_current_thread : PCefTaskRunner; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_task_runner_get_for_thread(threadId: TCefThreadId): PCefTaskRunner; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_currently_on(threadId: TCefThreadId): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_post_task(threadId: TCefThreadId; task: PCefTask): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_post_delayed_task(threadId: TCefThreadId; task: PCefTask; delay_ms: Int64): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_thread_capi.h
function  cef_thread_create(const display_name: PCefString; priority: TCefThreadPriority; message_loop_type: TCefMessageLoopType; stoppable: integer; com_init_mode: TCefCOMInitMode): PCefThread; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_trace_capi.h
function  cef_begin_tracing(const categories: PCefString; callback: PCefCompletionCallback): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_end_tracing(const tracing_file: PCefString; callback: PCefEndTracingCallback): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_now_from_system_trace_time : int64; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_urlrequest_capi.h
function  cef_urlrequest_create(request: PCefRequest; client: PCefUrlRequestClient; request_context: PCefRequestContext): PCefUrlRequest; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_v8_capi.h
function  cef_v8context_get_current_context : PCefv8Context; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_v8context_get_entered_context : PCefv8Context; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_v8context_in_context : Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_v8value_create_undefined : PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_v8value_create_null : PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_v8value_create_bool(value: Integer): PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_v8value_create_int(value: Integer): PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_v8value_create_uint(value: Cardinal): PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_v8value_create_double(value: Double): PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_v8value_create_date(const value: PCefTime): PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_v8value_create_string(const value: PCefString): PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_v8value_create_object(accessor: PCefV8Accessor; interceptor: PCefV8Interceptor): PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_v8value_create_array(length: Integer): PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_v8value_create_function(const name: PCefString; handler: PCefv8Handler): PCefv8Value; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_v8stack_trace_get_current(frame_limit: Integer): PCefV8StackTrace; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_register_extension(const extension_name, javascript_code: PCefString; handler: PCefv8Handler): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_values_capi.h
function  cef_value_create : PCefValue; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_binary_value_create(const data: Pointer; data_size: NativeUInt): PCefBinaryValue; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_dictionary_value_create: PCefDictionaryValue; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_list_value_create : PCefListValue; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_waitable_event_capi.h
function  cef_waitable_event_create(automatic_reset, initially_signaled : integer): PCefWaitableEvent; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_web_plugin_capi.h
procedure cef_visit_web_plugin_info(visitor: PCefWebPluginInfoVisitor); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_refresh_web_plugins; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_unregister_internal_web_plugin(const path: PCefString); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_register_web_plugin_crash(const path: PCefString); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_is_web_plugin_unstable(const path: PCefString; callback: PCefWebPluginUnstableCallback); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_register_widevine_cdm(const path: PCefString; callback: PCefRegisterCDMCallback); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_xml_reader_capi.h
function  cef_xml_reader_create(stream: PCefStreamReader; encodingType: TCefXmlEncodingType; const URI: PCefString): PCefXmlReader; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/capi/cef_zip_reader_capi.h
function  cef_zip_reader_create(stream: PCefStreamReader): PCefZipReader; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/internal/cef_logging_internal.h
function  cef_get_min_log_level : Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_get_vlog_level(const file_start: PAnsiChar; N: NativeInt): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_log(const file_: PAnsiChar; line, severity: Integer; const message: PAnsiChar); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/internal/cef_string_list.h
function  cef_string_list_alloc(): TCefStringList; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_list_size(list: TCefStringList): NativeUInt; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_list_value(list: TCefStringList; index: NativeUInt; value: PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_string_list_append(list: TCefStringList; const value: PCefString); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_string_list_clear(list: TCefStringList); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_string_list_free(list: TCefStringList); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_list_copy(list: TCefStringList): TCefStringList; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/internal/cef_string_map.h
function  cef_string_map_alloc: TCefStringMap; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_map_size(map: TCefStringMap): NativeUInt; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_map_find(map: TCefStringMap; const key: PCefString; var value: TCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_map_key(map: TCefStringMap; index: NativeUInt; var key: TCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_map_value(map: TCefStringMap; index: NativeUInt; var value: TCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_map_append(map: TCefStringMap; const key, value: PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_string_map_clear(map: TCefStringMap); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_string_map_free(map: TCefStringMap); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/internal/cef_string_multimap.h
function  cef_string_multimap_alloc : TCefStringMultimap; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_multimap_size(map: TCefStringMultimap): NativeUInt; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_multimap_find_count(map: TCefStringMultimap; const key: PCefString): NativeUInt; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_multimap_enumerate(map: TCefStringMultimap; const key: PCefString; value_index: NativeUInt; var value: TCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_multimap_key(map: TCefStringMultimap; index: NativeUInt; var key: TCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_multimap_value(map: TCefStringMultimap; index: NativeUInt; var value: TCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_multimap_append(map: TCefStringMultimap; const key, value: PCefString): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_string_multimap_clear(map: TCefStringMultimap); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_string_multimap_free(map: TCefStringMultimap); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/internal/cef_string_types.h
function  cef_string_wide_set(const src: PWideChar; src_len: NativeUInt;  output: PCefStringWide; copy: Integer): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_utf8_set(const src: PAnsiChar; src_len: NativeUInt; output: PCefStringUtf8; copy: Integer): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_utf16_set(const src: PChar16; src_len: NativeUInt; output: PCefStringUtf16; copy: Integer): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_string_wide_clear(str: PCefStringWide); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_string_utf8_clear(str: PCefStringUtf8); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_string_utf16_clear(str: PCefStringUtf16); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_wide_cmp(const str1, str2: PCefStringWide): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_utf8_cmp(const str1, str2: PCefStringUtf8): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_utf16_cmp(const str1, str2: PCefStringUtf16): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_wide_to_utf8(const src: PWideChar; src_len: NativeUInt; output: PCefStringUtf8): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_utf8_to_wide(const src: PAnsiChar; src_len: NativeUInt; output: PCefStringWide): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_wide_to_utf16 (const src: PWideChar; src_len: NativeUInt; output: PCefStringUtf16): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_utf16_to_wide(const src: PChar16; src_len: NativeUInt; output: PCefStringWide): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_utf8_to_utf16(const src: PAnsiChar; src_len: NativeUInt; output: PCefStringUtf16): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_utf16_to_utf8(const src: PChar16; src_len: NativeUInt; output: PCefStringUtf8): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_ascii_to_wide(const src: PAnsiChar; src_len: NativeUInt; output: PCefStringWide): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_ascii_to_utf16(const src: PAnsiChar; src_len: NativeUInt; output: PCefStringUtf16): Integer; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_userfree_wide_alloc : PCefStringUserFreeWide; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_userfree_utf8_alloc : PCefStringUserFreeUtf8; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_string_userfree_utf16_alloc : PCefStringUserFreeUtf16; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_string_userfree_wide_free(str: PCefStringUserFreeWide); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_string_userfree_utf8_free(str: PCefStringUserFreeUtf8); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_string_userfree_utf16_free(str: PCefStringUserFreeUtf16); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/internal/cef_thread_internal.h
function  cef_get_current_platform_thread_id : TCefPlatformThreadId; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
function  cef_get_current_platform_thread_handle : TCefPlatformThreadHandle; {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;

// /include/internal/cef_trace_event_internal.h
procedure cef_trace_event_instant(const category, name, arg1_name: PAnsiChar; arg1_val: uint64; const arg2_name: PAnsiChar; arg2_val: UInt64; copy: Integer); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_trace_event_begin(const category, name, arg1_name: PAnsiChar; arg1_val: UInt64; const arg2_name: PAnsiChar; arg2_val: UInt64; copy: Integer); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_trace_event_end(const category, name, arg1_name: PAnsiChar; arg1_val: UInt64; const arg2_name: PAnsiChar; arg2_val: UInt64; copy: Integer); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_trace_counter(const category, name, value1_name: PAnsiChar; value1_val: UInt64; const value2_name: PAnsiChar; value2_val: UInt64; copy: Integer); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_trace_counter_id(const category, name: PAnsiChar; id: UInt64; const value1_name: PAnsiChar; value1_val: UInt64; const value2_name: PAnsiChar; value2_val: UInt64; copy: Integer); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_trace_event_async_begin(const category, name: PAnsiChar; id: UInt64; const arg1_name: PAnsiChar; arg1_val: UInt64; const arg2_name: PAnsiChar; arg2_val: UInt64; copy: Integer); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_trace_event_async_step_into(const category, name: PAnsiChar; id, step: UInt64; const arg1_name: PAnsiChar; arg1_val: UInt64; copy: Integer); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_trace_event_async_step_past(const category, name: PAnsiChar; id, step: UInt64; const arg1_name: PAnsiChar; arg1_val: UInt64; copy: Integer); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;
procedure cef_trace_event_async_end(const category, name: PAnsiChar; id: UInt64; const arg1_name: PAnsiChar; arg1_val: UInt64; const arg2_name: PAnsiChar; arg2_val: UInt64; copy: Integer); {$IFDEF CPUX64}stdcall{$ELSE}cdecl{$ENDIF}; external LibcefDLL;


implementation

end.
