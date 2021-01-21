// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2021 Salvador Diaz Fau. All rights reserved.
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

unit uCEFConstants;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

{$IFDEF MSWINDOWS}
uses
  {$IFDEF DELPHI16_UP}
  Winapi.Messages;
  {$ELSE}
  Messages;
  {$ENDIF}
{$ENDIF}

const
  // Error list defined in /include/internal/cef_types.h (cef_errorcode_t)
  // which includes this file /include/base/internal/cef_net_error_list.h
  // which includes this Chromium source file /net/base/net_error_list.h
  // available here in the master branch -> https://chromium.googlesource.com/chromium/src/+/master/net/base/net_error_list.h
  // compare the values in the right Chromium branch.
  ERR_NONE                                            = 0;
  ERR_IO_PENDING                                      = -1;
  ERR_FAILED                                          = -2;
  ERR_ABORTED                                         = -3;
  ERR_INVALID_ARGUMENT                                = -4;
  ERR_INVALID_HANDLE                                  = -5;
  ERR_FILE_NOT_FOUND                                  = -6;
  ERR_TIMED_OUT                                       = -7;
  ERR_FILE_TOO_BIG                                    = -8;
  ERR_UNEXPECTED                                      = -9;
  ERR_ACCESS_DENIED                                   = -10;
  ERR_NOT_IMPLEMENTED                                 = -11;
  ERR_INSUFFICIENT_RESOURCES                          = -12;
  ERR_OUT_OF_MEMORY                                   = -13;
  ERR_UPLOAD_FILE_CHANGED                             = -14;
  ERR_SOCKET_NOT_CONNECTED                            = -15;
  ERR_FILE_EXISTS                                     = -16;
  ERR_FILE_PATH_TOO_LONG                              = -17;
  ERR_FILE_NO_SPACE                                   = -18;
  ERR_FILE_VIRUS_INFECTED                             = -19;
  ERR_BLOCKED_BY_CLIENT                               = -20;
  ERR_NETWORK_CHANGED                                 = -21;
  ERR_BLOCKED_BY_ADMINISTRATOR                        = -22;
  ERR_SOCKET_IS_CONNECTED                             = -23;
  ERR_BLOCKED_ENROLLMENT_CHECK_PENDING                = -24;
  ERR_UPLOAD_STREAM_REWIND_NOT_SUPPORTED              = -25;
  ERR_CONTEXT_SHUT_DOWN                               = -26;
  ERR_BLOCKED_BY_RESPONSE                             = -27;
  ERR_BLOCKED_BY_XSS_AUDITOR                          = -28;
  ERR_CLEARTEXT_NOT_PERMITTED                         = -29;
  ERR_CONNECTION_CLOSED                               = -100;
  ERR_CONNECTION_RESET                                = -101;
  ERR_CONNECTION_REFUSED                              = -102;
  ERR_CONNECTION_ABORTED                              = -103;
  ERR_CONNECTION_FAILED                               = -104;
  ERR_NAME_NOT_RESOLVED                               = -105;
  ERR_INTERNET_DISCONNECTED                           = -106;
  ERR_SSL_PROTOCOL_ERROR                              = -107;
  ERR_ADDRESS_INVALID                                 = -108;
  ERR_ADDRESS_UNREACHABLE                             = -109;
  ERR_SSL_CLIENT_AUTH_CERT_NEEDED                     = -110;
  ERR_TUNNEL_CONNECTION_FAILED                        = -111;
  ERR_NO_SSL_VERSIONS_ENABLED                         = -112;
  ERR_SSL_VERSION_OR_CIPHER_MISMATCH                  = -113;
  ERR_SSL_RENEGOTIATION_REQUESTED                     = -114;
  ERR_PROXY_AUTH_UNSUPPORTED                          = -115;
  ERR_CERT_ERROR_IN_SSL_RENEGOTIATION                 = -116;
  ERR_BAD_SSL_CLIENT_AUTH_CERT                        = -117;
  ERR_CONNECTION_TIMED_OUT                            = -118;
  ERR_HOST_RESOLVER_QUEUE_TOO_LARGE                   = -119;
  ERR_SOCKS_CONNECTION_FAILED                         = -120;
  ERR_SOCKS_CONNECTION_HOST_UNREACHABLE               = -121;
  ERR_ALPN_NEGOTIATION_FAILED                         = -122;
  ERR_SSL_NO_RENEGOTIATION                            = -123;
  ERR_WINSOCK_UNEXPECTED_WRITTEN_BYTES                = -124;
  ERR_SSL_DECOMPRESSION_FAILURE_ALERT                 = -125;
  ERR_SSL_BAD_RECORD_MAC_ALERT                        = -126;
  ERR_PROXY_AUTH_REQUESTED                            = -127;
  ERR_SSL_WEAK_SERVER_EPHEMERAL_DH_KEY                = -129;
  ERR_PROXY_CONNECTION_FAILED                         = -130;
  ERR_MANDATORY_PROXY_CONFIGURATION_FAILED            = -131;
  ERR_PRECONNECT_MAX_SOCKET_LIMIT                     = -133;
  ERR_SSL_CLIENT_AUTH_PRIVATE_KEY_ACCESS_DENIED       = -134;
  ERR_SSL_CLIENT_AUTH_CERT_NO_PRIVATE_KEY             = -135;
  ERR_PROXY_CERTIFICATE_INVALID                       = -136;
  ERR_NAME_RESOLUTION_FAILED                          = -137;
  ERR_NETWORK_ACCESS_DENIED                           = -138;
  ERR_TEMPORARILY_THROTTLED                           = -139;
  ERR_HTTPS_PROXY_TUNNEL_RESPONSE_REDIRECT            = -140;
  ERR_SSL_CLIENT_AUTH_SIGNATURE_FAILED                = -141;
  ERR_MSG_TOO_BIG                                     = -142;
  ERR_SPDY_SESSION_ALREADY_EXISTS                     = -143;
  ERR_WS_PROTOCOL_ERROR                               = -145;
  ERR_ADDRESS_IN_USE                                  = -147;
  ERR_SSL_HANDSHAKE_NOT_COMPLETED                     = -148;
  ERR_SSL_BAD_PEER_PUBLIC_KEY                         = -149;
  ERR_SSL_PINNED_KEY_NOT_IN_CERT_CHAIN                = -150;
  ERR_CLIENT_AUTH_CERT_TYPE_UNSUPPORTED               = -151;
  ERR_ORIGIN_BOUND_CERT_GENERATION_TYPE_MISMATCH      = -152;
  ERR_SSL_DECRYPT_ERROR_ALERT                         = -153;
  ERR_WS_THROTTLE_QUEUE_TOO_LARGE                     = -154;
  ERR_SSL_SERVER_CERT_CHANGED                         = -156;
  ERR_SSL_UNRECOGNIZED_NAME_ALERT                     = -159;
  ERR_SOCKET_SET_RECEIVE_BUFFER_SIZE_ERROR            = -160;
  ERR_SOCKET_SET_SEND_BUFFER_SIZE_ERROR               = -161;
  ERR_SOCKET_RECEIVE_BUFFER_SIZE_UNCHANGEABLE         = -162;
  ERR_SOCKET_SEND_BUFFER_SIZE_UNCHANGEABLE            = -163;
  ERR_SSL_CLIENT_AUTH_CERT_BAD_FORMAT                 = -164;
  ERR_ICANN_NAME_COLLISION                            = -166;
  ERR_SSL_SERVER_CERT_BAD_FORMAT                      = -167;
  ERR_CT_STH_PARSING_FAILED                           = -168;
  ERR_CT_STH_INCOMPLETE                               = -169;
  ERR_UNABLE_TO_REUSE_CONNECTION_FOR_PROXY_AUTH       = -170;
  ERR_CT_CONSISTENCY_PROOF_PARSING_FAILED             = -171;
  ERR_SSL_OBSOLETE_CIPHER                             = -172;
  ERR_WS_UPGRADE                                      = -173;
  ERR_READ_IF_READY_NOT_IMPLEMENTED                   = -174;
  ERR_SSL_VERSION_INTERFERENCE                        = -175;
  ERR_NO_BUFFER_SPACE                                 = -176;
  ERR_SSL_CLIENT_AUTH_NO_COMMON_ALGORITHMS            = -177;
  ERR_EARLY_DATA_REJECTED                             = -178;
  ERR_WRONG_VERSION_ON_EARLY_DATA                     = -179;
  ERR_TLS13_DOWNGRADE_DETECTED                        = -180;
  ERR_SSL_KEY_USAGE_INCOMPATIBLE                      = -181;
  ERR_CERT_COMMON_NAME_INVALID                        = -200;
  ERR_CERT_DATE_INVALID                               = -201;
  ERR_CERT_AUTHORITY_INVALID                          = -202;
  ERR_CERT_CONTAINS_ERRORS                            = -203;
  ERR_CERT_NO_REVOCATION_MECHANISM                    = -204;
  ERR_CERT_UNABLE_TO_CHECK_REVOCATION                 = -205;
  ERR_CERT_REVOKED                                    = -206;
  ERR_CERT_INVALID                                    = -207;
  ERR_CERT_WEAK_SIGNATURE_ALGORITHM                   = -208;
  ERR_CERT_NON_UNIQUE_NAME                            = -210;
  ERR_CERT_WEAK_KEY                                   = -211;
  ERR_CERT_NAME_CONSTRAINT_VIOLATION                  = -212;
  ERR_CERT_VALIDITY_TOO_LONG                          = -213;
  ERR_CERTIFICATE_TRANSPARENCY_REQUIRED               = -214;
  ERR_CERT_SYMANTEC_LEGACY                            = -215;
  ERR_CERT_END                                        = -216;
  ERR_INVALID_URL                                     = -300;
  ERR_DISALLOWED_URL_SCHEME                           = -301;
  ERR_UNKNOWN_URL_SCHEME                              = -302;
  ERR_INVALID_REDIRECT                                = -303;
  ERR_TOO_MANY_REDIRECTS                              = -310;
  ERR_UNSAFE_REDIRECT                                 = -311;
  ERR_UNSAFE_PORT                                     = -312;
  ERR_INVALID_RESPONSE                                = -320;
  ERR_INVALID_CHUNKED_ENCODING                        = -321;
  ERR_METHOD_NOT_SUPPORTED                            = -322;
  ERR_UNEXPECTED_PROXY_AUTH                           = -323;
  ERR_EMPTY_RESPONSE                                  = -324;
  ERR_RESPONSE_HEADERS_TOO_BIG                        = -325;
  ERR_PAC_STATUS_NOT_OK                               = -326;
  ERR_PAC_SCRIPT_FAILED                               = -327;
  ERR_REQUEST_RANGE_NOT_SATISFIABLE                   = -328;
  ERR_MALFORMED_IDENTITY                              = -329;
  ERR_CONTENT_DECODING_FAILED                         = -330;
  ERR_NETWORK_IO_SUSPENDED                            = -331;
  ERR_SYN_REPLY_NOT_RECEIVED                          = -332;
  ERR_ENCODING_CONVERSION_FAILED                      = -333;
  ERR_UNRECOGNIZED_FTP_DIRECTORY_LISTING_FORMAT       = -334;
  ERR_NO_SUPPORTED_PROXIES                            = -336;
  ERR_SPDY_PROTOCOL_ERROR                             = -337;
  ERR_INVALID_AUTH_CREDENTIALS                        = -338;
  ERR_UNSUPPORTED_AUTH_SCHEME                         = -339;
  ERR_ENCODING_DETECTION_FAILED                       = -340;
  ERR_MISSING_AUTH_CREDENTIALS                        = -341;
  ERR_UNEXPECTED_SECURITY_LIBRARY_STATUS              = -342;
  ERR_MISCONFIGURED_AUTH_ENVIRONMENT                  = -343;
  ERR_UNDOCUMENTED_SECURITY_LIBRARY_STATUS            = -344;
  ERR_RESPONSE_BODY_TOO_BIG_TO_DRAIN                  = -345;
  ERR_RESPONSE_HEADERS_MULTIPLE_CONTENT_LENGTH        = -346;
  ERR_INCOMPLETE_SPDY_HEADERS                         = -347;
  ERR_PAC_NOT_IN_DHCP                                 = -348;
  ERR_RESPONSE_HEADERS_MULTIPLE_CONTENT_DISPOSITION   = -349;
  ERR_RESPONSE_HEADERS_MULTIPLE_LOCATION              = -350;
  ERR_SPDY_SERVER_REFUSED_STREAM                      = -351;
  ERR_SPDY_PING_FAILED                                = -352;
  ERR_CONTENT_LENGTH_MISMATCH                         = -354;
  ERR_INCOMPLETE_CHUNKED_ENCODING                     = -355;
  ERR_QUIC_PROTOCOL_ERROR                             = -356;
  ERR_RESPONSE_HEADERS_TRUNCATED                      = -357;
  ERR_QUIC_HANDSHAKE_FAILED                           = -358;
  ERR_SPDY_INADEQUATE_TRANSPORT_SECURITY              = -360;
  ERR_SPDY_FLOW_CONTROL_ERROR                         = -361;
  ERR_SPDY_FRAME_SIZE_ERROR                           = -362;
  ERR_SPDY_COMPRESSION_ERROR                          = -363;
  ERR_PROXY_AUTH_REQUESTED_WITH_NO_CONNECTION         = -364;
  ERR_HTTP_1_1_REQUIRED                               = -365;
  ERR_PROXY_HTTP_1_1_REQUIRED                         = -366;
  ERR_PAC_SCRIPT_TERMINATED                           = -367;
  ERR_INVALID_HTTP_RESPONSE                           = -370;
  ERR_CONTENT_DECODING_INIT_FAILED                    = -371;
  ERR_SPDY_RST_STREAM_NO_ERROR_RECEIVED               = -372;
  ERR_SPDY_PUSHED_STREAM_NOT_AVAILABLE                = -373;
  ERR_SPDY_CLAIMED_PUSHED_STREAM_RESET_BY_SERVER      = -374;
  ERR_TOO_MANY_RETRIES                                = -375;
  ERR_SPDY_STREAM_CLOSED                              = -376;
  ERR_SPDY_CLIENT_REFUSED_STREAM                      = -377;
  ERR_SPDY_PUSHED_RESPONSE_DOES_NOT_MATCH             = -378;
  ERR_CACHE_MISS                                      = -400;
  ERR_CACHE_READ_FAILURE                              = -401;
  ERR_CACHE_WRITE_FAILURE                             = -402;
  ERR_CACHE_OPERATION_NOT_SUPPORTED                   = -403;
  ERR_CACHE_OPEN_FAILURE                              = -404;
  ERR_CACHE_CREATE_FAILURE                            = -405;
  ERR_CACHE_RACE                                      = -406;
  ERR_CACHE_CHECKSUM_READ_FAILURE                     = -407;
  ERR_CACHE_CHECKSUM_MISMATCH                         = -408;
  ERR_CACHE_LOCK_TIMEOUT                              = -409;
  ERR_CACHE_AUTH_FAILURE_AFTER_READ                   = -410;
  ERR_CACHE_ENTRY_NOT_SUITABLE                        = -411;
  ERR_CACHE_DOOM_FAILURE                              = -412;
  ERR_CACHE_OPEN_OR_CREATE_FAILURE                    = -413;
  ERR_INSECURE_RESPONSE                               = -501;
  ERR_NO_PRIVATE_KEY_FOR_CERT                         = -502;
  ERR_ADD_USER_CERT_FAILED                            = -503;
  ERR_INVALID_SIGNED_EXCHANGE                         = -504;
  ERR_FTP_FAILED                                      = -601;
  ERR_FTP_SERVICE_UNAVAILABLE                         = -602;
  ERR_FTP_TRANSFER_ABORTED                            = -603;
  ERR_FTP_FILE_BUSY                                   = -604;
  ERR_FTP_SYNTAX_ERROR                                = -605;
  ERR_FTP_COMMAND_NOT_SUPPORTED                       = -606;
  ERR_FTP_BAD_COMMAND_SEQUENCE                        = -607;
  ERR_PKCS12_IMPORT_BAD_PASSWORD                      = -701;
  ERR_PKCS12_IMPORT_FAILED                            = -702;
  ERR_IMPORT_CA_CERT_NOT_CA                           = -703;
  ERR_IMPORT_CERT_ALREADY_EXISTS                      = -704;
  ERR_IMPORT_CA_CERT_FAILED                           = -705;
  ERR_IMPORT_SERVER_CERT_FAILED                       = -706;
  ERR_PKCS12_IMPORT_INVALID_MAC                       = -707;
  ERR_PKCS12_IMPORT_INVALID_FILE                      = -708;
  ERR_PKCS12_IMPORT_UNSUPPORTED                       = -709;
  ERR_KEY_GENERATION_FAILED                           = -710;
  ERR_PRIVATE_KEY_EXPORT_FAILED                       = -712;
  ERR_SELF_SIGNED_CERT_GENERATION_FAILED              = -713;
  ERR_CERT_DATABASE_CHANGED                           = -714;
  ERR_DNS_MALFORMED_RESPONSE                          = -800;
  ERR_DNS_SERVER_REQUIRES_TCP                         = -801;
  ERR_DNS_SERVER_FAILED                               = -802;
  ERR_DNS_TIMED_OUT                                   = -803;
  ERR_NS_CACHE_MISS                                   = -804;
  ERR_DNS_SEARCH_EMPTY                                = -805;
  ERR_DNS_SORT_ERROR                                  = -806;
  ERR_DNS_HTTP_FAILED                                 = -807;

  // /include/internal/cef_types.h (cef_cert_status_t)
  CERT_STATUS_NONE                        = 0;
  CERT_STATUS_COMMON_NAME_INVALID         = 1 shl 0;
  CERT_STATUS_DATE_INVALID                = 1 shl 1;
  CERT_STATUS_AUTHORITY_INVALID           = 1 shl 2;
  CERT_STATUS_NO_REVOCATION_MECHANISM     = 1 shl 4;
  CERT_STATUS_UNABLE_TO_CHECK_REVOCATION  = 1 shl 5;
  CERT_STATUS_REVOKED                     = 1 shl 6;
  CERT_STATUS_INVALID                     = 1 shl 7;
  CERT_STATUS_WEAK_SIGNATURE_ALGORITHM    = 1 shl 8;
  CERT_STATUS_NON_UNIQUE_NAME             = 1 shl 10;
  CERT_STATUS_WEAK_KEY                    = 1 shl 11;
  CERT_STATUS_PINNED_KEY_MISSING          = 1 shl 13;
  CERT_STATUS_NAME_CONSTRAINT_VIOLATION   = 1 shl 14;
  CERT_STATUS_VALIDITY_TOO_LONG           = 1 shl 15;
  CERT_STATUS_IS_EV                       = 1 shl 16;
  CERT_STATUS_REV_CHECKING_ENABLED        = 1 shl 17;
  CERT_STATUS_SHA1_SIGNATURE_PRESENT      = 1 shl 19;
  CERT_STATUS_CT_COMPLIANCE_FAILED        = 1 shl 20;

  CERT_STATUS_FIRST_ERROR = CERT_STATUS_COMMON_NAME_INVALID;
  CERT_STATUS_LAST_ERROR  = CERT_STATUS_VALIDITY_TOO_LONG;

  // /include/internal/cef_types.h (cef_v8_accesscontrol_t)
  V8_ACCESS_CONTROL_DEFAULT               = 0;
  V8_ACCESS_CONTROL_ALL_CAN_READ          = 1 shl 0;
  V8_ACCESS_CONTROL_ALL_CAN_WRITE         = 1 shl 1;
  V8_ACCESS_CONTROL_PROHIBITS_OVERWRITING = 1 shl 2;

  // /include/internal/cef_types.h (cef_v8_propertyattribute_t)
  V8_PROPERTY_ATTRIBUTE_NONE       = 0;
  V8_PROPERTY_ATTRIBUTE_READONLY   = 1 shl 0;
  V8_PROPERTY_ATTRIBUTE_DONTENUM   = 1 shl 1;
  V8_PROPERTY_ATTRIBUTE_DONTDELETE = 1 shl 2;

  // /include/internal/cef_types.h (cef_transition_type_t)
  TT_LINK                 = 0;
  TT_EXPLICIT             = 1;
  TT_AUTO_SUBFRAME        = 3;
  TT_MANUAL_SUBFRAME      = 4;
  TT_FORM_SUBMIT          = 7;
  TT_RELOAD               = 8;
  TT_SOURCE_MASK          = $000000FF;
  TT_BLOCKED_FLAG         = $00800000;
  TT_FORWARD_BACK_FLAG    = $01000000;
  TT_DIRECT_LOAD_FLAG     = $02000000;
  TT_CHAIN_START_FLAG     = $10000000;
  TT_CHAIN_END_FLAG       = $20000000;
  TT_CLIENT_REDIRECT_FLAG = $40000000;
  TT_SERVER_REDIRECT_FLAG = $80000000;
  TT_IS_REDIRECT_MASK     = $C0000000;
  TT_QUALIFIER_MASK       = $FFFFFF00;

  // /include/internal/cef_types.h (cef_urlrequest_flags_t)
  UR_FLAG_NONE                     = 0;
  UR_FLAG_SKIP_CACHE               = 1 shl 0;
  UR_FLAG_ONLY_FROM_CACHE          = 1 shl 1;
  UR_FLAG_DISABLE_CACHE            = 1 shl 2;
  UR_FLAG_ALLOW_STORED_CREDENTIALS = 1 shl 3;
  UR_FLAG_REPORT_UPLOAD_PROGRESS   = 1 shl 4;
  UR_FLAG_NO_DOWNLOAD_DATA         = 1 shl 5;
  UR_FLAG_NO_RETRY_ON_5XX          = 1 shl 6;
  UR_FLAG_STOP_ON_REDIRECT         = 1 shl 7;

  // /include/internal/cef_types.h (cef_scheme_options_t)
  CEF_SCHEME_OPTION_NONE             = 0;
  CEF_SCHEME_OPTION_STANDARD         = 1 shl 0;
  CEF_SCHEME_OPTION_LOCAL            = 1 shl 1;
  CEF_SCHEME_OPTION_DISPLAY_ISOLATED = 1 shl 2;
  CEF_SCHEME_OPTION_SECURE           = 1 shl 3;
  CEF_SCHEME_OPTION_CORS_ENABLED     = 1 shl 4;
  CEF_SCHEME_OPTION_CSP_BYPASSING    = 1 shl 5;
  CEF_SCHEME_OPTION_FETCH_ENABLED    = 1 shl 6;

  // /include/internal/cef_types.h (cef_dom_event_category_t)
  DOM_EVENT_CATEGORY_UNKNOWN                 = 0;
  DOM_EVENT_CATEGORY_UI                      = 1 shl 0;
  DOM_EVENT_CATEGORY_MOUSE                   = 1 shl 1;
  DOM_EVENT_CATEGORY_MUTATION                = 1 shl 2;
  DOM_EVENT_CATEGORY_KEYBOARD                = 1 shl 3;
  DOM_EVENT_CATEGORY_TEXT                    = 1 shl 4;
  DOM_EVENT_CATEGORY_COMPOSITION             = 1 shl 5;
  DOM_EVENT_CATEGORY_DRAG                    = 1 shl 6;
  DOM_EVENT_CATEGORY_CLIPBOARD               = 1 shl 7;
  DOM_EVENT_CATEGORY_MESSAGE                 = 1 shl 8;
  DOM_EVENT_CATEGORY_WHEEL                   = 1 shl 9;
  DOM_EVENT_CATEGORY_BEFORE_TEXT_INSERTED    = 1 shl 10;
  DOM_EVENT_CATEGORY_OVERFLOW                = 1 shl 11;
  DOM_EVENT_CATEGORY_PAGE_TRANSITION         = 1 shl 12;
  DOM_EVENT_CATEGORY_POPSTATE                = 1 shl 13;
  DOM_EVENT_CATEGORY_PROGRESS                = 1 shl 14;
  DOM_EVENT_CATEGORY_XMLHTTPREQUEST_PROGRESS = 1 shl 15;

  // /include/internal/cef_types.h (cef_event_flags_t)
  EVENTFLAG_NONE                 = 0;
  EVENTFLAG_CAPS_LOCK_ON         = 1 shl 0;
  EVENTFLAG_SHIFT_DOWN           = 1 shl 1;
  EVENTFLAG_CONTROL_DOWN         = 1 shl 2;
  EVENTFLAG_ALT_DOWN             = 1 shl 3;
  EVENTFLAG_LEFT_MOUSE_BUTTON    = 1 shl 4;
  EVENTFLAG_MIDDLE_MOUSE_BUTTON  = 1 shl 5;
  EVENTFLAG_RIGHT_MOUSE_BUTTON   = 1 shl 6;
  EVENTFLAG_COMMAND_DOWN         = 1 shl 7;
  EVENTFLAG_NUM_LOCK_ON          = 1 shl 8;
  EVENTFLAG_IS_KEY_PAD           = 1 shl 9;
  EVENTFLAG_IS_LEFT              = 1 shl 10;
  EVENTFLAG_IS_RIGHT             = 1 shl 11;
  EVENTFLAG_ALTGR_DOWN           = 1 shl 12;

  // /include/internal/cef_types.h (cef_drag_operations_mask_t)
  DRAG_OPERATION_NONE     = 0;
  DRAG_OPERATION_COPY     = 1 shl 0;
  DRAG_OPERATION_LINK     = 1 shl 1;
  DRAG_OPERATION_GENERIC  = 1 shl 2;
  DRAG_OPERATION_PRIVATE  = 1 shl 3;
  DRAG_OPERATION_MOVE     = 1 shl 4;
  DRAG_OPERATION_DELETE   = 1 shl 5;
  DRAG_OPERATION_EVERY    = $FFFFFFFF;

  // /include/internal/cef_types.h (cef_file_dialog_mode_t)
  FILE_DIALOG_OPEN                 = $00000000;
  FILE_DIALOG_OPEN_MULTIPLE        = $00000001;
  FILE_DIALOG_OPEN_FOLDER          = $00000002;
  FILE_DIALOG_SAVE                 = $00000003;
  FILE_DIALOG_TYPE_MASK            = $000000FF;
  FILE_DIALOG_OVERWRITEPROMPT_FLAG = $01000000;
  FILE_DIALOG_HIDEREADONLY_FLAG    = $02000000;

  // /include/internal/cef_types.h (cef_uri_unescape_rule_t)
  UU_NONE                                     = 0;
  UU_NORMAL                                   = 1 shl 0;
  UU_SPACES                                   = 1 shl 1;
  UU_PATH_SEPARATORS                          = 1 shl 2;
  UU_URL_SPECIAL_CHARS_EXCEPT_PATH_SEPARATORS = 1 shl 3;
  UU_REPLACE_PLUS_WITH_SPACE                  = 1 shl 4;

  // /include/internal/cef_types.h (cef_menu_id_t)
  MENU_ID_BACK                       = 100;
  MENU_ID_FORWARD                    = 101;
  MENU_ID_RELOAD                     = 102;
  MENU_ID_RELOAD_NOCACHE             = 103;
  MENU_ID_STOPLOAD                   = 104;
  MENU_ID_UNDO                       = 110;
  MENU_ID_REDO                       = 111;
  MENU_ID_CUT                        = 112;
  MENU_ID_COPY                       = 113;
  MENU_ID_PASTE                      = 114;
  MENU_ID_DELETE                     = 115;
  MENU_ID_SELECT_ALL                 = 116;
  MENU_ID_FIND                       = 130;
  MENU_ID_PRINT                      = 131;
  MENU_ID_VIEW_SOURCE                = 132;
  MENU_ID_SPELLCHECK_SUGGESTION_0    = 200;
  MENU_ID_SPELLCHECK_SUGGESTION_1    = 201;
  MENU_ID_SPELLCHECK_SUGGESTION_2    = 202;
  MENU_ID_SPELLCHECK_SUGGESTION_3    = 203;
  MENU_ID_SPELLCHECK_SUGGESTION_4    = 204;
  MENU_ID_SPELLCHECK_SUGGESTION_LAST = 204;
  MENU_ID_NO_SPELLING_SUGGESTIONS    = 205;
  MENU_ID_ADD_TO_DICTIONARY          = 206;
  MENU_ID_CUSTOM_FIRST               = 220;
  MENU_ID_CUSTOM_LAST                = 250;
  MENU_ID_USER_FIRST                 = 26500;
  MENU_ID_USER_LAST                  = 28500;

  // /include/internal/cef_types.h (cef_context_menu_type_flags_t)
  CM_TYPEFLAG_NONE      = 0;
  CM_TYPEFLAG_PAGE      = 1 shl 0;
  CM_TYPEFLAG_FRAME     = 1 shl 1;
  CM_TYPEFLAG_LINK      = 1 shl 2;
  CM_TYPEFLAG_MEDIA     = 1 shl 3;
  CM_TYPEFLAG_SELECTION = 1 shl 4;
  CM_TYPEFLAG_EDITABLE  = 1 shl 5;

  // /include/internal/cef_types.h (cef_context_menu_media_state_flags_t)
  CM_MEDIAFLAG_NONE                  = 0;
  CM_MEDIAFLAG_ERROR                 = 1 shl 0;
  CM_MEDIAFLAG_PAUSED                = 1 shl 1;
  CM_MEDIAFLAG_MUTED                 = 1 shl 2;
  CM_MEDIAFLAG_LOOP                  = 1 shl 3;
  CM_MEDIAFLAG_CAN_SAVE              = 1 shl 4;
  CM_MEDIAFLAG_HAS_AUDIO             = 1 shl 5;
  CM_MEDIAFLAG_HAS_VIDEO             = 1 shl 6;
  CM_MEDIAFLAG_CONTROL_ROOT_ELEMENT  = 1 shl 7;
  CM_MEDIAFLAG_CAN_PRINT             = 1 shl 8;
  CM_MEDIAFLAG_CAN_ROTATE            = 1 shl 9;

  // /include/internal/cef_types.h (cef_context_menu_edit_state_flags_t)
  CM_EDITFLAG_NONE                   = 0;
  CM_EDITFLAG_CAN_UNDO               = 1 shl 0;
  CM_EDITFLAG_CAN_REDO               = 1 shl 1;
  CM_EDITFLAG_CAN_CUT                = 1 shl 2;
  CM_EDITFLAG_CAN_COPY               = 1 shl 3;
  CM_EDITFLAG_CAN_PASTE              = 1 shl 4;
  CM_EDITFLAG_CAN_DELETE             = 1 shl 5;
  CM_EDITFLAG_CAN_SELECT_ALL         = 1 shl 6;
  CM_EDITFLAG_CAN_TRANSLATE          = 1 shl 7;

  // /include/internal/cef_types.h (cef_ssl_version_t)
  SSL_CONNECTION_VERSION_UNKNOWN = 0;
  SSL_CONNECTION_VERSION_SSL2    = 1;
  SSL_CONNECTION_VERSION_SSL3    = 2;
  SSL_CONNECTION_VERSION_TLS1    = 3;
  SSL_CONNECTION_VERSION_TLS1_1  = 4;
  SSL_CONNECTION_VERSION_TLS1_2  = 5;
  SSL_CONNECTION_VERSION_TLS1_3  = 6;
  SSL_CONNECTION_VERSION_QUIC    = 7;

  // /include/internal/cef_types.h (cef_ssl_content_status_t)
  SSL_CONTENT_NORMAL_CONTENT             = 0;
  SSL_CONTENT_DISPLAYED_INSECURE_CONTENT = 1 shl 0;
  SSL_CONTENT_RAN_INSECURE_CONTENT       = 1 shl 1;

  // /include/internal/cef_types.h (cef_json_writer_options_t)
  JSON_WRITER_DEFAULT                       = 0;
  JSON_WRITER_OMIT_BINARY_VALUES            = 1 shl 0;
  JSON_WRITER_OMIT_DOUBLE_TYPE_PRESERVATION = 1 shl 1;
  JSON_WRITER_PRETTY_PRINT                  = 1 shl 2;

  // /include/internal/cef_types.h (cef_log_severity_t)
  LOGSEVERITY_DEFAULT  = 0;
  LOGSEVERITY_VERBOSE  = 1;
  LOGSEVERITY_DEBUG    = LOGSEVERITY_VERBOSE;
  LOGSEVERITY_INFO     = 2;
  LOGSEVERITY_WARNING  = 3;
  LOGSEVERITY_ERROR    = 4;
  LOGSEVERITY_FATAL    = 5;
  LOGSEVERITY_DISABLE  = 99;

  // /include/internal/cef_types.h (cef_duplex_mode_t)
  DUPLEX_MODE_UNKNOWN    = -1;
  DUPLEX_MODE_SIMPLEX    = 0;
  DUPLEX_MODE_LONG_EDGE  = 1;
  DUPLEX_MODE_SHORT_EDGE = 2;

  // /include/internal/cef_types.h (cef_media_route_create_result_t)
  CEF_MRCR_UNKNOWN_ERROR            = 0;
  CEF_MRCR_OK                       = 1;
  CEF_MRCR_TIMED_OUT                = 2;
  CEF_MRCR_ROUTE_NOT_FOUND          = 3;
  CEF_MRCR_SINK_NOT_FOUND           = 4;
  CEF_MRCR_INVALID_ORIGIN           = 5;
  CEF_MRCR_NO_SUPPORTED_PROVIDER    = 7;
  CEF_MRCR_CANCELLED                = 8;
  CEF_MRCR_ROUTE_ALREADY_EXISTS     = 9;
  CEF_MRCR_ROUTE_ALREADY_TERMINATED = 11;
  CEF_MRCR_TOTAL_COUNT              = 12;

  // /include/internal/cef_types.h (cef_cookie_priority_t)
  CEF_COOKIE_PRIORITY_LOW    = -1;
  CEF_COOKIE_PRIORITY_MEDIUM = 0;
  CEF_COOKIE_PRIORITY_HIGH   = 1;

  // /include/internal/cef_types.h (cef_text_field_commands_t)
  CEF_TFC_CUT        = 1;
  CEF_TFC_COPY       = 2;
  CEF_TFC_PASTE      = 3;
  CEF_TFC_UNDO       = 4;
  CEF_TFC_DELETE     = 5;
  CEF_TFC_SELECT_ALL = 6;


//******************************************************
//****************** OTHER CONSTANTS *******************
//******************************************************

  DEVTOOLS_WINDOWNAME = 'DevTools';

  CEF_PROXYTYPE_DIRECT        = 0;
  CEF_PROXYTYPE_AUTODETECT    = 1;
  CEF_PROXYTYPE_SYSTEM        = 2;
  CEF_PROXYTYPE_FIXED_SERVERS = 3;
  CEF_PROXYTYPE_PAC_SCRIPT    = 4;

  // Used in the severity parameter in the 'cef_log' function, also known as 'CefLog' in CEF4Delphi.
  CEF_LOG_SEVERITY_INFO    = 0;
  CEF_LOG_SEVERITY_WARNING = 1;
  CEF_LOG_SEVERITY_ERROR   = 2;

  CEF_MAX_CONNECTIONS_PER_PROXY_DEFAULT_VALUE = 32;
  CEF_MAX_CONNECTIONS_PER_PROXY_MIN_VALUE     = 7;
  CEF_MAX_CONNECTIONS_PER_PROXY_MAX_VALUE     = 99;

  CEF_COOKIE_PREF_DEFAULT = 0;
  CEF_COOKIE_PREF_ALLOW   = 1;
  CEF_COOKIE_PREF_BLOCK   = 2;

  // https://chromium.googlesource.com/chromium/src/+/refs/tags/77.0.3865.90/chrome/common/net/safe_search_util.h (YouTubeRestrictMode)
  // https://www.chromium.org/administrators/policy-list-3#ForceYouTubeRestrict
  YOUTUBE_RESTRICT_OFF      = 0;
  YOUTUBE_RESTRICT_MODERATE = 1;
  YOUTUBE_RESTRICT_STRICT   = 2;

  ZOOM_STEP_25  = 0;
  ZOOM_STEP_33  = 1;
  ZOOM_STEP_50  = 2;
  ZOOM_STEP_67  = 3;
  ZOOM_STEP_75  = 4;
  ZOOM_STEP_90  = 5;
  ZOOM_STEP_100 = 6;
  ZOOM_STEP_110 = 7;
  ZOOM_STEP_125 = 8;
  ZOOM_STEP_150 = 9;
  ZOOM_STEP_175 = 10;
  ZOOM_STEP_200 = 11;
  ZOOM_STEP_250 = 12;
  ZOOM_STEP_300 = 13;
  ZOOM_STEP_400 = 14;
  ZOOM_STEP_500 = 15;
  ZOOM_STEP_UNK = 16;
  ZOOM_STEP_MIN = ZOOM_STEP_25;
  ZOOM_STEP_MAX = ZOOM_STEP_500;
  ZOOM_STEP_DEF = ZOOM_STEP_100;

  ZOOM_PCT_DELTA = 5;

  ZoomStepValues : array[ZOOM_STEP_MIN..ZOOM_STEP_MAX] of integer = (25, 33, 50, 67, 75, 90, 100, 110, 125, 150, 175, 200, 250, 300, 400, 500);

  CEF_PREFERENCES_SAVED  = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A00;
  CEF_DOONCLOSE          = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A01;
  CEF_STARTDRAGGING      = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A02;
  CEF_AFTERCREATED       = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A03;
  CEF_PENDINGRESIZE      = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A04;
  CEF_PUMPHAVEWORK       = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A05;
  CEF_DESTROY            = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A06;
  CEF_DOONBEFORECLOSE    = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A07;
  CEF_PENDINGINVALIDATE  = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A08;
  CEF_IMERANGECHANGED    = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A09;
  CEF_SENTINEL_START     = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A0A;
  CEF_SENTINEL_DOCLOSE   = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A0B;
  CEF_BEFORECLOSE        = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A0C;

  // Lazarus and some old Delphi versions don't have these message contants
  {$IF NOT DECLARED(WM_TOUCH)}
  WM_TOUCH                 = $0240;
  {$IFEND}
  {$IF NOT DECLARED(WM_POINTERUPDATE)}
  WM_POINTERUPDATE         = $0245;
  WM_POINTERDOWN           = $0246;
  WM_POINTERUP             = $0247;
  {$IFEND}

  CEF_TIMER_MINIMUM            = $0000000A;
  CEF_TIMER_MAXIMUM            = $7FFFFFFF;
  CEF_TIMER_MAXDELAY           = 1000 div 30; // 30fps
  CEF_TIMER_DEPLETEWORK_CYCLES = 10;
  CEF_TIMER_DEPLETEWORK_DELAY  = 50;

  CEF4DELPHI_URL = 'https://github.com/salvadordf/CEF4Delphi';
  CRLF           = #13 + #10;

  // These contants are declared in the "Windows" unit but
  // some old Delphi versions don't have them.
  // We have to add "CEF_" to be compatible with C++ Builder.
  CEF_IMAGE_FILE_MACHINE_I386  = $014C;
  CEF_IMAGE_FILE_MACHINE_AMD64 = $8664;

  {$IF NOT DECLARED(USER_DEFAULT_SCREEN_DPI)}
  USER_DEFAULT_SCREEN_DPI = 96;
  {$IFEND}

  {$IFDEF LINUX}{$IFDEF FPC}{$IFDEF LCLGTK2}
  // These are the GDK constants included in the Lazarus gdkkeysyms.pp file
  GDK_VoidSymbol = $FFFFFF;
  GDK_BackSpace = $FF08;
  GDK_Tab = $FF09;
  GDK_Linefeed = $FF0A;
  GDK_Clear_Key = $FF0B;
  GDK_Return = $FF0D;
  GDK_Pause = $FF13;
  GDK_Scroll_Lock = $FF14;
  GDK_Sys_Req = $FF15;
  GDK_Escape = $FF1B;
  GDK_Delete_Key = $FFFF;
  GDK_Multi_key = $FF20;
  GDK_SingleCandidate = $FF3C;
  GDK_MultipleCandidate = $FF3D;
  GDK_PreviousCandidate = $FF3E;
  GDK_Kanji = $FF21;
  GDK_Muhenkan = $FF22;
  GDK_Henkan_Mode = $FF23;
  GDK_Henkan = $FF23;
  GDK_Romaji = $FF24;
  GDK_Hiragana = $FF25;
  GDK_Katakana = $FF26;
  GDK_Hiragana_Katakana = $FF27;
  GDK_Zenkaku = $FF28;
  GDK_Hankaku = $FF29;
  GDK_Zenkaku_Hankaku = $FF2A;
  GDK_Touroku = $FF2B;
  GDK_Massyo = $FF2C;
  GDK_Kana_Lock = $FF2D;
  GDK_Kana_Shift = $FF2E;
  GDK_Eisu_Shift = $FF2F;
  GDK_Eisu_toggle = $FF30;
  GDK_Zen_Koho = $FF3D;
  GDK_Mae_Koho = $FF3E;
  GDK_Home = $FF50;
  GDK_Left = $FF51;
  GDK_Up = $FF52;
  GDK_Right = $FF53;
  GDK_Down = $FF54;
  GDK_Prior = $FF55;
  GDK_Page_Up = $FF55;
  GDK_Next = $FF56;
  GDK_Page_Down = $FF56;
  GDK_End = $FF57;
  GDK_Begin = $FF58;
  GDK_Select = $FF60;
  GDK_Print = $FF61;
  GDK_Execute = $FF62;
  GDK_Insert = $FF63;
  GDK_Undo = $FF65;
  GDK_Redo = $FF66;
  GDK_Menu = $FF67;
  GDK_Find = $FF68;
  GDK_Cancel = $FF69;
  GDK_Help = $FF6A;
  GDK_Break = $FF6B;
  GDK_Mode_switch = $FF7E;
  GDK_script_switch = $FF7E;
  GDK_Num_Lock = $FF7F;
  GDK_KP_Space = $FF80;
  GDK_KP_Tab = $FF89;
  GDK_KP_Enter = $FF8D;
  GDK_KP_F1 = $FF91;
  GDK_KP_F2 = $FF92;
  GDK_KP_F3 = $FF93;
  GDK_KP_F4 = $FF94;
  GDK_KP_Home = $FF95;
  GDK_KP_Left = $FF96;
  GDK_KP_Up = $FF97;
  GDK_KP_Right = $FF98;
  GDK_KP_Down = $FF99;
  GDK_KP_Prior = $FF9A;
  GDK_KP_Page_Up = $FF9A;
  GDK_KP_Next = $FF9B;
  GDK_KP_Page_Down = $FF9B;
  GDK_KP_End = $FF9C;
  GDK_KP_Begin = $FF9D;
  GDK_KP_Insert = $FF9E;
  GDK_KP_Delete = $FF9F;
  GDK_KP_Equal = $FFBD;
  GDK_KP_Multiply = $FFAA;
  GDK_KP_Add = $FFAB;
  GDK_KP_Separator = $FFAC;
  GDK_KP_Subtract = $FFAD;
  GDK_KP_Decimal = $FFAE;
  GDK_KP_Divide = $FFAF;
  GDK_KP_0 = $FFB0;
  GDK_KP_1 = $FFB1;
  GDK_KP_2 = $FFB2;
  GDK_KP_3 = $FFB3;
  GDK_KP_4 = $FFB4;
  GDK_KP_5 = $FFB5;
  GDK_KP_6 = $FFB6;
  GDK_KP_7 = $FFB7;
  GDK_KP_8 = $FFB8;
  GDK_KP_9 = $FFB9;
  GDK_F1 = $FFBE;
  GDK_F2 = $FFBF;
  GDK_F3 = $FFC0;
  GDK_F4 = $FFC1;
  GDK_F5 = $FFC2;
  GDK_F6 = $FFC3;
  GDK_F7 = $FFC4;
  GDK_F8 = $FFC5;
  GDK_F9 = $FFC6;
  GDK_F10 = $FFC7;
  GDK_F11 = $FFC8;
  GDK_L1 = $FFC8;
  GDK_F12 = $FFC9;
  GDK_L2 = $FFC9;
  GDK_F13 = $FFCA;
  GDK_L3 = $FFCA;
  GDK_F14 = $FFCB;
  GDK_L4 = $FFCB;
  GDK_F15 = $FFCC;
  GDK_L5 = $FFCC;
  GDK_F16 = $FFCD;
  GDK_L6 = $FFCD;
  GDK_F17 = $FFCE;
  GDK_L7 = $FFCE;
  GDK_F18 = $FFCF;
  GDK_L8 = $FFCF;
  GDK_F19 = $FFD0;
  GDK_L9 = $FFD0;
  GDK_F20 = $FFD1;
  GDK_L10 = $FFD1;
  GDK_F21 = $FFD2;
  GDK_R1 = $FFD2;
  GDK_F22 = $FFD3;
  GDK_R2 = $FFD3;
  GDK_F23 = $FFD4;
  GDK_R3 = $FFD4;
  GDK_F24 = $FFD5;
  GDK_R4 = $FFD5;
  GDK_F25 = $FFD6;
  GDK_R5 = $FFD6;
  GDK_F26 = $FFD7;
  GDK_R6 = $FFD7;
  GDK_F27 = $FFD8;
  GDK_R7 = $FFD8;
  GDK_F28 = $FFD9;
  GDK_R8 = $FFD9;
  GDK_F29 = $FFDA;
  GDK_R9 = $FFDA;
  GDK_F30 = $FFDB;
  GDK_R10 = $FFDB;
  GDK_F31 = $FFDC;
  GDK_R11 = $FFDC;
  GDK_F32 = $FFDD;
  GDK_R12 = $FFDD;
  GDK_F33 = $FFDE;
  GDK_R13 = $FFDE;
  GDK_F34 = $FFDF;
  GDK_R14 = $FFDF;
  GDK_F35 = $FFE0;
  GDK_R15 = $FFE0;
  GDK_Shift_L = $FFE1;
  GDK_Shift_R = $FFE2;
  GDK_Control_L = $FFE3;
  GDK_Control_R = $FFE4;
  GDK_Caps_Lock = $FFE5;
  GDK_Shift_Lock = $FFE6;
  GDK_Meta_L = $FFE7;
  GDK_Meta_R = $FFE8;
  GDK_Alt_L = $FFE9;
  GDK_Alt_R = $FFEA;
  GDK_Super_L = $FFEB;
  GDK_Super_R = $FFEC;
  GDK_Hyper_L = $FFED;
  GDK_Hyper_R = $FFEE;
  GDK_ISO_Lock = $FE01;
  GDK_ISO_Level2_Latch = $FE02;
  GDK_ISO_Level3_Shift = $FE03;
  GDK_ISO_Level3_Latch = $FE04;
  GDK_ISO_Level3_Lock = $FE05;
  GDK_ISO_Group_Shift = $FF7E;
  GDK_ISO_Group_Latch = $FE06;
  GDK_ISO_Group_Lock = $FE07;
  GDK_ISO_Next_Group = $FE08;
  GDK_ISO_Next_Group_Lock = $FE09;
  GDK_ISO_Prev_Group = $FE0A;
  GDK_ISO_Prev_Group_Lock = $FE0B;
  GDK_ISO_First_Group = $FE0C;
  GDK_ISO_First_Group_Lock = $FE0D;
  GDK_ISO_Last_Group = $FE0E;
  GDK_ISO_Last_Group_Lock = $FE0F;
  GDK_ISO_Left_Tab = $FE20;
  GDK_ISO_Move_Line_Up = $FE21;
  GDK_ISO_Move_Line_Down = $FE22;
  GDK_ISO_Partial_Line_Up = $FE23;
  GDK_ISO_Partial_Line_Down = $FE24;
  GDK_ISO_Partial_Space_Left = $FE25;
  GDK_ISO_Partial_Space_Right = $FE26;
  GDK_ISO_Set_Margin_Left = $FE27;
  GDK_ISO_Set_Margin_Right = $FE28;
  GDK_ISO_Release_Margin_Left = $FE29;
  GDK_ISO_Release_Margin_Right = $FE2A;
  GDK_ISO_Release_Both_Margins = $FE2B;
  GDK_ISO_Fast_Cursor_Left = $FE2C;
  GDK_ISO_Fast_Cursor_Right = $FE2D;
  GDK_ISO_Fast_Cursor_Up = $FE2E;
  GDK_ISO_Fast_Cursor_Down = $FE2F;
  GDK_ISO_Continuous_Underline = $FE30;
  GDK_ISO_Discontinuous_Underline = $FE31;
  GDK_ISO_Emphasize = $FE32;
  GDK_ISO_Center_Object = $FE33;
  GDK_ISO_Enter = $FE34;
  GDK_dead_grave = $FE50;
  GDK_dead_acute = $FE51;
  GDK_dead_circumflex = $FE52;
  GDK_dead_tilde = $FE53;
  GDK_dead_macron = $FE54;
  GDK_dead_breve = $FE55;
  GDK_dead_abovedot = $FE56;
  GDK_dead_diaeresis = $FE57;
  GDK_dead_abovering = $FE58;
  GDK_dead_doubleacute = $FE59;
  GDK_dead_caron = $FE5A;
  GDK_dead_cedilla = $FE5B;
  GDK_dead_ogonek = $FE5C;
  GDK_dead_iota = $FE5D;
  GDK_dead_voiced_sound = $FE5E;
  GDK_dead_semivoiced_sound = $FE5F;
  GDK_dead_belowdot = $FE60;
  GDK_First_Virtual_Screen = $FED0;
  GDK_Prev_Virtual_Screen = $FED1;
  GDK_Next_Virtual_Screen = $FED2;
  GDK_Last_Virtual_Screen = $FED4;
  GDK_Terminate_Server = $FED5;
  GDK_AccessX_Enable = $FE70;
  GDK_AccessX_Feedback_Enable = $FE71;
  GDK_RepeatKeys_Enable = $FE72;
  GDK_SlowKeys_Enable = $FE73;
  GDK_BounceKeys_Enable = $FE74;
  GDK_StickyKeys_Enable = $FE75;
  GDK_MouseKeys_Enable = $FE76;
  GDK_MouseKeys_Accel_Enable = $FE77;
  GDK_Overlay1_Enable = $FE78;
  GDK_Overlay2_Enable = $FE79;
  GDK_AudibleBell_Enable = $FE7A;
  GDK_Pointer_Left = $FEE0;
  GDK_Pointer_Right = $FEE1;
  GDK_Pointer_Up = $FEE2;
  GDK_Pointer_Down = $FEE3;
  GDK_Pointer_UpLeft = $FEE4;
  GDK_Pointer_UpRight = $FEE5;
  GDK_Pointer_DownLeft = $FEE6;
  GDK_Pointer_DownRight = $FEE7;
  GDK_Pointer_Button_Dflt = $FEE8;
  GDK_Pointer_Button1 = $FEE9;
  GDK_Pointer_Button2 = $FEEA;
  GDK_Pointer_Button3 = $FEEB;
  GDK_Pointer_Button4 = $FEEC;
  GDK_Pointer_Button5 = $FEED;
  GDK_Pointer_DblClick_Dflt = $FEEE;
  GDK_Pointer_DblClick1 = $FEEF;
  GDK_Pointer_DblClick2 = $FEF0;
  GDK_Pointer_DblClick3 = $FEF1;
  GDK_Pointer_DblClick4 = $FEF2;
  GDK_Pointer_DblClick5 = $FEF3;
  GDK_Pointer_Drag_Dflt = $FEF4;
  GDK_Pointer_Drag1 = $FEF5;
  GDK_Pointer_Drag2 = $FEF6;
  GDK_Pointer_Drag3 = $FEF7;
  GDK_Pointer_Drag4 = $FEF8;
  GDK_Pointer_Drag5 = $FEFD;
  GDK_Pointer_EnableKeys = $FEF9;
  GDK_Pointer_Accelerate = $FEFA;
  GDK_Pointer_DfltBtnNext = $FEFB;
  GDK_Pointer_DfltBtnPrev = $FEFC;
  GDK_3270_Duplicate = $FD01;
  GDK_3270_FieldMark = $FD02;
  GDK_3270_Right2 = $FD03;
  GDK_3270_Left2 = $FD04;
  GDK_3270_BackTab = $FD05;
  GDK_3270_EraseEOF = $FD06;
  GDK_3270_EraseInput = $FD07;
  GDK_3270_Reset = $FD08;
  GDK_3270_Quit = $FD09;
  GDK_3270_PA1 = $FD0A;
  GDK_3270_PA2 = $FD0B;
  GDK_3270_PA3 = $FD0C;
  GDK_3270_Test = $FD0D;
  GDK_3270_Attn = $FD0E;
  GDK_3270_CursorBlink = $FD0F;
  GDK_3270_AltCursor = $FD10;
  GDK_3270_KeyClick = $FD11;
  GDK_3270_Jump = $FD12;
  GDK_3270_Ident = $FD13;
  GDK_3270_Rule = $FD14;
  GDK_3270_Copy = $FD15;
  GDK_3270_Play = $FD16;
  GDK_3270_Setup = $FD17;
  GDK_3270_Record = $FD18;
  GDK_3270_ChangeScreen = $FD19;
  GDK_3270_DeleteWord = $FD1A;
  GDK_3270_ExSelect = $FD1B;
  GDK_3270_CursorSelect = $FD1C;
  GDK_3270_PrintScreen = $FD1D;
  GDK_3270_Enter = $FD1E;
  GDK_space = $020;
  GDK_exclam = $021;
  GDK_quotedbl = $022;
  GDK_numbersign = $023;
  GDK_dollar = $024;
  GDK_percent = $025;
  GDK_ampersand = $026;
  GDK_apostrophe = $027;
  GDK_quoteright = $027;
  GDK_parenleft = $028;
  GDK_parenright = $029;
  GDK_asterisk = $02a;
  GDK_plus_key = $02b;
  GDK_comma = $02c;
  GDK_minus = $02d;
  GDK_period = $02e;
  GDK_slash = $02f;
  GDK_0 = $030;
  GDK_1 = $031;
  GDK_2 = $032;
  GDK_3 = $033;
  GDK_4 = $034;
  GDK_5 = $035;
  GDK_6 = $036;
  GDK_7 = $037;
  GDK_8 = $038;
  GDK_9 = $039;
  GDK_colon = $03a;
  GDK_semicolon = $03b;
  GDK_less = $03c;
  GDK_equal = $03d;
  GDK_greater = $03e;
  GDK_question = $03f;
  GDK_at = $040;
  GDK_Capital_A = $041;
  GDK_Capital_B = $042;
  GDK_Capital_C = $043;
  GDK_Capital_D = $044;
  GDK_Capital_E = $045;
  GDK_Capital_F = $046;
  GDK_Capital_G = $047;
  GDK_Capital_H = $048;
  GDK_Capital_I = $049;
  GDK_Capital_J = $04a;
  GDK_Capital_K = $04b;
  GDK_Capital_L = $04c;
  GDK_Capital_M = $04d;
  GDK_Capital_N = $04e;
  GDK_Capital_O = $04f;
  GDK_Capital_P = $050;
  GDK_Capital_Q = $051;
  GDK_Capital_R = $052;
  GDK_Capital_S = $053;
  GDK_Capital_T = $054;
  GDK_Capital_U = $055;
  GDK_Capital_V = $056;
  GDK_Capital_W = $057;
  GDK_Capital_X = $058;
  GDK_Capital_Y = $059;
  GDK_Capital_Z = $05a;
  GDK_bracketleft = $05b;
  GDK_backslash = $05c;
  GDK_bracketright = $05d;
  GDK_asciicircum = $05e;
  GDK_underscore = $05f;
  GDK_grave = $060;
  GDK_quoteleft = $060;
  GDK_a = $061;
  GDK_b = $062;
  GDK_c = $063;
  GDK_d = $064;
  GDK_e = $065;
  GDK_f = $066;
  GDK_g = $067;
  GDK_h = $068;
  GDK_i = $069;
  GDK_j = $06a;
  GDK_k = $06b;
  GDK_l = $06c;
  GDK_m = $06d;
  GDK_n = $06e;
  GDK_o = $06f;
  GDK_p = $070;
  GDK_q = $071;
  GDK_r = $072;
  GDK_s = $073;
  GDK_t = $074;
  GDK_u = $075;
  GDK_v = $076;
  GDK_w = $077;
  GDK_x = $078;
  GDK_y = $079;
  GDK_z = $07a;
  GDK_braceleft = $07b;
  GDK_bar = $07c;
  GDK_braceright = $07d;
  GDK_asciitilde = $07e;
  GDK_nobreakspace = $0a0;
  GDK_exclamdown = $0a1;
  GDK_cent = $0a2;
  GDK_sterling = $0a3;
  GDK_currency = $0a4;
  GDK_yen = $0a5;
  GDK_brokenbar = $0a6;
  GDK_section = $0a7;
  GDK_diaeresis = $0a8;
  GDK_copyright = $0a9;
  GDK_ordfeminine = $0aa;
  GDK_guillemotleft = $0ab;
  GDK_notsign = $0ac;
  GDK_hyphen = $0ad;
  GDK_registered = $0ae;
  GDK_macron = $0af;
  GDK_degree = $0b0;
  GDK_plusminus = $0b1;
  GDK_twosuperior = $0b2;
  GDK_threesuperior = $0b3;
  GDK_acute = $0b4;
  GDK_mu = $0b5;
  GDK_paragraph = $0b6;
  GDK_periodcentered = $0b7;
  GDK_cedilla = $0b8;
  GDK_onesuperior = $0b9;
  GDK_masculine = $0ba;
  GDK_guillemotright = $0bb;
  GDK_onequarter = $0bc;
  GDK_onehalf = $0bd;
  GDK_threequarters = $0be;
  GDK_questiondown = $0bf;
  GDK_Capital_Agrave = $0c0;
  GDK_Capital_Aacute = $0c1;
  GDK_Capital_Acircumflex = $0c2;
  GDK_Capital_Atilde = $0c3;
  GDK_Capital_Adiaeresis = $0c4;
  GDK_Capital_Aring = $0c5;
  GDK_Capital_AE = $0c6;
  GDK_Capital_Ccedilla = $0c7;
  GDK_Capital_Egrave = $0c8;
  GDK_Capital_Eacute = $0c9;
  GDK_Capital_Ecircumflex = $0ca;
  GDK_Capital_Ediaeresis = $0cb;
  GDK_Capital_Igrave = $0cc;
  GDK_Capital_Iacute = $0cd;
  GDK_Capital_Icircumflex = $0ce;
  GDK_Capital_Idiaeresis = $0cf;
  GDK_Capital_ETH = $0d0;
  GDK_Capital_Ntilde = $0d1;
  GDK_Capital_Ograve = $0d2;
  GDK_Capital_Oacute = $0d3;
  GDK_Capital_Ocircumflex = $0d4;
  GDK_Capital_Otilde = $0d5;
  GDK_Capital_Odiaeresis = $0d6;
  GDK_Capital_multiply = $0d7;
  GDK_Capital_Ooblique = $0d8;
  GDK_Capital_Ugrave = $0d9;
  GDK_Capital_Uacute = $0da;
  GDK_Capital_Ucircumflex = $0db;
  GDK_Capital_Udiaeresis = $0dc;
  GDK_Capital_Yacute = $0dd;
  GDK_Capital_THORN = $0de;
  GDK_Thorn = $0de;
  GDK_ssharp = $0df;
  GDK_agrave = $0e0;
  GDK_aacute = $0e1;
  GDK_acircumflex = $0e2;
  GDK_atilde = $0e3;
  GDK_adiaeresis = $0e4;
  GDK_aring = $0e5;
  GDK_ae = $0e6;
  GDK_ccedilla = $0e7;
  GDK_egrave = $0e8;
  GDK_eacute = $0e9;
  GDK_ecircumflex = $0ea;
  GDK_ediaeresis = $0eb;
  GDK_igrave = $0ec;
  GDK_iacute = $0ed;
  GDK_icircumflex = $0ee;
  GDK_idiaeresis = $0ef;
  GDK_eth = $0f0;
  GDK_ntilde = $0f1;
  GDK_ograve = $0f2;
  GDK_oacute = $0f3;
  GDK_ocircumflex = $0f4;
  GDK_otilde = $0f5;
  GDK_odiaeresis = $0f6;
  GDK_division = $0f7;
  GDK_oslash = $0f8;
  GDK_ugrave = $0f9;
  GDK_uacute = $0fa;
  GDK_ucircumflex = $0fb;
  GDK_udiaeresis = $0fc;
  GDK_yacute = $0fd;
  GDK_small_thorn = $0fe;
  GDK_ydiaeresis = $0ff;
  GDK_Capital_Aogonek = $1a1;
  GDK_Capital_breve = $1a2;
  GDK_Capital_Lstroke = $1a3;
  GDK_Capital_Lcaron = $1a5;
  GDK_Capital_Sacute = $1a6;
  GDK_Capital_Scaron = $1a9;
  GDK_Capital_Scedilla = $1aa;
  GDK_Capital_Tcaron = $1ab;
  GDK_Capital_Zacute = $1ac;
  GDK_Capital_Zcaron = $1ae;
  GDK_Capital_Zabovedot = $1af;
  GDK_aogonek = $1b1;
  GDK_ogonek = $1b2;
  GDK_lstroke = $1b3;
  GDK_lcaron = $1b5;
  GDK_sacute = $1b6;
  GDK_caron = $1b7;
  GDK_scaron = $1b9;
  GDK_scedilla = $1ba;
  GDK_tcaron = $1bb;
  GDK_zacute = $1bc;
  GDK_doubleacute = $1bd;
  GDK_zcaron = $1be;
  GDK_zabovedot = $1bf;
  GDK_Capital_Racute = $1c0;
  GDK_Capital_Abreve = $1c3;
  GDK_Capital_Lacute = $1c5;
  GDK_Capital_Cacute = $1c6;
  GDK_Capital_Ccaron = $1c8;
  GDK_Capital_Eogonek = $1ca;
  GDK_Capital_Ecaron = $1cc;
  GDK_Capital_Dcaron = $1cf;
  GDK_Capital_Dstroke = $1d0;
  GDK_Capital_Nacute = $1d1;
  GDK_Capital_Ncaron = $1d2;
  GDK_Capital_Odoubleacute = $1d5;
  GDK_Capital_Rcaron = $1d8;
  GDK_Capital_Uring = $1d9;
  GDK_Capital_Udoubleacute = $1db;
  GDK_Capital_Tcedilla = $1de;
  GDK_racute = $1e0;
  GDK_abreve = $1e3;
  GDK_lacute = $1e5;
  GDK_cacute = $1e6;
  GDK_ccaron = $1e8;
  GDK_eogonek = $1ea;
  GDK_ecaron = $1ec;
  GDK_dcaron = $1ef;
  GDK_dstroke = $1f0;
  GDK_nacute = $1f1;
  GDK_ncaron = $1f2;
  GDK_odoubleacute = $1f5;
  GDK_udoubleacute = $1fb;
  GDK_rcaron = $1f8;
  GDK_uring = $1f9;
  GDK_tcedilla = $1fe;
  GDK_abovedot = $1ff;
  GDK_Capital_Hstroke = $2a1;
  GDK_Capital_Hcircumflex = $2a6;
  GDK_Capital_Iabovedot = $2a9;
  GDK_Capital_Gbreve = $2ab;
  GDK_Capital_Jcircumflex = $2ac;
  GDK_hstroke = $2b1;
  GDK_hcircumflex = $2b6;
  GDK_idotless = $2b9;
  GDK_gbreve = $2bb;
  GDK_jcircumflex = $2bc;
  GDK_Capital_Cabovedot = $2c5;
  GDK_Capital_Ccircumflex = $2c6;
  GDK_Capital_Gabovedot = $2d5;
  GDK_Capital_Gcircumflex = $2d8;
  GDK_Capital_Ubreve = $2dd;
  GDK_Capital_Scircumflex = $2de;
  GDK_cabovedot = $2e5;
  GDK_ccircumflex = $2e6;
  GDK_gabovedot = $2f5;
  GDK_gcircumflex = $2f8;
  GDK_ubreve = $2fd;
  GDK_scircumflex = $2fe;
  GDK_kra = $3a2;
  GDK_kappa = $3a2;
  GDK_Capital_Rcedilla = $3a3;
  GDK_Capital_Itilde = $3a5;
  GDK_Capital_Lcedilla = $3a6;
  GDK_Capital_Emacron = $3aa;
  GDK_Capital_Gcedilla = $3ab;
  GDK_Capital_Tslash = $3ac;
  GDK_rcedilla = $3b3;
  GDK_itilde = $3b5;
  GDK_lcedilla = $3b6;
  GDK_emacron = $3ba;
  GDK_gcedilla = $3bb;
  GDK_tslash = $3bc;
  GDK_Capital_ENG = $3bd;
  GDK_eng = $3bf;
  GDK_Capital_Amacron = $3c0;
  GDK_Capital_Iogonek = $3c7;
  GDK_Capital_Eabovedot = $3cc;
  GDK_Capital_Imacron = $3cf;
  GDK_Capital_Ncedilla = $3d1;
  GDK_Capital_Omacron = $3d2;
  GDK_Capital_Kcedilla = $3d3;
  GDK_Capital_Uogonek = $3d9;
  GDK_Capital_Utilde = $3dd;
  GDK_Capital_Umacron = $3de;
  GDK_amacron = $3e0;
  GDK_iogonek = $3e7;
  GDK_eabovedot = $3ec;
  GDK_imacron = $3ef;
  GDK_ncedilla = $3f1;
  GDK_omacron = $3f2;
  GDK_kcedilla = $3f3;
  GDK_uogonek = $3f9;
  GDK_utilde = $3fd;
  GDK_umacron = $3fe;
  GDK_overline = $47e;
  GDK_kana_fullstop = $4a1;
  GDK_kana_openingbracket = $4a2;
  GDK_kana_closingbracket = $4a3;
  GDK_kana_comma = $4a4;
  GDK_kana_conjunctive = $4a5;
  GDK_kana_middledot = $4a5;
  GDK_kana_WO = $4a6;
  GDK_kana_a = $4a7;
  GDK_kana_i = $4a8;
  GDK_kana_u = $4a9;
  GDK_kana_e = $4aa;
  GDK_kana_o = $4ab;
  GDK_kana_ya = $4ac;
  GDK_kana_yu = $4ad;
  GDK_kana_yo = $4ae;
  GDK_kana_tsu = $4af;
  GDK_kana_tu = $4af;
  GDK_prolongedsound = $4b0;
  GDK_kana_Capital__A = $4b1;
  GDK_kana_Capital__I = $4b2;
  GDK_kana_Capital__U = $4b3;
  GDK_kana_Capital__E = $4b4;
  GDK_kana_Capital__O = $4b5;
  GDK_kana_Capital__KA = $4b6;
  GDK_kana_Capital__KI = $4b7;
  GDK_kana_Capital__KU = $4b8;
  GDK_kana_Capital__KE = $4b9;
  GDK_kana_Capital__KO = $4ba;
  GDK_kana_Capital__SA = $4bb;
  GDK_kana_Capital__SHI = $4bc;
  GDK_kana_Capital__SU = $4bd;
  GDK_kana_Capital__SE = $4be;
  GDK_kana_Capital__SO = $4bf;
  GDK_kana_Capital__TA = $4c0;
  GDK_kana_Capital__CHI = $4c1;
  GDK_kana_Capital__TI = $4c1;
  GDK_kana_Capital__TSU = $4c2;
  GDK_kana_Capital__TU = $4c2;
  GDK_kana_Capital__TE = $4c3;
  GDK_kana_Capital__TO = $4c4;
  GDK_kana_Capital__NA = $4c5;
  GDK_kana_Capital__NI = $4c6;
  GDK_kana_Capital__NU = $4c7;
  GDK_kana_Capital__NE = $4c8;
  GDK_kana_Capital__NO = $4c9;
  GDK_kana_Capital__HA = $4ca;
  GDK_kana_Capital__HI = $4cb;
  GDK_kana_Capital__FU = $4cc;
  GDK_kana_Capital__HU = $4cc;
  GDK_kana_Capital__HE = $4cd;
  GDK_kana_Capital__HO = $4ce;
  GDK_kana_Capital__MA = $4cf;
  GDK_kana_Capital__MI = $4d0;
  GDK_kana_Capital__MU = $4d1;
  GDK_kana_Capital__ME = $4d2;
  GDK_kana_Capital__MO = $4d3;
  GDK_kana_Capital__YA = $4d4;
  GDK_kana_Capital__YU = $4d5;
  GDK_kana_Capital__YO = $4d6;
  GDK_kana_Capital__RA = $4d7;
  GDK_kana_Capital__RI = $4d8;
  GDK_kana_Capital__RU = $4d9;
  GDK_kana_Capital__RE = $4da;
  GDK_kana_Capital__RO = $4db;
  GDK_kana_Capital__WA = $4dc;
  GDK_kana_Capital__N = $4dd;
  GDK_voicedsound = $4de;
  GDK_semivoicedsound = $4df;
  GDK_kana_switch = $FF7E;
  GDK_arabic_comma = $5ac;
  GDK_arabic_semicolon = $5bb;
  GDK_arabic_question_mark = $5bf;
  GDK_arabic_hamza = $5c1;
  GDK_arabic_maddaonalef = $5c2;
  GDK_arabic_hamzaonalef = $5c3;
  GDK_arabic_hamzaonwaw = $5c4;
  GDK_arabic_hamzaunderalef = $5c5;
  GDK_arabic_hamzaonyeh = $5c6;
  GDK_arabic_alef = $5c7;
  GDK_arabic_beh = $5c8;
  GDK_arabic_tehmarbuta = $5c9;
  GDK_arabic_teh = $5ca;
  GDK_arabic_theh = $5cb;
  GDK_arabic_jeem = $5cc;
  GDK_arabic_hah = $5cd;
  GDK_arabic_khah = $5ce;
  GDK_arabic_dal = $5cf;
  GDK_arabic_thal = $5d0;
  GDK_arabic_ra = $5d1;
  GDK_arabic_zain = $5d2;
  GDK_arabic_seen = $5d3;
  GDK_arabic_sheen = $5d4;
  GDK_arabic_sad = $5d5;
  GDK_arabic_dad = $5d6;
  GDK_arabic_tah = $5d7;
  GDK_arabic_zah = $5d8;
  GDK_arabic_ain = $5d9;
  GDK_arabic_ghain = $5da;
  GDK_arabic_tatweel = $5e0;
  GDK_arabic_feh = $5e1;
  GDK_arabic_qaf = $5e2;
  GDK_arabic_kaf = $5e3;
  GDK_arabic_lam = $5e4;
  GDK_arabic_meem = $5e5;
  GDK_arabic_noon = $5e6;
  GDK_arabic_ha = $5e7;
  GDK_arabic_heh = $5e7;
  GDK_arabic_waw = $5e8;
  GDK_arabic_alefmaksura = $5e9;
  GDK_arabic_yeh = $5ea;
  GDK_arabic_fathatan = $5eb;
  GDK_arabic_dammatan = $5ec;
  GDK_arabic_kasratan = $5ed;
  GDK_arabic_fatha = $5ee;
  GDK_arabic_damma = $5ef;
  GDK_arabic_kasra = $5f0;
  GDK_arabic_shadda = $5f1;
  GDK_arabic_sukun = $5f2;
  GDK_arabic_switch = $FF7E;
  GDK_serbian_dje = $6a1;
  GDK_macedonia_gje = $6a2;
  GDK_cyrillic_io = $6a3;
  GDK_ukrainian_ie = $6a4;
  GDK_ukrainian_je = $6a4;
  GDK_macedonia_dse = $6a5;
  GDK_ukrainian_i = $6a6;
  GDK_ukrainian_yi = $6a7;
  GDK_cyrillic_je = $6a8;
  GDK_serbian_je = $6a8;
  GDK_cyrillic_lje = $6a9;
  GDK_serbian_lje = $6a9;
  GDK_cyrillic_nje = $6aa;
  GDK_serbian_nje = $6aa;
  GDK_serbian_tshe = $6ab;
  GDK_macedonia_kje = $6ac;
  GDK_byelorussian_shortu = $6ae;
  GDK_cyrillic_dzhe = $6af;
  GDK_serbian_dze = $6af;
  GDK_numerosign = $6b0;
  GDK_serbian_Capital_DJE = $6b1;
  GDK_macedonia_Capital_GJE = $6b2;
  GDK_cyrillic_Capital_IO = $6b3;
  GDK_ukrainian_Capital_IE = $6b4;
  GDK_ukrainian_Capital_JE = $6b4;
  GDK_macedonia_Capital_DSE = $6b5;
  GDK_ukrainian_Capital_I = $6b6;
  GDK_ukrainian_Capital_YI = $6b7;
  GDK_cyrillic_Capital_JE = $6b8;
  GDK_serbian_Capital_JE = $6b8;
  GDK_cyrillic_Capital_LJE = $6b9;
  GDK_serbian_Capital_LJE = $6b9;
  GDK_cyrillic_Capital_NJE = $6ba;
  GDK_serbian_Capital_NJE = $6ba;
  GDK_serbian_Capital_TSHE = $6bb;
  GDK_macedonia_Capital_KJE = $6bc;
  GDK_byelorussian_Capital_SHORTU = $6be;
  GDK_cyrillic_Capital_DZHE = $6bf;
  GDK_serbian_Capital_DZE = $6bf;
  GDK_cyrillic_yu = $6c0;
  GDK_cyrillic_a = $6c1;
  GDK_cyrillic_be = $6c2;
  GDK_cyrillic_tse = $6c3;
  GDK_cyrillic_de = $6c4;
  GDK_cyrillic_ie = $6c5;
  GDK_cyrillic_ef = $6c6;
  GDK_cyrillic_ghe = $6c7;
  GDK_cyrillic_ha = $6c8;
  GDK_cyrillic_i = $6c9;
  GDK_cyrillic_shorti = $6ca;
  GDK_cyrillic_ka = $6cb;
  GDK_cyrillic_el = $6cc;
  GDK_cyrillic_em = $6cd;
  GDK_cyrillic_en = $6ce;
  GDK_cyrillic_o = $6cf;
  GDK_cyrillic_pe = $6d0;
  GDK_cyrillic_ya = $6d1;
  GDK_cyrillic_er = $6d2;
  GDK_cyrillic_es = $6d3;
  GDK_cyrillic_te = $6d4;
  GDK_cyrillic_u = $6d5;
  GDK_cyrillic_zhe = $6d6;
  GDK_cyrillic_ve = $6d7;
  GDK_cyrillic_softsign = $6d8;
  GDK_cyrillic_yeru = $6d9;
  GDK_cyrillic_ze = $6da;
  GDK_cyrillic_sha = $6db;
  GDK_cyrillic_e = $6dc;
  GDK_cyrillic_shcha = $6dd;
  GDK_cyrillic_che = $6de;
  GDK_cyrillic_hardsign = $6df;
  GDK_cyrillic_Capital_YU = $6e0;
  GDK_cyrillic_Capital_A = $6e1;
  GDK_cyrillic_Capital_BE = $6e2;
  GDK_cyrillic_Capital_TSE = $6e3;
  GDK_cyrillic_Capital_DE = $6e4;
  GDK_cyrillic_Capital_IE = $6e5;
  GDK_cyrillic_Capital_EF = $6e6;
  GDK_cyrillic_Capital_GHE = $6e7;
  GDK_cyrillic_Capital_HA = $6e8;
  GDK_cyrillic_Capital_I = $6e9;
  GDK_cyrillic_Capital_SHORTI = $6ea;
  GDK_cyrillic_Capital_KA = $6eb;
  GDK_cyrillic_Capital_EL = $6ec;
  GDK_cyrillic_Capital_EM = $6ed;
  GDK_cyrillic_Capital_EN = $6ee;
  GDK_cyrillic_Capital_O = $6ef;
  GDK_cyrillic_Capital_PE = $6f0;
  GDK_cyrillic_Capital_YA = $6f1;
  GDK_cyrillic_Capital_ER = $6f2;
  GDK_cyrillic_Capital_ES = $6f3;
  GDK_cyrillic_Capital_TE = $6f4;
  GDK_cyrillic_Capital_U = $6f5;
  GDK_cyrillic_Capital_ZHE = $6f6;
  GDK_cyrillic_Capital_VE = $6f7;
  GDK_cyrillic_Capital_SOFTSIGN = $6f8;
  GDK_cyrillic_Capital_YERU = $6f9;
  GDK_cyrillic_Capital_ZE = $6fa;
  GDK_cyrillic_Capital_SHA = $6fb;
  GDK_cyrillic_Capital_E = $6fc;
  GDK_cyrillic_Capital_SHCHA = $6fd;
  GDK_cyrillic_Capital_CHE = $6fe;
  GDK_cyrillic_Capital_HARDSIGN = $6ff;
  GDK_greek_Capital_ALPHAaccent = $7a1;
  GDK_greek_Capital_EPSILONaccent = $7a2;
  GDK_greek_Capital_ETAaccent = $7a3;
  GDK_greek_Capital_IOTAaccent = $7a4;
  GDK_greek_Capital_IOTAdiaeresis = $7a5;
  GDK_greek_Capital_OMICRONaccent = $7a7;
  GDK_greek_Capital_UPSILONaccent = $7a8;
  GDK_greek_Capital_UPSILONdieresis = $7a9;
  GDK_greek_Capital_OMEGAaccent = $7ab;
  GDK_greek_accentdieresis = $7ae;
  GDK_greek_horizbar = $7af;
  GDK_greek_alphaaccent = $7b1;
  GDK_greek_epsilonaccent = $7b2;
  GDK_greek_etaaccent = $7b3;
  GDK_greek_iotaaccent = $7b4;
  GDK_greek_iotadieresis = $7b5;
  GDK_greek_iotaaccentdieresis = $7b6;
  GDK_greek_omicronaccent = $7b7;
  GDK_greek_upsilonaccent = $7b8;
  GDK_greek_upsilondieresis = $7b9;
  GDK_greek_upsilonaccentdieresis = $7ba;
  GDK_greek_omegaaccent = $7bb;
  GDK_greek_Capital_ALPHA = $7c1;
  GDK_greek_Capital_BETA = $7c2;
  GDK_greek_Capital_GAMMA = $7c3;
  GDK_greek_Capital_DELTA = $7c4;
  GDK_greek_Capital_EPSILON = $7c5;
  GDK_greek_Capital_ZETA = $7c6;
  GDK_greek_Capital_ETA = $7c7;
  GDK_greek_Capital_THETA = $7c8;
  GDK_greek_Capital_IOTA = $7c9;
  GDK_greek_Capital_KAPPA = $7ca;
  GDK_greek_Capital_LAMDA = $7cb;
  GDK_greek_Capital_LAMBDA = $7cb;
  GDK_greek_Capital_MU = $7cc;
  GDK_greek_Capital_NU = $7cd;
  GDK_greek_Capital_XI = $7ce;
  GDK_greek_Capital_OMICRON = $7cf;
  GDK_greek_Capital_PI = $7d0;
  GDK_greek_Capital_RHO = $7d1;
  GDK_greek_Capital_SIGMA = $7d2;
  GDK_greek_Capital_TAU = $7d4;
  GDK_greek_Capital_UPSILON = $7d5;
  GDK_greek_Capital_PHI = $7d6;
  GDK_greek_Capital_CHI = $7d7;
  GDK_greek_Capital_PSI = $7d8;
  GDK_greek_Capital_OMEGA = $7d9;
  GDK_greek_alpha = $7e1;
  GDK_greek_beta = $7e2;
  GDK_greek_gamma = $7e3;
  GDK_greek_delta = $7e4;
  GDK_greek_epsilon = $7e5;
  GDK_greek_zeta = $7e6;
  GDK_greek_eta = $7e7;
  GDK_greek_theta = $7e8;
  GDK_greek_iota = $7e9;
  GDK_greek_kappa = $7ea;
  GDK_greek_lamda = $7eb;
  GDK_greek_lambda = $7eb;
  GDK_greek_mu = $7ec;
  GDK_greek_nu = $7ed;
  GDK_greek_xi = $7ee;
  GDK_greek_omicron = $7ef;
  GDK_greek_pi = $7f0;
  GDK_greek_rho = $7f1;
  GDK_greek_sigma = $7f2;
  GDK_greek_finalsmallsigma = $7f3;
  GDK_greek_tau = $7f4;
  GDK_greek_upsilon = $7f5;
  GDK_greek_phi = $7f6;
  GDK_greek_chi = $7f7;
  GDK_greek_psi = $7f8;
  GDK_greek_omega = $7f9;
  GDK_greek_switch = $FF7E;
  GDK_leftradical = $8a1;
  GDK_topleftradical = $8a2;
  GDK_horizconnector = $8a3;
  GDK_topintegral = $8a4;
  GDK_botintegral = $8a5;
  GDK_vertconnector = $8a6;
  GDK_topleftsqbracket = $8a7;
  GDK_botleftsqbracket = $8a8;
  GDK_toprightsqbracket = $8a9;
  GDK_botrightsqbracket = $8aa;
  GDK_topleftparens = $8ab;
  GDK_botleftparens = $8ac;
  GDK_toprightparens = $8ad;
  GDK_botrightparens = $8ae;
  GDK_leftmiddlecurlybrace = $8af;
  GDK_rightmiddlecurlybrace = $8b0;
  GDK_topleftsummation = $8b1;
  GDK_botleftsummation = $8b2;
  GDK_topvertsummationconnector = $8b3;
  GDK_botvertsummationconnector = $8b4;
  GDK_toprightsummation = $8b5;
  GDK_botrightsummation = $8b6;
  GDK_rightmiddlesummation = $8b7;
  GDK_lessthanequal = $8bc;
  GDK_notequal = $8bd;
  GDK_greaterthanequal = $8be;
  GDK_integral = $8bf;
  GDK_therefore = $8c0;
  GDK_variation = $8c1;
  GDK_infinity = $8c2;
  GDK_nabla = $8c5;
  GDK_approximate = $8c8;
  GDK_similarequal = $8c9;
  GDK_ifonlyif = $8cd;
  GDK_implies = $8ce;
  GDK_identical = $8cf;
  GDK_radical = $8d6;
  GDK_includedin = $8da;
  GDK_includes = $8db;
  GDK_intersection = $8dc;
  GDK_union = $8dd;
  GDK_logicaland = $8de;
  GDK_logicalor = $8df;
  GDK_partialderivative = $8ef;
  GDK_function = $8f6;
  GDK_leftarrow = $8fb;
  GDK_uparrow = $8fc;
  GDK_rightarrow = $8fd;
  GDK_downarrow = $8fe;
  GDK_blank = $9df;
  GDK_soliddiamond = $9e0;
  GDK_checkerboard = $9e1;
  GDK_ht = $9e2;
  GDK_ff = $9e3;
  GDK_cr = $9e4;
  GDK_lf = $9e5;
  GDK_nl = $9e8;
  GDK_vt = $9e9;
  GDK_lowrightcorner = $9ea;
  GDK_uprightcorner = $9eb;
  GDK_upleftcorner = $9ec;
  GDK_lowleftcorner = $9ed;
  GDK_crossinglines = $9ee;
  GDK_horizlinescan1 = $9ef;
  GDK_horizlinescan3 = $9f0;
  GDK_horizlinescan5 = $9f1;
  GDK_horizlinescan7 = $9f2;
  GDK_horizlinescan9 = $9f3;
  GDK_leftt = $9f4;
  GDK_rightt = $9f5;
  GDK_bott = $9f6;
  GDK_topt = $9f7;
  GDK_vertbar = $9f8;
  GDK_emspace = $aa1;
  GDK_enspace = $aa2;
  GDK_em3space = $aa3;
  GDK_em4space = $aa4;
  GDK_digitspace = $aa5;
  GDK_punctspace = $aa6;
  GDK_thinspace = $aa7;
  GDK_hairspace = $aa8;
  GDK_emdash = $aa9;
  GDK_endash = $aaa;
  GDK_signifblank = $aac;
  GDK_ellipsis = $aae;
  GDK_doubbaselinedot = $aaf;
  GDK_onethird = $ab0;
  GDK_twothirds = $ab1;
  GDK_onefifth = $ab2;
  GDK_twofifths = $ab3;
  GDK_threefifths = $ab4;
  GDK_fourfifths = $ab5;
  GDK_onesixth = $ab6;
  GDK_fivesixths = $ab7;
  GDK_careof = $ab8;
  GDK_figdash = $abb;
  GDK_leftanglebracket = $abc;
  GDK_decimalpoint = $abd;
  GDK_rightanglebracket = $abe;
  GDK_marker = $abf;
  GDK_oneeighth = $ac3;
  GDK_threeeighths = $ac4;
  GDK_fiveeighths = $ac5;
  GDK_seveneighths = $ac6;
  GDK_trademark = $ac9;
  GDK_signaturemark = $aca;
  GDK_trademarkincircle = $acb;
  GDK_leftopentriangle = $acc;
  GDK_rightopentriangle = $acd;
  GDK_emopencircle = $ace;
  GDK_emopenrectangle = $acf;
  GDK_leftsinglequotemark = $ad0;
  GDK_rightsinglequotemark = $ad1;
  GDK_leftdoublequotemark = $ad2;
  GDK_rightdoublequotemark = $ad3;
  GDK_prescription = $ad4;
  GDK_minutes = $ad6;
  GDK_seconds = $ad7;
  GDK_latincross = $ad9;
  GDK_hexagram = $ada;
  GDK_filledrectbullet = $adb;
  GDK_filledlefttribullet = $adc;
  GDK_filledrighttribullet = $add;
  GDK_emfilledcircle = $ade;
  GDK_emfilledrect = $adf;
  GDK_enopencircbullet = $ae0;
  GDK_enopensquarebullet = $ae1;
  GDK_openrectbullet = $ae2;
  GDK_opentribulletup = $ae3;
  GDK_opentribulletdown = $ae4;
  GDK_openstar = $ae5;
  GDK_enfilledcircbullet = $ae6;
  GDK_enfilledsqbullet = $ae7;
  GDK_filledtribulletup = $ae8;
  GDK_filledtribulletdown = $ae9;
  GDK_leftpointer = $aea;
  GDK_rightpointer = $aeb;
  GDK_club = $aec;
  GDK_diamond = $aed;
  GDK_heart_key = $aee;
  GDK_maltesecross = $af0;
  GDK_dagger = $af1;
  GDK_doubledagger = $af2;
  GDK_checkmark = $af3;
  GDK_ballotcross = $af4;
  GDK_musicalsharp = $af5;
  GDK_musicalflat = $af6;
  GDK_malesymbol = $af7;
  GDK_femalesymbol = $af8;
  GDK_telephone = $af9;
  GDK_telephonerecorder = $afa;
  GDK_phonographcopyright = $afb;
  GDK_caret = $afc;
  GDK_singlelowquotemark = $afd;
  GDK_doublelowquotemark = $afe;
  GDK_cursor = $aff;
  GDK_leftcaret = $ba3;
  GDK_rightcaret = $ba6;
  GDK_downcaret = $ba8;
  GDK_upcaret = $ba9;
  GDK_overbar = $bc0;
  GDK_downtack = $bc2;
  GDK_upshoe = $bc3;
  GDK_downstile = $bc4;
  GDK_underbar = $bc6;
  GDK_jot = $bca;
  GDK_quad = $bcc;
  GDK_uptack = $bce;
  GDK_circle_key = $bcf;
  GDK_upstile = $bd3;
  GDK_downshoe = $bd6;
  GDK_rightshoe = $bd8;
  GDK_leftshoe = $bda;
  GDK_lefttack = $bdc;
  GDK_righttack = $bfc;
  GDK_hebrew_doublelowline = $cdf;
  GDK_hebrew_aleph = $ce0;
  GDK_hebrew_bet = $ce1;
  GDK_hebrew_beth = $ce1;
  GDK_hebrew_gimel = $ce2;
  GDK_hebrew_gimmel = $ce2;
  GDK_hebrew_dalet = $ce3;
  GDK_hebrew_daleth = $ce3;
  GDK_hebrew_he = $ce4;
  GDK_hebrew_waw = $ce5;
  GDK_hebrew_zain = $ce6;
  GDK_hebrew_zayin = $ce6;
  GDK_hebrew_chet = $ce7;
  GDK_hebrew_het = $ce7;
  GDK_hebrew_tet = $ce8;
  GDK_hebrew_teth = $ce8;
  GDK_hebrew_yod = $ce9;
  GDK_hebrew_finalkaph = $cea;
  GDK_hebrew_kaph = $ceb;
  GDK_hebrew_lamed = $cec;
  GDK_hebrew_finalmem = $ced;
  GDK_hebrew_mem = $cee;
  GDK_hebrew_finalnun = $cef;
  GDK_hebrew_nun = $cf0;
  GDK_hebrew_samech = $cf1;
  GDK_hebrew_samekh = $cf1;
  GDK_hebrew_ayin = $cf2;
  GDK_hebrew_finalpe = $cf3;
  GDK_hebrew_pe = $cf4;
  GDK_hebrew_finalzade = $cf5;
  GDK_hebrew_finalzadi = $cf5;
  GDK_hebrew_zade = $cf6;
  GDK_hebrew_zadi = $cf6;
  GDK_hebrew_qoph = $cf7;
  GDK_hebrew_kuf = $cf7;
  GDK_hebrew_resh = $cf8;
  GDK_hebrew_shin = $cf9;
  GDK_hebrew_taw = $cfa;
  GDK_hebrew_taf = $cfa;
  GDK_Hebrew_switch = $FF7E;
  GDK_Thai_kokai = $da1;
  GDK_Thai_khokhai = $da2;
  GDK_Thai_khokhuat = $da3;
  GDK_Thai_khokhwai = $da4;
  GDK_Thai_khokhon = $da5;
  GDK_Thai_khorakhang = $da6;
  GDK_Thai_ngongu = $da7;
  GDK_Thai_chochan = $da8;
  GDK_Thai_choching = $da9;
  GDK_Thai_chochang = $daa;
  GDK_Thai_soso = $dab;
  GDK_Thai_chochoe = $dac;
  GDK_Thai_yoying = $dad;
  GDK_Thai_dochada = $dae;
  GDK_Thai_topatak = $daf;
  GDK_Thai_thothan = $db0;
  GDK_Thai_thonangmontho = $db1;
  GDK_Thai_thophuthao = $db2;
  GDK_Thai_nonen = $db3;
  GDK_Thai_dodek = $db4;
  GDK_Thai_totao = $db5;
  GDK_Thai_thothung = $db6;
  GDK_Thai_thothahan = $db7;
  GDK_Thai_thothong = $db8;
  GDK_Thai_nonu = $db9;
  GDK_Thai_bobaimai = $dba;
  GDK_Thai_popla = $dbb;
  GDK_Thai_phophung = $dbc;
  GDK_Thai_fofa = $dbd;
  GDK_Thai_phophan = $dbe;
  GDK_Thai_fofan = $dbf;
  GDK_Thai_phosamphao = $dc0;
  GDK_Thai_moma = $dc1;
  GDK_Thai_yoyak = $dc2;
  GDK_Thai_rorua = $dc3;
  GDK_Thai_ru = $dc4;
  GDK_Thai_loling = $dc5;
  GDK_Thai_lu = $dc6;
  GDK_Thai_wowaen = $dc7;
  GDK_Thai_sosala = $dc8;
  GDK_Thai_sorusi = $dc9;
  GDK_Thai_sosua = $dca;
  GDK_Thai_hohip = $dcb;
  GDK_Thai_lochula = $dcc;
  GDK_Thai_oang = $dcd;
  GDK_Thai_honokhuk = $dce;
  GDK_Thai_paiyannoi = $dcf;
  GDK_Thai_saraa = $dd0;
  GDK_Thai_maihanakat = $dd1;
  GDK_Thai_saraaa = $dd2;
  GDK_Thai_saraam = $dd3;
  GDK_Thai_sarai = $dd4;
  GDK_Thai_saraii = $dd5;
  GDK_Thai_saraue = $dd6;
  GDK_Thai_sarauee = $dd7;
  GDK_Thai_sarau = $dd8;
  GDK_Thai_sarauu = $dd9;
  GDK_Thai_phinthu = $dda;
  GDK_Thai_maihanakat_maitho = $dde;
  GDK_Thai_baht = $ddf;
  GDK_Thai_sarae = $de0;
  GDK_Thai_saraae = $de1;
  GDK_Thai_sarao = $de2;
  GDK_Thai_saraaimaimuan = $de3;
  GDK_Thai_saraaimaimalai = $de4;
  GDK_Thai_lakkhangyao = $de5;
  GDK_Thai_maiyamok = $de6;
  GDK_Thai_maitaikhu = $de7;
  GDK_Thai_maiek = $de8;
  GDK_Thai_maitho = $de9;
  GDK_Thai_maitri = $dea;
  GDK_Thai_maichattawa = $deb;
  GDK_Thai_thanthakhat = $dec;
  GDK_Thai_nikhahit = $ded;
  GDK_Thai_leksun = $df0;
  GDK_Thai_leknung = $df1;
  GDK_Thai_leksong = $df2;
  GDK_Thai_leksam = $df3;
  GDK_Thai_leksi = $df4;
  GDK_Thai_lekha = $df5;
  GDK_Thai_lekhok = $df6;
  GDK_Thai_lekchet = $df7;
  GDK_Thai_lekpaet = $df8;
  GDK_Thai_lekkao = $df9;
  GDK_Hangul = $ff31;
  GDK_Hangul_Start = $ff32;
  GDK_Hangul_End = $ff33;
  GDK_Hangul_Hanja = $ff34;
  GDK_Hangul_Jamo = $ff35;
  GDK_Hangul_Romaja = $ff36;
  GDK_Hangul_Codeinput = $ff37;
  GDK_Hangul_Jeonja = $ff38;
  GDK_Hangul_Banja = $ff39;
  GDK_Hangul_PreHanja = $ff3a;
  GDK_Hangul_PostHanja = $ff3b;
  GDK_Hangul_SingleCandidate = $ff3c;
  GDK_Hangul_MultipleCandidate = $ff3d;
  GDK_Hangul_PreviousCandidate = $ff3e;
  GDK_Hangul_Special = $ff3f;
  GDK_Hangul_switch = $FF7E;
  GDK_Hangul_Kiyeog = $ea1;
  GDK_Hangul_SsangKiyeog = $ea2;
  GDK_Hangul_KiyeogSios = $ea3;
  GDK_Hangul_Nieun = $ea4;
  GDK_Hangul_NieunJieuj = $ea5;
  GDK_Hangul_NieunHieuh = $ea6;
  GDK_Hangul_Dikeud = $ea7;
  GDK_Hangul_SsangDikeud = $ea8;
  GDK_Hangul_Rieul = $ea9;
  GDK_Hangul_RieulKiyeog = $eaa;
  GDK_Hangul_RieulMieum = $eab;
  GDK_Hangul_RieulPieub = $eac;
  GDK_Hangul_RieulSios = $ead;
  GDK_Hangul_RieulTieut = $eae;
  GDK_Hangul_RieulPhieuf = $eaf;
  GDK_Hangul_RieulHieuh = $eb0;
  GDK_Hangul_Mieum = $eb1;
  GDK_Hangul_Pieub = $eb2;
  GDK_Hangul_SsangPieub = $eb3;
  GDK_Hangul_PieubSios = $eb4;
  GDK_Hangul_Sios = $eb5;
  GDK_Hangul_SsangSios = $eb6;
  GDK_Hangul_Ieung = $eb7;
  GDK_Hangul_Jieuj = $eb8;
  GDK_Hangul_SsangJieuj = $eb9;
  GDK_Hangul_Cieuc = $eba;
  GDK_Hangul_Khieuq = $ebb;
  GDK_Hangul_Tieut = $ebc;
  GDK_Hangul_Phieuf = $ebd;
  GDK_Hangul_Hieuh = $ebe;
  GDK_Hangul_A = $ebf;
  GDK_Hangul_AE = $ec0;
  GDK_Hangul_YA = $ec1;
  GDK_Hangul_YAE = $ec2;
  GDK_Hangul_EO = $ec3;
  GDK_Hangul_E = $ec4;
  GDK_Hangul_YEO = $ec5;
  GDK_Hangul_YE = $ec6;
  GDK_Hangul_O = $ec7;
  GDK_Hangul_WA = $ec8;
  GDK_Hangul_WAE = $ec9;
  GDK_Hangul_OE = $eca;
  GDK_Hangul_YO = $ecb;
  GDK_Hangul_U = $ecc;
  GDK_Hangul_WEO = $ecd;
  GDK_Hangul_WE = $ece;
  GDK_Hangul_WI = $ecf;
  GDK_Hangul_YU = $ed0;
  GDK_Hangul_EU = $ed1;
  GDK_Hangul_YI = $ed2;
  GDK_Hangul_I = $ed3;
  GDK_Hangul_J_Kiyeog = $ed4;
  GDK_Hangul_J_SsangKiyeog = $ed5;
  GDK_Hangul_J_KiyeogSios = $ed6;
  GDK_Hangul_J_Nieun = $ed7;
  GDK_Hangul_J_NieunJieuj = $ed8;
  GDK_Hangul_J_NieunHieuh = $ed9;
  GDK_Hangul_J_Dikeud = $eda;
  GDK_Hangul_J_Rieul = $edb;
  GDK_Hangul_J_RieulKiyeog = $edc;
  GDK_Hangul_J_RieulMieum = $edd;
  GDK_Hangul_J_RieulPieub = $ede;
  GDK_Hangul_J_RieulSios = $edf;
  GDK_Hangul_J_RieulTieut = $ee0;
  GDK_Hangul_J_RieulPhieuf = $ee1;
  GDK_Hangul_J_RieulHieuh = $ee2;
  GDK_Hangul_J_Mieum = $ee3;
  GDK_Hangul_J_Pieub = $ee4;
  GDK_Hangul_J_PieubSios = $ee5;
  GDK_Hangul_J_Sios = $ee6;
  GDK_Hangul_J_SsangSios = $ee7;
  GDK_Hangul_J_Ieung = $ee8;
  GDK_Hangul_J_Jieuj = $ee9;
  GDK_Hangul_J_Cieuc = $eea;
  GDK_Hangul_J_Khieuq = $eeb;
  GDK_Hangul_J_Tieut = $eec;
  GDK_Hangul_J_Phieuf = $eed;
  GDK_Hangul_J_Hieuh = $eee;
  GDK_Hangul_RieulYeorinHieuh = $eef;
  GDK_Hangul_SunkyeongeumMieum = $ef0;
  GDK_Hangul_SunkyeongeumPieub = $ef1;
  GDK_Hangul_PanSios = $ef2;
  GDK_Hangul_KkogjiDalrinIeung = $ef3;
  GDK_Hangul_SunkyeongeumPhieuf = $ef4;
  GDK_Hangul_YeorinHieuh = $ef5;
  GDK_Hangul_AraeA = $ef6;
  GDK_Hangul_AraeAE = $ef7;
  GDK_Hangul_J_PanSios = $ef8;
  GDK_Hangul_J_KkogjiDalrinIeung = $ef9;
  GDK_Hangul_J_YeorinHieuh = $efa;
  GDK_Korean_Won = $eff;

  // From ui/events/keycodes/keyboard_codes_posix.h.
  // KeyboardCode
  VKEY_BACK = $08;
  VKEY_TAB = $09;
  VKEY_BACKTAB = $0A;
  VKEY_CLEAR = $0C;
  VKEY_RETURN = $0D;
  VKEY_SHIFT = $10;
  VKEY_CONTROL = $11;
  VKEY_MENU = $12;
  VKEY_PAUSE = $13;
  VKEY_CAPITAL = $14;
  VKEY_KANA = $15;
  VKEY_HANGUL = $15;
  VKEY_JUNJA = $17;
  VKEY_FINAL = $18;
  VKEY_HANJA = $19;
  VKEY_KANJI = $19;
  VKEY_ESCAPE = $1B;
  VKEY_CONVERT = $1C;
  VKEY_NONCONVERT = $1D;
  VKEY_ACCEPT = $1E;
  VKEY_MODECHANGE = $1F;
  VKEY_SPACE = $20;
  VKEY_PRIOR = $21;
  VKEY_NEXT = $22;
  VKEY_END = $23;
  VKEY_HOME = $24;
  VKEY_LEFT = $25;
  VKEY_UP = $26;
  VKEY_RIGHT = $27;
  VKEY_DOWN = $28;
  VKEY_SELECT = $29;
  VKEY_PRINT = $2A;
  VKEY_EXECUTE = $2B;
  VKEY_SNAPSHOT = $2C;
  VKEY_INSERT = $2D;
  VKEY_DELETE = $2E;
  VKEY_HELP = $2F;
  VKEY_0 = $30;
  VKEY_1 = $31;
  VKEY_2 = $32;
  VKEY_3 = $33;
  VKEY_4 = $34;
  VKEY_5 = $35;
  VKEY_6 = $36;
  VKEY_7 = $37;
  VKEY_8 = $38;
  VKEY_9 = $39;
  VKEY_A = $41;
  VKEY_B = $42;
  VKEY_C = $43;
  VKEY_D = $44;
  VKEY_E = $45;
  VKEY_F = $46;
  VKEY_G = $47;
  VKEY_H = $48;
  VKEY_I = $49;
  VKEY_J = $4A;
  VKEY_K = $4B;
  VKEY_L = $4C;
  VKEY_M = $4D;
  VKEY_N = $4E;
  VKEY_O = $4F;
  VKEY_P = $50;
  VKEY_Q = $51;
  VKEY_R = $52;
  VKEY_S = $53;
  VKEY_T = $54;
  VKEY_U = $55;
  VKEY_V = $56;
  VKEY_W = $57;
  VKEY_X = $58;
  VKEY_Y = $59;
  VKEY_Z = $5A;
  VKEY_LWIN = $5B;
  VKEY_COMMAND = VKEY_LWIN;
  VKEY_RWIN = $5C;
  VKEY_APPS = $5D;
  VKEY_SLEEP = $5F;
  VKEY_NUMPAD0 = $60;
  VKEY_NUMPAD1 = $61;
  VKEY_NUMPAD2 = $62;
  VKEY_NUMPAD3 = $63;
  VKEY_NUMPAD4 = $64;
  VKEY_NUMPAD5 = $65;
  VKEY_NUMPAD6 = $66;
  VKEY_NUMPAD7 = $67;
  VKEY_NUMPAD8 = $68;
  VKEY_NUMPAD9 = $69;
  VKEY_MULTIPLY = $6A;
  VKEY_ADD = $6B;
  VKEY_SEPARATOR = $6C;
  VKEY_SUBTRACT = $6D;
  VKEY_DECIMAL = $6E;
  VKEY_DIVIDE = $6F;
  VKEY_F1 = $70;
  VKEY_F2 = $71;
  VKEY_F3 = $72;
  VKEY_F4 = $73;
  VKEY_F5 = $74;
  VKEY_F6 = $75;
  VKEY_F7 = $76;
  VKEY_F8 = $77;
  VKEY_F9 = $78;
  VKEY_F10 = $79;
  VKEY_F11 = $7A;
  VKEY_F12 = $7B;
  VKEY_F13 = $7C;
  VKEY_F14 = $7D;
  VKEY_F15 = $7E;
  VKEY_F16 = $7F;
  VKEY_F17 = $80;
  VKEY_F18 = $81;
  VKEY_F19 = $82;
  VKEY_F20 = $83;
  VKEY_F21 = $84;
  VKEY_F22 = $85;
  VKEY_F23 = $86;
  VKEY_F24 = $87;
  VKEY_NUMLOCK = $90;
  VKEY_SCROLL = $91;
  VKEY_LSHIFT = $A0;
  VKEY_RSHIFT = $A1;
  VKEY_LCONTROL = $A2;
  VKEY_RCONTROL = $A3;
  VKEY_LMENU = $A4;
  VKEY_RMENU = $A5;
  VKEY_BROWSER_BACK = $A6;
  VKEY_BROWSER_FORWARD = $A7;
  VKEY_BROWSER_REFRESH = $A8;
  VKEY_BROWSER_STOP = $A9;
  VKEY_BROWSER_SEARCH = $AA;
  VKEY_BROWSER_FAVORITES = $AB;
  VKEY_BROWSER_HOME = $AC;
  VKEY_VOLUME_MUTE = $AD;
  VKEY_VOLUME_DOWN = $AE;
  VKEY_VOLUME_UP = $AF;
  VKEY_MEDIA_NEXT_TRACK = $B0;
  VKEY_MEDIA_PREV_TRACK = $B1;
  VKEY_MEDIA_STOP = $B2;
  VKEY_MEDIA_PLAY_PAUSE = $B3;
  VKEY_MEDIA_LAUNCH_MAIL = $B4;
  VKEY_MEDIA_LAUNCH_MEDIA_SELECT = $B5;
  VKEY_MEDIA_LAUNCH_APP1 = $B6;
  VKEY_MEDIA_LAUNCH_APP2 = $B7;
  VKEY_OEM_1 = $BA;
  VKEY_OEM_PLUS = $BB;
  VKEY_OEM_COMMA = $BC;
  VKEY_OEM_MINUS = $BD;
  VKEY_OEM_PERIOD = $BE;
  VKEY_OEM_2 = $BF;
  VKEY_OEM_3 = $C0;
  VKEY_OEM_4 = $DB;
  VKEY_OEM_5 = $DC;
  VKEY_OEM_6 = $DD;
  VKEY_OEM_7 = $DE;
  VKEY_OEM_8 = $DF;
  VKEY_OEM_102 = $E2;
  VKEY_OEM_103 = $E3;
  VKEY_OEM_104 = $E4;
  VKEY_PROCESSKEY = $E5;
  VKEY_PACKET = $E7;
  VKEY_DBE_SBCSCHAR = $F3;
  VKEY_DBE_DBCSCHAR = $F4;
  VKEY_ATTN = $F6;
  VKEY_CRSEL = $F7;
  VKEY_EXSEL = $F8;
  VKEY_EREOF = $F9;
  VKEY_PLAY = $FA;
  VKEY_ZOOM = $FB;
  VKEY_NONAME = $FC;
  VKEY_PA1 = $FD;
  VKEY_OEM_CLEAR = $FE;
  VKEY_UNKNOWN = 0;
  VKEY_WLAN = $97;
  VKEY_POWER = $98;
  VKEY_BRIGHTNESS_DOWN = $D8;
  VKEY_BRIGHTNESS_UP = $D9;
  VKEY_KBD_BRIGHTNESS_DOWN = $DA;
  VKEY_KBD_BRIGHTNESS_UP = $E8;
  VKEY_ALTGR = $E1;
  VKEY_COMPOSE = $E6;

  kHardwareCodeToGDKKeyval : array[0..116] of integer = (
    0,                 // 0x00:
    0,                 // 0x01:
    0,                 // 0x02:
    0,                 // 0x03:
    0,                 // 0x04:
    0,                 // 0x05:
    0,                 // 0x06:
    0,                 // 0x07:
    0,                 // 0x08:
    0,                 // 0x09: GDK_Escape
    GDK_1,             // 0x0A: GDK_1
    GDK_2,             // 0x0B: GDK_2
    GDK_3,             // 0x0C: GDK_3
    GDK_4,             // 0x0D: GDK_4
    GDK_5,             // 0x0E: GDK_5
    GDK_6,             // 0x0F: GDK_6
    GDK_7,             // 0x10: GDK_7
    GDK_8,             // 0x11: GDK_8
    GDK_9,             // 0x12: GDK_9
    GDK_0,             // 0x13: GDK_0
    GDK_minus,         // 0x14: GDK_minus
    GDK_equal,         // 0x15: GDK_equal
    0,                 // 0x16: GDK_BackSpace
    0,                 // 0x17: GDK_Tab
    GDK_q,             // 0x18: GDK_q
    GDK_w,             // 0x19: GDK_w
    GDK_e,             // 0x1A: GDK_e
    GDK_r,             // 0x1B: GDK_r
    GDK_t,             // 0x1C: GDK_t
    GDK_y,             // 0x1D: GDK_y
    GDK_u,             // 0x1E: GDK_u
    GDK_i,             // 0x1F: GDK_i
    GDK_o,             // 0x20: GDK_o
    GDK_p,             // 0x21: GDK_p
    GDK_bracketleft,   // 0x22: GDK_bracketleft
    GDK_bracketright,  // 0x23: GDK_bracketright
    0,                 // 0x24: GDK_Return
    0,                 // 0x25: GDK_Control_L
    GDK_a,             // 0x26: GDK_a
    GDK_s,             // 0x27: GDK_s
    GDK_d,             // 0x28: GDK_d
    GDK_f,             // 0x29: GDK_f
    GDK_g,             // 0x2A: GDK_g
    GDK_h,             // 0x2B: GDK_h
    GDK_j,             // 0x2C: GDK_j
    GDK_k,             // 0x2D: GDK_k
    GDK_l,             // 0x2E: GDK_l
    GDK_semicolon,     // 0x2F: GDK_semicolon
    GDK_apostrophe,    // 0x30: GDK_apostrophe
    GDK_grave,         // 0x31: GDK_grave
    0,                 // 0x32: GDK_Shift_L
    GDK_backslash,     // 0x33: GDK_backslash
    GDK_z,             // 0x34: GDK_z
    GDK_x,             // 0x35: GDK_x
    GDK_c,             // 0x36: GDK_c
    GDK_v,             // 0x37: GDK_v
    GDK_b,             // 0x38: GDK_b
    GDK_n,             // 0x39: GDK_n
    GDK_m,             // 0x3A: GDK_m
    GDK_comma,         // 0x3B: GDK_comma
    GDK_period,        // 0x3C: GDK_period
    GDK_slash,         // 0x3D: GDK_slash
    0,                 // 0x3E: GDK_Shift_R
    0,                 // 0x3F:
    0,                 // 0x40:
    0,                 // 0x41:
    0,                 // 0x42:
    0,                 // 0x43:
    0,                 // 0x44:
    0,                 // 0x45:
    0,                 // 0x46:
    0,                 // 0x47:
    0,                 // 0x48:
    0,                 // 0x49:
    0,                 // 0x4A:
    0,                 // 0x4B:
    0,                 // 0x4C:
    0,                 // 0x4D:
    0,                 // 0x4E:
    0,                 // 0x4F:
    0,                 // 0x50:
    0,                 // 0x51:
    0,                 // 0x52:
    0,                 // 0x53:
    0,                 // 0x54:
    0,                 // 0x55:
    0,                 // 0x56:
    0,                 // 0x57:
    0,                 // 0x58:
    0,                 // 0x59:
    0,                 // 0x5A:
    0,                 // 0x5B:
    0,                 // 0x5C:
    0,                 // 0x5D:
    0,                 // 0x5E:
    0,                 // 0x5F:
    0,                 // 0x60:
    0,                 // 0x61:
    0,                 // 0x62:
    0,                 // 0x63:
    0,                 // 0x64:
    0,                 // 0x65:
    0,                 // 0x66:
    0,                 // 0x67:
    0,                 // 0x68:
    0,                 // 0x69:
    0,                 // 0x6A:
    0,                 // 0x6B:
    0,                 // 0x6C:
    0,                 // 0x6D:
    0,                 // 0x6E:
    0,                 // 0x6F:
    0,                 // 0x70:
    0,                 // 0x71:
    0,                 // 0x72:
    GDK_Super_L,       // 0x73: GDK_Super_L
    GDK_Super_R        // 0x74: GDK_Super_R
    );
  {$ENDIF}{$ENDIF}{$ENDIF}

implementation

end.


