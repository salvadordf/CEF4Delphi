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
//        Copyright © 2023 Salvador Diaz Fau. All rights reserved.
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

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

{$IFDEF MSWINDOWS}
uses
  {$IFDEF DELPHI16_UP}
  System.Classes, Winapi.Messages;
  {$ELSE}
  Classes, Messages;
  {$ENDIF}
{$ENDIF}

const
  // Error list defined in /include/internal/cef_types.h (cef_errorcode_t)
  // which includes this file /include/base/internal/cef_net_error_list.h
  // which includes this Chromium source file /net/base/net_error_list.h
  // available here in the master branch :
  //     https://chromium.googlesource.com/chromium/src/+/master/net/base/net_error_list.h
  // Compare the values in the right Chromium branch :
  //     https://chromium.googlesource.com/chromium/src/+/refs/tags/98.0.4758.55/net/base/net_error_list.h
  //
  // Ranges:
  //     0- 99 System related errors
  //   100-199 Connection related errors
  //   200-299 Certificate errors
  //   300-399 HTTP errors
  //   400-499 Cache errors
  //   500-599 ?
  //   600-699 FTP errors
  //   700-799 Certificate manager errors
  //   800-899 DNS resolver errors
  //
  ERR_NONE                                                    = 0;
  ERR_IO_PENDING                                              = -1;
  ERR_FAILED                                                  = -2;
  ERR_ABORTED                                                 = -3;
  ERR_INVALID_ARGUMENT                                        = -4;
  ERR_INVALID_HANDLE                                          = -5;
  ERR_FILE_NOT_FOUND                                          = -6;
  ERR_TIMED_OUT                                               = -7;
  ERR_FILE_TOO_BIG                                            = -8;
  ERR_UNEXPECTED                                              = -9;
  ERR_ACCESS_DENIED                                           = -10;
  ERR_NOT_IMPLEMENTED                                         = -11;
  ERR_INSUFFICIENT_RESOURCES                                  = -12;
  ERR_OUT_OF_MEMORY                                           = -13;
  ERR_UPLOAD_FILE_CHANGED                                     = -14;
  ERR_SOCKET_NOT_CONNECTED                                    = -15;
  ERR_FILE_EXISTS                                             = -16;
  ERR_FILE_PATH_TOO_LONG                                      = -17;
  ERR_FILE_NO_SPACE                                           = -18;
  ERR_FILE_VIRUS_INFECTED                                     = -19;
  ERR_BLOCKED_BY_CLIENT                                       = -20;
  ERR_NETWORK_CHANGED                                         = -21;
  ERR_BLOCKED_BY_ADMINISTRATOR                                = -22;
  ERR_SOCKET_IS_CONNECTED                                     = -23;
  ERR_BLOCKED_ENROLLMENT_CHECK_PENDING                        = -24;
  ERR_UPLOAD_STREAM_REWIND_NOT_SUPPORTED                      = -25;
  ERR_CONTEXT_SHUT_DOWN                                       = -26;
  ERR_BLOCKED_BY_RESPONSE                                     = -27;
  ERR_BLOCKED_BY_XSS_AUDITOR                                  = -28;
  ERR_CLEARTEXT_NOT_PERMITTED                                 = -29;
  ERR_BLOCKED_BY_CSP                                          = -30;
  ERR_H2_OR_QUIC_REQUIRED                                     = -31;
  //
  ERR_CONNECTION_CLOSED                                       = -100;
  ERR_CONNECTION_RESET                                        = -101;
  ERR_CONNECTION_REFUSED                                      = -102;
  ERR_CONNECTION_ABORTED                                      = -103;
  ERR_CONNECTION_FAILED                                       = -104;
  ERR_NAME_NOT_RESOLVED                                       = -105;
  ERR_INTERNET_DISCONNECTED                                   = -106;
  ERR_SSL_PROTOCOL_ERROR                                      = -107;
  ERR_ADDRESS_INVALID                                         = -108;
  ERR_ADDRESS_UNREACHABLE                                     = -109;
  ERR_SSL_CLIENT_AUTH_CERT_NEEDED                             = -110;
  ERR_TUNNEL_CONNECTION_FAILED                                = -111;
  ERR_NO_SSL_VERSIONS_ENABLED                                 = -112;
  ERR_SSL_VERSION_OR_CIPHER_MISMATCH                          = -113;
  ERR_SSL_RENEGOTIATION_REQUESTED                             = -114;
  ERR_PROXY_AUTH_UNSUPPORTED                                  = -115;
  ERR_CERT_ERROR_IN_SSL_RENEGOTIATION                         = -116;
  ERR_BAD_SSL_CLIENT_AUTH_CERT                                = -117;
  ERR_CONNECTION_TIMED_OUT                                    = -118;
  ERR_HOST_RESOLVER_QUEUE_TOO_LARGE                           = -119;
  ERR_SOCKS_CONNECTION_FAILED                                 = -120;
  ERR_SOCKS_CONNECTION_HOST_UNREACHABLE                       = -121;
  ERR_ALPN_NEGOTIATION_FAILED                                 = -122;
  ERR_SSL_NO_RENEGOTIATION                                    = -123;
  ERR_WINSOCK_UNEXPECTED_WRITTEN_BYTES                        = -124;
  ERR_SSL_DECOMPRESSION_FAILURE_ALERT                         = -125;
  ERR_SSL_BAD_RECORD_MAC_ALERT                                = -126;
  ERR_PROXY_AUTH_REQUESTED                                    = -127;
  //
  ERR_PROXY_CONNECTION_FAILED                                 = -130;
  ERR_MANDATORY_PROXY_CONFIGURATION_FAILED                    = -131;
  //
  ERR_PRECONNECT_MAX_SOCKET_LIMIT                             = -133;
  ERR_SSL_CLIENT_AUTH_PRIVATE_KEY_ACCESS_DENIED               = -134;
  ERR_SSL_CLIENT_AUTH_CERT_NO_PRIVATE_KEY                     = -135;
  ERR_PROXY_CERTIFICATE_INVALID                               = -136;
  ERR_NAME_RESOLUTION_FAILED                                  = -137;
  ERR_NETWORK_ACCESS_DENIED                                   = -138;
  ERR_TEMPORARILY_THROTTLED                                   = -139;
  ERR_HTTPS_PROXY_TUNNEL_RESPONSE_REDIRECT                    = -140;
  ERR_SSL_CLIENT_AUTH_SIGNATURE_FAILED                        = -141;
  ERR_MSG_TOO_BIG                                             = -142;
  //
  ERR_WS_PROTOCOL_ERROR                                       = -145;
  //
  ERR_ADDRESS_IN_USE                                          = -147;
  ERR_SSL_HANDSHAKE_NOT_COMPLETED                             = -148;
  ERR_SSL_BAD_PEER_PUBLIC_KEY                                 = -149;
  ERR_SSL_PINNED_KEY_NOT_IN_CERT_CHAIN                        = -150;
  ERR_CLIENT_AUTH_CERT_TYPE_UNSUPPORTED                       = -151;
  //
  ERR_SSL_DECRYPT_ERROR_ALERT                                 = -153;
  ERR_WS_THROTTLE_QUEUE_TOO_LARGE                             = -154;
  //
  ERR_SSL_SERVER_CERT_CHANGED                                 = -156;
  //
  ERR_SSL_UNRECOGNIZED_NAME_ALERT                             = -159;
  ERR_SOCKET_SET_RECEIVE_BUFFER_SIZE_ERROR                    = -160;
  ERR_SOCKET_SET_SEND_BUFFER_SIZE_ERROR                       = -161;
  ERR_SOCKET_RECEIVE_BUFFER_SIZE_UNCHANGEABLE                 = -162;
  ERR_SOCKET_SEND_BUFFER_SIZE_UNCHANGEABLE                    = -163;
  ERR_SSL_CLIENT_AUTH_CERT_BAD_FORMAT                         = -164;
  //
  ERR_ICANN_NAME_COLLISION                                    = -166;
  ERR_SSL_SERVER_CERT_BAD_FORMAT                              = -167;
  ERR_CT_STH_PARSING_FAILED                                   = -168;
  ERR_CT_STH_INCOMPLETE                                       = -169;
  ERR_UNABLE_TO_REUSE_CONNECTION_FOR_PROXY_AUTH               = -170;
  ERR_CT_CONSISTENCY_PROOF_PARSING_FAILED                     = -171;
  ERR_SSL_OBSOLETE_CIPHER                                     = -172;
  ERR_WS_UPGRADE                                              = -173;
  ERR_READ_IF_READY_NOT_IMPLEMENTED                           = -174;
  //
  ERR_NO_BUFFER_SPACE                                         = -176;
  ERR_SSL_CLIENT_AUTH_NO_COMMON_ALGORITHMS                    = -177;
  ERR_EARLY_DATA_REJECTED                                     = -178;
  ERR_WRONG_VERSION_ON_EARLY_DATA                             = -179;
  ERR_TLS13_DOWNGRADE_DETECTED                                = -180;
  ERR_SSL_KEY_USAGE_INCOMPATIBLE                              = -181;
  ERR_INVALID_ECH_CONFIG_LIST                                 = -182;
  ERR_ECH_NOT_NEGOTIATED                                      = -183;
  ERR_ECH_FALLBACK_CERTIFICATE_INVALID                        = -184;
  //
  ERR_CERT_COMMON_NAME_INVALID                                = -200;
  ERR_CERT_DATE_INVALID                                       = -201;
  ERR_CERT_AUTHORITY_INVALID                                  = -202;
  ERR_CERT_CONTAINS_ERRORS                                    = -203;
  ERR_CERT_NO_REVOCATION_MECHANISM                            = -204;
  ERR_CERT_UNABLE_TO_CHECK_REVOCATION                         = -205;
  ERR_CERT_REVOKED                                            = -206;
  ERR_CERT_INVALID                                            = -207;
  ERR_CERT_WEAK_SIGNATURE_ALGORITHM                           = -208;
  //
  ERR_CERT_NON_UNIQUE_NAME                                    = -210;
  ERR_CERT_WEAK_KEY                                           = -211;
  ERR_CERT_NAME_CONSTRAINT_VIOLATION                          = -212;
  ERR_CERT_VALIDITY_TOO_LONG                                  = -213;
  ERR_CERTIFICATE_TRANSPARENCY_REQUIRED                       = -214;
  ERR_CERT_SYMANTEC_LEGACY                                    = -215;
  //
  ERR_CERT_KNOWN_INTERCEPTION_BLOCKED                         = -217;
  //
  ERR_CERT_END                                                = -219;
  //
  ERR_INVALID_URL                                             = -300;
  ERR_DISALLOWED_URL_SCHEME                                   = -301;
  ERR_UNKNOWN_URL_SCHEME                                      = -302;
  ERR_INVALID_REDIRECT                                        = -303;
  //
  ERR_TOO_MANY_REDIRECTS                                      = -310;
  ERR_UNSAFE_REDIRECT                                         = -311;
  ERR_UNSAFE_PORT                                             = -312;
  //
  ERR_INVALID_RESPONSE                                        = -320;
  ERR_INVALID_CHUNKED_ENCODING                                = -321;
  ERR_METHOD_NOT_SUPPORTED                                    = -322;
  ERR_UNEXPECTED_PROXY_AUTH                                   = -323;
  ERR_EMPTY_RESPONSE                                          = -324;
  ERR_RESPONSE_HEADERS_TOO_BIG                                = -325;
  //
  ERR_PAC_SCRIPT_FAILED                                       = -327;
  ERR_REQUEST_RANGE_NOT_SATISFIABLE                           = -328;
  ERR_MALFORMED_IDENTITY                                      = -329;
  ERR_CONTENT_DECODING_FAILED                                 = -330;
  ERR_NETWORK_IO_SUSPENDED                                    = -331;
  ERR_SYN_REPLY_NOT_RECEIVED                                  = -332;
  ERR_ENCODING_CONVERSION_FAILED                              = -333;
  ERR_UNRECOGNIZED_FTP_DIRECTORY_LISTING_FORMAT               = -334;
  //
  ERR_NO_SUPPORTED_PROXIES                                    = -336;
  ERR_SPDY_PROTOCOL_ERROR                                     = -337;
  ERR_INVALID_AUTH_CREDENTIALS                                = -338;
  ERR_UNSUPPORTED_AUTH_SCHEME                                 = -339;
  ERR_ENCODING_DETECTION_FAILED                               = -340;
  ERR_MISSING_AUTH_CREDENTIALS                                = -341;
  ERR_UNEXPECTED_SECURITY_LIBRARY_STATUS                      = -342;
  ERR_MISCONFIGURED_AUTH_ENVIRONMENT                          = -343;
  ERR_UNDOCUMENTED_SECURITY_LIBRARY_STATUS                    = -344;
  ERR_RESPONSE_BODY_TOO_BIG_TO_DRAIN                          = -345;
  ERR_RESPONSE_HEADERS_MULTIPLE_CONTENT_LENGTH                = -346;
  ERR_INCOMPLETE_SPDY_HEADERS                                 = -347;
  ERR_PAC_NOT_IN_DHCP                                         = -348;
  ERR_RESPONSE_HEADERS_MULTIPLE_CONTENT_DISPOSITION           = -349;
  ERR_RESPONSE_HEADERS_MULTIPLE_LOCATION                      = -350;
  ERR_SPDY_SERVER_REFUSED_STREAM                              = -351;
  ERR_SPDY_PING_FAILED                                        = -352;
  //
  ERR_CONTENT_LENGTH_MISMATCH                                 = -354;
  ERR_INCOMPLETE_CHUNKED_ENCODING                             = -355;
  ERR_QUIC_PROTOCOL_ERROR                                     = -356;
  ERR_RESPONSE_HEADERS_TRUNCATED                              = -357;
  ERR_QUIC_HANDSHAKE_FAILED                                   = -358;
  //
  ERR_SPDY_INADEQUATE_TRANSPORT_SECURITY                      = -360;
  ERR_SPDY_FLOW_CONTROL_ERROR                                 = -361;
  ERR_SPDY_FRAME_SIZE_ERROR                                   = -362;
  ERR_SPDY_COMPRESSION_ERROR                                  = -363;
  ERR_PROXY_AUTH_REQUESTED_WITH_NO_CONNECTION                 = -364;
  ERR_HTTP_1_1_REQUIRED                                       = -365;
  ERR_PROXY_HTTP_1_1_REQUIRED                                 = -366;
  ERR_PAC_SCRIPT_TERMINATED                                   = -367;
  //
  ERR_INVALID_HTTP_RESPONSE                                   = -370;
  ERR_CONTENT_DECODING_INIT_FAILED                            = -371;
  ERR_SPDY_RST_STREAM_NO_ERROR_RECEIVED                       = -372;
  ERR_SPDY_PUSHED_STREAM_NOT_AVAILABLE                        = -373;
  ERR_SPDY_CLAIMED_PUSHED_STREAM_RESET_BY_SERVER              = -374;
  ERR_TOO_MANY_RETRIES                                        = -375;
  ERR_SPDY_STREAM_CLOSED                                      = -376;
  ERR_SPDY_CLIENT_REFUSED_STREAM                              = -377;
  ERR_SPDY_PUSHED_RESPONSE_DOES_NOT_MATCH                     = -378;
  ERR_HTTP_RESPONSE_CODE_FAILURE                              = -379;
  ERR_QUIC_CERT_ROOT_NOT_KNOWN                                = -380;
  ERR_QUIC_GOAWAY_REQUEST_CAN_BE_RETRIED                      = -381;
  //
  ERR_CACHE_MISS                                              = -400;
  ERR_CACHE_READ_FAILURE                                      = -401;
  ERR_CACHE_WRITE_FAILURE                                     = -402;
  ERR_CACHE_OPERATION_NOT_SUPPORTED                           = -403;
  ERR_CACHE_OPEN_FAILURE                                      = -404;
  ERR_CACHE_CREATE_FAILURE                                    = -405;
  ERR_CACHE_RACE                                              = -406;
  ERR_CACHE_CHECKSUM_READ_FAILURE                             = -407;
  ERR_CACHE_CHECKSUM_MISMATCH                                 = -408;
  ERR_CACHE_LOCK_TIMEOUT                                      = -409;
  ERR_CACHE_AUTH_FAILURE_AFTER_READ                           = -410;
  ERR_CACHE_ENTRY_NOT_SUITABLE                                = -411;
  ERR_CACHE_DOOM_FAILURE                                      = -412;
  ERR_CACHE_OPEN_OR_CREATE_FAILURE                            = -413;
  //
  ERR_INSECURE_RESPONSE                                       = -501;
  ERR_NO_PRIVATE_KEY_FOR_CERT                                 = -502;
  ERR_ADD_USER_CERT_FAILED                                    = -503;
  ERR_INVALID_SIGNED_EXCHANGE                                 = -504;
  ERR_INVALID_WEB_BUNDLE                                      = -505;
  ERR_TRUST_TOKEN_OPERATION_FAILED                            = -506;
  ERR_TRUST_TOKEN_OPERATION_SUCCESS_WITHOUT_SENDING_REQUEST   = -507;
  //
  ERR_FTP_FAILED                                              = -601;
  ERR_FTP_SERVICE_UNAVAILABLE                                 = -602;
  ERR_FTP_TRANSFER_ABORTED                                    = -603;
  ERR_FTP_FILE_BUSY                                           = -604;
  ERR_FTP_SYNTAX_ERROR                                        = -605;
  ERR_FTP_COMMAND_NOT_SUPPORTED                               = -606;
  ERR_FTP_BAD_COMMAND_SEQUENCE                                = -607;
  //
  ERR_PKCS12_IMPORT_BAD_PASSWORD                              = -701;
  ERR_PKCS12_IMPORT_FAILED                                    = -702;
  ERR_IMPORT_CA_CERT_NOT_CA                                   = -703;
  ERR_IMPORT_CERT_ALREADY_EXISTS                              = -704;
  ERR_IMPORT_CA_CERT_FAILED                                   = -705;
  ERR_IMPORT_SERVER_CERT_FAILED                               = -706;
  ERR_PKCS12_IMPORT_INVALID_MAC                               = -707;
  ERR_PKCS12_IMPORT_INVALID_FILE                              = -708;
  ERR_PKCS12_IMPORT_UNSUPPORTED                               = -709;
  ERR_KEY_GENERATION_FAILED                                   = -710;
  //
  ERR_PRIVATE_KEY_EXPORT_FAILED                               = -712;
  ERR_SELF_SIGNED_CERT_GENERATION_FAILED                      = -713;
  ERR_CERT_DATABASE_CHANGED                                   = -714;
  //
  ERR_DNS_MALFORMED_RESPONSE                                  = -800;
  ERR_DNS_SERVER_REQUIRES_TCP                                 = -801;
  ERR_DNS_SERVER_FAILED                                       = -802;
  ERR_DNS_TIMED_OUT                                           = -803;
  ERR_NS_CACHE_MISS                                           = -804;
  ERR_DNS_SEARCH_EMPTY                                        = -805;
  ERR_DNS_SORT_ERROR                                          = -806;
  //
  ERR_DNS_SECURE_RESOLVER_HOSTNAME_RESOLUTION_FAILED          = -808;
  ERR_DNS_NAME_HTTPS_ONLY                                     = -809;

  // command_id constants declared in cef_command_ids.h and used by some callbacks in cef_command_handler_t
  // cef_command_ids.h is generated in /include/cef_command_ids.h and it's also available here :
  // https://source.chromium.org/chromium/chromium/src/+/main:chrome/app/chrome_command_ids.h
  IDC_MinimumLabelValue = 4000;
  IDC_BACK = 33000;
  IDC_FORWARD = 33001;
  IDC_RELOAD = 33002;
  IDC_HOME = 33003;
  IDC_OPEN_CURRENT_URL = 33004;
  IDC_STOP = 33006;
  IDC_RELOAD_BYPASSING_CACHE = 33007;
  IDC_RELOAD_CLEARING_CACHE = 33009;
  IDC_NEW_WINDOW = 34000;
  IDC_NEW_INCOGNITO_WINDOW = 34001;
  IDC_PIN_TO_START_SCREEN = 34005;
  IDC_CLOSE_WINDOW = 34012;
  IDC_ALWAYS_ON_TOP = 34013;
  IDC_NEW_TAB = 34014;
  IDC_CLOSE_TAB = 34015;
  IDC_SELECT_NEXT_TAB = 34016;
  IDC_SELECT_PREVIOUS_TAB = 34017;
  IDC_SELECT_TAB_0 = 34018;
  IDC_SELECT_TAB_1 = 34019;
  IDC_SELECT_TAB_2 = 34020;
  IDC_SELECT_TAB_3 = 34021;
  IDC_SELECT_TAB_4 = 34022;
  IDC_SELECT_TAB_5 = 34023;
  IDC_SELECT_TAB_6 = 34024;
  IDC_SELECT_TAB_7 = 34025;
  IDC_SELECT_LAST_TAB = 34026;
  IDC_DUPLICATE_TAB = 34027;
  IDC_RESTORE_TAB = 34028;
  IDC_SHOW_AS_TAB = 34029;
  IDC_FULLSCREEN = 34030;
  IDC_EXIT = 34031;
  IDC_MOVE_TAB_NEXT = 34032;
  IDC_MOVE_TAB_PREVIOUS = 34033;
  IDC_SEARCH = 34035;
  IDC_DEBUG_FRAME_TOGGLE = 34038;
  IDC_WINDOW_MENU = 34045;
  IDC_MINIMIZE_WINDOW = 34046;
  IDC_MAXIMIZE_WINDOW = 34047;
  IDC_ALL_WINDOWS_FRONT = 34048;
  IDC_NAME_WINDOW = 34049;
  IDC_TOGGLE_MULTITASK_MENU = 34050;
  IDC_USE_SYSTEM_TITLE_BAR = 34051;
  IDC_RESTORE_WINDOW = 34052;
  IDC_OPEN_IN_PWA_WINDOW = 34053;
  IDC_MOVE_TAB_TO_NEW_WINDOW = 34054;
  IDC_COPY_URL = 34060;
  IDC_OPEN_IN_CHROME = 34061;
  IDC_SITE_SETTINGS = 34062;
  IDC_WEB_APP_MENU_APP_INFO = 34063;
  IDC_VISIT_DESKTOP_OF_LRU_USER_2 = 34080;
  IDC_VISIT_DESKTOP_OF_LRU_USER_3 = 34081;
  IDC_VISIT_DESKTOP_OF_LRU_USER_4 = 34082;
  IDC_VISIT_DESKTOP_OF_LRU_USER_5 = 34083;
  IDC_BOOKMARK_THIS_TAB = 35000;
  IDC_BOOKMARK_ALL_TABS = 35001;
  IDC_VIEW_SOURCE = 35002;
  IDC_PRINT = 35003;
  IDC_SAVE_PAGE = 35004;
  IDC_EMAIL_PAGE_LOCATION = 35006;
  IDC_BASIC_PRINT = 35007;
  IDC_SAVE_CREDIT_CARD_FOR_PAGE = 35008;
  IDC_TRANSLATE_PAGE = 35009;
  IDC_MANAGE_PASSWORDS_FOR_PAGE = 35010;
  IDC_ROUTE_MEDIA = 35011;
  IDC_WINDOW_MUTE_SITE = 35012;
  IDC_WINDOW_PIN_TAB = 35013;
  IDC_WINDOW_GROUP_TAB = 35014;
  IDC_MIGRATE_LOCAL_CREDIT_CARD_FOR_PAGE = 35015;
  IDC_SEND_TAB_TO_SELF = 35016;
  IDC_FOCUS_THIS_TAB = 35017;
  IDC_QRCODE_GENERATOR = 35021;
  IDC_WINDOW_CLOSE_TABS_TO_RIGHT = 35022;
  IDC_WINDOW_CLOSE_OTHER_TABS = 35023;
  IDC_NEW_TAB_TO_RIGHT = 35024;
  IDC_SAVE_AUTOFILL_ADDRESS = 35025;
  IDC_OFFERS_AND_REWARDS_FOR_PAGE = 35026;
  IDC_WEBAUTHN = 35027;
  IDC_SHARING_HUB = 35028;
  IDC_SHARING_HUB_MENU = 35029;
  IDC_VIRTUAL_CARD_MANUAL_FALLBACK = 35030;
  IDC_SHARING_HUB_SCREENSHOT = 35031;
  IDC_VIRTUAL_CARD_ENROLL = 35032;
  IDC_FOLLOW = 35033;
  IDC_UNFOLLOW = 35034;
  IDC_SAVE_IBAN_FOR_PAGE = 35035;
  IDC_MUTE_TARGET_SITE = 35050;
  IDC_PIN_TARGET_TAB = 35051;
  IDC_GROUP_TARGET_TAB = 35052;
  IDC_DUPLICATE_TARGET_TAB = 35053;
  IDC_CUT = 36000;
  IDC_COPY = 36001;
  IDC_PASTE = 36003;
  IDC_EDIT_MENU = 36004;
  IDC_FIND = 37000;
  IDC_FIND_NEXT = 37001;
  IDC_FIND_PREVIOUS = 37002;
  IDC_CLOSE_FIND_OR_STOP = 37003;
  IDC_FIND_MENU = 37100;
  IDC_ZOOM_MENU = 38000;
  IDC_ZOOM_PLUS = 38001;
  IDC_ZOOM_NORMAL = 38002;
  IDC_ZOOM_MINUS = 38003;
  IDC_ZOOM_PERCENT_DISPLAY = 38004;
  IDC_FOCUS_TOOLBAR = 39000;
  IDC_FOCUS_LOCATION = 39001;
  IDC_FOCUS_SEARCH = 39002;
  IDC_FOCUS_MENU_BAR = 39003;
  IDC_FOCUS_NEXT_PANE = 39004;
  IDC_FOCUS_PREVIOUS_PANE = 39005;
  IDC_FOCUS_BOOKMARKS = 39006;
  IDC_FOCUS_INACTIVE_POPUP_FOR_ACCESSIBILITY = 39007;
  IDC_FOCUS_WEB_CONTENTS_PANE = 39009;
  IDC_OPEN_FILE = 40000;
  IDC_CREATE_SHORTCUT = 40002;
  IDC_DEVELOPER_MENU = 40003;
  IDC_DEV_TOOLS = 40004;
  IDC_DEV_TOOLS_CONSOLE = 40005;
  IDC_TASK_MANAGER = 40006;
  IDC_DEV_TOOLS_DEVICES = 40007;
  IDC_FEEDBACK = 40008;
  IDC_SHOW_BOOKMARK_BAR = 40009;
  IDC_SHOW_HISTORY = 40010;
  IDC_SHOW_BOOKMARK_MANAGER = 40011;
  IDC_SHOW_DOWNLOADS = 40012;
  IDC_CLEAR_BROWSING_DATA = 40013;
  IDC_IMPORT_SETTINGS = 40014;
  IDC_OPTIONS = 40015;
  IDC_EDIT_SEARCH_ENGINES = 40016;
  IDC_VIEW_PASSWORDS = 40017;
  IDC_ABOUT = 40018;
  IDC_HELP_PAGE_VIA_KEYBOARD = 40019;
  IDC_HELP_PAGE_VIA_MENU = 40020;
  IDC_SHOW_APP_MENU = 40021;
  IDC_MANAGE_EXTENSIONS = 40022;
  IDC_DEV_TOOLS_INSPECT = 40023;
  IDC_UPGRADE_DIALOG = 40024;
  IDC_PROFILING_ENABLED = 40028;
  IDC_BOOKMARKS_MENU = 40029;
  IDC_SHOW_SIGNIN = 40030;
  IDC_EXTENSION_ERRORS = 40031;
  IDC_SHOW_SETTINGS_CHANGE_FIRST = 40033;
  IDC_SHOW_SETTINGS_CHANGE_LAST = 40133;
  IDC_SHOW_AVATAR_MENU = 40134;
  IDC_EXTENSION_INSTALL_ERROR_FIRST = 40135;
  IDC_EXTENSION_INSTALL_ERROR_LAST = 40235;
  IDC_TOGGLE_REQUEST_TABLET_SITE = 40236;
  IDC_DEV_TOOLS_TOGGLE = 40237;
  IDC_RECENT_TABS_MENU = 40239;
  IDC_RECENT_TABS_NO_DEVICE_TABS = 40240;
  IDC_SHOW_SETTINGS_RESET_BUBBLE = 40241;
  IDC_SHOW_SYNC_ERROR = 40242;
  IDC_DISTILL_PAGE = 40243;
  IDC_HELP_MENU = 40244;
  IDC_SHOW_SRT_BUBBLE = 40246;
  IDC_ELEVATED_RECOVERY_DIALOG = 40247;
  IDC_TAKE_SCREENSHOT = 40248;
  IDC_MORE_TOOLS_MENU = 40249;
  IDC_TOGGLE_FULLSCREEN_TOOLBAR = 40250;
  IDC_CUSTOMIZE_TOUCH_BAR = 40251;
  IDC_SHOW_BETA_FORUM = 40252;
  IDC_TOGGLE_JAVASCRIPT_APPLE_EVENTS = 40253;
  IDC_INSTALL_PWA = 40254;
  IDC_SHOW_MANAGEMENT_PAGE = 40255;
  IDC_PASTE_AND_GO = 40256;
  IDC_SHOW_SAVE_LOCAL_CARD_SIGN_IN_PROMO_IF_APPLICABLE = 40257;
  IDC_CLOSE_SIGN_IN_PROMO = 40258;
  IDC_SHOW_FULL_URLS = 40259;
  IDC_CARET_BROWSING_TOGGLE = 40260;
  IDC_TOGGLE_QUICK_COMMANDS = 40261;
  IDC_CHROME_TIPS = 40263;
  IDC_CHROME_WHATS_NEW = 40264;
  IDC_LACROS_DATA_MIGRATION = 40265;
  IDC_PERFORMANCE = 40266;
  IDC_SPELLCHECK_SUGGESTION_0 = 41000;
  IDC_SPELLCHECK_SUGGESTION_1 = 41001;
  IDC_SPELLCHECK_SUGGESTION_2 = 41002;
  IDC_SPELLCHECK_SUGGESTION_3 = 41003;
  IDC_SPELLCHECK_SUGGESTION_4 = 41004;
  IDC_SPELLCHECK_MENU = 41005;
  IDC_SPELLCHECK_LANGUAGES_FIRST = 41006;
  IDC_SPELLCHECK_LANGUAGES_LAST = 41106;
  IDC_CHECK_SPELLING_WHILE_TYPING = 41107;
  IDC_SPELLPANEL_TOGGLE = 41109;
  IDC_SPELLCHECK_ADD_TO_DICTIONARY = 41110;
  IDC_SPELLCHECK_MULTI_LINGUAL = 41111;
  IDC_WRITING_DIRECTION_MENU = 41120;
  IDC_WRITING_DIRECTION_DEFAULT = 41121;
  IDC_WRITING_DIRECTION_LTR = 41122;
  IDC_WRITING_DIRECTION_RTL = 41123;
  IDC_TRANSLATE_ORIGINAL_LANGUAGE_BASE = 42100;
  IDC_TRANSLATE_TARGET_LANGUAGE_BASE = 42400;
  IDC_VIEW_MENU = 44000;
  IDC_FILE_MENU = 44001;
  IDC_CHROME_MENU = 44002;
  IDC_HIDE_APP = 44003;
  IDC_HISTORY_MENU = 46000;
  IDC_TAB_MENU = 46001;
  IDC_PROFILE_MAIN_MENU = 46100;
  IDC_INPUT_METHODS_MENU = 46300;
  IDC_CONTENT_CONTEXT_CUSTOM_FIRST = 47000;
  IDC_CONTENT_CONTEXT_CUSTOM_LAST = 48000;
  IDC_EXTENSIONS_CONTEXT_CUSTOM_FIRST = 49000;
  IDC_EXTENSIONS_CONTEXT_CUSTOM_LAST = 50000;
  IDC_CONTENT_CONTEXT_OPENLINKNEWTAB = 50100;
  IDC_CONTENT_CONTEXT_OPENLINKNEWWINDOW = 50101;
  IDC_CONTENT_CONTEXT_OPENLINKOFFTHERECORD = 50102;
  IDC_CONTENT_CONTEXT_SAVELINKAS = 50103;
  IDC_CONTENT_CONTEXT_COPYLINKLOCATION = 50104;
  IDC_CONTENT_CONTEXT_COPYEMAILADDRESS = 50105;
  IDC_CONTENT_CONTEXT_OPENLINKWITH = 50106;
  IDC_CONTENT_CONTEXT_COPYLINKTEXT = 50107;
  IDC_CONTENT_CONTEXT_OPENLINKINPROFILE = 50108;
  IDC_CONTENT_CONTEXT_OPENLINKBOOKMARKAPP = 50109;
  IDC_CONTENT_CONTEXT_SAVEIMAGEAS = 50110;
  IDC_CONTENT_CONTEXT_COPYIMAGELOCATION = 50111;
  IDC_CONTENT_CONTEXT_COPYIMAGE = 50112;
  IDC_CONTENT_CONTEXT_OPENIMAGENEWTAB = 50113;
  IDC_CONTENT_CONTEXT_SEARCHWEBFORIMAGE = 50114;
  IDC_CONTENT_CONTEXT_OPEN_ORIGINAL_IMAGE_NEW_TAB = 50115;
  IDC_CONTENT_CONTEXT_LOAD_IMAGE = 50116;
  IDC_CONTENT_CONTEXT_SEARCHLENSFORIMAGE = 50117;
  IDC_CONTENT_CONTEXT_TRANSLATEIMAGEWITHWEB = 50118;
  IDC_CONTENT_CONTEXT_TRANSLATEIMAGEWITHLENS = 50119;
  IDC_CONTENT_CONTEXT_SAVEAVAS = 50120;
  IDC_CONTENT_CONTEXT_COPYAVLOCATION = 50121;
  IDC_CONTENT_CONTEXT_OPENAVNEWTAB = 50122;
  IDC_CONTENT_CONTEXT_PICTUREINPICTURE = 50123;
  IDC_CONTENT_CONTEXT_PLAYPAUSE = 50130;
  IDC_CONTENT_CONTEXT_MUTE = 50131;
  IDC_CONTENT_CONTEXT_LOOP = 50132;
  IDC_CONTENT_CONTEXT_CONTROLS = 50133;
  IDC_CONTENT_CONTEXT_ROTATECW = 50134;
  IDC_CONTENT_CONTEXT_ROTATECCW = 50135;
  IDC_CONTENT_CONTEXT_COPY = 50140;
  IDC_CONTENT_CONTEXT_CUT = 50141;
  IDC_CONTENT_CONTEXT_PASTE = 50142;
  IDC_CONTENT_CONTEXT_DELETE = 50143;
  IDC_CONTENT_CONTEXT_UNDO = 50144;
  IDC_CONTENT_CONTEXT_REDO = 50145;
  IDC_CONTENT_CONTEXT_SELECTALL = 50146;
  IDC_CONTENT_CONTEXT_PASTE_AND_MATCH_STYLE = 50147;
  IDC_CONTENT_CONTEXT_COPYLINKTOTEXT = 50148;
  IDC_CONTENT_CONTEXT_RESHARELINKTOTEXT = 50149;
  IDC_CONTENT_CONTEXT_REMOVELINKTOTEXT = 50150;
  IDC_CONTENT_CONTEXT_TRANSLATE = 50151;
  IDC_CONTENT_CONTEXT_INSPECTELEMENT = 50152;
  IDC_CONTENT_CONTEXT_VIEWPAGEINFO = 50153;
  IDC_CONTENT_CONTEXT_LANGUAGE_SETTINGS = 50154;
  IDC_CONTENT_CONTEXT_LOOK_UP = 50155;
  IDC_CONTENT_CONTEXT_NO_SPELLING_SUGGESTIONS = 50156;
  IDC_CONTENT_CONTEXT_SPELLING_SUGGESTION = 50157;
  IDC_CONTENT_CONTEXT_SPELLING_TOGGLE = 50158;
  IDC_CONTENT_CONTEXT_OPEN_IN_READING_MODE = 50159;
  IDC_CONTENT_CONTEXT_INSPECTBACKGROUNDPAGE = 50161;
  IDC_CONTENT_CONTEXT_RELOAD_PACKAGED_APP = 50162;
  IDC_CONTENT_CONTEXT_RESTART_PACKAGED_APP = 50163;
  IDC_CONTENT_CONTEXT_LENS_REGION_SEARCH = 50164;
  IDC_CONTENT_CONTEXT_WEB_REGION_SEARCH = 50165;
  IDC_CONTENT_CONTEXT_GENERATEPASSWORD = 50166;
  IDC_CONTENT_CONTEXT_EXIT_FULLSCREEN = 50167;
  IDC_CONTENT_CONTEXT_SHOWALLSAVEDPASSWORDS = 50168;
  IDC_CONTENT_CONTEXT_PARTIAL_TRANSLATE = 50169;
  IDC_CONTENT_CONTEXT_RELOADFRAME = 50170;
  IDC_CONTENT_CONTEXT_VIEWFRAMESOURCE = 50171;
  IDC_CONTENT_CONTEXT_VIEWFRAMEINFO = 50172;
  IDC_CONTENT_CONTEXT_ADD_A_NOTE = 50175;
  IDC_CONTENT_CONTEXT_GOTOURL = 50180;
  IDC_CONTENT_CONTEXT_SEARCHWEBFOR = 50181;
  IDC_CONTENT_CONTEXT_OPEN_WITH1 = 50190;
  IDC_CONTENT_CONTEXT_OPEN_WITH2 = 50191;
  IDC_CONTENT_CONTEXT_OPEN_WITH3 = 50192;
  IDC_CONTENT_CONTEXT_OPEN_WITH4 = 50193;
  IDC_CONTENT_CONTEXT_OPEN_WITH5 = 50194;
  IDC_CONTENT_CONTEXT_OPEN_WITH6 = 50195;
  IDC_CONTENT_CONTEXT_OPEN_WITH7 = 50196;
  IDC_CONTENT_CONTEXT_OPEN_WITH8 = 50197;
  IDC_CONTENT_CONTEXT_OPEN_WITH9 = 50198;
  IDC_CONTENT_CONTEXT_OPEN_WITH10 = 50199;
  IDC_CONTENT_CONTEXT_OPEN_WITH11 = 50200;
  IDC_CONTENT_CONTEXT_OPEN_WITH12 = 50201;
  IDC_CONTENT_CONTEXT_OPEN_WITH13 = 50202;
  IDC_CONTENT_CONTEXT_OPEN_WITH14 = 50203;
  IDC_CONTENT_CONTEXT_EMOJI = 50210;
  IDC_BOOKMARK_BAR_OPEN_ALL = 51000;
  IDC_BOOKMARK_BAR_OPEN_ALL_NEW_WINDOW = 51001;
  IDC_BOOKMARK_BAR_OPEN_ALL_INCOGNITO = 51002;
  IDC_BOOKMARK_BAR_OPEN_INCOGNITO = 51003;
  IDC_BOOKMARK_BAR_OPEN_ALL_NEW_TAB_GROUP = 51004;
  IDC_BOOKMARK_BAR_RENAME_FOLDER = 51005;
  IDC_BOOKMARK_BAR_EDIT = 51006;
  IDC_BOOKMARK_BAR_REMOVE = 51007;
  IDC_BOOKMARK_BAR_UNDO = 51008;
  IDC_BOOKMARK_BAR_REDO = 51009;
  IDC_BOOKMARK_BAR_ADD_NEW_BOOKMARK = 51010;
  IDC_BOOKMARK_BAR_NEW_FOLDER = 51011;
  IDC_BOOKMARK_MANAGER = 51012;
  IDC_BOOKMARK_BAR_ALWAYS_SHOW = 51013;
  IDC_BOOKMARK_BAR_SHOW_APPS_SHORTCUT = 51014;
  IDC_BOOKMARK_BAR_SHOW_READING_LIST = 51015;
  IDC_BOOKMARK_BAR_SHOW_MANAGED_BOOKMARKS = 51016;
  IDC_BOOKMARK_BAR_TRACK_PRICE_FOR_SHOPPING_BOOKMARK = 51017;
  IDC_BOOKMARK_BAR_UNTRACK_PRICE_FOR_SHOPPING_BOOKMARK = 51018;
  IDC_BOOKMARK_BAR_ADD_TO_BOOKMARKS_BAR = 51019;
  IDC_BOOKMARK_BAR_REMOVE_FROM_BOOKMARKS_BAR = 51020;
  IDC_CONTENT_CONTEXT_SHARING_CLICK_TO_CALL_SINGLE_DEVICE = 51030;
  IDC_CONTENT_CONTEXT_SHARING_CLICK_TO_CALL_MULTIPLE_DEVICES = 51031;
  IDC_CONTENT_CONTEXT_SHARING_SHARED_CLIPBOARD_SINGLE_DEVICE = 51032;
  IDC_CONTENT_CONTEXT_SHARING_SHARED_CLIPBOARD_MULTIPLE_DEVICES = 51033;
  IDC_CONTENT_CONTEXT_GENERATE_QR_CODE = 51034;
  IDC_CONTENT_CONTEXT_SHARING_SUBMENU = 51035;
  IDC_CONTENT_CLIPBOARD_HISTORY_MENU = 51036;
  IDC_STATUS_TRAY_KEEP_CHROME_RUNNING_IN_BACKGROUND = 51100;
  IDC_MEDIA_ROUTER_ABOUT = 51200;
  IDC_MEDIA_ROUTER_HELP = 51201;
  IDC_MEDIA_ROUTER_LEARN_MORE = 51202;
  IDC_MEDIA_ROUTER_ALWAYS_SHOW_TOOLBAR_ACTION = 51204;
  IDC_MEDIA_ROUTER_SHOWN_BY_POLICY = 51206;
  IDC_MEDIA_ROUTER_SHOW_IN_TOOLBAR = 51207;
  IDC_MEDIA_ROUTER_TOGGLE_MEDIA_REMOTING = 51208;
  IDC_MEDIA_TOOLBAR_CONTEXT_REPORT_CAST_ISSUE = 51209;
  IDC_MEDIA_TOOLBAR_CONTEXT_SHOW_OTHER_SESSIONS = 51210;
  IDC_MEDIA_STREAM_DEVICE_STATUS_TRAY = 51300;
  IDC_MEDIA_CONTEXT_MEDIA_STREAM_CAPTURE_LIST_FIRST = 51301;
  IDC_MEDIA_CONTEXT_MEDIA_STREAM_CAPTURE_LIST_LAST = 51399;
  IDC_MEDIA_STREAM_DEVICE_ALWAYS_ALLOW = 51400;
  IDC_CONTENT_CONTEXT_PROTOCOL_HANDLER_FIRST = 52000;
  IDC_CONTENT_CONTEXT_PROTOCOL_HANDLER_LAST = 52199;
  IDC_CONTENT_CONTEXT_PROTOCOL_HANDLER_SETTINGS = 52200;
  IDC_OPEN_LINK_IN_PROFILE_FIRST = 52300;
  IDC_OPEN_LINK_IN_PROFILE_LAST = 52399;
  IDC_CONTENT_CONTEXT_START_SMART_SELECTION_ACTION1 = 52400;
  IDC_CONTENT_CONTEXT_START_SMART_SELECTION_ACTION2 = 52401;
  IDC_CONTENT_CONTEXT_START_SMART_SELECTION_ACTION3 = 52402;
  IDC_CONTENT_CONTEXT_START_SMART_SELECTION_ACTION4 = 52403;
  IDC_CONTENT_CONTEXT_START_SMART_SELECTION_ACTION5 = 52404;
  IDC_CONTENT_CONTEXT_ACCESSIBILITY_LABELS_TOGGLE = 52410;
  IDC_CONTENT_CONTEXT_ACCESSIBILITY_LABELS = 52411;
  IDC_CONTENT_CONTEXT_ACCESSIBILITY_LABELS_TOGGLE_ONCE = 52412;
  IDC_CONTENT_CONTEXT_QUICK_ANSWERS_INLINE_ANSWER = 52413;
  IDC_CONTENT_CONTEXT_QUICK_ANSWERS_INLINE_QUERY = 52414;
  IDC_RUN_SCREEN_AI_VISUAL_ANNOTATIONS = 52420;
  IDC_CONTENT_CONTEXT_PDF_OCR = 52421;
  IDC_CONTENT_CONTEXT_PDF_OCR_ALWAYS = 52422;
  IDC_CONTENT_CONTEXT_PDF_OCR_ONCE = 52423;
  IDC_TAB_SEARCH = 52500;
  IDC_TAB_SEARCH_CLOSE = 52501;
  IDC_DEBUG_TOGGLE_TABLET_MODE = 52510;
  IDC_DEBUG_PRINT_VIEW_TREE = 52511;
  IDC_DEBUG_PRINT_VIEW_TREE_DETAILS = 52512;
  IDC_CONTENT_CONTEXT_AUTOFILL_FEEDBACK = 52990;
  IDC_CONTENT_CONTEXT_AUTOFILL_CUSTOM_FIRST = 53000;
  IDC_CONTENT_CONTEXT_AUTOFILL_CUSTOM_LAST = 53250;
  IDC_LIVE_CAPTION = 53251;
  IDC_MANAGE_HID_DEVICES_FIRST = 53260;
  IDC_MANAGE_HID_DEVICES_LAST = 53269;
  IDC_FIRST_UNBOUNDED_MENU = 0;

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
  TT_AUTO_BOOKMARK        = 2;
  TT_AUTO_SUBFRAME        = 3;
  TT_MANUAL_SUBFRAME      = 4;
  TT_GENERATED            = 5;
  TT_AUTO_TOPLEVEL        = 6;
  TT_FORM_SUBMIT          = 7;
  TT_RELOAD               = 8;
  TT_KEYWORD              = 9;
  TT_KEYWORD_GENERATED    = 10;
  TT_SOURCE_MASK          = $000000FF;
  TT_BLOCKED_FLAG         = $00800000;
  TT_FORWARD_BACK_FLAG    = $01000000;
  TT_DIRECT_LOAD_FLAG     = $02000000;
  TT_HOME_PAGE_FLAG       = $04000000;
  TT_FROM_API_FLAG        = $08000000;
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
  EVENTFLAG_IS_REPEAT            = 1 shl 13;

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
  CM_MEDIAFLAG_NONE                   = 0;
  CM_MEDIAFLAG_IN_ERROR               = 1 shl 0;
  CM_MEDIAFLAG_PAUSED                 = 1 shl 1;
  CM_MEDIAFLAG_MUTED                  = 1 shl 2;
  CM_MEDIAFLAG_LOOP                   = 1 shl 3;
  CM_MEDIAFLAG_CAN_SAVE               = 1 shl 4;
  CM_MEDIAFLAG_HAS_AUDIO              = 1 shl 5;
  CM_MEDIAFLAG_CAN_TOGGLE_CONTROLS    = 1 shl 6;
  CM_MEDIAFLAG_CONTROLS               = 1 shl 7;
  CM_MEDIAFLAG_CAN_PRINT              = 1 shl 8;
  CM_MEDIAFLAG_CAN_ROTATE             = 1 shl 9;
  CM_MEDIAFLAG_CAN_PICTURE_IN_PICTURE = 1 shl 10;
  CM_MEDIAFLAG_PICTURE_IN_PICTURE     = 1 shl 11;
  CM_MEDIAFLAG_CAN_LOOP               = 1 shl 12;

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
  CM_EDITFLAG_CAN_EDIT_RICHLY        = 1 shl 8;

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

  // /include/internal/cef_types.h (cef_chrome_toolbar_type_t)
  CEF_CTT_NONE       = 1;
  CEF_CTT_NORMAL     = 2;
  CEF_CTT_LOCATION   = 3;

  // /include/internal/cef_types.h (cef_docking_mode_t)
  CEF_DOCKING_MODE_TOP_LEFT      = 1;
  CEF_DOCKING_MODE_TOP_RIGHT     = 2;
  CEF_DOCKING_MODE_BOTTOM_LEFT   = 3;
  CEF_DOCKING_MODE_BOTTOM_RIGHT  = 4;
  CEF_DOCKING_MODE_CUSTOM        = 5;

  // /include/internal/cef_types.h (cef_show_state_t)
  CEF_SHOW_STATE_NORMAL      = 1;
  CEF_SHOW_STATE_MINIMIZED   = 2;
  CEF_SHOW_STATE_MAXIMIZED   = 3;
  CEF_SHOW_STATE_FULLSCREEN  = 4;

  // /include/internal/cef_types.h (cef_quick_menu_edit_state_flags_t)
  QM_EDITFLAG_NONE         = 0;
  QM_EDITFLAG_CAN_ELLIPSIS = 1 shl 0;
  QM_EDITFLAG_CAN_CUT      = 1 shl 1;
  QM_EDITFLAG_CAN_COPY     = 1 shl 2;
  QM_EDITFLAG_CAN_PASTE    = 1 shl 3;

  // /include/internal/cef_types.h (cef_touch_handle_state_flags_t)
  CEF_THS_FLAG_NONE        = 0;
  CEF_THS_FLAG_ENABLED     = 1 shl 0;
  CEF_THS_FLAG_ORIENTATION = 1 shl 1;
  CEF_THS_FLAG_ORIGIN      = 1 shl 2;
  CEF_THS_FLAG_ALPHA       = 1 shl 3;

  // /include/internal/cef_types.h (cef_media_access_permission_types_t)
  CEF_MEDIA_PERMISSION_NONE                  = 0;
  CEF_MEDIA_PERMISSION_DEVICE_AUDIO_CAPTURE  = 1 shl 0;
  CEF_MEDIA_PERMISSION_DEVICE_VIDEO_CAPTURE  = 1 shl 1;
  CEF_MEDIA_PERMISSION_DESKTOP_AUDIO_CAPTURE = 1 shl 2;
  CEF_MEDIA_PERMISSION_DESKTOP_VIDEO_CAPTURE = 1 shl 3;

  // /include/internal/cef_types.h (cef_permission_request_types_t)
  CEF_PERMISSION_TYPE_NONE                       = 0;
  CEF_PERMISSION_TYPE_ACCESSIBILITY_EVENTS       = 1 shl 0;
  CEF_PERMISSION_TYPE_AR_SESSION                 = 1 shl 1;
  CEF_PERMISSION_TYPE_CAMERA_PAN_TILT_ZOOM       = 1 shl 2;
  CEF_PERMISSION_TYPE_CAMERA_STREAM              = 1 shl 3;
  CEF_PERMISSION_TYPE_CLIPBOARD                  = 1 shl 4;
  CEF_PERMISSION_TYPE_TOP_LEVEL_STORAGE_ACCESS   = 1 shl 5;
  CEF_PERMISSION_TYPE_DISK_QUOTA                 = 1 shl 6;
  CEF_PERMISSION_TYPE_LOCAL_FONTS                = 1 shl 7;
  CEF_PERMISSION_TYPE_GEOLOCATION                = 1 shl 8;
  CEF_PERMISSION_TYPE_IDLE_DETECTION             = 1 shl 9;
  CEF_PERMISSION_TYPE_MIC_STREAM                 = 1 shl 10;
  CEF_PERMISSION_TYPE_MIDI_SYSEX                 = 1 shl 11;
  CEF_PERMISSION_TYPE_MULTIPLE_DOWNLOADS         = 1 shl 12;
  CEF_PERMISSION_TYPE_NOTIFICATIONS              = 1 shl 13;
  CEF_PERMISSION_TYPE_PROTECTED_MEDIA_IDENTIFIER = 1 shl 14;
  CEF_PERMISSION_TYPE_REGISTER_PROTOCOL_HANDLER  = 1 shl 15;
  CEF_PERMISSION_TYPE_SECURITY_ATTESTATION       = 1 shl 16;
  CEF_PERMISSION_TYPE_STORAGE_ACCESS             = 1 shl 17;
  CEF_PERMISSION_TYPE_U2F_API_REQUEST            = 1 shl 18;
  CEF_PERMISSION_TYPE_VR_SESSION                 = 1 shl 19;
  CEF_PERMISSION_TYPE_WINDOW_MANAGEMENT          = 1 shl 20;

  // /include/cef_api_hash.h (used as "cef_api_hash" parameters)
  CEF_API_HASH_PLATFORM  = 0;
  CEF_API_HASH_UNIVERSAL = 1;
  CEF_COMMIT_HASH        = 2;


//******************************************************
//****************** OTHER CONSTANTS *******************
//******************************************************

  ABOUTBLANK_URI = 'about:blank';

  DEVTOOLS_WINDOWNAME = 'DevTools';

  CEF_PROXYTYPE_DIRECT        = 0;
  CEF_PROXYTYPE_AUTODETECT    = 1;
  CEF_PROXYTYPE_SYSTEM        = 2;
  CEF_PROXYTYPE_FIXED_SERVERS = 3;
  CEF_PROXYTYPE_PAC_SCRIPT    = 4;

  // /include/base/cef_logging.h (LogSeverity)
  // Used in the severity parameter in the 'cef_log' function, also known as 'CefLog' in CEF4Delphi.
  // The log severities are used to index into the array of names, see log_severity_names.
  // /base/allocator/partition_allocator/partition_alloc_base/logging.h
  // /base/logging.cc
  CEF_LOG_SEVERITY_VERBOSE = -1;
  CEF_LOG_SEVERITY_INFO    = 0;
  CEF_LOG_SEVERITY_WARNING = 1;
  CEF_LOG_SEVERITY_ERROR   = 2;
  CEF_LOG_SEVERITY_FATAL   = 3;   // This severity log level causes a crash

  CEF_MAX_CONNECTIONS_PER_PROXY_DEFAULT_VALUE = 32;
  CEF_MAX_CONNECTIONS_PER_PROXY_MIN_VALUE     = 7;
  CEF_MAX_CONNECTIONS_PER_PROXY_MAX_VALUE     = 99;

  CEF_COOKIE_PREF_DEFAULT = 0;
  CEF_COOKIE_PREF_ALLOW   = 1;
  CEF_COOKIE_PREF_BLOCK   = 2;

  CEF_DEFAULT_ENABLEFOCUSDELAY = 500; // delay in ms to enable the browser focus

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
  CEF_INVALIDATE         = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A0D;
  CEF_FOCUSENABLED       = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A0E;

  // Lazarus and some old Delphi versions don't have these message contants
  {$IF NOT DECLARED(WM_TOUCH)}
  WM_TOUCH                 = $0240;
  {$IFEND}
  {$IF NOT DECLARED(WM_POINTERUPDATE)}
  WM_POINTERUPDATE         = $0245;
  WM_POINTERDOWN           = $0246;
  WM_POINTERUP             = $0247;
  {$IFEND}

  // Default values for the Windowsless framerate setting in TChromiumOptions
  // The values are frames per second.
  CEF_OSR_FRAMERATE_DEFAULT                  = 30;  // Used when the shared textures are disabled.
  CEF_OSR_SHARED_TEXTURES_FRAMERATE_DEFAULT  = 60;  // Used when the shared textures are enabled.

  CEF_TIMER_MINIMUM            = $0000000A;
  CEF_TIMER_MAXIMUM            = $7FFFFFFF;
  CEF_TIMER_MAXDELAY           = 1000 div CEF_OSR_FRAMERATE_DEFAULT;
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

  // This constant is defined by Chromium in chrome/app/main_dll_loader_win.cc
  // It's used with SetProcessShutdownParameters to set a shutdown priority for the
  // subprocesses. $280 is the default value for applications.
  CHROMIUM_NONBROWSERSHUTDOWNPRIORITY = $280;

  {$IF NOT DECLARED(INFINITE)}
  INFINITE = Cardinal($FFFFFFFF);
  {$IFEND}

  {$IFDEF CEF4DELHI_ALLOC_DEBUG}
  CEF4DELPHI_ALLOC_PADDING = Pointer($44332211); // Some random value used as padding
  {$ENDIF}

  // If any of the platform IDs are not defined then we set them as 0 to avoid build errors on older Delphi versions.
  {$IF NOT DECLARED(pidWin32)}
  pidWin32 = 0;
  {$IFEND}

  {$IF NOT DECLARED(pidWin64)}
  pidWin64 = 0;
  {$IFEND}

  {$IF NOT DECLARED(pfidWindows)}
  pfidWindows = pidWin32 or pidWin64;
  {$IFEND}

  {$IF NOT DECLARED(pidOSX32)}
  pidOSX32 = 0;
  {$IFEND}

  {$IF NOT DECLARED(pidOSX64)}
  pidOSX64 = 0;
  {$IFEND}

  {$IF NOT DECLARED(pidOSXArm64)}
  pidOSXArm64 = 0;
  {$IFEND}

  {$IF NOT DECLARED(pfidOSX)}
  pfidOSX = pidOSX32 or pidOSX64 or pidOSXArm64;
  {$IFEND}

  {$IF NOT DECLARED(pidLinux64)}
  pidLinux64 = 0;
  {$IFEND}

  {$IF NOT DECLARED(pfidLinux)}
  pfidLinux = pidLinux64;
  {$IFEND}

  implementation

end.


