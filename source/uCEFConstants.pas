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
//        Copyright © 2018 Salvador Diaz Fau. All rights reserved.
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

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Messages;
  {$ELSE}
  Messages;
  {$ENDIF}

const
  // /include/internal/cef_types.h (cef_errorcode_t)
  ERR_NONE                             = 0;
  ERR_FAILED                           = -2;
  ERR_ABORTED                          = -3;
  ERR_INVALID_ARGUMENT                 = -4;
  ERR_INVALID_HANDLE                   = -5;
  ERR_FILE_NOT_FOUND                   = -6;
  ERR_TIMED_OUT                        = -7;
  ERR_FILE_TOO_BIG                     = -8;
  ERR_UNEXPECTED                       = -9;
  ERR_ACCESS_DENIED                    = -10;
  ERR_NOT_IMPLEMENTED                  = -11;
  ERR_CONNECTION_CLOSED                = -100;
  ERR_CONNECTION_RESET                 = -101;
  ERR_CONNECTION_REFUSED               = -102;
  ERR_CONNECTION_ABORTED               = -103;
  ERR_CONNECTION_FAILED                = -104;
  ERR_NAME_NOT_RESOLVED                = -105;
  ERR_INTERNET_DISCONNECTED            = -106;
  ERR_SSL_PROTOCOL_ERROR               = -107;
  ERR_ADDRESS_INVALID                  = -108;
  ERR_ADDRESS_UNREACHABLE              = -109;
  ERR_SSL_CLIENT_AUTH_CERT_NEEDED      = -110;
  ERR_TUNNEL_CONNECTION_FAILED         = -111;
  ERR_NO_SSL_VERSIONS_ENABLED          = -112;
  ERR_SSL_VERSION_OR_CIPHER_MISMATCH   = -113;
  ERR_SSL_RENEGOTIATION_REQUESTED      = -114;
  ERR_CERT_COMMON_NAME_INVALID         = -200;
  ERR_CERT_BEGIN                       = ERR_CERT_COMMON_NAME_INVALID;
  ERR_CERT_DATE_INVALID                = -201;
  ERR_CERT_AUTHORITY_INVALID           = -202;
  ERR_CERT_CONTAINS_ERRORS             = -203;
  ERR_CERT_NO_REVOCATION_MECHANISM     = -204;
  ERR_CERT_UNABLE_TO_CHECK_REVOCATION  = -205;
  ERR_CERT_REVOKED                     = -206;
  ERR_CERT_INVALID                     = -207;
  ERR_CERT_WEAK_SIGNATURE_ALGORITHM    = -208;
  ERR_CERT_NON_UNIQUE_NAME             = -210;
  ERR_CERT_WEAK_KEY                    = -211;
  ERR_CERT_NAME_CONSTRAINT_VIOLATION   = -212;
  ERR_CERT_VALIDITY_TOO_LONG           = -213;
  ERR_CERT_END                         = ERR_CERT_VALIDITY_TOO_LONG;
  ERR_INVALID_URL                      = -300;
  ERR_DISALLOWED_URL_SCHEME            = -301;
  ERR_UNKNOWN_URL_SCHEME               = -302;
  ERR_TOO_MANY_REDIRECTS               = -310;
  ERR_UNSAFE_REDIRECT                  = -311;
  ERR_UNSAFE_PORT                      = -312;
  ERR_INVALID_RESPONSE                 = -320;
  ERR_INVALID_CHUNKED_ENCODING         = -321;
  ERR_METHOD_NOT_SUPPORTED             = -322;
  ERR_UNEXPECTED_PROXY_AUTH            = -323;
  ERR_EMPTY_RESPONSE                   = -324;
  ERR_RESPONSE_HEADERS_TOO_BIG         = -325;
  ERR_CACHE_MISS                       = -400;
  ERR_INSECURE_RESPONSE                = -501;

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
  UR_FLAG_ALLOW_STORED_CREDENTIALS = 1 shl 2;
  UR_FLAG_REPORT_UPLOAD_PROGRESS   = 1 shl 3;
  UR_FLAG_NO_DOWNLOAD_DATA         = 1 shl 4;
  UR_FLAG_NO_RETRY_ON_5XX          = 1 shl 5;
  UR_FLAG_STOP_ON_REDIRECT         = 1 shl 6;

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
  UU_SPOOFING_AND_CONTROL_CHARS               = 1 shl 4;
  UU_REPLACE_PLUS_WITH_SPACE                  = 1 shl 5;

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
  LOGSEVERITY_DISABLE  = 99;

  // /include/internal/cef_types.h (cef_duplex_mode_t)
  DUPLEX_MODE_UNKNOWN    = -1;
  DUPLEX_MODE_SIMPLEX    = 0;
  DUPLEX_MODE_LONG_EDGE  = 1;
  DUPLEX_MODE_SHORT_EDGE = 2;


//******************************************************
//****************** OTHER CONSTANTS *******************
//******************************************************

  DEVTOOLS_WINDOWNAME = 'DevTools';

  CEF_PROXYTYPE_DIRECT        = 0;
  CEF_PROXYTYPE_AUTODETECT    = 1;
  CEF_PROXYTYPE_SYSTEM        = 2;
  CEF_PROXYTYPE_FIXED_SERVERS = 3;
  CEF_PROXYTYPE_PAC_SCRIPT    = 4;

  CEF_CONTENT_SETTING_DEFAULT       = 0;
  CEF_CONTENT_SETTING_ALLOW         = 1;
  CEF_CONTENT_SETTING_BLOCK         = 2;
  CEF_CONTENT_SETTING_ASK           = 3;
  CEF_CONTENT_SETTING_SESSION_ONLY  = 4;
  CEF_CONTENT_SETTING_NUM_SETTINGS  = 5;

  // Used in the severity parameter in the 'cef_log' function, also known as 'CefLog' in CEF4Delphi.
  CEF_LOG_SEVERITY_INFO    = 0;
  CEF_LOG_SEVERITY_WARNING = 1;
  CEF_LOG_SEVERITY_ERROR   = 2;

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
  ZOOM_STEP_MIN = ZOOM_STEP_25;
  ZOOM_STEP_MAX = ZOOM_STEP_500;
  ZOOM_STEP_DEF = ZOOM_STEP_100;

  {$IFDEF MSWINDOWS}
  CEF_PREFERENCES_SAVED  = WM_APP + $A00;
  CEF_DOONCLOSE          = WM_APP + $A01;
  CEF_STARTDRAGGING      = WM_APP + $A02;
  CEF_AFTERCREATED       = WM_APP + $A03;
  CEF_PENDINGRESIZE      = WM_APP + $A04;
  CEF_PUMPHAVEWORK       = WM_APP + $A05;
  CEF_DESTROY            = WM_APP + $A06;
  CEF_DOONBEFORECLOSE    = WM_APP + $A07;   
  CEF_PENDINGINVALIDATE  = WM_APP + $A08;
  {$ENDIF}

  CEF_TIMER_MINIMUM            = $0000000A;
  CEF_TIMER_MAXIMUM            = $7FFFFFFF;
  CEF_TIMER_MAXDELAY           = 1000 div 30; // 30fps
  CEF_TIMER_DEPLETEWORK_CYCLES = 10;
  CEF_TIMER_DEPLETEWORK_DELAY  = 50;

  CEF4DELPHI_URL = 'https://github.com/salvadordf/CEF4Delphi';
  CRLF           = #13 + #10;

  IMAGE_FILE_MACHINE_UNKNOWN  = 0;
  IMAGE_FILE_MACHINE_I386     = $014c; // Intel x86
  IMAGE_FILE_MACHINE_IA64     = $0200; // Intel Itanium Processor Family (IPF)
  IMAGE_FILE_MACHINE_AMD64    = $8664; // x64 (AMD64 or EM64T)
  IMAGE_FILE_MACHINE_R3000_BE = $160;  // MIPS big-endian
  IMAGE_FILE_MACHINE_R3000    = $162;  // MIPS little-endian, 0x160 big-endian
  IMAGE_FILE_MACHINE_R4000    = $166;  // MIPS little-endian
  IMAGE_FILE_MACHINE_R10000   = $168;  // MIPS little-endian
  IMAGE_FILE_MACHINE_ALPHA    = $184;  // Alpha_AXP }
  IMAGE_FILE_MACHINE_POWERPC  = $1F0;  // IBM PowerPC Little-Endian

implementation

end.


