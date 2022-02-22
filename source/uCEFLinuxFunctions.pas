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
//        Copyright © 2022 Salvador Diaz Fau. All rights reserved.
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

unit uCEFLinuxFunctions;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF LINUX}
    {$IFDEF FPC}
      ctypes, keysym, xf86keysym, x, xlib,
      {$IFDEF LCLGTK2}gtk2, glib2, gdk2, gtk2proc, gtk2int, Gtk2Def, gdk2x, Gtk2Extra,{$ENDIF}
    {$ENDIF}
  {$ENDIF}
  uCEFLinuxTypes, uCEFTypes;

{$IFDEF LINUX}
procedure GdkEventKeyToCEFKeyEvent(GdkEvent: PGdkEventKey; var aCEFKeyEvent : TCEFKeyEvent);
function  KeyboardCodeFromXKeysym(keysym : uint32) : integer;
function  GetCefStateModifiers(state : uint32) : integer;
function  GdkEventToWindowsKeyCode(Event: PGdkEventKey) : integer;
function  GetWindowsKeyCodeWithoutLocation(key_code : integer) : integer;
function  GetControlCharacter(windows_key_code : integer; shift : boolean) : integer;
{$IFDEF FMX}
type
   TXErrorHandler   = function (para1:PDisplay; para2:PXErrorEvent):longint; cdecl;
   TXIOErrorHandler = function (para1:PDisplay):longint; cdecl;

function XSetErrorHandler(para1:TXErrorHandler):TXErrorHandler; cdecl; external 'libX11.so';
function XSetIOErrorHandler(para1:TXIOErrorHandler):TXIOErrorHandler; cdecl; external 'libX11.so';

function gdk_keyval_to_unicode(keyval: guint): guint32; cdecl; external 'libgdk-3.so';
function g_signal_connect_data(instance: gpointer; detailed_signal: Pgchar; c_handler: TGCallback; data: gpointer; destroy_data: TGClosureNotify; connect_flags: TGConnectFlags): gulong; cdecl; external 'libgobject-2.0.so';
function g_signal_connect(instance: gpointer; detailed_signal: Pgchar; c_handler: TGCallback; data: gpointer): gulong; overload;
function g_signal_connect(instance: gpointer; const detailed_signal: AnsiString; c_handler: TGCallback; data: gpointer): gulong; overload;
function gdk_screen_width:gint; cdecl; external 'libgdk-3.so';
function gdk_screen_width_mm:gint; cdecl; external 'libgdk-3.so';
function gdk_screen_get_default:PGdkScreen; cdecl; external 'libgdk-3.so';
function gdk_screen_get_resolution(screen:PGdkScreen):gdouble; cdecl; external 'libgdk-3.so';
{$ENDIF}
{$IFDEF FPC}
procedure ShowX11Message(const aMessage : string);
{$ENDIF}{$ENDIF}

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFLinuxConstants, uCEFConstants;

{$IFDEF LINUX}
function KeyboardCodeFromXKeysym(keysym : uint32) : integer;
begin
  case keysym of
    XK_BackSpace:
      Result := VKEY_BACK;
    XK_Delete,
    XK_KP_Delete:
      Result := VKEY_DELETE;
    XK_Tab,
    XK_KP_Tab,
    XK_ISO_Left_Tab,
    XK_3270_BackTab:
      Result := VKEY_TAB;
    XK_Linefeed,
    XK_Return,
    XK_KP_Enter,
    XK_ISO_Enter:
      Result := VKEY_Return;
    XK_Clear,
    XK_KP_Begin:
      Result := VKEY_CLEAR;
    XK_KP_Space,
    XK_space:
      Result := VKEY_SPACE;
    XK_Home,
    XK_KP_Home:
      Result := VKEY_HOME;
    XK_End,
    XK_KP_End:
      Result := VKEY_END;
    XK_Page_Up,
    XK_KP_Page_Up:
      Result := VKEY_PRIOR;
    XK_Page_Down,
    XK_KP_Page_Down:
      Result := VKEY_NEXT;
    XK_Left,
    XK_KP_Left:
      Result := VKEY_LEFT;
    XK_Right,
    XK_KP_Right:
      Result := VKEY_RIGHT;
    XK_Down,
    XK_KP_Down:
      Result := VKEY_DOWN;
    XK_Up,
    XK_KP_Up:
      Result := VKEY_UP;
    XK_Escape:
      Result := VKEY_ESCAPE;
    XK_Kana_Lock,
    XK_Kana_Shift:
      Result := VKEY_KANA;
    XK_Hangul:
      Result := VKEY_HANGUL;
    XK_Hangul_Hanja:
      Result := VKEY_HANJA;
    XK_Kanji:
      Result := VKEY_KANJI;
    XK_Henkan:
      Result := VKEY_CONVERT;
    XK_Muhenkan:
      Result := VKEY_NONCONVERT;
    XK_Zenkaku_Hankaku:
      Result := VKEY_DBE_DBCSCHAR;
    XKc_A,
    XK_a:
      Result := VKEY_A;
    XKc_B,
    XK_b:
      Result :=  VKEY_B;
    XKc_C,
    XK_c:
      Result :=  VKEY_C;
    XKc_D,
    XK_d:
      Result :=  VKEY_D;
    XKc_E,
    XK_e:
      Result :=  VKEY_E;
    XKc_F,
    XK_f:
      Result :=  VKEY_F;
    XKc_G,
    XK_g:
      Result :=  VKEY_G;
    XKc_H,
    XK_h:
      Result :=  VKEY_H;
    XKc_I,
    XK_i:
      Result :=  VKEY_I;
    XKc_J,
    XK_j:
      Result :=  VKEY_J;
    XKc_K,
    XK_k:
      Result :=  VKEY_K;
    XKc_L,
    XK_l:
      Result :=  VKEY_L;
    XKc_M,
    XK_m:
      Result :=  VKEY_M;
    XKc_N,
    XK_n:
      Result :=  VKEY_N;
    XKc_O,
    XK_o:
      Result :=  VKEY_O;
    XKc_P,
    XK_p:
      Result :=  VKEY_P;
    XKc_Q,
    XK_q:
      Result :=  VKEY_Q;
    XKc_R,
    XK_r:
      Result :=  VKEY_R;
    XKc_S,
    XK_s:
      Result :=  VKEY_S;
    XKc_T,
    XK_t:
      Result :=  VKEY_T;
    XKc_U,
    XK_u:
      Result :=  VKEY_U;
    XKc_V,
    XK_v:
      Result :=  VKEY_V;
    XKc_W,
    XK_w:
      Result :=  VKEY_W;
    XKc_X,
    XK_x:
      Result :=  VKEY_X;
    XKc_Y,
    XK_y:
      Result :=  VKEY_Y;
    XKc_Z,
    XK_z:
      Result :=  VKEY_Z;
    XK_0,
    XK_1,
    XK_2,
    XK_3,
    XK_4,
    XK_5,
    XK_6,
    XK_7,
    XK_8,
    XK_9:
      Result := VKEY_0 + (keysym - XK_0);
    XK_parenright:
      Result :=  VKEY_0;
    XK_exclam:
      Result :=  VKEY_1;
    XK_at:
      Result :=  VKEY_2;
    XK_numbersign:
      Result :=  VKEY_3;
    XK_dollar:
      Result :=  VKEY_4;
    XK_percent:
      Result :=  VKEY_5;
    XK_asciicircum:
      Result :=  VKEY_6;
    XK_ampersand:
      Result :=  VKEY_7;
    XK_asterisk:
      Result :=  VKEY_8;
    XK_parenleft:
      Result :=  VKEY_9;
    XK_KP_0,
    XK_KP_1,
    XK_KP_2,
    XK_KP_3,
    XK_KP_4,
    XK_KP_5,
    XK_KP_6,
    XK_KP_7,
    XK_KP_8,
    XK_KP_9:
      Result :=  VKEY_NUMPAD0 + (keysym - XK_KP_0);
    XK_multiply,
    XK_KP_Multiply:
      Result :=  VKEY_MULTIPLY;
    XK_KP_Add:
      Result :=  VKEY_ADD;
    XK_KP_Separator:
      Result :=  VKEY_SEPARATOR;
    XK_KP_Subtract:
      Result :=  VKEY_SUBTRACT;
    XK_KP_Decimal:
      Result :=  VKEY_DECIMAL;
    XK_KP_Divide:
      Result :=  VKEY_DIVIDE;
    XK_KP_Equal,
    XK_equal,
    XK_plus:
      Result :=  VKEY_OEM_PLUS;
    XK_comma,
    XK_less:
      Result :=  VKEY_OEM_COMMA;
    XK_minus,
    XK_underscore:
      Result :=  VKEY_OEM_MINUS;
    XK_greater,
    XK_period:
      Result :=  VKEY_OEM_PERIOD;
    XK_colon,
    XK_semicolon:
      Result :=  VKEY_OEM_1;
    XK_question,
    XK_slash:
      Result :=  VKEY_OEM_2;
    XK_asciitilde,
    XK_quoteleft:
      Result :=  VKEY_OEM_3;
    XK_bracketleft,
    XK_braceleft:
      Result :=  VKEY_OEM_4;
    XK_backslash,
    XK_bar:
      Result :=  VKEY_OEM_5;
    XK_bracketright,
    XK_braceright:
      Result :=  VKEY_OEM_6;
    XK_quoteright,
    XK_quotedbl:
      Result :=  VKEY_OEM_7;
    XK_ISO_Level5_Shift:
      Result :=  VKEY_OEM_8;
    XK_Shift_L,
    XK_Shift_R:
      Result :=  VKEY_SHIFT;
    XK_Control_L,
    XK_Control_R:
      Result :=  VKEY_CONTROL;
    XK_Meta_L,
    XK_Meta_R,
    XK_Alt_L,
    XK_Alt_R:
      Result :=  VKEY_MENU;
    XK_ISO_Level3_Shift:
      Result :=  VKEY_ALTGR;
    XK_Multi_key:
      Result :=  VKEY_COMPOSE;
    XK_Pause:
      Result :=  VKEY_PAUSE;
    XK_Caps_Lock:
      Result :=  VKEY_CAPITAL;
    XK_Num_Lock:
      Result :=  VKEY_NUMLOCK;
    XK_Scroll_Lock:
      Result :=  VKEY_SCROLL;
    XK_Select:
      Result :=  VKEY_SELECT;
    XK_Print:
      Result :=  VKEY_PRINT;
    XK_Execute:
      Result :=  VKEY_EXECUTE;
    XK_Insert,
    XK_KP_Insert:
      Result :=  VKEY_INSERT;
    XK_Help:
      Result :=  VKEY_HELP;
    XK_Super_L:
      Result :=  VKEY_LWIN;
    XK_Super_R:
      Result :=  VKEY_RWIN;
    XK_Menu:
      Result :=  VKEY_APPS;
    XK_F1,
    XK_F2,
    XK_F3,
    XK_F4,
    XK_F5,
    XK_F6,
    XK_F7,
    XK_F8,
    XK_F9,
    XK_F10,
    XK_F11,
    XK_F12,
    XK_F13,
    XK_F14,
    XK_F15,
    XK_F16,
    XK_F17,
    XK_F18,
    XK_F19,
    XK_F20,
    XK_F21,
    XK_F22,
    XK_F23,
    XK_F24:
      Result := VKEY_F1 + (keysym - XK_F1);
    XK_KP_F1,
    XK_KP_F2,
    XK_KP_F3,
    XK_KP_F4:
      Result := VKEY_F1 + (keysym - XK_KP_F1);
    XK_guillemotleft,
    XK_guillemotright,
    XK_degree,
    XK_ugrave,
    XKc_Ugrave,
    XK_brokenbar:
      Result :=  VKEY_OEM_102;
    XF86XK_Tools:
      Result :=  VKEY_F13;
    XF86XK_Launch5:
      Result :=  VKEY_F14;
    XF86XK_Launch6:
      Result :=  VKEY_F15;
    XF86XK_Launch7:
      Result :=  VKEY_F16;
    XF86XK_Launch8:
      Result :=  VKEY_F17;
    XF86XK_Launch9:
      Result :=  VKEY_F18;
    XF86XK_Refresh,
    XF86XK_History,
    XF86XK_OpenURL,
    XF86XK_AddFavorite,
    XF86XK_Go,
    XF86XK_ZoomIn,
    XF86XK_ZoomOut:
      Result :=  VKEY_UNKNOWN;
    XF86XK_Back:
      Result :=  VKEY_BROWSER_BACK;
    XF86XK_Forward:
      Result :=  VKEY_BROWSER_FORWARD;
    XF86XK_Reload:
      Result :=  VKEY_BROWSER_REFRESH;
    XF86XK_Stop:
      Result :=  VKEY_BROWSER_STOP;
    XF86XK_Search:
      Result :=  VKEY_BROWSER_SEARCH;
    XF86XK_Favorites:
      Result :=  VKEY_BROWSER_FAVORITES;
    XF86XK_HomePage:
      Result :=  VKEY_BROWSER_HOME;
    XF86XK_AudioMute:
      Result :=  VKEY_VOLUME_MUTE;
    XF86XK_AudioLowerVolume:
      Result :=  VKEY_VOLUME_DOWN;
    XF86XK_AudioRaiseVolume:
      Result :=  VKEY_VOLUME_UP;
    XF86XK_AudioNext:
      Result :=  VKEY_MEDIA_NEXT_TRACK;
    XF86XK_AudioPrev:
      Result :=  VKEY_MEDIA_PREV_TRACK;
    XF86XK_AudioStop:
      Result :=  VKEY_MEDIA_STOP;
    XF86XK_AudioPlay:
      Result :=  VKEY_MEDIA_PLAY_PAUSE;
    XF86XK_Mail:
      Result :=  VKEY_MEDIA_LAUNCH_MAIL;
    XF86XK_LaunchA:
      Result :=  VKEY_MEDIA_LAUNCH_APP1;
    XF86XK_LaunchB,
    XF86XK_Calculator:
      Result :=  VKEY_MEDIA_LAUNCH_APP2;
    XF86XK_WLAN:
      Result :=  VKEY_WLAN;
    XF86XK_PowerOff:
      Result :=  VKEY_POWER;
    XF86XK_MonBrightnessDown:
      Result :=  VKEY_BRIGHTNESS_DOWN;
    XF86XK_MonBrightnessUp:
      Result :=  VKEY_BRIGHTNESS_UP;
    XF86XK_KbdBrightnessDown:
      Result :=  VKEY_KBD_BRIGHTNESS_DOWN;
    XF86XK_KbdBrightnessUp:
      Result :=  VKEY_KBD_BRIGHTNESS_UP;
    else Result :=  VKEY_UNKNOWN;
  end;
end;

function GetCefStateModifiers(state : uint32) : integer;
begin
  Result := EVENTFLAG_NONE;

  if ((state and GDK_SHIFT_MASK) <> 0) then
    Result := Result or EVENTFLAG_SHIFT_DOWN;

  if ((state and GDK_LOCK_MASK) <> 0) then
    Result := Result or EVENTFLAG_CAPS_LOCK_ON;

  if ((state and GDK_CONTROL_MASK) <> 0) then
    Result := Result or EVENTFLAG_CONTROL_DOWN;

  if ((state and GDK_MOD1_MASK) <> 0) then
    Result := Result or EVENTFLAG_ALT_DOWN;

  if ((state and GDK_BUTTON1_MASK) <> 0) then
    Result := Result or EVENTFLAG_LEFT_MOUSE_BUTTON;

  if ((state and GDK_BUTTON2_MASK) <> 0) then
    Result := Result or EVENTFLAG_MIDDLE_MOUSE_BUTTON;

  if ((state and GDK_BUTTON3_MASK) <> 0) then
    Result := Result or EVENTFLAG_RIGHT_MOUSE_BUTTON;
end;

function GdkEventToWindowsKeyCode(event: PGdkEventKey) : integer;
var
  windows_key_code, keyval : integer;
begin
  windows_key_code := KeyboardCodeFromXKeysym(event^.keyval);
  if (windows_key_code <> 0) then
    begin
      Result := windows_key_code;
      exit;
    end;

  if (event^.hardware_keycode < length(kHardwareCodeToGDKKeyval)) then
    begin
      keyval := kHardwareCodeToGDKKeyval[event^.hardware_keycode];
      if (keyval <> 0) then
        begin
          Result := KeyboardCodeFromXKeysym(keyval);
          exit;
        end;
    end;

  Result := KeyboardCodeFromXKeysym(event^.keyval);
end;

function GetWindowsKeyCodeWithoutLocation(key_code : integer) : integer;
begin
  case key_code of
    VKEY_LCONTROL, VKEY_RCONTROL : Result := VKEY_CONTROL;
    VKEY_LSHIFT, VKEY_RSHIFT     : Result := VKEY_SHIFT;
    VKEY_LMENU, VKEY_RMENU       : Result := VKEY_MENU;
    else                           Result := key_code;
  end;
end;

function GetControlCharacter(windows_key_code : integer; shift : boolean) : integer;
begin
  if (windows_key_code >= VKEY_A) and (windows_key_code <= VKEY_Z) then
    Result := windows_key_code - VKEY_A + 1
   else
    if shift then
      case windows_key_code of
        VKEY_2         : Result := 0;
        VKEY_6         : Result := $1E;
        VKEY_OEM_MINUS : Result := $1F;
        else             Result := 0;
      end
     else
      case windows_key_code of
        VKEY_OEM_4  : Result := $1B;
        VKEY_OEM_5  : Result := $1C;
        VKEY_OEM_6  : Result := $1D;
        VKEY_RETURN : Result := $0A;
        else          Result := 0;
      end;
end;

procedure GdkEventKeyToCEFKeyEvent(GdkEvent: PGdkEventKey; var aCEFKeyEvent : TCEFKeyEvent);
var
  windows_key_code : integer;
begin
  windows_key_code                     := GdkEventToWindowsKeyCode(GdkEvent);
  aCEFKeyEvent.windows_key_code        := GetWindowsKeyCodeWithoutLocation(windows_key_code);
  aCEFKeyEvent.native_key_code         := GdkEvent^.hardware_keycode;
  aCEFKeyEvent.modifiers               := GetCefStateModifiers(GdkEvent^.state);
  aCEFKeyEvent.focus_on_editable_field := 0;

  if (GdkEvent^.keyval >= GDK_KP_Space) and (GdkEvent^.keyval <= GDK_KP_9) then
    aCEFKeyEvent.modifiers := aCEFKeyEvent.modifiers or EVENTFLAG_IS_KEY_PAD;

  aCEFKeyEvent.is_system_key := ord((aCEFKeyEvent.modifiers and EVENTFLAG_ALT_DOWN) <> 0);

  if (windows_key_code = VKEY_RETURN) then
    aCEFKeyEvent.unmodified_character := #13
   else
    aCEFKeyEvent.unmodified_character := WideChar(gdk_keyval_to_unicode(GdkEvent^.keyval));

  if ((aCEFKeyEvent.modifiers and EVENTFLAG_CONTROL_DOWN) <> 0) then
    aCEFKeyEvent.character := WideChar(GetControlCharacter(windows_key_code, ((aCEFKeyEvent.modifiers and EVENTFLAG_SHIFT_DOWN) <> 0)))
   else
    aCEFKeyEvent.character := aCEFKeyEvent.unmodified_character;
end;

{$IFDEF FMX}
function g_signal_connect(instance: gpointer; detailed_signal: Pgchar; c_handler: TGCallback; data: gpointer): gulong;
begin
  Result := g_signal_connect_data(instance, detailed_signal, c_handler, data, nil, TGConnectFlags(0));
end;

function g_signal_connect(instance: gpointer; const detailed_signal: AnsiString; c_handler: TGCallback; data: gpointer): gulong;
begin
  Result := g_signal_connect(instance, @detailed_signal[1], c_handler, data);
end;
{$ENDIF}

{$IFDEF FPC}
// This function is almost an identical copy of "ModalShowX11Window" available
// at https://wiki.lazarus.freepascal.org/X11
procedure ShowX11Message(const aMessage : string);
var
  TempDisplay : PDisplay;
  TempWindow  : TWindow;
  TempEvent   : TXEvent;
  TempMessage : PChar;
  TempScreen  : cint;
begin
  TempMessage := PChar(trim(copy(aMessage, 1, pred(pos(#13, aMessage)))));
  TempDisplay := XOpenDisplay(nil);

  if (TempDisplay = nil) then
    begin
      WriteLn(aMessage);
      exit;
    end;

  TempScreen := DefaultScreen(TempDisplay);
  TempWindow := XCreateSimpleWindow(TempDisplay,
                                    RootWindow(TempDisplay, TempScreen),
                                    10, 10, 200, 100, 1,
                                    BlackPixel(TempDisplay, TempScreen),
                                    WhitePixel(TempDisplay, TempScreen));

  XSelectInput(TempDisplay, TempWindow, ExposureMask or KeyPressMask);

  XMapWindow(TempDisplay, TempWindow);

  while (True) do
    begin
      XNextEvent(TempDisplay, @TempEvent);

      if (TempEvent._type = Expose) then
        XDrawString(TempDisplay,
                    TempWindow,
                    DefaultGC(TempDisplay, TempScreen),
                    40, 50,
                    TempMessage,
                    strlen(TempMessage));

      if (TempEvent._type = KeyPress) then Break;
    end;

  XCloseDisplay(TempDisplay);
end;
{$ENDIF}{$ENDIF}

end.
