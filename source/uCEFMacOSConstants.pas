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

unit uCEFMacOSConstants;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

{$IFDEF MACOSX}
const
  // Virtual Keycode constants defined in <HIToolbox/Events.h>
  // /System/Library/Frameworks/Carbon.framework/Versions/A/Frameworks/HIToolbox.framework/Versions/A/Headers/Events.h
  // /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/System/Library/Frameworks/Carbon.framework/Versions/A/Frameworks/HIToolbox.framework/Versions/A/Headers/Events.h

	// These constants are the virtual keycodes defined originally in
	// Inside Mac Volume V, pg. V-191. They identify physical keys on a
	// keyboard. Those constants with "ANSI" in the name are labeled
	// according to the key position on an ANSI-standard US keyboard.
	// For example, kVK_ANSI_A indicates the virtual keycode for the key
	// with the letter 'A' in the US keyboard layout. Other keyboard
	// layouts may have the 'A' key label on a different physical key;
	// in this case, pressing 'A' will generate a different virtual
	// keycode.

  kVK_ANSI_A                    = $00;
  kVK_ANSI_S                    = $01;
  kVK_ANSI_D                    = $02;
  kVK_ANSI_F                    = $03;
  kVK_ANSI_H                    = $04;
  kVK_ANSI_G                    = $05;
  kVK_ANSI_Z                    = $06;
  kVK_ANSI_X                    = $07;
  kVK_ANSI_C                    = $08;
  kVK_ANSI_V                    = $09;
  kVK_ANSI_B                    = $0B;
  kVK_ANSI_Q                    = $0C;
  kVK_ANSI_W                    = $0D;
  kVK_ANSI_E                    = $0E;
  kVK_ANSI_R                    = $0F;
  kVK_ANSI_Y                    = $10;
  kVK_ANSI_T                    = $11;
  kVK_ANSI_1                    = $12;
  kVK_ANSI_2                    = $13;
  kVK_ANSI_3                    = $14;
  kVK_ANSI_4                    = $15;
  kVK_ANSI_6                    = $16;
  kVK_ANSI_5                    = $17;
  kVK_ANSI_Equal                = $18;
  kVK_ANSI_9                    = $19;
  kVK_ANSI_7                    = $1A;
  kVK_ANSI_Minus                = $1B;
  kVK_ANSI_8                    = $1C;
  kVK_ANSI_0                    = $1D;
  kVK_ANSI_RightBracket         = $1E;
  kVK_ANSI_O                    = $1F;
  kVK_ANSI_U                    = $20;
  kVK_ANSI_LeftBracket          = $21;
  kVK_ANSI_I                    = $22;
  kVK_ANSI_P                    = $23;
  kVK_ANSI_L                    = $25;
  kVK_ANSI_J                    = $26;
  kVK_ANSI_Quote                = $27;
  kVK_ANSI_K                    = $28;
  kVK_ANSI_Semicolon            = $29;
  kVK_ANSI_Backslash            = $2A;
  kVK_ANSI_Comma                = $2B;
  kVK_ANSI_Slash                = $2C;
  kVK_ANSI_N                    = $2D;
  kVK_ANSI_M                    = $2E;
  kVK_ANSI_Period               = $2F;
  kVK_ANSI_Grave                = $32;
  kVK_ANSI_KeypadDecimal        = $41;
  kVK_ANSI_KeypadMultiply       = $43;
  kVK_ANSI_KeypadPlus           = $45;
  kVK_ANSI_KeypadClear          = $47;
  kVK_ANSI_KeypadDivide         = $4B;
  kVK_ANSI_KeypadEnter          = $4C;
  kVK_ANSI_KeypadMinus          = $4E;
  kVK_ANSI_KeypadEquals         = $51;
  kVK_ANSI_Keypad0              = $52;
  kVK_ANSI_Keypad1              = $53;
  kVK_ANSI_Keypad2              = $54;
  kVK_ANSI_Keypad3              = $55;
  kVK_ANSI_Keypad4              = $56;
  kVK_ANSI_Keypad5              = $57;
  kVK_ANSI_Keypad6              = $58;
  kVK_ANSI_Keypad7              = $59;
  kVK_ANSI_Keypad8              = $5B;
  kVK_ANSI_Keypad9              = $5C;

	// keycodes for keys that are independent of keyboard layout
  kVK_Return                    = $24;
  kVK_Tab                       = $30;
  kVK_Space                     = $31;
  kVK_Delete                    = $33;
  kVK_Escape                    = $35;
  kVK_Command                   = $37;
  kVK_Shift                     = $38;
  kVK_CapsLock                  = $39;
  kVK_Option                    = $3A;
  kVK_Control                   = $3B;
  kVK_RightShift                = $3C;
  kVK_RightOption               = $3D;
  kVK_RightControl              = $3E;
  kVK_Function                  = $3F;
  kVK_F17                       = $40;
  kVK_VolumeUp                  = $48;
  kVK_VolumeDown                = $49;
  kVK_Mute                      = $4A;
  kVK_F18                       = $4F;
  kVK_F19                       = $50;
  kVK_F20                       = $5A;
  kVK_F5                        = $60;
  kVK_F6                        = $61;
  kVK_F7                        = $62;
  kVK_F3                        = $63;
  kVK_F8                        = $64;
  kVK_F9                        = $65;
  kVK_F11                       = $67;
  kVK_F13                       = $69;
  kVK_F16                       = $6A;
  kVK_F14                       = $6B;
  kVK_F10                       = $6D;
  kVK_F12                       = $6F;
  kVK_F15                       = $71;
  kVK_Help                      = $72;
  kVK_Home                      = $73;
  kVK_PageUp                    = $74;
  kVK_ForwardDelete             = $75;
  kVK_F4                        = $76;
  kVK_End                       = $77;
  kVK_F2                        = $78;
  kVK_PageDown                  = $79;
  kVK_F1                        = $7A;
  kVK_LeftArrow                 = $7B;
  kVK_RightArrow                = $7C;
  kVK_DownArrow                 = $7D;
  kVK_UpArrow                   = $7E;

	// ISO keyboards only
  kVK_ISO_Section               = $0A;

	// JIS keyboards only
  kVK_JIS_Yen                   = $5D;
  kVK_JIS_Underscore            = $5E;
  kVK_JIS_KeypadComma           = $5F;
  kVK_JIS_Eisu                  = $66;
  kVK_JIS_Kana                  = $68;

  CEF_MACOS_KEYPAD_KEYS = [kVK_ANSI_KeypadDecimal,
                           kVK_ANSI_KeypadMultiply,
                           kVK_ANSI_KeypadPlus,
                           kVK_ANSI_KeypadClear,
                           kVK_ANSI_KeypadDivide,
                           kVK_ANSI_KeypadEnter,
                           kVK_ANSI_KeypadMinus,
                           kVK_ANSI_KeypadEquals,
                           kVK_ANSI_Keypad0,
                           kVK_ANSI_Keypad1,
                           kVK_ANSI_Keypad2,
                           kVK_ANSI_Keypad3,
                           kVK_ANSI_Keypad4,
                           kVK_ANSI_Keypad5,
                           kVK_ANSI_Keypad6,
                           kVK_ANSI_Keypad7,
                           kVK_ANSI_Keypad8,
                           kVK_ANSI_Keypad9];

  CEF_MACOS_ARROW_KEYS = [kVK_LeftArrow,
                          kVK_RightArrow,
                          kVK_DownArrow,
                          kVK_UpArrow];

  CEF_MACOS_FUNCTION_KEYS = [kVK_F1,
                             kVK_F2,
                             kVK_F3,
                             kVK_F4,
                             kVK_F5,
                             kVK_F6,
                             kVK_F7,
                             kVK_F8,
                             kVK_F9,
                             kVK_F10,
                             kVK_F11,
                             kVK_F12,
                             kVK_F13,
                             kVK_F14,
                             kVK_F15,
                             kVK_F16,
                             kVK_F17,
                             kVK_F18,
                             kVK_F19,
                             kVK_F20];
{$ENDIF}

implementation

end.
