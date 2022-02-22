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

unit uCEFMacOSInterfaces;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

{$IFDEF MACOS}
uses
  System.TypInfo, Macapi.Foundation, Macapi.CoreFoundation, Macapi.ObjectiveC,
  Macapi.Helpers, Macapi.CocoaTypes, Macapi.AppKit, FMX.Platform;

type
  IFMXApplicationDelegate = interface(NSApplicationDelegate)
    ['{A54E08CA-77CC-4F22-B6D9-833DD6AB696D}']
    procedure onMenuClicked(sender: NSMenuItem); cdecl;
  end;

  CrAppProtocol = interface(NSObject)
    ['{2071D289-9A54-4AD7-BD83-E521ACD5C528}']
    function isHandlingSendEvent: boolean; cdecl;
  end;

  //CrAppControlProtocol = interface(CrAppProtocol)
  CrAppControlProtocol = interface(NSObject)
    ['{BCCDF64D-E8D7-4E0B-83BC-30F87145576C}']
    function isHandlingSendEvent: boolean; cdecl;
    procedure setHandlingSendEvent(handlingSendEvent: boolean); cdecl;
  end;

  ICustomCocoaTimer = interface(NSObject)
    ['{17D92D03-614A-4D4A-B938-FA0D4A3A07F9}']
    procedure timerTimeout(timer: NSTimer); cdecl;
  end;
{$ENDIF}

implementation

end.
