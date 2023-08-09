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
