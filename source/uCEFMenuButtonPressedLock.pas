unit uCEFMenuButtonPressedLock;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefMenuButtonPressedLockRef = class(TCefBaseRefCountedRef, ICefMenuButtonPressedLock)
    public
      class function UnWrap(data: Pointer): ICefMenuButtonPressedLock;
  end;

implementation

class function TCefMenuButtonPressedLockRef.UnWrap(data: Pointer): ICefMenuButtonPressedLock;
begin
  if (data <> nil) then
    Result := Create(data) as ICefMenuButtonPressedLock
   else
    Result := nil;
end;

end.

