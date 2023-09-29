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
  /// <summary>
  /// MenuButton pressed lock is released when this object is destroyed.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_menu_button_delegate_capi.h">CEF source file: /include/capi/views/cef_menu_button_delegate_capi.h (cef_menu_button_pressed_lock_t)</see></para>
  /// </remarks>
  TCefMenuButtonPressedLockRef = class(TCefBaseRefCountedRef, ICefMenuButtonPressedLock)
    public
      /// <summary>
      /// Returns a ICefMenuButtonPressedLock instance using a PCefMenuButtonPressedLock data pointer.
      /// </summary>
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

