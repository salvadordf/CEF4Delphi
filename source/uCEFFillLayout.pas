unit uCEFFillLayout;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFLayout;

type
  /// <summary>
  /// A simple Layout that causes the associated Panel's one child to be sized to
  /// match the bounds of its parent. Methods must be called on the browser
  /// process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_fill_layout_capi.h">CEF source file: /include/capi/views/cef_fill_layout_capi.h (cef_fill_layout_t)</see></para>
  /// </remarks>
  TCefFillLayoutRef = class(TCefLayoutRef, ICefFillLayout)
    public
      /// <summary>
      /// Returns a ICefFillLayout instance using a PCefFillLayout data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefFillLayout;
  end;

implementation

class function TCefFillLayoutRef.UnWrap(data: Pointer): ICefFillLayout;
begin
  if (data <> nil) then
    Result := Create(data) as ICefFillLayout
   else
    Result := nil;
end;

end.

