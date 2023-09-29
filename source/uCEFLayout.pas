unit uCEFLayout;

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
  /// A Layout handles the sizing of the children of a Panel according to
  /// implementation-specific heuristics. Methods must be called on the browser
  /// process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_layout_capi.h">CEF source file: /include/capi/views/cef_layout_capi.h (cef_layout_t)</see></para>
  /// </remarks>
  TCefLayoutRef = class(TCefBaseRefCountedRef, ICefLayout)
    protected
      /// <summary>
      /// Returns this Layout as a BoxLayout or NULL if this is not a BoxLayout.
      /// </summary>
      function AsBoxLayout : ICefBoxLayout;
      /// <summary>
      /// Returns this Layout as a FillLayout or NULL if this is not a FillLayout.
      /// </summary>
      function AsFillLayout : ICefFillLayout;
      /// <summary>
      /// Returns true (1) if this Layout is valid.
      /// </summary>
      function IsValid : boolean;

    public
      /// <summary>
      /// Returns a ICefLayout instance using a PCefLayout data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefLayout;
  end;

implementation

uses
  uCEFLibFunctions, uCEFBoxLayout, uCEFFillLayout;

function TCefLayoutRef.AsBoxLayout : ICefBoxLayout;
begin
  Result := TCefBoxLayoutRef.UnWrap(PCefLayout(FData)^.as_box_layout(PCefLayout(FData)));
end;

function TCefLayoutRef.AsFillLayout : ICefFillLayout;
begin
  Result := TCefFillLayoutRef.UnWrap(PCefLayout(FData)^.as_fill_layout(PCefLayout(FData)));
end;

function TCefLayoutRef.IsValid : boolean;
begin
  Result := (PCefLayout(FData)^.is_valid(PCefLayout(FData)) <> 0);
end;

class function TCefLayoutRef.UnWrap(data: Pointer): ICefLayout;
begin
  if (data <> nil) then
    Result := Create(data) as ICefLayout
   else
    Result := nil;
end;

end.

