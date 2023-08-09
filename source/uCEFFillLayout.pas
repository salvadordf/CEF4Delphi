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
  TCefFillLayoutRef = class(TCefLayoutRef, ICefFillLayout)
    public
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

