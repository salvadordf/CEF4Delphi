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
  TCefLayoutRef = class(TCefBaseRefCountedRef, ICefLayout)
    protected
      function AsBoxLayout : ICefBoxLayout;
      function AsFillLayout : ICefFillLayout;
      function IsValid : boolean;

    public
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

