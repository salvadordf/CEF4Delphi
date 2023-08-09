unit uCEFBoxLayout;

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
  TCefBoxLayoutRef = class(TCefLayoutRef, ICefBoxLayout)
    protected
      procedure SetFlexForView(const view: ICefView; flex: Integer);
      procedure ClearFlexForView(const view: ICefView);

    public
      class function UnWrap(data: Pointer): ICefBoxLayout;
  end;

implementation

uses
  uCEFMiscFunctions;

procedure TCefBoxLayoutRef.SetFlexForView(const view: ICefView; flex: Integer);
begin
  PCefBoxLayout(FData)^.set_flex_for_view(PCefBoxLayout(FData),
                                          CefGetData(view),
                                          ord(flex));
end;

procedure TCefBoxLayoutRef.ClearFlexForView(const view: ICefView);
begin
  PCefBoxLayout(FData)^.clear_flex_for_view(PCefBoxLayout(FData),
                                            CefGetData(view));
end;

class function TCefBoxLayoutRef.UnWrap(data: Pointer): ICefBoxLayout;
begin
  if (data <> nil) then
    Result := Create(data) as ICefBoxLayout
   else
    Result := nil;
end;

end.

