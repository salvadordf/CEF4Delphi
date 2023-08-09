unit uCEFMenuButton;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFLabelButton;

type
  TCefMenuButtonRef = class(TCefLabelButtonRef, ICefMenuButton)
    protected
      procedure ShowMenu(const menu_model: ICefMenuModel; const screen_point: TCefPoint; anchor_position: TCefMenuAnchorPosition);
      procedure TriggerMenu;

    public
      class function UnWrap(data: Pointer): ICefMenuButton;
      class function CreateMenuButton(const delegate: ICefMenuButtonDelegate; const text: ustring): ICefMenuButton;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions;

procedure TCefMenuButtonRef.ShowMenu(const menu_model      : ICefMenuModel;
                                     const screen_point    : TCefPoint;
                                           anchor_position : TCefMenuAnchorPosition);
begin
  PCefMenuButton(FData)^.show_menu(PCefMenuButton(FData),
                                   CefGetData(menu_model),
                                   @screen_point,
                                   anchor_position);
end;

procedure TCefMenuButtonRef.TriggerMenu;
begin
  PCefMenuButton(FData)^.trigger_menu(PCefMenuButton(FData));
end;

class function TCefMenuButtonRef.UnWrap(data: Pointer): ICefMenuButton;
begin
  if (data <> nil) then
    Result := Create(data) as ICefMenuButton
   else
    Result := nil;
end;

class function TCefMenuButtonRef.CreateMenuButton(const delegate : ICefMenuButtonDelegate;
                                                  const text     : ustring): ICefMenuButton;
var
  TempText   : TCefString;
  TempButton : PCefMenuButton;
begin
  Result := nil;

  if (delegate <> nil) then
    begin
      TempText   := CefString(text);
      TempButton := cef_menu_button_create(CefGetData(delegate), @TempText);

      if (TempButton <> nil) then
        Result := Create(TempButton) as ICefMenuButton;
    end;
end;

end.

