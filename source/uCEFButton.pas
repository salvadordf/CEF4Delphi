unit uCEFButton;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFView;

type
  TCefButtonRef = class(TCefViewRef, ICefButton)
    protected
      function  AsLabelButton : ICefLabelButton;
      procedure SetState(state_: TCefButtonState);
      function  GetState : TCefButtonState;
      procedure SetInkDropEnabled(enabled_: boolean);
      procedure SetTooltipText(const tooltip_text: ustring);
      procedure SetAccessibleName(const name: ustring);

    public
      class function UnWrap(data: Pointer): ICefButton;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFLabelButton;

function TCefButtonRef.AsLabelButton : ICefLabelButton;
begin
  Result := TCefLabelButtonRef.UnWrap(PCefButton(FData)^.as_label_button(PCefButton(FData)));
end;

procedure TCefButtonRef.SetState(state_: TCefButtonState);
begin
  PCefButton(FData)^.set_state(PCefButton(FData), state_);
end;

function TCefButtonRef.GetState : TCefButtonState;
begin
  Result := PCefButton(FData)^.get_state(PCefButton(FData));
end;

procedure TCefButtonRef.SetInkDropEnabled(enabled_: boolean);
begin
  PCefButton(FData)^.set_ink_drop_enabled(PCefButton(FData), ord(enabled_));
end;

procedure TCefButtonRef.SetTooltipText(const tooltip_text: ustring);
var
  TempText : TCefString;
begin
  TempText := CefString(tooltip_text);
  PCefButton(FData)^.set_tooltip_text(PCefButton(FData), @TempText);
end;

procedure TCefButtonRef.SetAccessibleName(const name: ustring);
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  PCefButton(FData)^.set_accessible_name(PCefButton(FData), @TempName);
end;

class function TCefButtonRef.UnWrap(data: Pointer): ICefButton;
begin
  if (data <> nil) then
    Result := Create(data) as ICefButton
   else
    Result := nil;
end;

end.

