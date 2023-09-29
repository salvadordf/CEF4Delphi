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
  /// <summary>
  /// A View representing a button. Depending on the specific type, the button
  /// could be implemented by a native control or custom rendered. Methods must be
  /// called on the browser process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_button_capi.h">CEF source file: /include/capi/views/cef_button_capi.h (cef_button_t)</see></para>
  /// </remarks>
  TCefButtonRef = class(TCefViewRef, ICefButton)
    protected
      /// <summary>
      /// Returns this Button as a LabelButton or NULL if this is not a LabelButton.
      /// </summary>
      function  AsLabelButton : ICefLabelButton;
      /// <summary>
      /// Sets the current display state of the Button.
      /// </summary>
      procedure SetState(state_: TCefButtonState);
      /// <summary>
      /// Returns the current display state of the Button.
      /// </summary>
      function  GetState : TCefButtonState;
      /// <summary>
      /// Sets the Button will use an ink drop effect for displaying state changes.
      /// </summary>
      procedure SetInkDropEnabled(enabled_: boolean);
      /// <summary>
      /// Sets the tooltip text that will be displayed when the user hovers the
      /// mouse cursor over the Button.
      /// </summary>
      procedure SetTooltipText(const tooltip_text: ustring);
      /// <summary>
      /// Sets the accessible name that will be exposed to assistive technology
      /// (AT).
      /// </summary>
      procedure SetAccessibleName(const name: ustring);

    public
      /// <summary>
      /// Returns a ICefButton instance using a PCefButton data pointer.
      /// </summary>
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

