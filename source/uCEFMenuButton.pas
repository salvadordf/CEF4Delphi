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
  /// <summary>
  /// MenuButton is a button with optional text, icon and/or menu marker that
  /// shows a menu when clicked with the left mouse button. All size and position
  /// values are in density independent pixels (DIP) unless otherwise indicated.
  /// Methods must be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_menu_button_capi.h">CEF source file: /include/capi/views/cef_menu_button_capi.h (cef_menu_button_t)</see></para>
  /// </remarks>
  TCefMenuButtonRef = class(TCefLabelButtonRef, ICefMenuButton)
    protected
      /// <summary>
      /// Show a menu with contents |menu_model|. |screen_point| specifies the menu
      /// position in screen coordinates. |anchor_position| specifies how the menu
      /// will be anchored relative to |screen_point|. This function should be
      /// called from ICefMenuButtonDelegate.OnMenuButtonPressed().
      /// </summary>
      procedure ShowMenu(const menu_model: ICefMenuModel; const screen_point: TCefPoint; anchor_position: TCefMenuAnchorPosition);
      /// <summary>
      /// Show the menu for this button. Results in a call to
      /// ICefMenuButtonDelegate.OnMenuButtonPressed().
      /// </summary>
      procedure TriggerMenu;

    public
      /// <summary>
      /// Returns a ICefMenuButton instance using a PCefMenuButton data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefMenuButton;
      /// <summary>
      /// Create a new MenuButton. A |delegate| must be provided to call show_menu()
      /// when the button is clicked. |text| will be shown on the MenuButton and used
      /// as the default accessible name. If |with_frame| is true (1) the button will
      /// have a visible frame at all times, center alignment, additional padding and
      /// a default minimum size of 70x33 DIP. If |with_frame| is false (0) the button
      /// will only have a visible frame on hover/press, left alignment, less padding
      /// and no default minimum size.
      /// </summary>
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

