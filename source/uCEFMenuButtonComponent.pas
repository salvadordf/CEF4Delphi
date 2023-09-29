unit uCEFMenuButtonComponent;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes,
    {$IFDEF FPC}
    LCLProc, LCLType, LCLIntf, LResources, InterfaceBase,
    {$ENDIF}
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFViewsFrameworkEvents, uCEFLabelButtonComponent;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]{$ENDIF}{$ENDIF}
  TCEFMenuButtonComponent = class(TCEFLabelButtonComponent, ICefMenuButtonDelegateEvents)
    protected
      FMenuButton                : ICefMenuButton;

      // ICefMenuButtonDelegateEvents
      FOnMenuButtonPressed       : TOnMenuButtonPressedEvent;

      procedure DestroyView; override;
      procedure Initialize; override;

      function  GetInitialized : boolean; override;
      function  GetAsView : ICefView; override;
      function  GetAsButton : ICefButton; override;
      function  GetAsLabelButton : ICefLabelButton; override;
      function  GetAsMenuButton : ICefMenuButton; override;

      // ICefMenuButtonDelegateEvents
      procedure doOnMenuButtonPressed(const menu_button: ICefMenuButton; const screen_point: TCefPoint; const button_pressed_lock: ICefMenuButtonPressedLock);

      // ICefViewDelegateEvents
      procedure doCreateCustomView; override;

    public
      /// <summary>
      /// Create a new MenuButton.
      /// </summary>
      procedure CreateMenuButton(const aText : ustring);
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

    published
      /// <summary>
      /// Called when |button| is pressed. Call ICefMenuButton.ShowMenu() to
      /// show a popup menu at |screen_point|. When showing a custom popup such as a
      /// window keep a reference to |button_pressed_lock| until the popup is hidden
      /// to maintain the pressed button state.
      /// </summary>
      property OnMenuButtonPressed   : TOnMenuButtonPressedEvent     read FOnMenuButtonPressed     write FOnMenuButtonPressed;
  end;

{$IFDEF FPC}
procedure Register;
{$ENDIF}

// *********************************************************
// ********************** ATTENTION ! **********************
// *********************************************************
// **                                                     **
// **  MANY OF THE EVENTS IN CEF4DELPHI COMPONENTS LIKE   **
// **  TCHROMIUM, TFMXCHROMIUM OR TCEFAPPLICATION ARE     **
// **  EXECUTED IN A CEF THREAD BY DEFAULT.               **
// **                                                     **
// **  WINDOWS CONTROLS MUST BE CREATED AND DESTROYED IN  **
// **  THE SAME THREAD TO AVOID ERRORS.                   **
// **  SOME OF THEM RECREATE THE HANDLERS IF THEY ARE     **
// **  MODIFIED AND CAN CAUSE THE SAME ERRORS.            **
// **                                                     **
// **  DON'T CREATE, MODIFY OR DESTROY WINDOWS CONTROLS   **
// **  INSIDE THE CEF4DELPHI EVENTS AND USE               **
// **  SYNCHRONIZATION OBJECTS TO PROTECT VARIABLES AND   **
// **  FIELDS IF THEY ARE ALSO USED IN THE MAIN THREAD.   **
// **                                                     **
// **  READ THIS FOR MORE INFORMATION :                   **
// **  https://www.briskbard.com/index.php?pageid=cef     **
// **                                                     **
// **  USE OUR FORUMS FOR MORE QUESTIONS :                **
// **  https://www.briskbard.com/forum/                   **
// **                                                     **
// *********************************************************
// *********************************************************

implementation

uses
  uCEFMenuButtonDelegate, uCEFMenuButton;

procedure TCEFMenuButtonComponent.CreateMenuButton(const aText : ustring);
begin
  CreateLabelButton(aText);
end;

procedure TCEFMenuButtonComponent.doCreateCustomView;
var
  TempDelegate : ICefMenuButtonDelegate;
begin
  if (FMenuButton = nil) then
    try
      TempDelegate := TCustomMenuButtonDelegate.Create(self);
      FMenuButton  := TCefMenuButtonRef.CreateMenuButton(TempDelegate, FText);
    finally
      TempDelegate := nil;
    end;
end;

procedure TCEFMenuButtonComponent.Initialize;
begin
  inherited Initialize;

  FMenuButton          := nil;
  FOnMenuButtonPressed := nil;
end;

procedure TCEFMenuButtonComponent.DestroyView;
begin
  FMenuButton := nil;
end;

function TCEFMenuButtonComponent.GetInitialized : boolean;
begin
  Result := (FMenuButton <> nil);
end;

function TCEFMenuButtonComponent.GetAsView : ICefView;
begin
  Result := FMenuButton as ICefView;
end;

function TCEFMenuButtonComponent.GetAsButton : ICefButton;
begin
  if Initialized then
    Result := FMenuButton as ICefButton
   else
    Result := nil;
end;

function TCEFMenuButtonComponent.GetAsLabelButton : ICefLabelButton;
begin
  if Initialized then
    Result := FMenuButton as ICefLabelButton
   else
    Result := nil;
end;

function TCEFMenuButtonComponent.GetAsMenuButton : ICefMenuButton;
begin
  Result := FMenuButton;
end;

procedure TCEFMenuButtonComponent.doOnMenuButtonPressed(const menu_button         : ICefMenuButton;
                                                        const screen_point        : TCefPoint;
                                                        const button_pressed_lock : ICefMenuButtonPressedLock);
begin
  if assigned(FOnMenuButtonPressed) then
    FOnMenuButtonPressed(self, menu_button, screen_point, button_pressed_lock);
end;

procedure TCEFMenuButtonComponent.ShowMenu(const menu_model      : ICefMenuModel;
                                           const screen_point    : TCefPoint;
                                                 anchor_position : TCefMenuAnchorPosition);
begin
  if Initialized then
    FMenuButton.ShowMenu(menu_model, screen_point, anchor_position);
end;

procedure TCEFMenuButtonComponent.TriggerMenu;
begin
  if Initialized then
    FMenuButton.TriggerMenu;
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tcefmenubuttoncomponent.lrs}
  RegisterComponents('Chromium Views Framework', [TCEFMenuButtonComponent]);
end;
{$ENDIF}

end.
