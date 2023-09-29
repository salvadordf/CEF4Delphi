unit uCEFButtonComponent;

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
  uCEFTypes, uCEFInterfaces, uCEFViewsFrameworkEvents, uCEFViewComponent;

type
  TCEFButtonComponent = class(TCEFViewComponent, ICefButtonDelegateEvents)
    protected
      FButton                     : ICefButton;

      // ICefButtonDelegateEvents
      FOnButtonPressed            : TOnButtonPressedEvent;
      FOnButtonStateChanged       : TOnButtonStateChangedEvent;

      procedure DestroyView; override;
      procedure Initialize; override;

      function  GetInitialized : boolean; override;
      function  GetAsView : ICefView; override;
      function  GetState : TCefButtonState;
      function  GetAsButton : ICefButton; override;
      function  GetAsLabelButton : ICefLabelButton; virtual;

      procedure SetState(state_: TCefButtonState);

      // ICefButtonDelegateEvents
      procedure doOnButtonPressed(const button: ICefButton);
      procedure doOnButtonStateChanged(const button: ICefButton);

    public
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
      procedure SetAccessibleName(const name_: ustring);
      /// <summary>
      /// Returns this Button as a LabelButton or NULL if this is not a LabelButton.
      /// </summary>
      property AsLabelButton          : ICefLabelButton             read GetAsLabelButton;
      /// <summary>
      /// Returns the current display state of the Button.
      /// </summary>
      property State                  : TCefButtonState             read GetState                  write SetState;

    published
      /// <summary>
      /// Called when |button| is pressed.
      /// </summary>
      property OnButtonPressed        : TOnButtonPressedEvent       read FOnButtonPressed          write FOnButtonPressed;
      /// <summary>
      /// Called when the state of |button| changes.
      /// </summary>
      property OnButtonStateChanged   : TOnButtonStateChangedEvent  read FOnButtonStateChanged     write FOnButtonStateChanged;
  end;

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
  uCEFButtonDelegate;

procedure TCEFButtonComponent.Initialize;
begin
  inherited Initialize;

  FButton                     := nil;
  FOnButtonPressed            := nil;
  FOnButtonStateChanged       := nil;
end;

procedure TCEFButtonComponent.DestroyView;
begin
  FButton := nil;
end;

function TCEFButtonComponent.GetInitialized : boolean;
begin
  Result := (FButton <> nil);
end;

function TCEFButtonComponent.GetAsView : ICefView;
begin
  Result := FButton as ICefView;
end;

function TCEFButtonComponent.GetAsButton : ICefButton;
begin
  Result := FButton;
end;

function TCEFButtonComponent.GetAsLabelButton : ICefLabelButton;
begin
  Result := nil;
end;

procedure TCEFButtonComponent.doOnButtonPressed(const button: ICefButton);
begin
  if assigned(FOnButtonPressed) then
    FOnButtonPressed(self, button);
end;

procedure TCEFButtonComponent.doOnButtonStateChanged(const button: ICefButton);
begin
  if assigned(FOnButtonStateChanged) then
    FOnButtonStateChanged(self, button);
end;

procedure TCEFButtonComponent.SetState(state_: TCefButtonState);
begin
  if Initialized then AsButton.SetState(state_);
end;

function TCEFButtonComponent.GetState : TCefButtonState;
begin
  if Initialized then
    Result := AsButton.GetState
   else
    Result := CEF_BUTTON_STATE_NORMAL;
end;

procedure TCEFButtonComponent.SetInkDropEnabled(enabled_: boolean);
begin
  if Initialized then AsButton.SetInkDropEnabled(enabled_);
end;

procedure TCEFButtonComponent.SetTooltipText(const tooltip_text: ustring);
begin
  if Initialized then AsButton.SetTooltipText(tooltip_text);
end;

procedure TCEFButtonComponent.SetAccessibleName(const name_: ustring);
begin
  if Initialized then AsButton.SetAccessibleName(name_);
end;


end.
