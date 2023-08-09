unit uCEFPanelComponent;

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
  uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFViewsFrameworkEvents, uCEFViewComponent;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]{$ENDIF}{$ENDIF}
  TCEFPanelComponent = class(TCEFViewComponent, ICefPanelDelegateEvents)
    protected
      FPanel    : ICefPanel;

      procedure DestroyView; override;
      procedure Initialize; override;

      function  GetInitialized : boolean; override;
      function  GetAsView : ICefView; override;
      function  GetAsPanel : ICefPanel; override;
      function  GetAsWindow : ICefWindow; virtual;

      // ICefViewDelegateEvents
      procedure doCreateCustomView; override;

    public
      procedure CreatePanel;
      function  SetToFillLayout : ICefFillLayout;
      function  SetToBoxLayout(const settings: TCefBoxLayoutSettings): ICefBoxLayout;
      function  GetLayout : ICefLayout;
      procedure Layout;
      procedure AddChildView(const view: ICefView);
      procedure AddChildViewAt(const view: ICefView; index: Integer);
      procedure ReorderChildView(const view: ICefView; index: Integer);
      procedure RemoveChildView(const view: ICefView);
      procedure RemoveAllChildViews;
      function  GetChildViewCount : NativeUInt;
      function  GetChildViewAt(index: Integer): ICefView;

      property AsWindow : ICefWindow   read GetAsWindow;
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
  uCEFPanelDelegate, uCEFPanel, uCEFMiscFunctions, uCEFTask;

procedure TCEFPanelComponent.CreatePanel;
begin
  CreateView;
end;

procedure TCEFPanelComponent.doCreateCustomView;
var
  TempDelegate : ICefPanelDelegate;
begin
  if (FPanel = nil) then
    try
      TempDelegate := TCustomPanelDelegate.Create(self);
      FPanel       := TCefPanelRef.CreatePanel(TempDelegate);
    finally
      TempDelegate := nil;
    end;
end;

procedure TCEFPanelComponent.DestroyView;
begin
  FPanel := nil;
end;

procedure TCEFPanelComponent.Initialize;
begin
  inherited Initialize;

  FPanel := nil;
end;

function TCEFPanelComponent.GetInitialized : boolean;
begin
  Result := (FPanel <> nil);
end;

function TCEFPanelComponent.GetAsView : ICefView;
begin
  Result := FPanel as ICefView;
end;

function TCEFPanelComponent.GetAsPanel : ICefPanel;
begin
  Result := FPanel;
end;

function TCEFPanelComponent.GetAsWindow : ICefWindow;
begin
  if Initialized then
    Result := AsPanel.AsWindow
   else
    Result := nil;
end;

function TCEFPanelComponent.SetToFillLayout : ICefFillLayout;
begin
  if Initialized then
    Result := AsPanel.SetToFillLayout
   else
    Result := nil;
end;

function TCEFPanelComponent.SetToBoxLayout(const settings: TCefBoxLayoutSettings): ICefBoxLayout;
begin
  if Initialized then
    Result := AsPanel.SetToBoxLayout(settings)
   else
    Result := nil;
end;

function TCEFPanelComponent.GetLayout : ICefLayout;
begin
  if Initialized then
    Result := AsPanel.GetLayout
   else
    Result := nil;
end;

procedure TCEFPanelComponent.Layout;
begin
  if Initialized then AsPanel.Layout;
end;

procedure TCEFPanelComponent.AddChildView(const view: ICefView);
begin
  if Initialized then AsPanel.AddChildView(view);
end;

procedure TCEFPanelComponent.AddChildViewAt(const view: ICefView; index: Integer);
begin
  if Initialized then AsPanel.AddChildViewAt(view, index);
end;

procedure TCEFPanelComponent.ReorderChildView(const view: ICefView; index: Integer);
begin
  if Initialized then AsPanel.ReorderChildView(view, index);
end;

procedure TCEFPanelComponent.RemoveChildView(const view: ICefView);
begin
  if Initialized then AsPanel.RemoveChildView(view);
end;

procedure TCEFPanelComponent.RemoveAllChildViews;
begin
  if Initialized then AsPanel.RemoveAllChildViews;
end;

function TCEFPanelComponent.GetChildViewCount : NativeUInt;
begin
  if Initialized then
    Result := AsPanel.GetChildViewCount
   else
    Result := 0;
end;

function TCEFPanelComponent.GetChildViewAt(index: Integer): ICefView;
begin
  if Initialized then
    Result := AsPanel.GetChildViewAt(index)
   else
    Result := nil;
end;


{$IFDEF FPC}
procedure Register;
begin
  {$I res/tcefpanelcomponent.lrs}
  RegisterComponents('Chromium Views Framework', [TCEFPanelComponent]);
end;
{$ENDIF}

end.
