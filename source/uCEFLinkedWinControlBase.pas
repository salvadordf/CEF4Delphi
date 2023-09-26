unit uCEFLinkedWinControlBase;

{$I cef.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
  {$IFDEF MACOSX}
    {$ModeSwitch objectivec1}
  {$ENDIF}
{$ENDIF}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows, WinApi.Messages,{$ENDIF} System.Classes, Vcl.Controls, Vcl.Graphics,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows, Messages,{$ENDIF} Classes, Controls,
    {$IFDEF FPC}{$IFDEF MACOSX}CocoaAll,{$ENDIF}LCLProc, LCLType, LCLIntf,{$ENDIF}
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFWinControl, uCEFChromium;

type
  /// <summary>
  /// TCEFLinkedWinControlBase is a custom TWinControl to host the child controls created by the web browser
  /// to show the web contents and it's linked to the TChromium instance that handles that web browser.
  /// TCEFLinkedWinControlBase is the parent class of TChromiumWindow, TBrowserWindow and TCEFLinkedWindowParent.
  /// </summary>
  TCEFLinkedWinControlBase = class(TCEFWinControl)
    protected
      {$IFDEF MSWINDOWS}
      FUseSetFocus    : boolean;
      {$ENDIF}
      function  GetChromium: TChromium; virtual; abstract;
      {$IFDEF MSWINDOWS}
      function  GetUseSetFocus: Boolean; virtual;
      procedure SetUseSetFocus(aValue : boolean); virtual;
      {$ENDIF}

      {$IFDEF FPC}
      procedure SetVisible(Value: Boolean); override;
      {$ENDIF}
      function  GetChildWindowHandle : {$IFNDEF MSWINDOWS}{$IFDEF FPC}LclType.{$ENDIF}{$ENDIF}THandle; override;
      {$IFDEF MSWINDOWS}
      procedure WndProc(var aMessage: TMessage); override;
      {$ENDIF}
      /// <summary>
      /// TChromium instance used by this component.
      /// </summary>
      property  Chromium   : TChromium    read GetChromium;
    public
      constructor Create(AOwner: TComponent); override;
      procedure UpdateSize; override;
    published
      {$IFDEF MSWINDOWS}
      /// <summary>
      /// Use TChromium.SetFocus when the component receives a WM_SETFOCUS message in Windows.
      /// </summary>
      property UseSetFocus      : boolean         read GetUseSetFocus      write SetUseSetFocus  default True;
      {$ENDIF}
  end;

implementation

{ TCEFLinkedWinControlBase }

constructor TCEFLinkedWinControlBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF MSWINDOWS}
  FUseSetFocus := True;
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
function TCEFLinkedWinControlBase.GetUseSetFocus: Boolean;
begin
  Result := FUseSetFocus;
end;

procedure TCEFLinkedWinControlBase.SetUseSetFocus(aValue : boolean);
begin
  FUseSetFocus := aValue;
end;
{$ENDIF}

{$IFDEF FPC}
procedure TCEFLinkedWinControlBase.SetVisible(Value: Boolean);
{$IFDEF LINUX}
var
  TempChanged : boolean;
{$ENDIF}
begin
  {$IFDEF LINUX}
  TempChanged := (Visible <> Value);
  {$ENDIF}

  inherited SetVisible(Value);

  {$IFDEF LINUX}
  if not(csDesigning in ComponentState) and
     TempChanged and
     (Chromium <> nil) and
     Chromium.Initialized then
    Chromium.UpdateXWindowVisibility(Visible);
  {$ENDIF}
end;
{$ENDIF}

function TCEFLinkedWinControlBase.GetChildWindowHandle: THandle;
begin
  Result := 0;

  if (Chromium <> nil) then Result := Chromium.WindowHandle;

  if (Result = 0) then Result := inherited GetChildWindowHandle;
end;

{$IFDEF MSWINDOWS}
procedure TCEFLinkedWinControlBase.WndProc(var aMessage: TMessage);
var
  TempHandle : THandle;
begin
  case aMessage.Msg of
    WM_SETFOCUS:
      begin
        if UseSetFocus and (Chromium <> nil) then
          Chromium.SetFocus(True)
         else
          begin
            TempHandle := ChildWindowHandle;
            if (TempHandle <> 0) then PostMessage(TempHandle, WM_SETFOCUS, aMessage.WParam, 0);
          end;

        inherited WndProc(aMessage);
      end;

    WM_ERASEBKGND:
      if (ChildWindowHandle = 0) then inherited WndProc(aMessage);

    CM_WANTSPECIALKEY:
      if not(TWMKey(aMessage).CharCode in [VK_LEFT .. VK_DOWN, VK_RETURN, VK_ESCAPE]) then
        aMessage.Result := 1
       else
        inherited WndProc(aMessage);

    WM_GETDLGCODE : aMessage.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;

    else inherited WndProc(aMessage);
  end;
end;
{$ENDIF}

procedure TCEFLinkedWinControlBase.UpdateSize;
{$IFDEF MACOSX}{$IFDEF FPC}
var
  TempSize: NSSize;
{$ENDIF}{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  inherited UpdateSize;
  {$ENDIF}

  {$IFDEF LINUX}
  if not(csDesigning in ComponentState) and
     (Chromium <> nil) and
     Chromium.Initialized then
    Chromium.UpdateBrowserSize(Left, Top, Width, Height);
  {$ENDIF}

  {$IFDEF MACOSX}
  {$IFDEF FPC}
  if not(csDesigning in ComponentState) and
     (Chromium <> nil) and
     Chromium.Initialized then
    begin
      TempSize.width:= Width;
      TempSize.height:= Height;
      NSView(Chromium.WindowHandle).setFrameSize(TempSize);
    end;
  {$ENDIF}
  {$ENDIF}
end;


end.

