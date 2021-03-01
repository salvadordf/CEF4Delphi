// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2021 Salvador Diaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit uCEFLazarusBrowserWindow;

{$mode objfpc}{$H+}
{$i cef.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  uCEFApplication, uCEFChromiumWindow, uCEFTypes, uCEFInterfaces, uCEFChromium,
  uCEFLinkedWinControlBase, uCEFLazApplication, Forms, ExtCtrls, Classes, sysutils;

type

  (* On cocoa closing a browser does not work, while the application is in any other event.
     I.e. if the App is in a button-press event, then the browser will
     only close once that event was finished.
  *)

  TLazarusBrowserWindow = class;

  { TChromiumWrapper }

  TChromiumWrapper = class
    protected type
      TWrapperChromiumState   = (csNoBrowser, csCreatingBrowser, csHasBrowser, csClosingBrowser, csCloseAfterCreate);
      TWrapperState           = (wsNone, wsWaitingForClose, wsSentCloseEventAfterWait, wsDestroyAfterWait);
    protected
      FChromium        : TChromium;
      FChromiumState   : TWrapperChromiumState;
      FWrapperState    : TWrapperState;

      FBrowserWindow   : TLazarusBrowserWindow;
      FLoadUrl         : ustring;

      procedure WebBrowser_OnAfterCreated(Sender: TObject; const browser: ICefBrowser);
      procedure WebBrowser_OnClose(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction); reintroduce;
      procedure WebBrowser_OnBeforeClose(Sender: TObject; const browser: ICefBrowser); reintroduce;
      {$IFDEF FPC}
      procedure   WebBrowser_OnGotFocus(Sender: TObject; const browser: ICefBrowser);
      {$ENDIF}
      procedure   DoCreated(Data: PtrInt);

      procedure   MaybeDestroy;
    public
      constructor Create(AOwner: TLazarusBrowserWindow); reintroduce;
      destructor  Destroy; override;

      function    CreateBrowser: boolean;
      procedure   LoadURL(aURL: ustring);
      procedure   CloseBrowser(aForceClose: boolean);
      function    IsClosed: boolean;
      (* WaitForBrowserClosed calls ProcessMessages.
         It therefore is possible that the TLazarusBrowserWindow will be destroyed
         when this method returns.
         It is the callers responsibility to take any necessary precaution.
      *)
      procedure   WaitForBrowserClosed;
  end;

  { TLazarusBrowserWindow }

  (* On MacOs TLazarusBrowserWindow must wait for OnBrowserClosed before it can
     be destroyed or before its handle can be closed
  *)

  TLazarusBrowserWindow = class(TCEFLinkedWinControlBase)
    private
      FChromiumWrapper  : TChromiumWrapper;

      FOnBrowserClosed  : TNotifyEvent;
      FOnBrowserCreated : TNotifyEvent;
      FTimer            : TTimer;

      procedure   DoCreateBrowser(Sender: TObject);
      procedure DoCreateBrowserAfterContext(Sender: TObject);
    protected
      function    GetChromium: TChromium; override;
      procedure   DestroyHandle; override;
      procedure   RealizeBounds; override;

      procedure   DoEnter; override;
      procedure   DoExit; override;
      procedure   DoOnCreated;
      procedure   DoOnClosed(Data: PtrInt);
      procedure   DoOnFocus(Data: PtrInt);
    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure   CreateHandle; override;

      procedure   CloseBrowser(aForceClose: boolean);
      procedure   WaitForBrowserClosed;
      function    IsClosed: boolean;
      procedure   LoadURL(aURL: ustring);

    published
      property    Chromium; //        : TChromium    read GetChromium;

      property    OnBrowserCreated : TNotifyEvent read FOnBrowserCreated write FOnBrowserCreated;
      (* OnBrowserClosed will not be called, if the TLazarusBrowserWindow is
         destroyed/destroying before the browser is closed.
      *)
      property    OnBrowserClosed  : TNotifyEvent read FOnBrowserClosed write FOnBrowserClosed;
  end;

{$IFDEF FPC}
procedure Register;
{$ENDIF}

implementation

{ TChromiumWrapper }

procedure TChromiumWrapper.WebBrowser_OnAfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  (* We may still be in Chromium.CreateBrowserSync
     In that case initialization will happen after this event,
     but before the call to CreateBrowser returns
  *)
  Application.QueueAsyncCall(@DoCreated, 0);
end;

procedure TChromiumWrapper.WebBrowser_OnClose(Sender: TObject;
  const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
begin
  (* FBrowserWindow should always be <> nil
     If FBrowserWindow is nil (MacOS) then the FBrowserWindow.Handle is destroyed too,
     and CEF should call BeforeClose, without calling DoClose
  *)
  if (FBrowserWindow <> nil) and FBrowserWindow.DestroyChildWindow then
    aAction := cbaDelay
  else
    aAction := cbaClose;
end;

procedure TChromiumWrapper.WebBrowser_OnBeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  FChromiumState := csNoBrowser;

  if (FBrowserWindow <> nil) then begin
    if FWrapperState = wsWaitingForClose then
      FWrapperState := wsSentCloseEventAfterWait
    else
      Application.QueueAsyncCall(@FBrowserWindow.DoOnClosed, 0);
  end;
end;

procedure TChromiumWrapper.WebBrowser_OnGotFocus(Sender: TObject;
  const browser: ICefBrowser);
begin
  if (FBrowserWindow <> nil) then
    Application.QueueAsyncCall(@FBrowserWindow.DoOnFocus, 0);
end;

procedure TChromiumWrapper.DoCreated(Data: PtrInt);
begin

  // Any other state, means this is a late async call
  case FChromiumState of
    csCreatingBrowser: begin
        FChromiumState := csHasBrowser;
        if  FLoadUrl <> '' then
          LoadURL(FLoadUrl);

        if (FBrowserWindow <> nil) then
          FBrowserWindow.DoOnCreated;
      end;
    csCloseAfterCreate: begin
        FChromiumState := csHasBrowser;
        CloseBrowser(True);
      end;
  end;
end;

procedure TChromiumWrapper.MaybeDestroy;
begin
  CloseBrowser(True);
  FBrowserWindow := nil;

  if FWrapperState in [wsWaitingForClose, wsSentCloseEventAfterWait] then
    FWrapperState := wsDestroyAfterWait;

  if FChromiumState = csNoBrowser then
    Destroy;
end;

constructor TChromiumWrapper.Create(AOwner: TLazarusBrowserWindow);
begin
  FBrowserWindow := AOwner;
  FChromiumState := csNoBrowser;
  FWrapperState  := wsNone;

  if not(csDesigning in AOwner.ComponentState) then
    begin
      FChromium                := TChromium.Create(nil);
      FChromium.OnClose        := {$IFDEF FPC}@{$ENDIF}WebBrowser_OnClose;
      FChromium.OnBeforeClose  := {$IFDEF FPC}@{$ENDIF}WebBrowser_OnBeforeClose;
      FChromium.OnAfterCreated := {$IFDEF FPC}@{$ENDIF}WebBrowser_OnAfterCreated;
      {$IFDEF LINUX}
      // This is a workaround for the CEF issue #2026. Read below for more info.
      FChromium.OnGotFocus     := {$IFDEF FPC}@{$ENDIF}WebBrowser_OnGotFocus;
      {$ENDIF}
    end;

  inherited Create;
end;

destructor TChromiumWrapper.Destroy;
begin

  if FChromiumState <> csNoBrowser then
    WaitForBrowserClosed;

  inherited Destroy;
  FChromium.Destroy;
  Application.RemoveAsyncCalls(Self);
end;

function TChromiumWrapper.CreateBrowser: boolean;
begin
  if FChromiumState <> csNoBrowser then
    exit(False);

  FChromiumState := csCreatingBrowser;
  Result := FChromium.CreateBrowser(FBrowserWindow, '');
  if Result then begin
    if FChromium.Initialized then
      DoCreated(0);
  end
  else begin
    FChromiumState := csNoBrowser;
  end;
end;

procedure TChromiumWrapper.LoadURL(aURL: ustring);
begin
  FLoadUrl := '';
  if FChromiumState = csHasBrowser then
    FChromium.LoadURL(aURL)
  else
    FLoadUrl := aURL;
end;

procedure TChromiumWrapper.CloseBrowser(aForceClose: boolean);
begin
  if FChromiumState = csCreatingBrowser then begin
    FChromiumState := csCloseAfterCreate;
  end
  else
  if FChromiumState in [csHasBrowser] then
  begin
    FChromiumState := csClosingBrowser;
    FChromium.CloseBrowser(aForceClose);
  end;
end;

function TChromiumWrapper.IsClosed: boolean;
begin
  Result := FChromiumState = csNoBrowser;
end;

procedure TChromiumWrapper.WaitForBrowserClosed;
begin
  if FChromiumState = csNoBrowser then
    exit;
  if FChromiumState <> csClosingBrowser then
    CloseBrowser(True);

  FWrapperState := wsWaitingForClose;
  while FChromiumState <> csNoBrowser do begin
    Application.ProcessMessages;
    if GlobalCEFApp.ExternalMessagePump then
      GlobalCEFApp.DoMessageLoopWork;
    sleep(5);
  end;

  if (FBrowserWindow <> nil) and
     (FWrapperState = wsSentCloseEventAfterWait)
  then
    Application.QueueAsyncCall(@FBrowserWindow.DoOnClosed, 0);

  if FWrapperState = wsDestroyAfterWait then
    Destroy
  else
    FWrapperState := wsNone;
end;

{ TLazarusBrowserWindow }

procedure TLazarusBrowserWindow.DoCreateBrowser(Sender: TObject);
begin
  if FTimer <> nil then
    FTimer.Enabled := False;

  case FChromiumWrapper.FChromiumState of
    csCreatingBrowser, csHasBrowser: begin
      FreeAndNil(FTimer);
      exit;
    end;
    csClosingBrowser, csCloseAfterCreate: begin
      // need new wrapper // This could prevent an OnBrowserClosed event
      FChromiumWrapper.MaybeDestroy;
      FChromiumWrapper := TChromiumWrapper.Create(Self);
    end;
  end;

  if FChromiumWrapper.CreateBrowser then begin
    FreeAndNil(FTimer);
  end
  else begin
    if GlobalCEFApp.ExternalMessagePump then
      GlobalCEFApp.DoMessageLoopWork;

    if FTimer = nil then
      FTimer := TTimer.Create(Self);
    FTimer.OnTimer := @DoCreateBrowser;
    FTimer.Interval := 100;
    FTimer.Enabled  := True;
  end;
end;

procedure TLazarusBrowserWindow.DoCreateBrowserAfterContext(Sender: TObject);
begin
  {$IFDEF LINUX}
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 20;
  FTimer.OnTimer := @DoCreateBrowser;
  FTimer.Enabled := True;
  {$ELSE}
    DoCreateBrowser(nil);
  {$ENDIF}
end;

function TLazarusBrowserWindow.GetChromium: TChromium;
begin
  Result := FChromiumWrapper.FChromium;
end;

procedure TLazarusBrowserWindow.CreateHandle;
begin
  inherited CreateHandle;
  if not (csDesigning in ComponentState) then begin
    (* On Windows we can create the browser immediately.
       But at least on Linux, we need to wait
    *)

    if GlobalCEFApp is TCefLazApplication then
      TCefLazApplication(GlobalCEFApp).AddContextInitializedHandler(@DoCreateBrowserAfterContext)
    else
      DoCreateBrowserAfterContext(nil);
  end;
end;

procedure TLazarusBrowserWindow.DestroyHandle;
begin
  if FTimer <> nil then
    FreeAndNil(FTimer);

  if (GlobalCEFApp = nil) or
     (FChromiumWrapper.FChromiumState = csNoBrowser) or
     (csDesigning in ComponentState)
  then begin
    inherited DestroyHandle;
    exit;
  end;

  {$IFDEF MACOSX}
  inherited DestroyHandle;
  FChromiumWrapper.CloseBrowser(True);
  {$ELSE}
  FChromiumWrapper.WaitForBrowserClosed;
  inherited DestroyHandle;
  {$ENDIF}
end;

procedure TLazarusBrowserWindow.RealizeBounds;
begin
  inherited RealizeBounds;

  if not (csDesigning in ComponentState) and HandleAllocated then
    Chromium.NotifyMoveOrResizeStarted;
end;

procedure TLazarusBrowserWindow.DoEnter;
begin
  inherited DoEnter;
  If not(csDesigning in ComponentState) then Chromium.SetFocus(True);
end;

procedure TLazarusBrowserWindow.DoExit;
begin
  inherited DoExit;
  if not(csDesigning in ComponentState) then
    Chromium.SendCaptureLostEvent;
end;

procedure TLazarusBrowserWindow.DoOnCreated;
begin
  {$IFDEF FPC}{$IFDEF LINUX}
  Chromium.UpdateXWindowVisibility(Visible);
  Chromium.UpdateBrowserSize(Left, Top, Width, Height);
  {$ENDIF}{$ENDIF}
  if Assigned(FOnBrowserCreated) then
    FOnBrowserCreated(Self);
end;

procedure TLazarusBrowserWindow.DoOnClosed(Data: PtrInt);
begin
  if (not(csDestroying in ComponentState)) and
     Assigned(FOnBrowserClosed)
  then
    FOnBrowserClosed(Self);
end;

procedure TLazarusBrowserWindow.DoOnFocus(Data: PtrInt);
begin
  SetFocus;
end;

constructor TLazarusBrowserWindow.Create(AOwner: TComponent);
begin
  FChromiumWrapper := TChromiumWrapper.Create(Self);
  inherited Create(AOwner);
end;

destructor TLazarusBrowserWindow.Destroy;
begin
  inherited Destroy;
  FChromiumWrapper.MaybeDestroy;
  Application.RemoveAsyncCalls(Self);
end;

procedure TLazarusBrowserWindow.CloseBrowser(aForceClose: boolean);
begin
  FChromiumWrapper.CloseBrowser(aForceClose);
end;

procedure TLazarusBrowserWindow.WaitForBrowserClosed;
begin
  FChromiumWrapper.WaitForBrowserClosed;
end;

function TLazarusBrowserWindow.IsClosed: boolean;
begin
  Result := FChromiumWrapper.IsClosed;
end;

procedure TLazarusBrowserWindow.LoadURL(aURL: ustring);
begin
  FChromiumWrapper.LoadURL(aURL);
end;

{$IFDEF FPC}

procedure Register;
begin
  {$I res/tlazarusbrowserwindow.lrs}
  RegisterComponents('Chromium', [TLazarusBrowserWindow]);
end;
{$ENDIF}

end.

