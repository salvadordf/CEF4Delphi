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
  uCEFLinkedWinControlBase, uCEFLazApplication, uCEFBrowserViewComponent, Forms,
  ExtCtrls, Controls, Classes, sysutils;

type

  (* On cocoa closing a browser does not work, while the application is in any other event.
     I.e. if the App is in a button-press event, then the browser will
     only close once that event was finished.
  *)

  { TLazChromium }

  TLazChromium = class(TChromium)
    private type
      TLazChromiumState   = (csNoBrowser, csCreatingBrowser, csHasBrowser, csClosingBrowser, csCloseAfterCreate);
    private
      FState               : TLazChromiumState;
      FOnBrowserClosed     : TNotifyEvent;
      FOnBrowserCreated    : TNotifyEvent;

      FLoadUrl, FFrameName : ustring;
      function GetIsClosing: Boolean;

    protected
      function    GetHasBrowser : boolean; reintroduce;

      procedure   doOnBeforeClose(const ABrowser: ICefBrowser); override;
      procedure   doOnAfterCreated(const ABrowser: ICefBrowser); override;

      procedure   DoCreated(Data: PtrInt);
      procedure   DoOnClosed(Data: PtrInt);
    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;

      function    CreateBrowser(const aBrowserParent: TWinControl = nil;
                  const aWindowName: ustring = ''; const aContext: ICefRequestContext =
                  nil; const aExtraInfo: ICefDictionaryValue = nil): boolean; overload; override;
      function    CreateBrowser(aParentHandle: TCefWindowHandle;
                  aParentRect: TRect; const aWindowName: ustring = '';
                  const aContext: ICefRequestContext = nil;
                  const aExtraInfo: ICefDictionaryValue = nil): boolean; overload; override;
      procedure   CreateBrowser(const aWindowName: ustring); overload; override;
      function    CreateBrowser(const aURL: ustring;
                  const aBrowserViewComp: TCEFBrowserViewComponent;
                  const aContext: ICefRequestContext = nil;
                  const aExtraInfo: ICefDictionaryValue = nil): boolean; overload; override;

      // CloseBrowser will work, even if the browser is still in creation, and Initialized is still false
      procedure   CloseBrowser(aForceClose: boolean); reintroduce;

      // LoadURL will work, even if the browser is still in creation, and Initialized is still false
      procedure   LoadURL(const aURL: ustring; const aFrameName: ustring = ''); overload;

      property    HasBrowser: Boolean read GetHasBrowser; // Includes browser in creation
      property    IsClosing : Boolean read GetIsClosing;

      (* - Events to be called in main thread
         - OnBrowserCreated: the parent event may be called when procedure Initialized is still false.
         - OnBrowserCreated: may not be called, if the CloseBrowser has already been called
      *)
      property    OnBrowserCreated : TNotifyEvent read FOnBrowserCreated write FOnBrowserCreated;
      property    OnBrowserClosed  : TNotifyEvent read FOnBrowserClosed write FOnBrowserClosed;
  end;

  TLazarusBrowserWindow = class;

  { TChromiumWrapper }

  TChromiumWrapper = class
    protected type
      TWrapperState           = (wsNone, wsWaitingForClose, wsSentCloseEventAfterWait, wsDestroyAfterWait);
    protected
      FChromium        : TLazChromium;
      FWrapperState    : TWrapperState;
      FBrowserWindow   : TLazarusBrowserWindow;

      procedure   DoOnAfterCreated(Sender: TObject);
      procedure   DoOnBeforeClose(Sender: TObject);

      procedure   BrowserThread_OnClose(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
      {$IFDEF FPC}
      procedure   BrowserThread_OnGotFocus(Sender: TObject; const browser: ICefBrowser);
      {$ENDIF}

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

      property Chromium: TLazChromium read FChromium;
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
      procedure   DoCreateBrowserAfterContext(Sender: TObject);
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

{ TLazChromium }

function TLazChromium.GetIsClosing: Boolean;
begin
  Result := FState in [csCloseAfterCreate, csClosingBrowser];
end;

function TLazChromium.GetHasBrowser: boolean;
begin
  Result := (FState <> csNoBrowser) or (inherited GetHasBrowser);
end;

procedure TLazChromium.doOnBeforeClose(const ABrowser: ICefBrowser);
begin
  inherited doOnBeforeClose(ABrowser);

  FState := csNoBrowser;
  Application.QueueAsyncCall(@DoOnClosed, 0);
end;

procedure TLazChromium.doOnAfterCreated(const ABrowser: ICefBrowser);
begin
  inherited doOnAfterCreated(ABrowser);
  (* We may still be in Chromium.CreateBrowserSync
     In that case initialization will happen after this event,
     but before the call to CreateBrowser returns
  *)
  Application.QueueAsyncCall(@DoCreated, 0);
end;

procedure TLazChromium.DoCreated(Data: PtrInt);
var
  u, f: ustring;
begin
  // Any other state, means this is a late async call
  case FState of
    csCreatingBrowser: begin
        FState := csHasBrowser;
        if  FLoadUrl <> '' then begin
          u := FLoadUrl;
          f := FFrameName;
          LoadURL(u, f);
        end;

        if (FOnBrowserCreated <> nil) then
          FOnBrowserCreated(Self);
      end;
    csCloseAfterCreate: begin
        FState := csHasBrowser;
        CloseBrowser(True);
      end;
  end;
end;

procedure TLazChromium.DoOnClosed(Data: PtrInt);
begin
  if (FOnBrowserClosed <> nil) then
    FOnBrowserClosed(Self);
end;

constructor TLazChromium.Create(AOwner: TComponent);
begin
  FState := csNoBrowser;
  inherited Create(AOwner);
end;

destructor TLazChromium.Destroy;
begin
  inherited Destroy;
  Application.RemoveAsyncCalls(Self);
end;

function TLazChromium.CreateBrowser(const aBrowserParent: TWinControl;
  const aWindowName: ustring; const aContext: ICefRequestContext;
  const aExtraInfo: ICefDictionaryValue): boolean;
begin
  FState := csCreatingBrowser;
  Result := inherited CreateBrowser(aBrowserParent, aWindowName, aContext,
    aExtraInfo);
  if Initialized then
    DoCreated(0);
end;

function TLazChromium.CreateBrowser(aParentHandle: TCefWindowHandle;
  aParentRect: TRect; const aWindowName: ustring;
  const aContext: ICefRequestContext; const aExtraInfo: ICefDictionaryValue): boolean;
begin
  FState := csCreatingBrowser;
  Result := inherited CreateBrowser(aParentHandle, aParentRect, aWindowName,
    aContext, aExtraInfo);
  if Initialized then
    DoCreated(0);
end;

procedure TLazChromium.CreateBrowser(const aWindowName: ustring);
begin
  FState := csCreatingBrowser;
  inherited CreateBrowser(aWindowName);
  if Initialized then
    DoCreated(0);
end;

function TLazChromium.CreateBrowser(const aURL: ustring;
  const aBrowserViewComp: TCEFBrowserViewComponent;
  const aContext: ICefRequestContext; const aExtraInfo: ICefDictionaryValue
  ): boolean;
begin
  FState := csCreatingBrowser;
  Result := inherited CreateBrowser(aURL, aBrowserViewComp, aContext, aExtraInfo);
  if Initialized then
    DoCreated(0);
end;

procedure TLazChromium.CloseBrowser(aForceClose: boolean);
begin
  if FState = csCreatingBrowser then begin
    FState := csCloseAfterCreate;
    exit;
  end
  else
  if FState in [csHasBrowser] then
  begin
    FState := csClosingBrowser;
    inherited CloseBrowser(aForceClose);
  end;
end;

procedure TLazChromium.LoadURL(const aURL: ustring; const aFrameName: ustring);
begin
  FLoadUrl := '';
  FFrameName := '';
  if FState = csHasBrowser then
    begin
      inherited LoadURL(aURL, aFrameName);
    end
  else
    begin
      FLoadUrl := aURL;
      FFrameName := aFrameName;
    end;
end;

{ TChromiumWrapper }

procedure TChromiumWrapper.DoOnAfterCreated(Sender: TObject);
begin
  if (FBrowserWindow <> nil) then
    FBrowserWindow.DoOnCreated;
end;

procedure TChromiumWrapper.BrowserThread_OnClose(Sender: TObject;
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

procedure TChromiumWrapper.DoOnBeforeClose(Sender: TObject);
begin
  if (FBrowserWindow <> nil) then begin
    if FWrapperState = wsWaitingForClose then
      FWrapperState := wsSentCloseEventAfterWait
    else
     FBrowserWindow.DoOnClosed(0);
  end;
end;

procedure TChromiumWrapper.BrowserThread_OnGotFocus(Sender: TObject;
  const browser: ICefBrowser);
begin
  if (FBrowserWindow <> nil) then
    Application.QueueAsyncCall(@FBrowserWindow.DoOnFocus, 0);
end;

procedure TChromiumWrapper.MaybeDestroy;
begin
  CloseBrowser(True);
  FBrowserWindow := nil;

  if FWrapperState in [wsWaitingForClose, wsSentCloseEventAfterWait] then
    FWrapperState := wsDestroyAfterWait;

  if not FChromium.HasBrowser then
    Destroy;
end;

constructor TChromiumWrapper.Create(AOwner: TLazarusBrowserWindow);
begin
  FBrowserWindow := AOwner;
  FWrapperState  := wsNone;

  if not(csDesigning in AOwner.ComponentState) then
    begin
      FChromium                  := TLazChromium.Create(nil);
      FChromium.OnClose          := {$IFDEF FPC}@{$ENDIF}BrowserThread_OnClose;
      FChromium.OnBrowserClosed  := {$IFDEF FPC}@{$ENDIF}DoOnBeforeClose;
      FChromium.OnBrowserCreated := {$IFDEF FPC}@{$ENDIF}DoOnAfterCreated;
      {$IFDEF LINUX}
      // This is a workaround for the CEF issue #2026. Read below for more info.
      FChromium.OnGotFocus     := {$IFDEF FPC}@{$ENDIF}BrowserThread_OnGotFocus;
      {$ENDIF}
    end;

  inherited Create;
end;

destructor TChromiumWrapper.Destroy;
begin
  if FChromium.HasBrowser then
    WaitForBrowserClosed;

  inherited Destroy;
  FChromium.Destroy;
  Application.RemoveAsyncCalls(Self);
end;

function TChromiumWrapper.CreateBrowser: boolean;
begin
  if FChromium.HasBrowser then
    exit(False);

  Result := FChromium.CreateBrowser(FBrowserWindow, '');
end;

procedure TChromiumWrapper.LoadURL(aURL: ustring);
begin
  FChromium.LoadURL(aURL);
end;

procedure TChromiumWrapper.CloseBrowser(aForceClose: boolean);
begin
  FChromium.CloseBrowser(aForceClose);
end;

function TChromiumWrapper.IsClosed: boolean;
begin
  Result := not FChromium.HasBrowser;
end;

procedure TChromiumWrapper.WaitForBrowserClosed;
begin
  if not FChromium.HasBrowser then
    exit;
  FChromium.CloseBrowser(True);

  FWrapperState := wsWaitingForClose;
  while FChromium.HasBrowser do begin
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

  if FChromiumWrapper.Chromium.HasBrowser then begin
    if not FChromiumWrapper.Chromium.IsClosing then begin
      FreeAndNil(FTimer);
      exit;
    end
    else begin
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
  {$IFnDEF WINDOWS}
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
     (not FChromiumWrapper.Chromium.HasBrowser) or
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

