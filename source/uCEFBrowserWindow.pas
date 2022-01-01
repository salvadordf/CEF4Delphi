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
//        Copyright © 2022 Salvador Diaz Fau. All rights reserved.
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

unit uCEFBrowserWindow;

{$mode objfpc}{$H+}
{$i cef.inc}

interface

uses
  {$IFDEF FPC}
  LResources, PropEdits,
  {$ENDIF}
  uCEFApplication, uCEFChromiumWindow, uCEFTypes, uCEFInterfaces, uCEFChromium,
  uCEFLinkedWinControlBase, uCEFBrowserViewComponent,
  uCEFChromiumEvents, Forms, ExtCtrls, Controls, Classes, sysutils;

type

  (* On cocoa closing a browser does not work, while the application is in any other event.
     I.e. if the App is in a button-press event, then the browser will
     only close once that event was finished.
  *)

  { TEmbeddedChromium

    1) TEmbeddedChromium keeps track of the browser while it is created.
       This allows for CloseBrowser to function, even if the Browser object is not
       yet known.
       Also calls to "LoadUrl" are cached until the browser object is created.

    2) TEmbeddedChromium adds InternalEvents that can be hooked by the
       component that owns the TEmbeddedChromium.
       This means the default published events are available to the end user.
       Published events that should not be available are hidden via THiddenPropertyEditor
       * Hidden event properties must not be assigned by any end user code. *
  }

  TEmbeddedChromium = class(TChromium)
    private type
      TChromiumBrowserState   = (csNoBrowser, csCreatingBrowser, csHasBrowser, csClosingBrowser, csCloseAfterCreate);
    private
      FInternalOnGotFocus: TOnGotFocus;
      FState                    : TChromiumBrowserState;
      FInternalOnBrowserClosed  : TNotifyEvent;
      FInternalOnBrowserCreated : TNotifyEvent;

      FLoadUrl, FFrameName : ustring;
      function GetIsClosing: Boolean;
      procedure SetInternalOnClose(AValue: TOnClose);

    protected
      function    GetHasBrowser : boolean; reintroduce;

      procedure   doOnBeforeClose(const ABrowser: ICefBrowser); override;
      procedure   doOnAfterCreated(const ABrowser: ICefBrowser); override;
      procedure   doOnGotFocus(const Abrowser: ICefBrowser); override;
      function    MustCreateFocusHandler: boolean; override;

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
                  const aExtraInfo: ICefDictionaryValue = nil;
                  aForceAsPopup : boolean = False): boolean; overload; override;
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

      (* - Events for use by the Owning component ONLY
         - Events are called in main thread
         - OnBrowserCreated: the parent event may be called when procedure Initialized is still false.
         - OnBrowserCreated: may not be called, if the CloseBrowser has already been called
      *)
      property    InternalOnBrowserCreated : TNotifyEvent read FInternalOnBrowserCreated write FInternalOnBrowserCreated;
      property    InternalOnBrowserClosed  : TNotifyEvent read FInternalOnBrowserClosed  write FInternalOnBrowserClosed;
      property    InternalOnGotFocus       : TOnGotFocus  read FInternalOnGotFocus       write FInternalOnGotFocus;
  end;

  TBrowserWindow = class;

  { TChromiumWrapper }

  TChromiumWrapper = class
    protected type
      TWrapperState           = (wsNone, wsWaitingForClose, wsSentCloseEventAfterWait, wsDestroyAfterWait);
    protected
      FChromium        : TEmbeddedChromium;
      FWrapperState    : TWrapperState;
      FBrowserWindow   : TBrowserWindow;

      procedure   DoOnAfterCreated(Sender: TObject);
      procedure   DoOnBeforeClose(Sender: TObject);

      procedure   BrowserThread_OnClose(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
      {$IFDEF FPC}
      procedure   BrowserThread_OnGotFocus(Sender: TObject; const browser: ICefBrowser);
      {$ENDIF}

      procedure   MaybeDestroy;
    public
      constructor Create(AOwner: TBrowserWindow); reintroduce;
      destructor  Destroy; override;

      function    CreateBrowser: boolean;
      procedure   LoadURL(aURL: ustring);
      procedure   CloseBrowser(aForceClose: boolean);
      function    IsClosed: boolean;
      (* WaitForBrowserClosed calls ProcessMessages.
         It therefore is possible that the TBrowserWindow will be destroyed
         when this method returns.
         It is the callers responsibility to take any necessary precaution.
      *)
      procedure   WaitForBrowserClosed;

    published
      property Chromium: TEmbeddedChromium read FChromium;
  end;

  { TBrowserWindow

    A simple "drop on the Form" component for an full embedded browser.

    The component handles most events required by CEF.
    The only additions needed to be made by the User in their code are:

    * Implement TForm.OnCloseQuery
      CEF must be able to destroy the browser, before the main form is closed.
      (That is while the Form still has a Handle, and the event loop is still
       running)
      It is adviced to do the same for any other form (other than the main form).

      TForm.OnCloseQuery should call (for each TBrowserWindow)
        TBrowserWindow.CloseBrowser(True);
      The Form can be allowed to close by setting (checking for all BrowserWindows)
        CanClose := BrowserWindow.IsClosed;

      On Windows and Linux it is also possible to Destroy the TBrowserWindow.
      This will wait for the browser to close, and after that the form can be closed.
      - However, this must be done in OnCloseQuery (or before).
      - Once TForm.Destroy is called, it is to late. By that time the event loop
        no longer runs.

      *** IMPORTANT: (MacOS) ***
        On MacOs CloseBrowser() must be called, and the *event* must be awaited.
        Neither destroying the component, nor waiting with App.ProcessMessages will
        work.
        On MacOS, CEF will not finish until the OnCloseQuery event returned to the
        main event loop. (Hence ProcessMessage does not work).
        The same is true for any action taken in OnClick or other event.
        CEF always waits for any event to return to the main event loop.
        See also the BrowserWindowEX example how that affect modal forms.

    * Implement TBrowserWindow.OnBrowserClosed
      If TForm.OnCloseQuery called CloseBrowser, this callback can be used to
      call Form.Close again (the callback should check if the browser was
      closed by OnCloseQuery.

    * On Windows:
      handle the WM_ENTERMENULOOP and WM_EXITMENULOOP, as shown in examples

    * Optional prevent pop-up windows by implementing
      Chromium.BeforePopup
      Chromium.OpenUrlFromTab
  }

  TBrowserWindow = class(TCEFLinkedWinControlBase)
    private
      FChromiumWrapper  : TChromiumWrapper;

      FOnBrowserClosed  : TNotifyEvent;
      FOnBrowserCreated : TNotifyEvent;
      FTimer            : TTimer;

      procedure   DoCreateBrowser(Sender: TObject);
      procedure   DoCreateBrowserAfterContext(Sender: TObject);
      function    GetEmbeddedChromium: TEmbeddedChromium;
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
      property    Chromium: TEmbeddedChromium read GetEmbeddedChromium;

      property    OnBrowserCreated : TNotifyEvent read FOnBrowserCreated write FOnBrowserCreated;
      (* OnBrowserClosed will not be called, if the TBrowserWindow is
         destroyed/destroying before the browser is closed.
      *)
      property    OnBrowserClosed  : TNotifyEvent read FOnBrowserClosed write FOnBrowserClosed;
  end;

{$IFDEF FPC}
procedure Register;
{$ENDIF}

implementation

{ TEmbeddedChromium }

function TEmbeddedChromium.GetIsClosing: Boolean;
begin
  Result := FState in [csCloseAfterCreate, csClosingBrowser];
end;

procedure TEmbeddedChromium.SetInternalOnClose(AValue: TOnClose);
begin
  inherited OnClose := AValue;
end;

function TEmbeddedChromium.GetHasBrowser: boolean;
begin
  Result := (FState <> csNoBrowser) or (inherited GetHasBrowser);
end;

procedure TEmbeddedChromium.doOnBeforeClose(const ABrowser: ICefBrowser);
begin
  inherited doOnBeforeClose(ABrowser);

  FState := csNoBrowser;
  Application.QueueAsyncCall(@DoOnClosed, 0);
end;

procedure TEmbeddedChromium.doOnAfterCreated(const ABrowser: ICefBrowser);
begin
  inherited doOnAfterCreated(ABrowser);
  (* We may still be in Chromium.CreateBrowserSync
     In that case initialization will happen after this event,
     but before the call to CreateBrowser returns
  *)
  Application.QueueAsyncCall(@DoCreated, 0);
end;

procedure TEmbeddedChromium.doOnGotFocus(const Abrowser: ICefBrowser);
begin
  inherited doOnGotFocus(Abrowser);
  if Assigned(FInternalOnGotFocus) then
    FInternalOnGotFocus(Self, Abrowser);
end;

function TEmbeddedChromium.MustCreateFocusHandler: boolean;
begin
  Result := assigned(FInternalOnGotFocus) or
            inherited MustCreateFocusHandler;
end;

procedure TEmbeddedChromium.DoCreated(Data: PtrInt);
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

        if (FInternalOnBrowserCreated <> nil) then
          FInternalOnBrowserCreated(Self);
      end;
    csCloseAfterCreate: begin
        FState := csHasBrowser;
        CloseBrowser(True);
      end;
  end;
end;

procedure TEmbeddedChromium.DoOnClosed(Data: PtrInt);
begin
  if (FInternalOnBrowserClosed <> nil) then
    FInternalOnBrowserClosed(Self);
end;

constructor TEmbeddedChromium.Create(AOwner: TComponent);
begin
  FState := csNoBrowser;
  inherited Create(AOwner);
  SetSubComponent(True);
  Name := 'Chromium';
end;

destructor TEmbeddedChromium.Destroy;
begin
  inherited Destroy;
  Application.RemoveAsyncCalls(Self);
end;

function TEmbeddedChromium.CreateBrowser(const aBrowserParent: TWinControl;
  const aWindowName: ustring; const aContext: ICefRequestContext;
  const aExtraInfo: ICefDictionaryValue): boolean;
begin
  FState := csCreatingBrowser;
  Result := inherited CreateBrowser(aBrowserParent, aWindowName, aContext,
    aExtraInfo);
  if Initialized then
    DoCreated(0);
end;

function TEmbeddedChromium.CreateBrowser(aParentHandle: TCefWindowHandle;
  aParentRect: TRect; const aWindowName: ustring;
  const aContext: ICefRequestContext; const aExtraInfo: ICefDictionaryValue;
  aForceAsPopup : boolean): boolean;
begin
  FState := csCreatingBrowser;
  Result := inherited CreateBrowser(aParentHandle, aParentRect, aWindowName,
    aContext, aExtraInfo, aForceAsPopup);
  if Initialized then
    DoCreated(0);
end;

function TEmbeddedChromium.CreateBrowser(const aURL: ustring;
  const aBrowserViewComp: TCEFBrowserViewComponent;
  const aContext: ICefRequestContext; const aExtraInfo: ICefDictionaryValue
  ): boolean;
begin
  FState := csCreatingBrowser;
  Result := inherited CreateBrowser(aURL, aBrowserViewComp, aContext, aExtraInfo);
  if Initialized then
    DoCreated(0);
end;

procedure TEmbeddedChromium.CloseBrowser(aForceClose: boolean);
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

procedure TEmbeddedChromium.LoadURL(const aURL: ustring; const aFrameName: ustring);
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
  if FChromium.Owner <> nil then
    FBrowserWindow.RemoveComponent(FChromium);
  CloseBrowser(True);
  FBrowserWindow := nil;

  if FWrapperState in [wsWaitingForClose, wsSentCloseEventAfterWait] then
    FWrapperState := wsDestroyAfterWait;

  if not FChromium.HasBrowser then
    Destroy;
end;

constructor TChromiumWrapper.Create(AOwner: TBrowserWindow);
begin
  FBrowserWindow := AOwner;
  FWrapperState  := wsNone;

  FChromium                  := TEmbeddedChromium.Create(AOwner);
  if not(csDesigning in AOwner.ComponentState) then
    begin
      FChromium.OnClose                  := {$IFDEF FPC}@{$ENDIF}BrowserThread_OnClose;
      FChromium.InternalOnBrowserClosed  := {$IFDEF FPC}@{$ENDIF}DoOnBeforeClose;
      FChromium.InternalOnBrowserCreated := {$IFDEF FPC}@{$ENDIF}DoOnAfterCreated;
      {$IFDEF LINUX}
      // This is a workaround for the CEF issue #2026. Read below for more info.
      FChromium.InternalOnGotFocus     := {$IFDEF FPC}@{$ENDIF}BrowserThread_OnGotFocus;
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

{ TBrowserWindow }

procedure TBrowserWindow.DoCreateBrowser(Sender: TObject);
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

procedure TBrowserWindow.DoCreateBrowserAfterContext(Sender: TObject);
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

function TBrowserWindow.GetEmbeddedChromium: TEmbeddedChromium;
begin
  Result := FChromiumWrapper.Chromium;
end;

function TBrowserWindow.GetChromium: TChromium;
begin
  Result := FChromiumWrapper.FChromium;
end;

procedure TBrowserWindow.CreateHandle;
begin
  inherited CreateHandle;
  if not (csDesigning in ComponentState) then begin
    (* On Windows we can create the browser immediately.
       But at least on Linux, we need to wait
    *)

    GlobalCEFApp.AddContextInitializedHandler(@DoCreateBrowserAfterContext);
  end;
end;

procedure TBrowserWindow.DestroyHandle;
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

procedure TBrowserWindow.RealizeBounds;
begin
  inherited RealizeBounds;

  if not (csDesigning in ComponentState) and HandleAllocated then
    Chromium.NotifyMoveOrResizeStarted;
end;

procedure TBrowserWindow.DoEnter;
begin
  inherited DoEnter;
  If not(csDesigning in ComponentState) then Chromium.SetFocus(True);
end;

procedure TBrowserWindow.DoExit;
begin
  inherited DoExit;
  if not(csDesigning in ComponentState) then
    Chromium.SendCaptureLostEvent;
end;

procedure TBrowserWindow.DoOnCreated;
begin
  {$IFDEF FPC}{$IFDEF LINUX}
  Chromium.UpdateXWindowVisibility(Visible);
  Chromium.UpdateBrowserSize(Left, Top, Width, Height);
  {$ENDIF}{$ENDIF}
  if Assigned(FOnBrowserCreated) then
    FOnBrowserCreated(Self);
end;

procedure TBrowserWindow.DoOnClosed(Data: PtrInt);
begin
  if (not(csDestroying in ComponentState)) and
     Assigned(FOnBrowserClosed)
  then
    FOnBrowserClosed(Self);
end;

procedure TBrowserWindow.DoOnFocus(Data: PtrInt);
begin
  SetFocus;
end;

constructor TBrowserWindow.Create(AOwner: TComponent);
begin
  FChromiumWrapper := TChromiumWrapper.Create(Self);
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOwnedChildrenNotSelectable];
end;

destructor TBrowserWindow.Destroy;
begin
  RemoveComponent(FChromiumWrapper.FChromium);
  inherited Destroy;
  FChromiumWrapper.MaybeDestroy;
  Application.RemoveAsyncCalls(Self);
end;

procedure TBrowserWindow.CloseBrowser(aForceClose: boolean);
begin
  FChromiumWrapper.CloseBrowser(aForceClose);
end;

procedure TBrowserWindow.WaitForBrowserClosed;
begin
  FChromiumWrapper.WaitForBrowserClosed;
end;

function TBrowserWindow.IsClosed: boolean;
begin
  Result := FChromiumWrapper.IsClosed;
end;

procedure TBrowserWindow.LoadURL(aURL: ustring);
begin
  FChromiumWrapper.LoadURL(aURL);
end;

{$IFDEF FPC}

procedure Register;
begin
  {$I res/TBrowserWindow.lrs}
  RegisterComponents('Chromium', [TBrowserWindow]);
  RegisterClass(TEmbeddedChromium);
  RegisterPropertyEditor(TypeInfo(TOnClose), TEmbeddedChromium, 'OnClose', THiddenPropertyEditor);
end;
{$ENDIF}

end.

