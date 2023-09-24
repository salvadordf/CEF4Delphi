unit uCEFChromiumWindow;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows, WinApi.Messages,{$ENDIF} System.Classes, Vcl.Controls,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, Forms, Controls, Graphics,
    {$IFDEF FPC}
    LCLProc, LCLType, LCLIntf, LResources, LMessages, InterfaceBase,
    {$ELSE}
    Messages,
    {$ENDIF}
  {$ENDIF}
  uCEFWindowParent, uCEFChromium, uCEFInterfaces, uCEFConstants, uCEFTypes,
  uCEFWinControl, uCEFLinkedWinControlBase;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pfidWindows)]{$ENDIF}{$ENDIF}
  /// <summary>
  /// <para>This component puts together a TChromium and a TCEFWindowParent to embbed a
  /// web browser with only one component.</para>
  /// <para>This component should only be used in extremely simple applications with simple browsers.
  /// In other cases it's recomended using a TChromium with a TCEFWindowParent as shown in the
  /// SimpleBrowser2 demo.</para>
  /// </summary>
  TChromiumWindow = class(TCEFLinkedWinControlBase)
    protected
      FChromium       : TChromium;
      FOnClose        : TNotifyEvent;
      FOnBeforeClose  : TNotifyEvent;
      FOnAfterCreated : TNotifyEvent;

      function    GetChromium: TChromium; override;
      function    GetBrowserInitialized : boolean;
      {$IFDEF MSWINDOWS}
      procedure   OnCloseMsg(var aMessage : TMessage); message CEF_DOONCLOSE;
      procedure   OnAfterCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
      {$ENDIF}
      procedure   WebBrowser_OnClose(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
      procedure   WebBrowser_OnBeforeClose(Sender: TObject; const browser: ICefBrowser);
      procedure   WebBrowser_OnAfterCreated(Sender: TObject; const browser: ICefBrowser);
      {$IFDEF FPC}
      procedure   WebBrowser_OnGotFocus(Sender: TObject; const browser: ICefBrowser);
      procedure   BrowserSetFocusMsg(Data: PtrInt);
      procedure   BrowserAfterCreated(Data: PtrInt);
      procedure   BrowserOnCLose(Data: PtrInt);
      {$ENDIF}
      procedure   DoEnter; override;
      procedure   DoExit; override;

   public
      constructor Create(AOwner: TComponent); override;
      procedure   AfterConstruction; override;
      /// <summary>
      /// <para>Used to create the browser after the global request context has been
      /// initialized. You need to set all properties and events before calling
      /// this function because it will only create the internal handlers needed
      /// for those events and the property values will be used in the browser
      /// initialization.</para>
      /// <para>The browser will be fully initialized when the TChromiumWindow.OnAfterCreated
      /// event is triggered.</para>
      /// </summary>
      function    CreateBrowser : boolean;
      /// <summary>
      /// Request that the browser close. The JavaScript 'onbeforeunload' event will
      /// be fired. If |aForceClose| is false (0) the event handler, if any, will be
      /// allowed to prompt the user and the user can optionally cancel the close.
      /// If |aForceClose| is true (1) the prompt will not be displayed and the
      /// close will proceed. Results in a call to
      /// ICefLifeSpanHandler.DoClose() if the event handler allows the close
      /// or if |aForceClose| is true (1). See ICefLifeSpanHandler.DoClose()
      /// documentation for additional usage information.
      /// </summary>
      procedure   CloseBrowser(aForceClose : boolean);
      /// <summary>
      /// Used to navigate to a URL.
      /// </summary>
      procedure   LoadURL(const aURL : ustring);
      /// <summary>
      /// Notify the browser that the window hosting it is about to be moved or
      /// resized. This function is only used on Windows and Linux.
      /// </summary>
      procedure   NotifyMoveOrResizeStarted;
      /// <summary>
      /// TChromium instance used by this component.
      /// </summary>
      property ChromiumBrowser  : TChromium       read GetChromium;
      /// <summary>
      /// Returns true when the browser is fully initialized and it's not being closed.
      /// </summary>
      property Initialized      : boolean         read GetBrowserInitialized;

    published
      /// <summary>
      /// <para>Called when a browser has recieved a request to close. This may result
      /// directly from a call to ICefBrowserHost.*CloseBrowser or indirectly
      /// if the browser is parented to a top-level window created by CEF and the
      /// user attempts to close that window (by clicking the 'X', for example). The
      /// OnClose function will be called after the JavaScript 'onunload' event
      /// has been fired.</para>
      ///
      /// <para>An application should handle top-level owner window close notifications by
      /// calling ICefBrowserHost.TryCloseBrowser or
      /// ICefBrowserHost.CloseBrowser(false) instead of allowing the window
      /// to close immediately (see the examples below). This gives CEF an
      /// opportunity to process the 'onbeforeunload' event and optionally cancel
      /// the close before OnClose is called.</para>
      ///
      /// <para>When windowed rendering is enabled CEF will internally create a window or
      /// view to host the browser. In that case returning false (0) from OnClose()
      /// will send the standard close notification to the browser's top-level owner
      /// window (e.g. WM_CLOSE on Windows, performClose: on OS X, "delete_event" on
      /// Linux or ICefWindowDelegate.CanClose callback from Views). If the
      /// browser's host window/view has already been destroyed (via view hierarchy
      /// tear-down, for example) then OnClose() will not be called for that
      /// browser since is no longer possible to cancel the close.</para>
      ///
      /// <para>When windowed rendering is disabled returning false (0) from OnClose()
      /// will cause the browser object to be destroyed immediately.</para>
      ///
      /// <para>If the browser's top-level owner window requires a non-standard close
      /// notification then send that notification from OnClose() and return true.</para>
      ///
      /// <para>The ICefLifeSpanHandler.OnBeforeClose function will be called
      /// after OnClose() (if OnClose() is called) and immediately before the
      /// browser object is destroyed. The application should only exit after
      /// OnBeforeClose() has been called for all existing browsers.</para>
      ///
      /// <para>The below examples describe what should happen during window close when
      /// the browser is parented to an application-provided top-level window.</para>
      ///
      /// <para>Example 1: Using ICefBrowserHost.TryCloseBrowser(). This is
      /// recommended for clients using standard close handling and windows created
      /// on the browser process UI thread.</para>
      /// <code>
      /// 1.  User clicks the window close button which sends a close notification
      ///     to the application's top-level window.
      /// 2.  Application's top-level window receives the close notification and
      ///     calls TryCloseBrowser() (which internally calls CloseBrowser(false)).
      ///     TryCloseBrowser() returns false so the client cancels the window
      ///     close.
      /// 3.  JavaScript 'onbeforeunload' handler executes and shows the close
      ///     confirmation dialog (which can be overridden via
      ///     ICefJSDialogHandler.OnBeforeUnloadDialog()).
      /// 4.  User approves the close.
      /// 5.  JavaScript 'onunload' handler executes.
      /// 6.  CEF sends a close notification to the application's top-level window
      ///     (because OnClose() returned false by default).
      /// 7.  Application's top-level window receives the close notification and
      ///     calls TryCloseBrowser(). TryCloseBrowser() returns true so the client
      ///     allows the window close.
      /// 8.  Application's top-level window is destroyed.
      /// 9.  Application's OnBeforeClose() handler is called and the browser object is destroyed.
      /// 10. Application exits by calling cef_quit_message_loop() if no other browsers exist.
      /// </code>
      /// <para>Example 2: Using ICefBrowserHost::CloseBrowser(false) and
      /// implementing the OnClose() callback. This is recommended for clients
      /// using non-standard close handling or windows that were not created on the
      /// browser process UI thread.</para>
      /// <code>
      /// 1.  User clicks the window close button which sends a close notification
      ///     to the application's top-level window.
      /// 2.  Application's top-level window receives the close notification and:
      ///     A. Calls ICefBrowserHost.CloseBrowser(false).
      ///     B. Cancels the window close.
      /// 3.  JavaScript 'onbeforeunload' handler executes and shows the close
      ///     confirmation dialog (which can be overridden via
      ///     ICefJSDialogHandler.OnBeforeUnloadDialog()).
      /// 4.  User approves the close.
      /// 5.  JavaScript 'onunload' handler executes.
      /// 6.  Application's OnClose() handler is called. Application will:
      ///     A. Set a flag to indicate that the next close attempt will be allowed.
      ///     B. Return false.
      /// 7.  CEF sends an close notification to the application's top-level window.
      /// 8.  Application's top-level window receives the close notification and
      ///     allows the window to close based on the flag from #6B.
      /// 9.  Application's top-level window is destroyed.
      /// 10. Application's OnBeforeClose() handler is called and the browser object is destroyed.
      /// 11. Application exits by calling cef_quit_message_loop() if no other browsers exist.
      /// </code>
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the main application thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_life_span_handler_capi.h">CEF source file: /include/capi/cef_life_span_handler_capi.h (cef_life_span_handler_t)</see></para>
      /// </remarks>
      property OnClose          : TNotifyEvent    read FOnClose          write FOnClose;
      /// <summary>
      /// Called just before a browser is destroyed. Release all references to the
      /// browser object and do not attempt to execute any functions on the browser
      /// object (other than IsValid, GetIdentifier or IsSame) after this callback
      /// returns. ICefFrameHandler callbacks related to final main frame
      /// destruction will arrive after this callback and ICefBrowser.IsValid
      /// will return false (0) at that time. Any in-progress network requests
      /// associated with |browser| will be aborted when the browser is destroyed,
      /// and ICefResourceRequestHandler callbacks related to those requests may
      /// still arrive on the IO thread after this callback. See ICefFrameHandler
      /// and OnClose() documentation for additional usage information.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_life_span_handler_capi.h">CEF source file: /include/capi/cef_life_span_handler_capi.h (cef_life_span_handler_t)</see></para>
      /// </remarks>
      property OnBeforeClose    : TNotifyEvent    read FOnBeforeClose    write FOnBeforeClose;
      /// <summary>
      /// Called after a new browser is created. It is now safe to begin performing
      /// actions with |browser|. ICefFrameHandler callbacks related to initial
      /// main frame creation will arrive before this callback. See
      /// ICefFrameHandler documentation for additional usage information.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the main application thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_life_span_handler_capi.h">CEF source file: /include/capi/cef_life_span_handler_capi.h (cef_life_span_handler_t)</see></para>
      /// </remarks>
      property OnAfterCreated   : TNotifyEvent    read FOnAfterCreated   write FOnAfterCreated;
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
  {$IFDEF DELPHI16_UP}
  System.SysUtils;
  {$ELSE}
  SysUtils;
  {$ENDIF}

constructor TChromiumWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FChromium       := nil;
  FOnClose        := nil;
  FOnBeforeClose  := nil;
  FOnAfterCreated := nil;
end;

procedure TChromiumWindow.AfterConstruction;
begin
  inherited AfterConstruction;

  if not(csDesigning in ComponentState) then
    begin
      FChromium                := TChromium.Create(self);
      FChromium.OnClose        := {$IFDEF FPC}@{$ENDIF}WebBrowser_OnClose;
      FChromium.OnBeforeClose  := {$IFDEF FPC}@{$ENDIF}WebBrowser_OnBeforeClose;
      FChromium.OnAfterCreated := {$IFDEF FPC}@{$ENDIF}WebBrowser_OnAfterCreated;
      {$IFDEF LINUX}
      // This is a workaround for the CEF issue #2026. Read below for more info.
      FChromium.OnGotFocus     := {$IFDEF FPC}@{$ENDIF}WebBrowser_OnGotFocus;
      TabStop                  := True;
      {$ENDIF}
    end;
end;

function TChromiumWindow.GetBrowserInitialized : boolean;
begin
  Result := (FChromium <> nil) and FChromium.Initialized;
end;

procedure TChromiumWindow.WebBrowser_OnClose(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
begin
  aAction := cbaClose;
  if assigned(FOnClose) then
    begin
      {$IFDEF MSWINDOWS}
        PostMessage(Handle, CEF_DOONCLOSE, 0, 0);
        aAction := cbaDelay;
      {$ELSE}
        {$IFDEF FPC}
        Application.QueueAsyncCall(@BrowserOnClose, 0);
        {$ENDIF}
      {$ENDIF}
    end;
end;

procedure TChromiumWindow.WebBrowser_OnBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  if assigned(FOnBeforeClose) then FOnBeforeClose(self);
end;

procedure TChromiumWindow.WebBrowser_OnAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  {$IFDEF MSWINDOWS}
    PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
  {$ELSE}
    {$IFDEF FPC}
    Application.QueueAsyncCall(@BrowserAfterCreated, 0);
    {$ENDIF}
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
procedure TChromiumWindow.OnCloseMsg(var aMessage : TMessage);
begin
  if assigned(FOnClose) then FOnClose(self);
end;

procedure TChromiumWindow.OnAfterCreatedMsg(var aMessage : TMessage);
begin
  UpdateSize;
  if assigned(FOnAfterCreated) then FOnAfterCreated(self);
end;
{$ENDIF}

{$IFDEF FPC}
procedure TChromiumWindow.WebBrowser_OnGotFocus(Sender: TObject; const browser: ICefBrowser);
begin
  Application.QueueAsyncCall(@BrowserSetFocusMsg, 0);
end;

procedure TChromiumWindow.BrowserSetFocusMsg(Data: PtrInt);
begin
  SetFocus;
end;

procedure TChromiumWindow.BrowserAfterCreated(Data: PtrInt);
begin
  UpdateSize;
  if assigned(FOnAfterCreated) then FOnAfterCreated(self);
end;

procedure TChromiumWindow.BrowserOnCLose(Data: PtrInt);
begin
  if assigned(FOnClose) then FOnClose(self);
end;
{$ENDIF}

function TChromiumWindow.CreateBrowser : boolean;
begin
  Result := not(csDesigning in ComponentState) and
            (FChromium <> nil) and
            FChromium.CreateBrowser(self, '');
end;

procedure TChromiumWindow.CloseBrowser(aForceClose : boolean);
begin
  if (FChromium <> nil) then FChromium.CloseBrowser(aForceClose);
end;

procedure TChromiumWindow.LoadURL(const aURL : ustring);
begin
  if not(csDesigning in ComponentState) and (FChromium <> nil) then
    FChromium.LoadURL(aURL);
end;

procedure TChromiumWindow.NotifyMoveOrResizeStarted;
begin
  if (FChromium <> nil) then FChromium.NotifyMoveOrResizeStarted;
end;

function TChromiumWindow.GetChromium: TChromium;
begin
  result := FChromium;
end;

// This is a workaround for the CEF issue #2026
// https://bitbucket.org/chromiumembedded/cef/issues/2026/multiple-major-keyboard-focus-issues-on
// We use ChromiumWindow1.OnEnter, ChromiumWindow1.OnExit and
// TChromium.OnGotFocus to avoid most of the focus issues.
// ChromiumWindow1.TabStop must be TRUE.
procedure TChromiumWindow.DoEnter;
begin
  inherited DoEnter;

  {$IFDEF LINUX}
  if not(csDesigning in ComponentState) and
     FChromium.Initialized and
     not(FChromium.FrameIsFocused) then
    FChromium.SetFocus(True);
  {$ENDIF}
end;

procedure TChromiumWindow.DoExit;
begin
  inherited DoExit;

  {$IFDEF LINUX}
  if not(csDesigning in ComponentState) then
    FChromium.SendCaptureLostEvent;
  {$ENDIF}
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tchromiumwindow.lrs}
  RegisterComponents('Chromium', [TChromiumWindow]);
end;
{$ENDIF}

end.
