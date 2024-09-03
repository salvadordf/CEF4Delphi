unit uCEFBrowserViewComponent;

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
  /// <summary>
  /// Component hosting a ICefBrowserView instance.
  /// </summary>
  TCEFBrowserViewComponent = class(TCEFViewComponent, ICefBrowserViewDelegateEvents)
    protected
      FBrowserView                              : ICefBrowserView;
      FBrowserViewDlg                           : ICefBrowserViewDelegate;

      // ICefBrowserViewDelegateEvents
      FOnBrowserCreated                         : TOnBrowserCreatedEvent;
      FOnBrowserDestroyed                       : TOnBrowserDestroyedEvent;
      FOnGetDelegateForPopupBrowserView         : TOnGetDelegateForPopupBrowserViewEvent;
      FOnPopupBrowserViewCreated                : TOnPopupBrowserViewCreatedEvent;
      FOnGetChromeToolbarType                   : TOnGetChromeToolbarTypeEvent;
      FOnUseFramelessWindowForPictureInPicture  : TOnUseFramelessWindowForPictureInPicture;
      FOnGestureCommand                         : TOnGestureCommandEvent;
      FOnGetBrowserRuntimeStyle                 : TOnGetBrowserRuntimeStyleEvent;

      procedure DestroyView; override;
      procedure Initialize; override;

      function  GetInitialized : boolean; override;
      function  GetAsView : ICefView; override;
      function  GetAsBrowserView : ICefBrowserView; override;
      function  GetBrowser : ICefBrowser;
      function  GetChromeToolbar : ICefView;
      function  GetRuntimeStyle : TCefRuntimeStyle;

      // ICefBrowserViewDelegateEvents
      procedure doOnBrowserCreated(const browser_view: ICefBrowserView; const browser: ICefBrowser);
      procedure doOnBrowserDestroyed(const browser_view: ICefBrowserView; const browser: ICefBrowser);
      procedure doOnGetDelegateForPopupBrowserView(const browser_view: ICefBrowserView; const settings: TCefBrowserSettings; const client: ICefClient; is_devtools: boolean; var aResult : ICefBrowserViewDelegate);
      procedure doOnPopupBrowserViewCreated(const browser_view, popup_browser_view: ICefBrowserView; is_devtools: boolean; var aResult : boolean);
      procedure doOnGetChromeToolbarType(const browser_view: ICefBrowserView; var aChromeToolbarType: TCefChromeToolbarType);
      procedure doOnUseFramelessWindowForPictureInPicture(const browser_view: ICefBrowserView; var aResult: boolean);
      procedure doOnGestureCommand(const browser_view: ICefBrowserView; gesture_command: TCefGestureCommand; var aResult : boolean);
      procedure doOnGetBrowserRuntimeStyle(var aResult : TCefRuntimeStyle);

    public
      /// <summary>
      /// Create a new ICefBrowserView. The underlying ICefBrowser will not be created
      /// until this view is added to the views hierarchy. The optional |extra_info|
      /// parameter provides an opportunity to specify extra information specific to
      /// the created browser that will be passed to
      /// ICefRenderProcessHandler.OnBrowserCreated in the render process.
      /// </summary>
      function  CreateBrowserView(const client: ICefClient; const url: ustring; const settings: TCefBrowserSettings; const extra_info: ICefDictionaryValue; const request_context: ICefRequestContext): boolean;
      /// <summary>
      /// Updates the internal ICefBrowserView with the ICefBrowserView associated with |browser|.
      /// </summary>
      function  GetForBrowser(const browser: ICefBrowser): boolean;
      /// <summary>
      /// Sets whether normal priority accelerators are first forwarded to the web
      /// content (`keydown` event handler) or ICefKeyboardHandler. Normal priority
      /// accelerators can be registered via ICefWindow.SetAccelerator (with
      /// |high_priority|=false) or internally for standard accelerators supported
      /// by Chrome style. If |prefer_accelerators| is true then the matching
      /// accelerator will be triggered immediately (calling
      /// ICefWindowDelegate.OnAccelerator or ICefCommandHandler.OnChromeCommand
      /// respectively) and the event will not be forwarded to the web content or
      /// ICefKeyboardHandler first. If |prefer_accelerators| is false then the
      /// matching accelerator will only be triggered if the event is not handled by
      /// web content (`keydown` event handler that calls `event.preventDefault()`)
      /// or by ICefKeyboardHandler. The default value is false.
      /// </summary>
      procedure SetPreferAccelerators(prefer_accelerators: boolean);

      /// <summary>
      /// Returns the ICefBrowser hosted by this BrowserView. Will return NULL if
      /// the browser has not yet been created or has already been destroyed.
      /// </summary>
      property Browser                                  : ICefBrowser                               read GetBrowser;
      /// <summary>
      /// ICefBrowserView assiciated to this component.
      /// </summary>
      property BrowserView                              : ICefBrowserView                           read FBrowserView;
      /// <summary>
      /// Returns the Chrome toolbar associated with this BrowserView. Only
      /// supported when using Chrome style. The ICefBrowserViewDelegate.GetChromeToolbarType
      /// function must return a value other than
      /// CEF_CTT_NONE and the toolbar will not be available until after this
      /// BrowserView is added to a ICefWindow and
      /// ICefViewDelegate.OnWindowChanged() has been called.
      /// </summary>
      property ChromeToolbar                            : ICefView                                  read GetChromeToolbar;
      /// <summary>
      /// Returns the runtime style for this BrowserView (ALLOY or CHROME). See
      /// TCefRuntimeStyle documentation for details.
      /// </summary>
      property RuntimeStyle                             : TCefRuntimeStyle                          read GetRuntimeStyle;

    published
      /// <summary>
      /// Called when |browser| associated with |browser_view| is created. This
      /// function will be called after ICefLifeSpanHandler.OnAfterCreated()
      /// is called for |browser| and before OnPopupBrowserViewCreated() is
      /// called for |browser|'s parent delegate if |browser| is a popup.
      /// </summary>
      property OnBrowserCreated                         : TOnBrowserCreatedEvent                    read FOnBrowserCreated                         write FOnBrowserCreated;
      /// <summary>
      /// Called when |browser| associated with |browser_view| is destroyed. Release
      /// all references to |browser| and do not attempt to execute any functions on
      /// |browser| after this callback returns. This function will be called before
      /// ICefLifeSpanHandler.OnBeforeClose() is called for |browser|.
      /// </summary>
      property OnBrowserDestroyed                       : TOnBrowserDestroyedEvent                  read FOnBrowserDestroyed                       write FOnBrowserDestroyed;
      /// <summary>
      /// Called before a new popup BrowserView is created. The popup originated
      /// from |browser_view|. |settings| and |client| are the values returned from
      /// ICefLifeSpanHandler.OnBeforePopup(). |is_devtools| will be true (1)
      /// if the popup will be a DevTools browser. Return the delegate that will be
      /// used for the new popup BrowserView.
      /// </summary>
      property OnGetDelegateForPopupBrowserView         : TOnGetDelegateForPopupBrowserViewEvent    read FOnGetDelegateForPopupBrowserView         write FOnGetDelegateForPopupBrowserView;
      /// <summary>
      /// Called after |popup_browser_view| is created. This function will be called
      /// after ICefLifeSpanHandler.OnAfterCreated() and OnBrowserCreated()
      /// are called for the new popup browser. The popup originated from
      /// |browser_view|. |is_devtools| will be true (1) if the popup is a DevTools
      /// browser. Optionally add |popup_browser_view| to the views hierarchy
      /// yourself and return true (1). Otherwise return false (0) and a default
      /// ICefWindow will be created for the popup.
      /// </summary>
      property OnPopupBrowserViewCreated                : TOnPopupBrowserViewCreatedEvent           read FOnPopupBrowserViewCreated                write FOnPopupBrowserViewCreated;
      /// <summary>
      /// Returns the Chrome toolbar type that will be available via
      /// ICefBrowserView.GetChromeToolbar(). See that function for related
      /// documentation.
      /// </summary>
      property OnGetChromeToolbarType                   : TOnGetChromeToolbarTypeEvent              read FOnGetChromeToolbarType                   write FOnGetChromeToolbarType;
      /// <summary>
      /// Return true (1) to create frameless windows for Document picture-in-
      /// picture popups. Content in frameless windows should specify draggable
      /// regions using "-webkit-app-region: drag" CSS.
      /// </summary>
      property OnUseFramelessWindowForPictureInPicture  : TOnUseFramelessWindowForPictureInPicture  read FOnUseFramelessWindowForPictureInPicture  write FOnUseFramelessWindowForPictureInPicture;
      /// <summary>
      /// Called when |browser_view| receives a gesture command. Return true (1) to
      /// handle (or disable) a |gesture_command| or false (0) to propagate the
      /// gesture to the browser for default handling. With Chrome style these
      /// commands can also be handled via ICefCommandHandler.OnChromeCommand.
      /// </summary>
      property OnGestureCommand                         : TOnGestureCommandEvent                    read FOnGestureCommand                         write FOnGestureCommand;
      /// <summary>
      /// Optionally change the runtime style for this BrowserView. See
      /// TCefRuntimeStyle documentation for details.
      /// </summary>
      property OnGetBrowserRuntimeStyle                 : TOnGetBrowserRuntimeStyleEvent            read FOnGetBrowserRuntimeStyle                 write FOnGetBrowserRuntimeStyle;
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
  uCEFBrowserView, uCEFBrowserViewDelegate, uCEFMiscFunctions;

procedure TCEFBrowserViewComponent.Initialize;
begin
  inherited Initialize;

  FBrowserView                             := nil;
  FBrowserViewDlg                          := nil;
  FOnBrowserCreated                        := nil;
  FOnBrowserDestroyed                      := nil;
  FOnGetDelegateForPopupBrowserView        := nil;
  FOnPopupBrowserViewCreated               := nil;
  FOnGetChromeToolbarType                  := nil;
  FOnUseFramelessWindowForPictureInPicture := nil;
  FOnGestureCommand                        := nil;
  FOnGetBrowserRuntimeStyle                := nil;
end;

procedure TCEFBrowserViewComponent.DestroyView;
begin
  if (FBrowserViewDlg <> nil) then
    begin
      FBrowserViewDlg.DestroyOtherRefs;
      FBrowserViewDlg := nil;
    end;

  FBrowserView := nil;
end;

function TCEFBrowserViewComponent.CreateBrowserView(const client          : ICefClient;
                                                    const url             : ustring;
                                                    const settings        : TCefBrowserSettings;
                                                    const extra_info      : ICefDictionaryValue;
                                                    const request_context : ICefRequestContext): boolean;
begin
  Result := False;

  if CefCurrentlyOn(TID_UI) and (client <> nil) then
    begin
      if (FBrowserViewDlg = nil) then
        FBrowserViewDlg := TCustomBrowserViewDelegate.Create(self);

      FBrowserView := TCefBrowserViewRef.CreateBrowserView(client, url, settings, extra_info, request_context, FBrowserViewDlg);
      Result       := (FBrowserView <> nil);
    end;
end;

function TCEFBrowserViewComponent.GetForBrowser(const browser: ICefBrowser): boolean;
begin
  Result := False;

  if CefCurrentlyOn(TID_UI) and (browser <> nil) then
    begin
      FBrowserView := TCefBrowserViewRef.GetForBrowser(browser);
      Result       := (FBrowserView <> nil);
    end;
end;

function TCEFBrowserViewComponent.GetInitialized : boolean;
begin
  Result := (FBrowserView <> nil);
end;

function TCEFBrowserViewComponent.GetAsView : ICefView;
begin
  Result := FBrowserView as ICefView;
end;

function TCEFBrowserViewComponent.GetAsBrowserView : ICefBrowserView;
begin
  Result := FBrowserView;
end;

function TCEFBrowserViewComponent.GetBrowser : ICefBrowser;
begin
  if Initialized then
    Result := FBrowserView.GetBrowser
   else
    Result := nil;
end;

function TCEFBrowserViewComponent.GetChromeToolbar : ICefView;
begin
  if Initialized then
    Result := FBrowserView.GetChromeToolbar
   else
    Result := nil;
end;

function TCEFBrowserViewComponent.GetRuntimeStyle : TCefRuntimeStyle;
begin
  if Initialized then
    Result := FBrowserView.RuntimeStyle
   else
    Result := CEF_RUNTIME_STYLE_DEFAULT;
end;

procedure TCEFBrowserViewComponent.SetPreferAccelerators(prefer_accelerators: boolean);
begin
  if Initialized then FBrowserView.SetPreferAccelerators(prefer_accelerators);
end;

procedure TCEFBrowserViewComponent.doOnBrowserCreated(const browser_view : ICefBrowserView;
                                                      const browser      : ICefBrowser);
begin
  if assigned(FOnBrowserCreated) then
    FOnBrowserCreated(self, browser_view, browser);
end;

procedure TCEFBrowserViewComponent.doOnBrowserDestroyed(const browser_view : ICefBrowserView;
                                                        const browser      : ICefBrowser);
begin
  if assigned(FOnBrowserDestroyed) then
    FOnBrowserDestroyed(self, browser_view, browser);
end;

procedure TCEFBrowserViewComponent.doOnGetDelegateForPopupBrowserView(const browser_view : ICefBrowserView;
                                                                      const settings     : TCefBrowserSettings;
                                                                      const client       : ICefClient;
                                                                            is_devtools  : boolean;
                                                                      var   aResult      : ICefBrowserViewDelegate);
begin
  if assigned(FOnGetDelegateForPopupBrowserView) then
    FOnGetDelegateForPopupBrowserView(self, browser_view, settings, client, is_devtools, aResult);
end;

procedure TCEFBrowserViewComponent.doOnPopupBrowserViewCreated(const browser_view       : ICefBrowserView;
                                                               const popup_browser_view : ICefBrowserView;
                                                                     is_devtools        : boolean;
                                                               var   aResult            : boolean);
begin
  if assigned(FOnPopupBrowserViewCreated) then
    FOnPopupBrowserViewCreated(self, browser_view, popup_browser_view, is_devtools, aResult);
end;

procedure TCEFBrowserViewComponent.doOnGetChromeToolbarType(const browser_view: ICefBrowserView; var aChromeToolbarType: TCefChromeToolbarType);
begin
  if assigned(FOnGetChromeToolbarType) then
    FOnGetChromeToolbarType(self, browser_view, aChromeToolbarType);
end;

procedure TCEFBrowserViewComponent.doOnUseFramelessWindowForPictureInPicture(const browser_view : ICefBrowserView;
                                                                             var   aResult      : boolean);
begin
  if assigned(FOnUseFramelessWindowForPictureInPicture) then
    FOnUseFramelessWindowForPictureInPicture(self, browser_view, aResult);
end;

procedure TCEFBrowserViewComponent.doOnGestureCommand(const browser_view    : ICefBrowserView;
                                                            gesture_command : TCefGestureCommand;
                                                      var   aResult         : boolean);
begin
  if assigned(FOnGestureCommand) then
    FOnGestureCommand(self, browser_view, gesture_command, aResult);
end;

procedure TCEFBrowserViewComponent.doOnGetBrowserRuntimeStyle(var aResult : TCefRuntimeStyle);
begin
  aResult := CEF_RUNTIME_STYLE_DEFAULT;

  if assigned(FOnGetBrowserRuntimeStyle) then
    FOnGetBrowserRuntimeStyle(self, aResult);
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tcefbrowserviewcomponent.lrs}
  RegisterComponents('Chromium Views Framework', [TCEFBrowserViewComponent]);
end;
{$ENDIF}

end.
