unit umainwindow;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs,
  LazGtk3, LazGdk3, LazGObject2, LazGLib2, xlib,
  uCEFApplication, uCEFConstants, uCEFTypes, uCEFChromium,
  uCEFMiscFunctions, uCEFLinuxFunctions, uCEFInterfaces;

type
  TMainWindow = class
    private                     
      FCanClose     : boolean;
      FClosing      : boolean;
      FInitializing : boolean;
      FLoading      : boolean;
      FWindow       : PGtkWidget;
      FChromium     : TChromium;

      function  GetTitle : string;
      function  GetWidth : integer;
      function  GetHeight : integer;

      procedure SetTitle(const aValue : string);

      procedure DoAfterCreated;
      procedure DoBeforeClose;
      procedure DoCloseQuery(var aCanClose: Boolean);
      procedure DoResize;

      procedure UpdateBrowserSize(aLeft, aTop, aWidth, aHeight : integer);
      procedure UpdateXWindowVisibility(aVisible : boolean);
      procedure NotifyMoveOrResizeStarted;                      
      procedure CloseBrowser(aForceClose : boolean);  
      procedure CreateBrowser;
      procedure CreateWidgets;

      procedure OnAfterCreated(Sender: TObject; const browser: ICefBrowser);              
      procedure OnBeforeClose(Sender: TObject; const browser: ICefBrowser);  
      procedure OnBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; popup_id: Integer; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess, Result: Boolean);
      procedure OnOpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);
                                       
    public
      constructor Create;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      procedure   Show;
      procedure   Run;

      property Width    : integer   read GetWidth;
      property Height   : integer   read GetHeight;
      property Title    : string    read GetTitle      write SetTitle;
  end;

var
  MainWindow : TMainWindow = nil;

procedure CreateGlobalCEFApp;
function StartMainProcess: boolean;

implementation

var
  MainAppEvent : TEventObject = nil;

{GlobalCEFApp functions}
{%Region}
procedure GlobalCEFApp_OnContextInitialized();
begin
  MainAppEvent.SetEvent;
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.LogFile                    := 'debug.log';
  GlobalCEFApp.LogSeverity                := LOGSEVERITY_INFO;
  GlobalCEFApp.RootCache                  := 'RootCache';
  GlobalCEFApp.DisableZygote              := True;
  GlobalCEFApp.SetCurrentDir              := True;
  GlobalCEFApp.MultiThreadedMessageLoop   := False;
  GlobalCEFApp.ExternalMessagePump        := False;
  GlobalCEFApp.GTKVersion                 := gtkVersion3;          
  GlobalCEFApp.OzonePlatform              := ozpX11;
  GlobalCEFApp.OnContextInitialized       := @GlobalCEFApp_OnContextInitialized;
end;

function StartMainProcess: boolean;
begin
  Result := False;

  if GlobalCEFApp.StartMainProcess then
    begin
      // Wait until the context is initialized before initializing GTK.
      if (MainAppEvent.WaitFor(10000) = wrTimeout) then
        CefDebugLog('CEF initialization failure!')
       else
        Result := True;
    end;
end;
{%Endregion}

{Message handlers}
{%Region}
function DeleteEventHandler(widget: PGtkWidget; event: PGdkEventAny): gboolean; cdecl;
var
  TempCanClose : boolean;
begin
  MainWindow.DoCloseQuery(TempCanClose);
  Result := not(TempCanClose);
end;

function DestroyEventHandler(widget: PGtkWidget; event: PGdkEventAny): gboolean; cdecl;
begin
  Result := False;
  GlobalCEFApp.QuitMessageLoop;
end;

function ShowEventHandler(Widget: PGtkWidget; Data: gPointer): gboolean; cdecl;
begin
  Result := False;
  MainWindow.CreateBrowser;
end;        

function ConfigureEvent(widget: PGtkWidget; event: PGdkEventConfigure): gboolean; cdecl;
begin
  Result := False;
  MainWindow.DoResize;
  MainWindow.NotifyMoveOrResizeStarted;
end;

function CustomX11ErrorHandler(Display: PDisplay; ErrorEv: PXErrorEvent) : longint; cdecl;
begin
  Result := 0;
end;

function CustomXIOErrorHandler(Display: PDisplay) : longint; cdecl;
begin
  Result := 0;
end;
{%Endregion}

{Public methods}
{%Region}
constructor TMainWindow.Create;
begin
  inherited Create;

  FCanClose              := False;
  FClosing               := False;
  FInitializing          := True;
  FLoading               := False;
  FWindow                := nil;
  FChromium              := nil;
end;

destructor TMainWindow.Destroy;
begin
  if (FChromium <> nil) then
    FreeAndNil(FChromium);

  inherited Destroy;
end;

procedure TMainWindow.AfterConstruction;
begin
  inherited AfterConstruction;

  // Force Gtk to use Xwayland (in case a Wayland compositor is being used).
  gdk_set_allowed_backends('x11');

  // The Chromium sandbox requires that there only be a single thread during
  // initialization. Therefore initialize GTK after CEF.
  gtk_init(@argc, @argv);

  // Install xlib error handlers so that the application won't be terminated
  // on non-fatal errors. Must be done after initializing GTK.
  XSetErrorHandler(@CustomX11ErrorHandler);
  XSetIOErrorHandler(@CustomXIOErrorHandler);

  FChromium                       := TChromium.Create(nil);
  FChromium.DefaultURL            := 'https://www.google.com';
  FChromium.OnAfterCreated        := @OnAfterCreated;     
  FChromium.OnBeforeClose         := @OnBeforeClose;
  FChromium.OnBeforePopup         := @OnBeforePopup;
  FChromium.OnOpenUrlFromTab      := @OnOpenUrlFromTab;

  CreateWidgets;
end;

procedure TMainWindow.Show;
begin
  // Show the GTK window.
  UseDefaultX11VisualForGtk(FWindow);
  gtk_widget_show_all(FWindow);

  // Flush the display to make sure the underlying X11 window gets created
  // immediately.
  FlushDisplay(FWindow);
end;

procedure TMainWindow.Run;
begin
  GlobalCEFApp.RunMessageLoop;
end;
{%Endregion}

{Property setters and getters}
{%Region}
function TMainWindow.GetTitle: string;
begin
  Result := gtk_window_get_title(PGtkWindow(FWindow));
end;

function TMainWindow.GetWidth : integer;
begin
  Result := gtk_widget_get_allocated_width(FWindow);
end;

function TMainWindow.GetHeight : integer;
begin
  Result := gtk_widget_get_allocated_height(FWindow);
end;

procedure TMainWindow.SetTitle(const aValue : string);
begin
  gtk_window_set_title(PGtkWindow(FWindow), PGChar(aValue));
end;
{%Endregion}

{Chromium events}
{%Region}
procedure TMainWindow.OnAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  TThread.Synchronize(nil, @DoAfterCreated);
end;

procedure TMainWindow.OnBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  TThread.Synchronize(nil, @DoBeforeClose);
end;              

procedure TMainWindow.OnBeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; popup_id: Integer;
  const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue;
  var noJavascriptAccess, Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;

procedure TMainWindow.OnOpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; out Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;
{%Endregion}

{Private methods}
{%Region}
procedure TMainWindow.UpdateBrowserSize(aLeft, aTop, aWidth, aHeight : integer);
begin
  if (FChromium <> nil) and FChromium.Initialized then
    FChromium.UpdateBrowserSize(aLeft, aTop, aWidth, aHeight);
end;

procedure TMainWindow.UpdateXWindowVisibility(aVisible : boolean);
begin
  if (FChromium <> nil) and FChromium.Initialized then
    FChromium.UpdateXWindowVisibility(aVisible);
end;

procedure TMainWindow.DoAfterCreated;
begin
  UpdateXWindowVisibility(True);
  UpdateBrowserSize(0, 0, Width, Height);
end;

procedure TMainWindow.DoBeforeClose;
begin
  gtk_window_close(PGtkWindow(FWindow));
end;

procedure TMainWindow.DoResize;
begin
  UpdateBrowserSize(0, 0, Width, Height);
end;

procedure TMainWindow.DoCloseQuery(var aCanClose: Boolean);
begin
  aCanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      FChromium.CloseBrowser(True);
    end;
end;

procedure TMainWindow.CreateBrowser;
begin
  if (FChromium <> nil) and not(FChromium.Initialized) then
    begin
      if not(FChromium.CreateBrowser(TCefWindowHandle(FWindow), Rect(0, 0, Width, Height))) then
        CefDebugLog('CreateBrowser failed');
    end;
end;

procedure TMainWindow.CreateWidgets;
begin
  FWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);

  gtk_window_set_default_size(PGtkWindow(FWindow), 1024, 768);
  gtk_window_move(PGtkWindow(FWindow), 300, 200);

  g_signal_connect_data(FWindow, 'delete_event',    TGCallback(@DeleteEventHandler),  nil, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(FWindow, 'destroy',         TGCallback(@DestroyEventHandler), nil, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(FWindow, 'show',            TGCallback(@ShowEventHandler),    nil, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(FWindow, 'configure-event', TGCallback(@ConfigureEvent),      nil, nil, G_CONNECT_DEFAULT);

  Title := 'GTKBrowser';
end;

procedure TMainWindow.NotifyMoveOrResizeStarted;
begin
  if (FChromium <> nil) then
    FChromium.NotifyMoveOrResizeStarted;
end;

procedure TMainWindow.CloseBrowser(aForceClose : boolean);
begin
  if (FChromium <> nil) then
    FChromium.CloseBrowser(aForceClose);
end;
{%Endregion}

initialization
  MainAppEvent := TEventObject.Create(nil, True, False, 'MainAppEvent');

finalization
  if assigned(MainAppEvent) then
    FreeAndNil(MainAppEvent);

end.

