unit ucefbrowserthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs,
  uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFChromium,
  uworkerthread, ucustommessage;

type
  TThreadStatus = (tsInitializing, tsIdle, tsLoading, tsClosing, tsDestroyed, tsInitError);

  TSize = record
    cx : integer;
    cy : integer;
  end;

  TCEFBrowserThread = class(TWorkerThread)
    protected
      FBrowser               : TChromium;
      FStatus                : TThreadStatus;
      FBrowserSize           : TSize;
      FBrowserCS             : TCriticalSection;
      FErrorCode             : integer;
      FErrorText             : string;
      FFailedURL             : string;
      FDefaultURL            : string;
      FFileName              : string;
      FMessageID             : integer;
      FOnInitialized         : TNotifyEvent;
      FOnSnapshotAvailable   : TNotifyEvent;
      FOnError               : TNotifyEvent;

      function  GetErrorCode : integer;
      function  GetErrorText : string;
      function  GetFailedURL : string;
      function  GetInitialized : boolean;
      function  GetClosing : boolean;
      function  GetStatus : TThreadStatus;
      function  GetFileName : string;

      procedure SetErrorText(const aValue : string);
      procedure SetFileName(const aValue : string);
      procedure SetStatus(aValue : TThreadStatus);

      procedure Browser_OnAfterCreated(Sender: TObject; const browser: ICefBrowser);
      procedure Browser_OnGetViewRect(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect);
      procedure Browser_OnGetScreenInfo(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean);
      procedure Browser_OnBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
      procedure Browser_OnBeforeClose(Sender: TObject; const browser: ICefBrowser);
      procedure Browser_OnLoadError(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring);
      procedure Browser_OnLoadingStateChange(Sender: TObject; const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
      procedure Browser_OnDevToolsMethodResult(Sender: TObject; const browser: ICefBrowser; message_id: Integer; success: Boolean; const result: ICefValue);
      procedure Browser_OnOpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);

      procedure DoOnInitialized;
      procedure DoOnError;
      procedure DoOnSnapshotAvailable;
                                           
      procedure ProcessValue(const aInfo : TMsgInfo); override;
      procedure DoLoadURL(const aURL : string);
      function  CreateBrowser : boolean;
      procedure CloseBrowser;
      procedure InitError;
      procedure Execute; override;

    public
      constructor Create(aWidth, aHeight : integer; const aDefaultURL, aFileName : string);
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      procedure   TerminateBrowserThread;
      procedure   LoadURL(const aURL : string);

      class procedure CreateGlobalCEFApp;
      class procedure DestroyGlobalCEFApp;

      property ErrorCode             : integer           read GetErrorCode;
      property ErrorText             : string            read GetErrorText             write SetErrorText;
      property FailedUrl             : string            read GetFailedUrl;
      property Initialized           : boolean           read GetInitialized;
      property Closing               : boolean           read GetClosing;       
      property Status                : TThreadStatus     read GetStatus                write SetStatus;
      property FileName              : string            read GetFileName              write SetFileName;

      property OnInitialized         : TNotifyEvent      read FOnInitialized           write FOnInitialized;
      property OnSnapshotAvailable   : TNotifyEvent      read FOnSnapshotAvailable     write FOnSnapshotAvailable;
      property OnError               : TNotifyEvent      read FOnError                 write FOnError;
  end;

implementation                                

uses
  uCEFDictionaryValue, uCEFJson, uCEFApplication, uCEFMiscFunctions;

const
  WORKERTHREADMSG_LOADURL      = WORKERTHREADMSG_QUIT + 1;
  WORKERTHREADMSG_DOONERROR    = WORKERTHREADMSG_QUIT + 2;
  WORKERTHREADMSG_CLOSEBROWSER = WORKERTHREADMSG_QUIT + 3;

class procedure TCEFBrowserThread.CreateGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.ShowMessageDlg             := False;               // This demo shouldn't show any window, just console messages.
  GlobalCEFApp.BrowserSubprocessPath      := 'librarybrowser_sp'; // This is the other EXE for the CEF subprocesses. It's on the same directory as this app.
  GlobalCEFApp.BlinkSettings              := 'hideScrollbars';    // This setting removes all scrollbars to capture a cleaner snapshot
  GlobalCEFApp.SetCurrentDir              := True;
  GlobalCEFApp.DisableZygote              := True;
  GlobalCEFApp.StartMainProcess;
end;

class procedure TCEFBrowserThread.DestroyGlobalCEFApp;
begin
  uCEFApplication.DestroyGlobalCEFApp;
end;

constructor TCEFBrowserThread.Create(aWidth, aHeight : integer; const aDefaultURL, aFileName : string);
begin
  inherited Create;

  FStatus                := tsInitializing;
  FBrowser               := nil;
  FBrowserSize.cx        := aWidth;
  FBrowserSize.cy        := aHeight;
  FDefaultURL            := aDefaultURL;
  FFileName              := aFileName;
  FBrowserCS             := nil;
  FMessageID             := -1;
  FOnInitialized         := nil;
  FOnSnapshotAvailable   := nil;
  FOnError               := nil;
end;

destructor TCEFBrowserThread.Destroy;
begin
  if (FBrowser <> nil) then
    FreeAndNil(FBrowser);

  if (FBrowserCS <> nil) then
    FreeAndNil(FBrowserCS);

  inherited Destroy;
end;

procedure TCEFBrowserThread.AfterConstruction;
begin
  inherited AfterConstruction;

  FBrowserCS                       := TCriticalSection.Create;

  FBrowser                         := TChromium.Create(nil);
  FBrowser.DefaultURL              := FDefaultURL;
  FBrowser.Options.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);
  FBrowser.OnAfterCreated          := @Browser_OnAfterCreated;
  FBrowser.OnGetViewRect           := @Browser_OnGetViewRect;
  FBrowser.OnGetScreenInfo         := @Browser_OnGetScreenInfo;
  FBrowser.OnBeforePopup           := @Browser_OnBeforePopup;
  FBrowser.OnBeforeClose           := @Browser_OnBeforeClose;
  FBrowser.OnLoadError             := @Browser_OnLoadError;
  FBrowser.OnLoadingStateChange    := @Browser_OnLoadingStateChange;
  FBrowser.OnOpenUrlFromTab        := @Browser_OnOpenUrlFromTab;
  FBrowser.OnDevToolsMethodResult  := @Browser_OnDevToolsMethodResult;
end;

procedure TCEFBrowserThread.TerminateBrowserThread;
begin
  Terminate;
  EnqueueMessage(WORKERTHREADMSG_CLOSEBROWSER);
end;

procedure TCEFBrowserThread.LoadURL(const aURL : string);
begin
  EnqueueMessage(WORKERTHREADMSG_LOADURL, 0, aURL);
end;

function TCEFBrowserThread.GetErrorCode : integer;
begin
  FBrowserCS.Acquire;
  Result := FErrorCode;
  FBrowserCS.Release;
end;

function TCEFBrowserThread.GetErrorText : string;
begin
  FBrowserCS.Acquire;
  Result := FErrorText;
  FBrowserCS.Release;
end;

function TCEFBrowserThread.GetFailedURL : string;
begin
  FBrowserCS.Acquire;
  Result := FFailedURL;
  FBrowserCS.Release;
end;

function TCEFBrowserThread.GetInitialized : boolean;
begin
  FBrowserCS.Acquire;
  Result := not(Terminated) and (FStatus in [tsIdle, tsLoading]);
  FBrowserCS.Release;
end;

function TCEFBrowserThread.GetClosing : boolean;
begin
  FBrowserCS.Acquire;
  Result := (FStatus = tsClosing);
  FBrowserCS.Release;
end;

function TCEFBrowserThread.GetStatus : TThreadStatus;
begin
  FBrowserCS.Acquire;
  Result := FStatus;
  FBrowserCS.Release;
end;  

function TCEFBrowserThread.GetFileName : string;
begin
  FBrowserCS.Acquire;
  Result := FFileName;
  FBrowserCS.Release;
end;

procedure TCEFBrowserThread.SetErrorText(const aValue : string);
begin
  FBrowserCS.Acquire;
  FErrorText := aValue;
  FBrowserCS.Release;
end;          

procedure TCEFBrowserThread.SetFileName(const aValue : string);
begin
  FBrowserCS.Acquire;
  FFileName := aValue;
  FBrowserCS.Release;
end;

procedure TCEFBrowserThread.SetStatus(aValue : TThreadStatus);
begin
  FBrowserCS.Acquire;
  FStatus := aValue;
  FBrowserCS.Release;
end;

procedure TCEFBrowserThread.DoOnInitialized;
begin
  if assigned(FOnInitialized) then
    FOnInitialized(self);
end;

procedure TCEFBrowserThread.DoOnError;
begin
  if assigned(FOnError) then
    FOnError(self);
end;

procedure TCEFBrowserThread.DoOnSnapshotAvailable;
begin
  if assigned(FOnSnapshotAvailable) then
    FOnSnapshotAvailable(self);
end;

procedure TCEFBrowserThread.Browser_OnAfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  Status := tsIdle;
  DoOnInitialized;
end;

procedure TCEFBrowserThread.Browser_OnGetViewRect(Sender: TObject;
  const browser: ICefBrowser; var rect: TCefRect);
begin
  rect.x      := 0;
  rect.y      := 0;
  rect.width  := FBrowserSize.cx;
  rect.height := FBrowserSize.cy;
end;

procedure TCEFBrowserThread.Browser_OnGetScreenInfo(Sender: TObject;
  const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean);
var
  TempRect : TCEFRect;
begin
  TempRect.x      := 0;
  TempRect.y      := 0;
  TempRect.width  := FBrowserSize.cx;
  TempRect.height := FBrowserSize.cy;

  screenInfo.device_scale_factor := 1;
  screenInfo.depth               := 0;
  screenInfo.depth_per_component := 0;
  screenInfo.is_monochrome       := Ord(False);
  screenInfo.rect                := TempRect;
  screenInfo.available_rect      := TempRect;

  Result := True;
end;

procedure TCEFBrowserThread.Browser_OnBeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const targetUrl, targetFrameName: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
  const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var client: ICefClient; var settings: TCefBrowserSettings;
  var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean;
  var Result: Boolean);
begin
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB,
                                   CEF_WOD_NEW_BACKGROUND_TAB,
                                   CEF_WOD_NEW_POPUP,
                                   CEF_WOD_NEW_WINDOW]);
end;

procedure TCEFBrowserThread.Browser_OnBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  Status := tsDestroyed;
  EnqueueMessage(WORKERTHREADMSG_QUIT);
end;

procedure TCEFBrowserThread.Browser_OnLoadError(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode;
  const errorText, failedUrl: ustring);
begin
  if not(Terminated) and (frame <> nil) and frame.IsValid and frame.IsMain then
    try
      FBrowserCS.Acquire;
      FErrorCode := errorCode;
      FErrorText := errorText;
      FFailedUrl := failedUrl;
    finally
      FBrowserCS.Release;
      EnqueueMessage(WORKERTHREADMSG_DOONERROR);
    end;
end;

procedure TCEFBrowserThread.Browser_OnLoadingStateChange(Sender: TObject;
  const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
var
  TempParams : ICefDictionaryValue;
begin
  if isLoading then
    Status := tsLoading
   else
    begin
      Status     := tsIdle;
      TempParams := TCefDictionaryValueRef.New;
      TempParams.SetString('format', 'png');
      FMessageID := FBrowser.ExecuteDevToolsMethod(0, 'Page.captureScreenshot', TempParams);
      TempParams := nil;
    end;
end;

procedure TCEFBrowserThread.Browser_OnDevToolsMethodResult(Sender: TObject;
  const browser: ICefBrowser; message_id: Integer; success: Boolean;
  const result: ICefValue);
var
  TempRsltDict : ICefDictionaryValue;
  TempString   : ustring;
  TempBin      : ICefBinaryValue;
  TempStream   : TFileStream;
  TempSuccess  : boolean;
begin
  if not(success) or (FMessageID <> message_id) or not(assigned(result)) then exit;

  TempSuccess  := False;
  TempStream   := nil;
  TempRsltDict := result.GetDictionary;

  if assigned(TempRsltDict) then
    try
      if TCEFJson.ReadString(TempRsltDict, 'data', TempString) then
        try
          TempBin := CefBase64Decode(TempString);

          if assigned(TempBin) and (TempBin.Size > 0) then
            try
              try
                TempStream := TFileStream.Create(FileName, fmCreate);
                TempStream.WriteBuffer(TempBin.GetRawData^, TempBin.Size);
                TempSuccess := True;
              except
                on e : exception do
                  if CustomExceptionHandler('TCEFBrowserThread.Browser_OnDevToolsMethodResult', e) then raise;
              end;
            finally
              if assigned(TempStream) then
                FreeAndNil(TempStream);
            end;
        finally
          TempBin := nil;
        end;
    finally
      TempRsltDict := nil;
      if TempSuccess then DoOnSnapshotAvailable;
    end;
end;

procedure TCEFBrowserThread.Browser_OnOpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
  out Result: Boolean);
begin
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB,
                                   CEF_WOD_NEW_BACKGROUND_TAB,
                                   CEF_WOD_NEW_POPUP,
                                   CEF_WOD_NEW_WINDOW]);
end;

procedure TCEFBrowserThread.ProcessValue(const aInfo : TMsgInfo);
begin
  case aInfo.Msg of
    WORKERTHREADMSG_LOADURL      : DoLoadURL(aInfo.StrParam);
    WORKERTHREADMSG_DOONERROR    : DoOnError;
    WORKERTHREADMSG_CLOSEBROWSER : CloseBrowser;
  end;
end;

function TCEFBrowserThread.CreateBrowser : boolean;
begin
  Result := assigned(FBrowser) and FBrowser.CreateBrowser;
end;

procedure TCEFBrowserThread.DoLoadURL(const aURL : string);
begin
  if not(Terminated) and Initialized and assigned(FBrowser) then
    FBrowser.LoadURL(aURL);
end;

procedure TCEFBrowserThread.CloseBrowser;
begin
  if Initialized then
    begin
      if assigned(FBrowser) then
        begin
          Status := tsClosing;
          FBrowser.CloseBrowser(True);
        end;
    end
   else
    if not(Closing) then
      EnqueueMessage(WORKERTHREADMSG_QUIT);
end;

procedure TCEFBrowserThread.InitError;
begin
  Status    := tsInitError;
  ErrorText := 'There was an error initializing the CEF browser.';
  DoOnError;
end;

procedure TCEFBrowserThread.Execute;
begin
  if CreateBrowser then
    inherited Execute
   else
    InitError;
end;

end.
