unit uCEFDownloadHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefDownloadHandlerOwn = class(TCefBaseRefCountedOwn, ICefDownloadHandler)
    protected
      function  CanDownload(const browser: ICefBrowser; const url, request_method: ustring): boolean; virtual;
      function  OnBeforeDownload(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback): boolean; virtual;
      procedure OnDownloadUpdated(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const callback: ICefDownloadItemCallback); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomDownloadHandler = class(TCefDownloadHandlerOwn)
    protected
      FEvents : Pointer;

      function  CanDownload(const browser: ICefBrowser; const url, request_method: ustring): boolean; override;
      function  OnBeforeDownload(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback): boolean; override;
      procedure OnDownloadUpdated(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const callback: ICefDownloadItemCallback); override;

      procedure RemoveReferences; override;

    public
      constructor Create(const events : IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFDownloadItem, uCEFBeforeDownloadCallback,
  uCEFDownloadItemCallback;

function cef_download_handler_can_download(      self           : PCefDownloadHandler;
                                                 browser        : PCefBrowser;
                                           const url            : PCefString;
                                           const request_method : PCefString): integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(True);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDownloadHandlerOwn) then
    Result := Ord(TCefDownloadHandlerOwn(TempObject).CanDownload(TCefBrowserRef.UnWrap(browser),
                                                                 CefString(url),
                                                                 CefString(request_method)));
end;

function  cef_download_handler_on_before_download(      self           : PCefDownloadHandler;
                                                        browser        : PCefBrowser;
                                                        download_item  : PCefDownloadItem;
                                                  const suggested_name : PCefString;
                                                        callback       : PCefBeforeDownloadCallback): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDownloadHandlerOwn) then
    Result := Ord(TCefDownloadHandlerOwn(TempObject).OnBeforeDownload(TCefBrowserRef.UnWrap(browser),
                                                                      TCefDownLoadItemRef.UnWrap(download_item),
                                                                      CefString(suggested_name),
                                                                      TCefBeforeDownloadCallbackRef.UnWrap(callback)));
end;

procedure cef_download_handler_on_download_updated(self          : PCefDownloadHandler;
                                                   browser       : PCefBrowser;
                                                   download_item : PCefDownloadItem;
                                                   callback      : PCefDownloadItemCallback); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDownloadHandlerOwn) then
    TCefDownloadHandlerOwn(TempObject).OnDownloadUpdated(TCefBrowserRef.UnWrap(browser),
                                                         TCefDownLoadItemRef.UnWrap(download_item),
                                                         TCefDownloadItemCallbackRef.UnWrap(callback));
end;

constructor TCefDownloadHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDownloadHandler));

  with PCefDownloadHandler(FData)^ do
    begin
      can_download        := {$IFDEF FPC}@{$ENDIF}cef_download_handler_can_download;
      on_before_download  := {$IFDEF FPC}@{$ENDIF}cef_download_handler_on_before_download;
      on_download_updated := {$IFDEF FPC}@{$ENDIF}cef_download_handler_on_download_updated;
    end;
end;

function TCefDownloadHandlerOwn.CanDownload(const browser: ICefBrowser; const url, request_method: ustring): boolean;
begin
  Result := True;
end;

function  TCefDownloadHandlerOwn.OnBeforeDownload(const browser       : ICefBrowser;
                                                  const downloadItem  : ICefDownloadItem;
                                                  const suggestedName : ustring;
                                                  const callback      : ICefBeforeDownloadCallback): boolean;
begin
  Result := False;
end;

procedure TCefDownloadHandlerOwn.OnDownloadUpdated(const browser      : ICefBrowser;
                                                   const downloadItem : ICefDownloadItem;
                                                   const callback     : ICefDownloadItemCallback);
begin

end;

procedure TCefDownloadHandlerOwn.RemoveReferences;
begin
  //
end;

// TCustomDownloadHandler

constructor TCustomDownloadHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomDownloadHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomDownloadHandler.RemoveReferences;
begin
  FEvents := nil;
end;

function TCustomDownloadHandler.CanDownload(const browser        : ICefBrowser;
                                            const url            : ustring;
                                            const request_method : ustring): boolean;
begin
  Result := True;

  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnCanDownload(browser, url, request_method);
end;

function  TCustomDownloadHandler.OnBeforeDownload(const browser       : ICefBrowser;
                                                  const downloadItem  : ICefDownloadItem;
                                                  const suggestedName : ustring;
                                                  const callback      : ICefBeforeDownloadCallback): boolean;
begin
  Result := (FEvents <> nil) and
            IChromiumEvents(FEvents).doOnBeforeDownload(browser, downloadItem, suggestedName, callback);
end;

procedure TCustomDownloadHandler.OnDownloadUpdated(const browser      : ICefBrowser;
                                                   const downloadItem : ICefDownloadItem;
                                                   const callback     : ICefDownloadItemCallback);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnDownloadUpdated(browser, downloadItem, callback);
end;

end.