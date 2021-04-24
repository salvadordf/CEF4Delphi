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

unit uCEFDownloadHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefDownloadHandlerOwn = class(TCefBaseRefCountedOwn, ICefDownloadHandler)
    protected
      procedure OnBeforeDownload(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback); virtual;
      procedure OnDownloadUpdated(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const callback: ICefDownloadItemCallback); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomDownloadHandler = class(TCefDownloadHandlerOwn)
    protected
      FEvents : Pointer;

      procedure OnBeforeDownload(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback); override;
      procedure OnDownloadUpdated(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const callback: ICefDownloadItemCallback); override;

      procedure RemoveReferences; override;

    public
      constructor Create(const events : IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFDownLoadItem, uCEFBeforeDownloadCallback,
  uCEFDownloadItemCallback;

procedure cef_download_handler_on_before_download(      self           : PCefDownloadHandler;
                                                        browser        : PCefBrowser;
                                                        download_item  : PCefDownloadItem;
                                                  const suggested_name : PCefString;
                                                        callback       : PCefBeforeDownloadCallback); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDownloadHandlerOwn) then
    TCefDownloadHandlerOwn(TempObject).OnBeforeDownload(TCefBrowserRef.UnWrap(browser),
                                                        TCefDownLoadItemRef.UnWrap(download_item),
                                                        CefString(suggested_name),
                                                        TCefBeforeDownloadCallbackRef.UnWrap(callback));
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
      on_before_download  := {$IFDEF FPC}@{$ENDIF}cef_download_handler_on_before_download;
      on_download_updated := {$IFDEF FPC}@{$ENDIF}cef_download_handler_on_download_updated;
    end;
end;

procedure TCefDownloadHandlerOwn.OnBeforeDownload(const browser       : ICefBrowser;
                                                  const downloadItem  : ICefDownloadItem;
                                                  const suggestedName : ustring;
                                                  const callback      : ICefBeforeDownloadCallback);
begin

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

procedure TCustomDownloadHandler.OnBeforeDownload(const browser       : ICefBrowser;
                                                  const downloadItem  : ICefDownloadItem;
                                                  const suggestedName : ustring;
                                                  const callback      : ICefBeforeDownloadCallback);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnBeforeDownload(browser, downloadItem, suggestedName, callback);
end;

procedure TCustomDownloadHandler.OnDownloadUpdated(const browser      : ICefBrowser;
                                                   const downloadItem : ICefDownloadItem;
                                                   const callback     : ICefDownloadItemCallback);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnDownloadUpdated(browser, downloadItem, callback);
end;

end.
