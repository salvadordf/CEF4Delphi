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

unit uCEFDialogHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefDialogHandlerOwn = class(TCefBaseRefCountedOwn, ICefDialogHandler)
    protected
      function  OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: ICefFileDialogCallback): Boolean; virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomDialogHandler = class(TCefDialogHandlerOwn)
    protected
      FEvents : Pointer;

      function  OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title: ustring; const defaultFilePath: ustring; const acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: ICefFileDialogCallback): Boolean; override;

      procedure RemoveReferences; override;

    public
      constructor Create(const events : IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFileDialogCallback, uCEFStringList;

function cef_dialog_handler_on_file_dialog(self                    : PCefDialogHandler;
                                           browser                 : PCefBrowser;
                                           mode                    : TCefFileDialogMode;
                                           const title             : PCefString;
                                           const default_file_path : PCefString;
                                           accept_filters          : TCefStringList;
                                           selected_accept_filter  : Integer;
                                           callback                : PCefFileDialogCallback): Integer; stdcall;
var
  TempSL     : TStringList;
  TempCefSL  : ICefStringList;
  TempObject : TObject;
begin
  TempSL := nil;
  Result := Ord(False);

  try
    try
      TempObject := CefGetObject(self);

      if (TempObject <> nil) and (TempObject is TCefDialogHandlerOwn) then
        begin
          TempSL    := TStringList.Create;
          TempCefSL := TCefStringListRef.Create(accept_filters);
          TempCefSL.CopyToStrings(TempSL);

          Result := Ord(TCefDialogHandlerOwn(TempObject).OnFileDialog(TCefBrowserRef.UnWrap(browser),
                                                                      mode,
                                                                      CefString(title),
                                                                      CefString(default_file_path),
                                                                      TempSL,
                                                                      selected_accept_filter,
                                                                      TCefFileDialogCallbackRef.UnWrap(callback)));
        end;
    except
      on e : exception do
        if CustomExceptionHandler('cef_dialog_handler_on_file_dialog', e) then raise;
    end;
  finally
    if (TempSL <> nil) then FreeAndNil(TempSL);
  end;
end;

constructor TCefDialogHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDialogHandler));

  PCefDialogHandler(FData)^.on_file_dialog := {$IFDEF FPC}@{$ENDIF}cef_dialog_handler_on_file_dialog;
end;

function TCefDialogHandlerOwn.OnFileDialog(const browser                : ICefBrowser;
                                                 mode                   : TCefFileDialogMode;
                                           const title                  : ustring;
                                           const defaultFilePath        : ustring;
                                           const acceptFilters          : TStrings;
                                                 selectedAcceptFilter   : Integer;
                                           const callback               : ICefFileDialogCallback): Boolean;
begin
  Result := False;
end;

procedure TCefDialogHandlerOwn.RemoveReferences;
begin
  //
end;

// TCustomDialogHandler

constructor TCustomDialogHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomDialogHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomDialogHandler.RemoveReferences;
begin
  FEvents := nil;
end;

function TCustomDialogHandler.OnFileDialog(const browser              : ICefBrowser;
                                                 mode                 : TCefFileDialogMode;
                                           const title                : ustring;
                                           const defaultFilePath      : ustring;
                                           const acceptFilters        : TStrings;
                                                 selectedAcceptFilter : Integer;
                                           const callback             : ICefFileDialogCallback): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnFileDialog(browser, mode, title, defaultFilePath,
                                                      acceptFilters, selectedAcceptFilter, callback)
   else
    Result := inherited OnFileDialog(browser, mode, title, defaultFilePath,
                                     acceptFilters, selectedAcceptFilter, callback);
end;

end.

