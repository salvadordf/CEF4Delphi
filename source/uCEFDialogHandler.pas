// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2018 Salvador Díaz Fau. All rights reserved.
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

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

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
      function  OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title, defaultFilePath: ustring; acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: ICefFileDialogCallback): Boolean; virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomDialogHandler = class(TCefDialogHandlerOwn)
    protected
      FEvents : Pointer;

      function  OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title: ustring; const defaultFilePath: ustring; acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: ICefFileDialogCallback): Boolean; override;

      procedure RemoveReferences; override;

    public
      constructor Create(const events: Pointer); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFileDialogCallback;

function cef_dialog_handler_on_file_dialog(self                    : PCefDialogHandler;
                                           browser                 : PCefBrowser;
                                           mode                    : TCefFileDialogMode;
                                           const title             : PCefString;
                                           const default_file_path : PCefString;
                                           accept_filters          : TCefStringList;
                                           selected_accept_filter  : Integer;
                                           callback                : PCefFileDialogCallback): Integer; stdcall;
var
  TempSL : TStringList;
  i, j : NativeUInt;
  TempString : TCefString;
begin
  TempSL := nil;
  Result := 0; // False

  try
    try
      TempSL := TStringList.Create;
      i      := 0;
      j      := cef_string_list_size(accept_filters);

      while (i < j) do
        begin
          FillChar(TempString, SizeOf(TempString), 0);
          cef_string_list_value(accept_filters, i, @TempString);
          TempSL.Add(CefStringClearAndGet(TempString));
          inc(i);
        end;

      Result := Ord(TCefDialogHandlerOwn(CefGetObject(self)).OnFileDialog(TCefBrowserRef.UnWrap(browser),
                                                                          mode,
                                                                          CefString(title),
                                                                          CefString(default_file_path),
                                                                          TempSL,
                                                                          selected_accept_filter,
                                                                          TCefFileDialogCallbackRef.UnWrap(callback)));
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
  CreateData(SizeOf(TCefDialogHandler));

  with PCefDialogHandler(FData)^ do
    on_file_dialog := cef_dialog_handler_on_file_dialog;
end;

function TCefDialogHandlerOwn.OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title, defaultFilePath: ustring; acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: ICefFileDialogCallback): Boolean;
begin
  Result := False;
end;

procedure TCefDialogHandlerOwn.RemoveReferences;
begin
  //
end;

// TCustomDialogHandler

constructor TCustomDialogHandler.Create(const events: Pointer);
begin
  inherited Create;

  FEvents := events;
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
                                                 acceptFilters        : TStrings;
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

