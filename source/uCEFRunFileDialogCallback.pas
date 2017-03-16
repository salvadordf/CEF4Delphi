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
//        Copyright © 2017 Salvador Díaz Fau. All rights reserved.
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

unit uCEFRunFileDialogCallback;

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
  uCEFBaseRefCounted, uCEFInterfaces;

type
  TCefRunFileDialogCallbackOwn = class(TCefBaseRefCountedOwn, ICefRunFileDialogCallback)
    protected
      procedure OnFileDialogDismissed(selectedAcceptFilter: Integer; filePaths: TStrings); virtual;

    public
      constructor Create;
  end;

  TCefFastRunFileDialogCallback = class(TCefRunFileDialogCallbackOwn)
    protected
      FCallback: TCefRunFileDialogCallbackProc;

      procedure OnFileDialogDismissed(selectedAcceptFilter: Integer; filePaths: TStrings); override;

    public
      constructor Create(callback: TCefRunFileDialogCallbackProc); reintroduce; virtual;
  end;

implementation

uses
  uCEFTypes, uCEFMiscFunctions, uCEFLibFunctions;

procedure cef_run_file_dialog_callback_on_file_dialog_dismissed(self: PCefRunFileDialogCallback; selected_accept_filter: Integer; file_paths: TCefStringList); stdcall;
var
  TempSL : TStringList;
  i: Integer;
  str: TCefString;
begin
  TempSL := TStringList.Create;
  try
    for i := 0 to cef_string_list_size(file_paths) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(file_paths, i, @str);
      TempSL.Add(CefStringClearAndGet(str));
    end;
    with TCefRunFileDialogCallbackOwn(CefGetObject(self)) do
      OnFileDialogDismissed(selected_accept_filter, TempSL);
  finally
    TempSL.Free;
  end;
end;

// TCefRunFileDialogCallbackOwn

constructor TCefRunFileDialogCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefRunFileDialogCallback));

  with PCefRunFileDialogCallback(FData)^ do on_file_dialog_dismissed := cef_run_file_dialog_callback_on_file_dialog_dismissed;
end;

procedure TCefRunFileDialogCallbackOwn.OnFileDialogDismissed(selectedAcceptFilter: Integer; filePaths: TStrings);
begin
 //
end;

// TCefFastRunFileDialogCallback

procedure TCefFastRunFileDialogCallback.OnFileDialogDismissed(selectedAcceptFilter: Integer; filePaths: TStrings);
begin
  FCallback(selectedAcceptFilter, filePaths);
end;

constructor TCefFastRunFileDialogCallback.Create(callback: TCefRunFileDialogCallbackProc);
begin
  inherited Create;

  FCallback := callback;
end;

end.
