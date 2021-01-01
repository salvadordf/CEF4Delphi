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

unit uCEFRunFileDialogCallback;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces;

type
  TCefRunFileDialogCallbackOwn = class(TCefBaseRefCountedOwn, ICefRunFileDialogCallback)
    protected
      procedure OnFileDialogDismissed(selectedAcceptFilter: Integer; const filePaths: TStrings); virtual;

    public
      constructor Create;
  end;

  TCefFastRunFileDialogCallback = class(TCefRunFileDialogCallbackOwn)
    protected
      FCallback: TCefRunFileDialogCallbackProc;

      procedure OnFileDialogDismissed(selectedAcceptFilter: Integer; const filePaths: TStrings); override;

    public
      constructor Create(callback: TCefRunFileDialogCallbackProc); reintroduce; virtual;
  end;

implementation

uses
  uCEFTypes, uCEFMiscFunctions, uCEFLibFunctions, uCEFStringList;

procedure cef_run_file_dialog_callback_on_file_dialog_dismissed(self                   : PCefRunFileDialogCallback;
                                                                selected_accept_filter : Integer;
                                                                file_paths             : TCefStringList); stdcall;
var
  TempSL     : TStringList;
  TempCefSL  : ICefStringList;
  TempObject : TObject;
begin
  TempSL     := nil;
  TempObject := CefGetObject(self);

  try
    try
      if (TempObject <> nil) and (TempObject is TCefRunFileDialogCallbackOwn) then
        begin
          TempSL    := TStringList.Create;
          TempCefSL := TCefStringListRef.Create(file_paths);
          TempCefSL.CopyToStrings(TempSL);

          TCefRunFileDialogCallbackOwn(TempObject).OnFileDialogDismissed(selected_accept_filter, TempSL);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('cef_run_file_dialog_callback_on_file_dialog_dismissed', e) then raise;
    end;
  finally
    if (TempSL <> nil) then FreeAndNil(TempSL);
  end;
end;

// TCefRunFileDialogCallbackOwn

constructor TCefRunFileDialogCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefRunFileDialogCallback));

  with PCefRunFileDialogCallback(FData)^ do
    on_file_dialog_dismissed := {$IFDEF FPC}@{$ENDIF}cef_run_file_dialog_callback_on_file_dialog_dismissed;
end;

procedure TCefRunFileDialogCallbackOwn.OnFileDialogDismissed(selectedAcceptFilter: Integer; const filePaths: TStrings);
begin
 //
end;

// TCefFastRunFileDialogCallback

procedure TCefFastRunFileDialogCallback.OnFileDialogDismissed(selectedAcceptFilter: Integer; const filePaths: TStrings);
begin
  FCallback(selectedAcceptFilter, filePaths);
end;

constructor TCefFastRunFileDialogCallback.Create(callback: TCefRunFileDialogCallbackProc);
begin
  inherited Create;

  FCallback := callback;
end;

end.
