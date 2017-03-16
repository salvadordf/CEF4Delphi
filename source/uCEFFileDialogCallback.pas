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

unit uCEFFileDialogCallback;

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
  TCefFileDialogCallbackRef = class(TCefBaseRefCountedRef, ICefFileDialogCallback)
  protected
    procedure Cont(selectedAcceptFilter: Integer; filePaths: TStrings);
    procedure Cancel;
  public
    class function UnWrap(data: Pointer): ICefFileDialogCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefFileDialogCallbackRef.Cancel;
begin
  PCefFileDialogCallback(FData).cancel(FData);
end;

procedure TCefFileDialogCallbackRef.Cont(selectedAcceptFilter: Integer; filePaths: TStrings);
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    for i := 0 to filePaths.Count - 1 do
    begin
      str := CefString(filePaths[i]);
      cef_string_list_append(list, @str);
    end;
    PCefFileDialogCallback(FData).cont(FData, selectedAcceptFilter, list);
  finally
    cef_string_list_free(list);
  end;
end;

class function TCefFileDialogCallbackRef.UnWrap(
  data: Pointer): ICefFileDialogCallback;
begin
  if data <> nil then
    Result := Create(data) as ICefFileDialogCallback else
    Result := nil;
end;

end.
