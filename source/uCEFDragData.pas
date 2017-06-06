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

unit uCEFDragData;

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
  TCefDragDataRef = class(TCefBaseRefCountedRef, ICefDragData)
  protected
    function Clone: ICefDragData;
    function IsReadOnly: Boolean;
    function IsLink: Boolean;
    function IsFragment: Boolean;
    function IsFile: Boolean;
    function GetLinkUrl: ustring;
    function GetLinkTitle: ustring;
    function GetLinkMetadata: ustring;
    function GetFragmentText: ustring;
    function GetFragmentHtml: ustring;
    function GetFragmentBaseUrl: ustring;
    function GetFileName: ustring;
    function GetFileContents(const writer: ICefStreamWriter): NativeUInt;
    function GetFileNames(names: TStrings): Integer;
    procedure SetLinkUrl(const url: ustring);
    procedure SetLinkTitle(const title: ustring);
    procedure SetLinkMetadata(const data: ustring);
    procedure SetFragmentText(const text: ustring);
    procedure SetFragmentHtml(const html: ustring);
    procedure SetFragmentBaseUrl(const baseUrl: ustring);
    procedure ResetFileContents;
    procedure AddFile(const path, displayName: ustring);
    function  GetImage : ICefImage;
    function  GetImageHotspot : TCefPoint;
    function  HasImage : boolean;
  public
    class function UnWrap(data: Pointer): ICefDragData;
    class function New: ICefDragData;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFImage;

procedure TCefDragDataRef.AddFile(const path, displayName: ustring);
var
  p, d: TCefString;
begin
  p := CefString(path);
  d := CefString(displayName);
  PCefDragData(FData).add_file(FData, @p, @d);
end;

function TCefDragDataRef.GetImage : ICefImage;
begin
  Result := TCefImageRef.UnWrap(PCefDragData(FData).get_image(FData));
end;

function TCefDragDataRef.GetImageHotspot : TCefPoint;
begin
  Result := PCefDragData(FData).get_image_hotspot(FData)^;
end;

function TCefDragDataRef.HasImage : boolean;
begin
  Result := (PCefDragData(FData).has_image(FData) <> 0);
end;

function TCefDragDataRef.Clone: ICefDragData;
begin
  Result := UnWrap(PCefDragData(FData).clone(FData));
end;

function TCefDragDataRef.GetFileContents(
  const writer: ICefStreamWriter): NativeUInt;
begin
  Result := PCefDragData(FData).get_file_contents(FData, CefGetData(writer))
end;

function TCefDragDataRef.GetFileName: ustring;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData).get_file_name(FData));
end;

function TCefDragDataRef.GetFileNames(names: TStrings): Integer;
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    Result := PCefDragData(FData).get_file_names(FData, list);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      names.Add(CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefDragDataRef.GetFragmentBaseUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData).get_fragment_base_url(FData));
end;

function TCefDragDataRef.GetFragmentHtml: ustring;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData).get_fragment_html(FData));
end;

function TCefDragDataRef.GetFragmentText: ustring;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData).get_fragment_text(FData));
end;

function TCefDragDataRef.GetLinkMetadata: ustring;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData).get_link_metadata(FData));
end;

function TCefDragDataRef.GetLinkTitle: ustring;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData).get_link_title(FData));
end;

function TCefDragDataRef.GetLinkUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData).get_link_url(FData));
end;

function TCefDragDataRef.IsFile: Boolean;
begin
  Result := PCefDragData(FData).is_file(FData) <> 0;
end;

function TCefDragDataRef.IsFragment: Boolean;
begin
  Result := PCefDragData(FData).is_fragment(FData) <> 0;
end;

function TCefDragDataRef.IsLink: Boolean;
begin
  Result := PCefDragData(FData).is_link(FData) <> 0;
end;

function TCefDragDataRef.IsReadOnly: Boolean;
begin
  Result := PCefDragData(FData).is_read_only(FData) <> 0;
end;

class function TCefDragDataRef.New: ICefDragData;
begin
  Result := UnWrap(cef_drag_data_create());
end;

procedure TCefDragDataRef.ResetFileContents;
begin
  PCefDragData(FData).reset_file_contents(FData);
end;

procedure TCefDragDataRef.SetFragmentBaseUrl(const baseUrl: ustring);
var
  s: TCefString;
begin
  s := CefString(baseUrl);
  PCefDragData(FData).set_fragment_base_url(FData, @s);
end;

procedure TCefDragDataRef.SetFragmentHtml(const html: ustring);
var
  s: TCefString;
begin
  s := CefString(html);
  PCefDragData(FData).set_fragment_html(FData, @s);
end;

procedure TCefDragDataRef.SetFragmentText(const text: ustring);
var
  s: TCefString;
begin
  s := CefString(text);
  PCefDragData(FData).set_fragment_text(FData, @s);
end;

procedure TCefDragDataRef.SetLinkMetadata(const data: ustring);
var
  s: TCefString;
begin
  s := CefString(data);
  PCefDragData(FData).set_link_metadata(FData, @s);
end;

procedure TCefDragDataRef.SetLinkTitle(const title: ustring);
var
  s: TCefString;
begin
  s := CefString(title);
  PCefDragData(FData).set_link_title(FData, @s);
end;

procedure TCefDragDataRef.SetLinkUrl(const url: ustring);
var
  s: TCefString;
begin
  s := CefString(url);
  PCefDragData(FData).set_link_url(FData, @s);
end;

class function TCefDragDataRef.UnWrap(data: Pointer): ICefDragData;
begin
  if data <> nil then
    Result := Create(data) as ICefDragData else
    Result := nil;
end;


end.
