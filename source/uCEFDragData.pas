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

unit uCEFDragData;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefDragDataRef = class(TCefBaseRefCountedRef, ICefDragData)
  protected
    function  Clone: ICefDragData;
    function  IsReadOnly: Boolean;
    function  IsLink: Boolean;
    function  IsFragment: Boolean;
    function  IsFile: Boolean;
    function  GetLinkUrl: ustring;
    function  GetLinkTitle: ustring;
    function  GetLinkMetadata: ustring;
    function  GetFragmentText: ustring;
    function  GetFragmentHtml: ustring;
    function  GetFragmentBaseUrl: ustring;
    function  GetFileName: ustring;
    function  GetFileContents(const writer: ICefStreamWriter): NativeUInt;
    function  GetFileNames(var names: TStrings): Integer;
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
  uCEFMiscFunctions, uCEFLibFunctions, uCEFImage, uCEFStringList;

procedure TCefDragDataRef.AddFile(const path, displayName: ustring);
var
  TempPath, TempName : TCefString;
begin
  TempPath := CefString(path);
  TempName := CefString(displayName);
  PCefDragData(FData)^.add_file(FData, @TempPath, @TempName);
end;

function TCefDragDataRef.GetImage : ICefImage;
begin
  Result := TCefImageRef.UnWrap(PCefDragData(FData)^.get_image(FData));
end;

function TCefDragDataRef.GetImageHotspot : TCefPoint;
begin
  Result := PCefDragData(FData)^.get_image_hotspot(FData)^;
end;

function TCefDragDataRef.HasImage : boolean;
begin
  Result := (PCefDragData(FData)^.has_image(FData) <> 0);
end;

function TCefDragDataRef.Clone: ICefDragData;
begin
  Result := UnWrap(PCefDragData(FData)^.clone(FData));
end;

function TCefDragDataRef.GetFileContents(const writer: ICefStreamWriter): NativeUInt;
begin
  Result := PCefDragData(FData)^.get_file_contents(FData, CefGetData(writer))
end;

function TCefDragDataRef.GetFileName: ustring;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData)^.get_file_name(FData));
end;

function TCefDragDataRef.GetFileNames(var names: TStrings): Integer;
var
  TempSL : ICefStringList;
begin
  Result := 0;

  if (names <> nil) then
    begin
      TempSL := TCefStringListOwn.Create;

      if (PCefDragData(FData)^.get_file_names(FData, TempSL.Handle) <> 0) then
        begin
          TempSL.CopyToStrings(names);
          Result := names.Count;
        end;
    end;
end;

function TCefDragDataRef.GetFragmentBaseUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData)^.get_fragment_base_url(FData));
end;

function TCefDragDataRef.GetFragmentHtml: ustring;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData)^.get_fragment_html(FData));
end;

function TCefDragDataRef.GetFragmentText: ustring;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData)^.get_fragment_text(FData));
end;

function TCefDragDataRef.GetLinkMetadata: ustring;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData)^.get_link_metadata(FData));
end;

function TCefDragDataRef.GetLinkTitle: ustring;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData)^.get_link_title(FData));
end;

function TCefDragDataRef.GetLinkUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData)^.get_link_url(FData));
end;

function TCefDragDataRef.IsFile: Boolean;
begin
  Result := PCefDragData(FData)^.is_file(FData) <> 0;
end;

function TCefDragDataRef.IsFragment: Boolean;
begin
  Result := PCefDragData(FData)^.is_fragment(FData) <> 0;
end;

function TCefDragDataRef.IsLink: Boolean;
begin
  Result := PCefDragData(FData)^.is_link(FData) <> 0;
end;

function TCefDragDataRef.IsReadOnly: Boolean;
begin
  Result := PCefDragData(FData)^.is_read_only(FData) <> 0;
end;

class function TCefDragDataRef.New: ICefDragData;
begin
  Result := UnWrap(cef_drag_data_create());
end;

procedure TCefDragDataRef.ResetFileContents;
begin
  PCefDragData(FData)^.reset_file_contents(FData);
end;

procedure TCefDragDataRef.SetFragmentBaseUrl(const baseUrl: ustring);
var
  TempURL : TCefString;
begin
  TempURL := CefString(baseUrl);
  PCefDragData(FData)^.set_fragment_base_url(FData, @TempURL);
end;

procedure TCefDragDataRef.SetFragmentHtml(const html: ustring);
var
  TempHTML : TCefString;
begin
  TempHTML := CefString(html);
  PCefDragData(FData)^.set_fragment_html(FData, @TempHTML);
end;

procedure TCefDragDataRef.SetFragmentText(const text: ustring);
var
  TempText : TCefString;
begin
  TempText := CefString(text);
  PCefDragData(FData)^.set_fragment_text(FData, @TempText);
end;

procedure TCefDragDataRef.SetLinkMetadata(const data: ustring);
var
  TempData : TCefString;
begin
  TempData := CefString(data);
  PCefDragData(FData)^.set_link_metadata(FData, @TempData);
end;

procedure TCefDragDataRef.SetLinkTitle(const title: ustring);
var
  TempTitle : TCefString;
begin
  TempTitle := CefString(title);
  PCefDragData(FData)^.set_link_title(FData, @TempTitle);
end;

procedure TCefDragDataRef.SetLinkUrl(const url: ustring);
var
  TempURL : TCefString;
begin
  TempURL := CefString(url);
  PCefDragData(FData)^.set_link_url(FData, @TempURL);
end;

class function TCefDragDataRef.UnWrap(data: Pointer): ICefDragData;
begin
  if (data <> nil) then
    Result := Create(data) as ICefDragData
   else
    Result := nil;
end;


end.
