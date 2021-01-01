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

unit uCEFContextMenuParams;

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
  TCefContextMenuParamsRef = class(TCefBaseRefCountedRef, ICefContextMenuParams)
  protected
    function GetXCoord: Integer;
    function GetYCoord: Integer;
    function GetTypeFlags: TCefContextMenuTypeFlags;
    function GetLinkUrl: ustring;
    function GetUnfilteredLinkUrl: ustring;
    function GetSourceUrl: ustring;
    function HasImageContents: Boolean;
    function GetTitleText: ustring;
    function GetPageUrl: ustring;
    function GetFrameUrl: ustring;
    function GetFrameCharset: ustring;
    function GetMediaType: TCefContextMenuMediaType;
    function GetMediaStateFlags: TCefContextMenuMediaStateFlags;
    function GetSelectionText: ustring;
    function GetMisspelledWord: ustring;
    function GetDictionarySuggestions(const suggestions: TStringList): Boolean;
    function IsEditable: Boolean;
    function IsSpellCheckEnabled: Boolean;
    function GetEditStateFlags: TCefContextMenuEditStateFlags;
    function IsCustomMenu: Boolean;
    function IsPepperMenu: Boolean;
  public
    class function UnWrap(data: Pointer): ICefContextMenuParams;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFStringList;


function TCefContextMenuParamsRef.GetDictionarySuggestions(const suggestions : TStringList): Boolean;
var
  TempSL : ICefStringList;
begin
  Result := False;

  if (suggestions <> nil) then
    begin
      TempSL := TCefStringListOwn.Create;

      if (PCefContextMenuParams(FData)^.get_dictionary_suggestions(PCefContextMenuParams(FData), TempSL.Handle) <> 0) then
        begin
          TempSL.CopyToStrings(suggestions);
          Result := True;
        end;
    end;
end;

function TCefContextMenuParamsRef.GetEditStateFlags: TCefContextMenuEditStateFlags;
begin
  Result := PCefContextMenuParams(FData)^.get_edit_state_flags(PCefContextMenuParams(FData));
end;

function TCefContextMenuParamsRef.GetFrameCharset: ustring;
begin
  Result := CefStringFreeAndGet(PCefContextMenuParams(FData)^.get_frame_charset(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetFrameUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefContextMenuParams(FData)^.get_frame_url(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetLinkUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefContextMenuParams(FData)^.get_link_url(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetMediaStateFlags: TCefContextMenuMediaStateFlags;
begin
  Result := PCefContextMenuParams(FData)^.get_media_state_flags(PCefContextMenuParams(FData));
end;

function TCefContextMenuParamsRef.GetMediaType: TCefContextMenuMediaType;
begin
  Result := PCefContextMenuParams(FData)^.get_media_type(PCefContextMenuParams(FData));
end;

function TCefContextMenuParamsRef.GetMisspelledWord: ustring;
begin
  Result := CefStringFreeAndGet(PCefContextMenuParams(FData)^.get_misspelled_word(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetTitleText: ustring;
begin
  Result := CefStringFreeAndGet(PCefContextMenuParams(FData)^.get_title_text(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetPageUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefContextMenuParams(FData)^.get_page_url(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetSelectionText: ustring;
begin
  Result := CefStringFreeAndGet(PCefContextMenuParams(FData)^.get_selection_text(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetSourceUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefContextMenuParams(FData)^.get_source_url(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetTypeFlags: TCefContextMenuTypeFlags;
begin
  Result := PCefContextMenuParams(FData)^.get_type_flags(PCefContextMenuParams(FData));
end;

function TCefContextMenuParamsRef.GetUnfilteredLinkUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefContextMenuParams(FData)^.get_unfiltered_link_url(PCefContextMenuParams(FData)));
end;

function TCefContextMenuParamsRef.GetXCoord: Integer;
begin
  Result := PCefContextMenuParams(FData)^.get_xcoord(PCefContextMenuParams(FData));
end;

function TCefContextMenuParamsRef.GetYCoord: Integer;
begin
  Result := PCefContextMenuParams(FData)^.get_ycoord(PCefContextMenuParams(FData));
end;

function TCefContextMenuParamsRef.IsCustomMenu: Boolean;
begin
  Result := PCefContextMenuParams(FData)^.is_custom_menu(PCefContextMenuParams(FData)) <> 0;
end;

function TCefContextMenuParamsRef.IsEditable: Boolean;
begin
  Result := PCefContextMenuParams(FData)^.is_editable(PCefContextMenuParams(FData)) <> 0;
end;

function TCefContextMenuParamsRef.IsPepperMenu: Boolean;
begin
  Result := PCefContextMenuParams(FData)^.is_pepper_menu(PCefContextMenuParams(FData)) <> 0;
end;

function TCefContextMenuParamsRef.IsSpellCheckEnabled: Boolean;
begin
  Result := PCefContextMenuParams(FData)^.is_spell_check_enabled(PCefContextMenuParams(FData)) <> 0;
end;

function TCefContextMenuParamsRef.HasImageContents: Boolean;
begin
  Result := PCefContextMenuParams(FData)^.has_image_contents(PCefContextMenuParams(FData)) <> 0;
end;

class function TCefContextMenuParamsRef.UnWrap(data: Pointer): ICefContextMenuParams;
begin
  if (data <> nil) then
    Result := Create(data) as ICefContextMenuParams
   else
    Result := nil;
end;

end.
