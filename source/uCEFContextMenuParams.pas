unit uCEFContextMenuParams;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

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
