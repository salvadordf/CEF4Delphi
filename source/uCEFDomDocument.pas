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

unit uCEFDomDocument;

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
  TCefDomDocumentRef = class(TCefBaseRefCountedRef, ICefDomDocument)
    protected
      function GetType: TCefDomDocumentType;
      function GetDocument: ICefDomNode;
      function GetBody: ICefDomNode;
      function GetHead: ICefDomNode;
      function GetTitle: ustring;
      function GetElementById(const id: ustring): ICefDomNode;
      function GetFocusedNode: ICefDomNode;
      function HasSelection: Boolean;
      function GetSelectionStartOffset: Integer;
      function GetSelectionEndOffset: Integer;
      function GetSelectionAsMarkup: ustring;
      function GetSelectionAsText: ustring;
      function GetBaseUrl: ustring;
      function GetCompleteUrl(const partialURL: ustring): ustring;

    public
      class function UnWrap(data: Pointer): ICefDomDocument;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFDomNode;

function TCefDomDocumentRef.GetBaseUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomDocument(FData)^.get_base_url(PCefDomDocument(FData)))
end;

function TCefDomDocumentRef.GetBody: ICefDomNode;
begin
  Result :=  TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_body(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetCompleteUrl(const partialURL: ustring): ustring;
var
  TempPartialURL : TCefString;
begin
  TempPartialURL := CefString(partialURL);
  Result         := CefStringFreeAndGet(PCefDomDocument(FData)^.get_complete_url(PCefDomDocument(FData), @TempPartialURL));
end;

function TCefDomDocumentRef.GetDocument: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_document(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetElementById(const id: ustring): ICefDomNode;
var
  TempID : TCefString;
begin
  TempID := CefString(id);
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_element_by_id(PCefDomDocument(FData), @TempID));
end;

function TCefDomDocumentRef.GetFocusedNode: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_focused_node(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetHead: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_head(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetSelectionAsMarkup: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomDocument(FData)^.get_selection_as_markup(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetSelectionAsText: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomDocument(FData)^.get_selection_as_text(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetSelectionEndOffset: Integer;
begin
  Result := PCefDomDocument(FData)^.get_selection_end_offset(PCefDomDocument(FData));
end;

function TCefDomDocumentRef.GetSelectionStartOffset: Integer;
begin
  Result := PCefDomDocument(FData)^.get_selection_start_offset(PCefDomDocument(FData));
end;

function TCefDomDocumentRef.GetTitle: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomDocument(FData)^.get_title(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetType: TCefDomDocumentType;
begin
  Result := PCefDomDocument(FData)^.get_type(PCefDomDocument(FData));
end;

function TCefDomDocumentRef.HasSelection: Boolean;
begin
  Result := PCefDomDocument(FData)^.has_selection(PCefDomDocument(FData)) <> 0;
end;

class function TCefDomDocumentRef.UnWrap(data: Pointer): ICefDomDocument;
begin
  if (data <> nil) then
    Result := Create(data) as ICefDomDocument
   else
    Result := nil;
end;

end.
