unit uCEFDomDocument;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

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
