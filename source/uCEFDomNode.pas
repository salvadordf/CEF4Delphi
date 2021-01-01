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

unit uCEFDomNode;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes, System.SysUtils,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefDomNodeRef = class(TCefBaseRefCountedRef, ICefDomNode)
    protected
      function  GetType: TCefDomNodeType;
      function  IsText: Boolean;
      function  IsElement: Boolean;
      function  IsEditable: Boolean;
      function  IsFormControlElement: Boolean;
      function  GetFormControlElementType: ustring;
      function  IsSame(const that: ICefDomNode): Boolean;
      function  GetName: ustring;
      function  GetValue: ustring;
      function  SetValue(const value: ustring): Boolean;
      function  GetAsMarkup: ustring;
      function  GetDocument: ICefDomDocument;
      function  GetParent: ICefDomNode;
      function  GetPreviousSibling: ICefDomNode;
      function  GetNextSibling: ICefDomNode;
      function  HasChildren: Boolean;
      function  GetFirstChild: ICefDomNode;
      function  GetLastChild: ICefDomNode;
      function  GetElementTagName: ustring;
      function  HasElementAttributes: Boolean;
      function  HasElementAttribute(const attrName: ustring): Boolean;
      function  GetElementAttribute(const attrName: ustring): ustring;
      procedure GetElementAttributes(const attrMap: ICefStringMap); overload;
      procedure GetElementAttributes(var attrList: TStrings); overload;
      function  SetElementAttribute(const attrName, value: ustring): Boolean;
      function  GetElementInnerText: ustring;
      function  GetElementBounds: TCefRect;

    public
      class function UnWrap(data: Pointer): ICefDomNode;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFDomDocument, uCEFStringMap;

function TCefDomNodeRef.GetAsMarkup: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomNode(FData)^.get_as_markup(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetDocument: ICefDomDocument;
begin
  Result := TCefDomDocumentRef.UnWrap(PCefDomNode(FData)^.get_document(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetElementAttribute(const attrName: ustring): ustring;
var
  TempName : TCefString;
begin
  TempName := CefString(attrName);
  Result   := CefStringFreeAndGet(PCefDomNode(FData)^.get_element_attribute(PCefDomNode(FData), @TempName));
end;

procedure TCefDomNodeRef.GetElementAttributes(const attrMap: ICefStringMap);
begin
  PCefDomNode(FData)^.get_element_attributes(PCefDomNode(FData), attrMap.Handle);
end;

procedure TCefDomNodeRef.GetElementAttributes(var attrList: TStrings);
var
  TempStrMap : ICefStringMap;
  i, j : NativeUInt;
  TempKey, TempValue : ustring;
begin
  TempStrMap := nil;

  try
    try
      if (attrList <> nil) then
        begin
          TempStrMap := TCefStringMapOwn.Create;
          PCefDomNode(FData)^.get_element_attributes(PCefDomNode(FData), TempStrMap.Handle);

          i := 0;
          j := TempStrMap.Size;

          while (i < j) do
            begin
              TempKey   := TempStrMap.Key[i];
              TempValue := TempStrMap.Value[i];

              if (length(TempKey) > 0) and (length(TempValue) > 0) then
                attrList.Add(TempKey + attrList.NameValueSeparator + TempValue)
               else
                if (length(TempKey) > 0) then
                  attrList.Add(TempKey)
                 else
                  if (length(TempValue) > 0) then
                    attrList.Add(TempValue);

              inc(i);
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCefDomNodeRef.GetElementAttributes', e) then raise;
    end;
  finally
    TempStrMap := nil;
  end;
end;

function TCefDomNodeRef.GetElementInnerText: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomNode(FData)^.get_element_inner_text(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetElementBounds: TCefRect;
begin
  Result := PCefDomNode(FData)^.get_element_bounds(PCefDomNode(FData));
end;

function TCefDomNodeRef.GetElementTagName: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomNode(FData)^.get_element_tag_name(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetFirstChild: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)^.get_first_child(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetFormControlElementType: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomNode(FData)^.get_form_control_element_type(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetLastChild: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)^.get_last_child(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetName: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomNode(FData)^.get_name(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetNextSibling: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)^.get_next_sibling(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetParent: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)^.get_parent(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetPreviousSibling: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)^.get_previous_sibling(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetType: TCefDomNodeType;
begin
  Result := PCefDomNode(FData)^.get_type(PCefDomNode(FData));
end;

function TCefDomNodeRef.GetValue: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomNode(FData)^.get_value(PCefDomNode(FData)));
end;

function TCefDomNodeRef.HasChildren: Boolean;
begin
  Result := PCefDomNode(FData)^.has_children(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.HasElementAttribute(const attrName: ustring): Boolean;
var
  TempName : TCefString;
begin
  TempName := CefString(attrName);
  Result   := PCefDomNode(FData)^.has_element_attribute(PCefDomNode(FData), @TempName) <> 0;
end;

function TCefDomNodeRef.HasElementAttributes: Boolean;
begin
  Result := PCefDomNode(FData)^.has_element_attributes(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.IsEditable: Boolean;
begin
  Result := PCefDomNode(FData)^.is_editable(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.IsElement: Boolean;
begin
  Result := PCefDomNode(FData)^.is_element(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.IsFormControlElement: Boolean;
begin
  Result := PCefDomNode(FData)^.is_form_control_element(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.IsSame(const that: ICefDomNode): Boolean;
begin
  Result := PCefDomNode(FData)^.is_same(PCefDomNode(FData), CefGetData(that)) <> 0;
end;

function TCefDomNodeRef.IsText: Boolean;
begin
  Result := PCefDomNode(FData)^.is_text(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.SetElementAttribute(const attrName, value: ustring): Boolean;
var
  TempName, TempValue : TCefString;
begin
  TempName  := CefString(attrName);
  TempValue := CefString(value);
  Result    := PCefDomNode(FData)^.set_element_attribute(PCefDomNode(FData), @TempName, @TempValue) <> 0;
end;

function TCefDomNodeRef.SetValue(const value: ustring): Boolean;
var
  TempValue : TCefString;
begin
  TempValue := CefString(value);
  Result    := PCefDomNode(FData)^.set_value(PCefDomNode(FData), @TempValue) <> 0;
end;

class function TCefDomNodeRef.UnWrap(data: Pointer): ICefDomNode;
begin
  if (data <> nil) then
    Result := Create(data) as ICefDomNode
   else
    Result := nil;
end;


end.
