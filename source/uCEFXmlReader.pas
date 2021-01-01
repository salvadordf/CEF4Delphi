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

unit uCEFXmlReader;

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
  TCefXmlReaderRef = class(TCefBaseRefCountedRef, ICefXmlReader)
  protected
    function MoveToNextNode: Boolean;
    function Close: Boolean;
    function HasError: Boolean;
    function GetError: ustring;
    function GetType: TCefXmlNodeType;
    function GetDepth: Integer;
    function GetLocalName: ustring;
    function GetPrefix: ustring;
    function GetQualifiedName: ustring;
    function GetNamespaceUri: ustring;
    function GetBaseUri: ustring;
    function GetXmlLang: ustring;
    function IsEmptyElement: Boolean;
    function HasValue: Boolean;
    function GetValue: ustring;
    function HasAttributes: Boolean;
    function GetAttributeCount: NativeUInt;
    function GetAttributeByIndex(index: Integer): ustring;
    function GetAttributeByQName(const qualifiedName: ustring): ustring;
    function GetAttributeByLName(const localName, namespaceURI: ustring): ustring;
    function GetInnerXml: ustring;
    function GetOuterXml: ustring;
    function GetLineNumber: Integer;
    function MoveToAttributeByIndex(index: Integer): Boolean;
    function MoveToAttributeByQName(const qualifiedName: ustring): Boolean;
    function MoveToAttributeByLName(const localName, namespaceURI: ustring): Boolean;
    function MoveToFirstAttribute: Boolean;
    function MoveToNextAttribute: Boolean;
    function MoveToCarryingElement: Boolean;
  public
    class function UnWrap(data: Pointer): ICefXmlReader;
    class function New(const stream: ICefStreamReader; encodingType: TCefXmlEncodingType; const URI: ustring): ICefXmlReader;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

function TCefXmlReaderRef.Close: Boolean;
begin
  Result := PCefXmlReader(FData)^.close(PCefXmlReader(FData)) <> 0;
end;

class function TCefXmlReaderRef.New(const stream: ICefStreamReader; encodingType: TCefXmlEncodingType; const URI: ustring): ICefXmlReader;
var
  TempURI : TCefString;
begin
  TempURI := CefString(URI);
  Result := UnWrap(cef_xml_reader_create(CefGetData(stream), encodingType, @TempURI));
end;

function TCefXmlReaderRef.GetAttributeByIndex(index: Integer): ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData)^.get_attribute_byindex(PCefXmlReader(FData), index));
end;

function TCefXmlReaderRef.GetAttributeByLName(const localName, namespaceURI: ustring): ustring;
var
  TempLocalname, TempNamespaceURI : TCefString;
begin
  TempLocalname    := CefString(localName);
  TempNamespaceURI := CefString(namespaceURI);
  Result           := CefStringFreeAndGet(PCefXmlReader(FData)^.get_attribute_bylname(PCefXmlReader(FData),
                                                                                      @TempLocalname,
                                                                                      @TempNamespaceURI));
end;

function TCefXmlReaderRef.GetAttributeByQName(const qualifiedName: ustring): ustring;
var
  TempName : TCefString;
begin
  TempName := CefString(qualifiedName);
  Result   := CefStringFreeAndGet(PCefXmlReader(FData)^.get_attribute_byqname(PCefXmlReader(FData), @TempName));
end;

function TCefXmlReaderRef.GetAttributeCount: NativeUInt;
begin
  Result := PCefXmlReader(FData)^.get_attribute_count(PCefXmlReader(FData));
end;

function TCefXmlReaderRef.GetBaseUri: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData)^.get_base_uri(PCefXmlReader(FData)));
end;

function TCefXmlReaderRef.GetDepth: Integer;
begin
  Result := PCefXmlReader(FData)^.get_depth(PCefXmlReader(FData));
end;

function TCefXmlReaderRef.GetError: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData)^.get_error(PCefXmlReader(FData)));
end;

function TCefXmlReaderRef.GetInnerXml: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData)^.get_inner_xml(PCefXmlReader(FData)));
end;

function TCefXmlReaderRef.GetLineNumber: Integer;
begin
  Result := PCefXmlReader(FData)^.get_line_number(PCefXmlReader(FData));
end;

function TCefXmlReaderRef.GetLocalName: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData)^.get_local_name(PCefXmlReader(FData)));
end;

function TCefXmlReaderRef.GetNamespaceUri: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData)^.get_namespace_uri(PCefXmlReader(FData)));
end;

function TCefXmlReaderRef.GetOuterXml: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData)^.get_outer_xml(PCefXmlReader(FData)));
end;

function TCefXmlReaderRef.GetPrefix: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData)^.get_prefix(PCefXmlReader(FData)));
end;

function TCefXmlReaderRef.GetQualifiedName: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData)^.get_qualified_name(PCefXmlReader(FData)));
end;

function TCefXmlReaderRef.GetType: TCefXmlNodeType;
begin
  Result := PCefXmlReader(FData)^.get_type(PCefXmlReader(FData));
end;

function TCefXmlReaderRef.GetValue: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData)^.get_value(PCefXmlReader(FData)));
end;

function TCefXmlReaderRef.GetXmlLang: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData)^.get_xml_lang(PCefXmlReader(FData)));
end;

function TCefXmlReaderRef.HasAttributes: Boolean;
begin
  Result := PCefXmlReader(FData)^.has_attributes(PCefXmlReader(FData)) <> 0;
end;

function TCefXmlReaderRef.HasError: Boolean;
begin
  Result := PCefXmlReader(FData)^.has_error(PCefXmlReader(FData)) <> 0;
end;

function TCefXmlReaderRef.HasValue: Boolean;
begin
  Result := PCefXmlReader(FData)^.has_value(PCefXmlReader(FData)) <> 0;
end;

function TCefXmlReaderRef.IsEmptyElement: Boolean;
begin
  Result := PCefXmlReader(FData)^.is_empty_element(PCefXmlReader(FData)) <> 0;
end;

function TCefXmlReaderRef.MoveToAttributeByIndex(index: Integer): Boolean;
begin
  Result := PCefXmlReader(FData)^.move_to_attribute_byindex(PCefXmlReader(FData), index) <> 0;
end;

function TCefXmlReaderRef.MoveToAttributeByLName(const localName, namespaceURI: ustring): Boolean;
var
  TempLocalname, TempNamespaceURI : TCefString;
begin
  TempLocalname    := CefString(localName);
  TempNamespaceURI := CefString(namespaceURI);
  Result           := PCefXmlReader(FData)^.move_to_attribute_bylname(PCefXmlReader(FData),
                                                                      @TempLocalname,
                                                                      @TempNamespaceURI) <> 0;
end;

function TCefXmlReaderRef.MoveToAttributeByQName(const qualifiedName: ustring): Boolean;
var
  TempName : TCefString;
begin
  TempName := CefString(qualifiedName);
  Result   := PCefXmlReader(FData)^.move_to_attribute_byqname(PCefXmlReader(FData), @TempName) <> 0;
end;

function TCefXmlReaderRef.MoveToCarryingElement: Boolean;
begin
  Result := PCefXmlReader(FData)^.move_to_carrying_element(PCefXmlReader(FData)) <> 0;
end;

function TCefXmlReaderRef.MoveToFirstAttribute: Boolean;
begin
  Result := PCefXmlReader(FData)^.move_to_first_attribute(PCefXmlReader(FData)) <> 0;
end;

function TCefXmlReaderRef.MoveToNextAttribute: Boolean;
begin
  Result := PCefXmlReader(FData)^.move_to_next_attribute(PCefXmlReader(FData)) <> 0;
end;

function TCefXmlReaderRef.MoveToNextNode: Boolean;
begin
  Result := PCefXmlReader(FData)^.move_to_next_node(PCefXmlReader(FData)) <> 0;
end;

class function TCefXmlReaderRef.UnWrap(data: Pointer): ICefXmlReader;
begin
  if data <> nil then
    Result := Create(data) as ICefXmlReader
   else
    Result := nil;
end;

end.
