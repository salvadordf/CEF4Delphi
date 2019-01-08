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
//        Copyright © 2019 Salvador Diaz Fau. All rights reserved.
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

unit uCEFPostData;

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
  TCefPostDataRef = class(TCefBaseRefCountedRef, ICefPostData)
    protected
      function  IsReadOnly: Boolean;
      function  HasExcludedElements: Boolean;
      function  GetCount: NativeUInt;
      function  GetElements(Count: NativeUInt): IInterfaceList; // ICefPostDataElement
      function  RemoveElement(const element: ICefPostDataElement): Integer;
      function  AddElement(const element: ICefPostDataElement): Integer;
      procedure RemoveElements;

    public
      class function UnWrap(data: Pointer): ICefPostData;
      class function New: ICefPostData;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFPostDataElement;


function TCefPostDataRef.IsReadOnly: Boolean;
begin
  Result := PCefPostData(FData)^.is_read_only(PCefPostData(FData)) <> 0;
end;

function TCefPostDataRef.HasExcludedElements: Boolean;
begin
  Result := PCefPostData(FData)^.has_excluded_elements(PCefPostData(FData)) <> 0;
end;

function TCefPostDataRef.AddElement(const element: ICefPostDataElement): Integer;
begin
  Result := PCefPostData(FData)^.add_element(PCefPostData(FData), CefGetData(element));
end;

function TCefPostDataRef.GetCount: NativeUInt;
begin
  Result := PCefPostData(FData)^.get_element_count(PCefPostData(FData))
end;

function TCefPostDataRef.GetElements(Count: NativeUInt): IInterfaceList;
var
  items : PCefPostDataElementArray;
  i     : NativeUInt;
begin
  Result := nil;
  items  := nil;

  try
    try
      GetMem(items, SizeOf(PCefPostDataElement) * Count);
      FillChar(items^, SizeOf(PCefPostDataElement) * Count, 0);

      PCefPostData(FData)^.get_elements(PCefPostData(FData), @Count, items);

      Result := TInterfaceList.Create;
      i      := 0;

      while (i < Count) do
        begin
          Result.Add(TCefPostDataElementRef.UnWrap(items^[i]));
          inc(i);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCefPostDataRef.GetElements', e) then raise;
    end;
  finally
    if (items <> nil) then FreeMem(items);
  end;
end;

class function TCefPostDataRef.New: ICefPostData;
begin
  Result := UnWrap(cef_post_data_create());
end;

function TCefPostDataRef.RemoveElement(const element: ICefPostDataElement): Integer;
begin
  Result := PCefPostData(FData)^.remove_element(PCefPostData(FData), CefGetData(element));
end;

procedure TCefPostDataRef.RemoveElements;
begin
  PCefPostData(FData)^.remove_elements(PCefPostData(FData));
end;

class function TCefPostDataRef.UnWrap(data: Pointer): ICefPostData;
begin
  if (data <> nil) then
    Result := Create(data) as ICefPostData
   else
    Result := nil;
end;

end.
