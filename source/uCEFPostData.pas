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
      function  GetElementCount: NativeUInt;
      procedure GetElements(elementsCount: NativeUInt; var elements: TCefPostDataElementArray);
      function  RemoveElement(const element: ICefPostDataElement): Boolean;
      function  AddElement(const element: ICefPostDataElement): Boolean;
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

function TCefPostDataRef.AddElement(const element: ICefPostDataElement): Boolean;
begin
  Result := PCefPostData(FData)^.add_element(PCefPostData(FData), CefGetData(element)) <> 0;
end;

function TCefPostDataRef.GetElementCount: NativeUInt;
begin
  Result := PCefPostData(FData)^.get_element_count(PCefPostData(FData))
end;

procedure TCefPostDataRef.GetElements(elementsCount: NativeUInt; var elements: TCefPostDataElementArray);
var
  TempArray : array of PCefPostDataElement;
  i : NativeUInt;
begin
  TempArray := nil;

  try
    try
      if (elementsCount > 0) then
        begin
          SetLength(TempArray, elementsCount);

          i := 0;
          while (i < elementsCount) do
            begin
              TempArray[i] := nil;
              inc(i);
            end;

          PCefPostData(FData)^.get_elements(PCefPostData(FData), elementsCount, TempArray[0]);

          if (elementsCount > 0) then
            begin
              SetLength(elements, elementsCount);

              i := 0;
              while (i < elementsCount) do
                begin
                  elements[i] := TCefPostDataElementRef.UnWrap(TempArray[i]);
                  inc(i);
                end;
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCefPostDataRef.GetElements', e) then raise;
    end;
  finally
    if (TempArray <> nil) then
      begin
        Finalize(TempArray);
        TempArray := nil;
      end;
  end;
end;

class function TCefPostDataRef.New: ICefPostData;
begin
  Result := UnWrap(cef_post_data_create());
end;

function TCefPostDataRef.RemoveElement(const element: ICefPostDataElement): Boolean;
begin
  Result := PCefPostData(FData)^.remove_element(PCefPostData(FData), CefGetData(element)) <> 0;
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
