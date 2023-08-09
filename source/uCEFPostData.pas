unit uCEFPostData;

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
