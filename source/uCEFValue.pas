unit uCEFValue;

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
  TCefValueRef = class(TCefBaseRefCountedRef, ICefValue)
    protected
      function IsValid: Boolean;
      function IsOwned: Boolean;
      function IsReadOnly: Boolean;
      function IsSame(const that: ICefValue): Boolean;
      function IsEqual(const that: ICefValue): Boolean;
      function Copy: ICefValue;
      function GetType: TCefValueType;
      function GetBool: Boolean;
      function GetInt: Integer;
      function GetDouble: Double;
      function GetString: ustring;
      function GetBinary: ICefBinaryValue;
      function GetDictionary: ICefDictionaryValue;
      function GetList: ICefListValue;
      function SetNull: Boolean;
      function SetBool(value: boolean): Boolean;
      function SetInt(value: Integer): Boolean;
      function SetDouble(value: Double): Boolean;
      function SetString(const value: ustring): Boolean;
      function SetBinary(const value: ICefBinaryValue): Boolean;
      function SetDictionary(const value: ICefDictionaryValue): Boolean;
      function SetList(const value: ICefListValue): Boolean;

    public
      class function UnWrap(data: Pointer): ICefValue;
      class function New: ICefValue;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBinaryValue, uCEFListValue, uCEFDictionaryValue;

function TCefValueRef.Copy: ICefValue;
begin
  Result := UnWrap(PCefValue(FData)^.copy(PCefValue(FData)));
end;

function TCefValueRef.GetBinary: ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefValue(FData)^.get_binary(PCefValue(FData)));
end;

function TCefValueRef.GetBool: Boolean;
begin
  Result := PCefValue(FData)^.get_bool(PCefValue(FData)) <> 0;
end;

function TCefValueRef.GetDictionary: ICefDictionaryValue;
begin
  Result := TCefDictionaryValueRef.UnWrap(PCefValue(FData)^.get_dictionary(PCefValue(FData)));
end;

function TCefValueRef.GetDouble: Double;
begin
  Result := PCefValue(FData)^.get_double(PCefValue(FData));
end;

function TCefValueRef.GetInt: Integer;
begin
  Result := PCefValue(FData)^.get_int(PCefValue(FData));
end;

function TCefValueRef.GetList: ICefListValue;
begin
  Result := TCefListValueRef.UnWrap(PCefValue(FData)^.get_list(PCefValue(FData)));
end;

function TCefValueRef.GetString: ustring;
begin
  Result := CefStringFreeAndGet(PCefValue(FData)^.get_string(PCefValue(FData)));
end;

function TCefValueRef.GetType: TCefValueType;
begin
  Result := PCefValue(FData)^.get_type(PCefValue(FData));
end;

function TCefValueRef.IsEqual(const that: ICefValue): Boolean;
begin
  Result := PCefValue(FData)^.is_equal(PCefValue(FData), CefGetData(that)) <> 0;
end;

function TCefValueRef.IsOwned: Boolean;
begin
  Result := PCefValue(FData)^.is_owned(PCefValue(FData)) <> 0;
end;

function TCefValueRef.IsReadOnly: Boolean;
begin
  Result := PCefValue(FData)^.is_read_only(PCefValue(FData)) <> 0;
end;

function TCefValueRef.IsSame(const that: ICefValue): Boolean;
begin
  Result := PCefValue(FData)^.is_same(PCefValue(FData), CefGetData(that)) <> 0;
end;

function TCefValueRef.IsValid: Boolean;
begin
  Result := PCefValue(FData)^.is_valid(PCefValue(FData)) <> 0;
end;

class function TCefValueRef.New: ICefValue;
begin
  Result := UnWrap(cef_value_create());
end;

function TCefValueRef.SetBinary(const value: ICefBinaryValue): Boolean;
begin
  Result := PCefValue(FData)^.set_binary(PCefValue(FData), CefGetData(value)) <> 0;
end;

function TCefValueRef.SetBool(value: boolean): Boolean;
begin
  Result := PCefValue(FData)^.set_bool(PCefValue(FData), ord(value)) <> 0;
end;

function TCefValueRef.SetDictionary(const value: ICefDictionaryValue): Boolean;
begin
  Result := PCefValue(FData)^.set_dictionary(PCefValue(FData), CefGetData(value)) <> 0;
end;

function TCefValueRef.SetDouble(value: Double): Boolean;
begin
  Result := PCefValue(FData)^.set_double(PCefValue(FData), value) <> 0;
end;

function TCefValueRef.SetInt(value: Integer): Boolean;
begin
  Result := PCefValue(FData)^.set_int(PCefValue(FData), value) <> 0;
end;

function TCefValueRef.SetList(const value: ICefListValue): Boolean;
begin
  Result := PCefValue(FData)^.set_list(PCefValue(FData), CefGetData(value)) <> 0;
end;

function TCefValueRef.SetNull: Boolean;
begin
  Result := PCefValue(FData)^.set_null(PCefValue(FData)) <> 0;
end;

function TCefValueRef.SetString(const value: ustring): Boolean;
var
  TempValue : TCefString;
begin
  TempValue := CefString(value);
  Result    := PCefValue(FData)^.set_string(PCefValue(FData), @TempValue) <> 0;
end;

class function TCefValueRef.UnWrap(data: Pointer): ICefValue;
begin
  if (data <> nil) then
    Result := Create(data) as ICefValue
   else
    Result := nil;
end;

end.
