unit uCEFBinaryValue;

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
  TCefBinaryValueRef = class(TCefBaseRefCountedRef, ICefBinaryValue)
    protected
      function IsValid: Boolean;
      function IsOwned: Boolean;
      function IsSame(const that: ICefBinaryValue): Boolean;
      function IsEqual(const that: ICefBinaryValue): Boolean;
      function Copy: ICefBinaryValue;
      function GetRawData: Pointer;
      function GetSize: NativeUInt;
      function GetData(buffer: Pointer; bufferSize, dataOffset: NativeUInt): NativeUInt;

    public
      class function UnWrap(data: Pointer): ICefBinaryValue;
      class function New(const data: Pointer; dataSize: NativeUInt): ICefBinaryValue;
  end;

  TCefBinaryValueOwn = class(TCefBaseRefCountedOwn, ICefBinaryValue)
    protected
      function IsValid: Boolean;
      function IsOwned: Boolean;
      function IsSame(const that: ICefBinaryValue): Boolean;
      function IsEqual(const that: ICefBinaryValue): Boolean;
      function Copy: ICefBinaryValue;
      function GetRawData: Pointer;
      function GetSize: NativeUInt;
      function GetData(buffer: Pointer; bufferSize, dataOffset: NativeUInt): NativeUInt;

    public
      constructor Create;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


// **********************************************
// **********  TCefBinaryValueRef  **************
// **********************************************


function TCefBinaryValueRef.Copy: ICefBinaryValue;
begin
  Result := UnWrap(PCefBinaryValue(FData)^.copy(PCefBinaryValue(FData)));
end;

function TCefBinaryValueRef.GetRawData: Pointer;
begin
  Result := PCefBinaryValue(FData)^.get_raw_data(PCefBinaryValue(FData));
end;

function TCefBinaryValueRef.GetData(buffer: Pointer; bufferSize, dataOffset: NativeUInt): NativeUInt;
begin
  Result := PCefBinaryValue(FData)^.get_data(PCefBinaryValue(FData), buffer, bufferSize, dataOffset);
end;

function TCefBinaryValueRef.GetSize: NativeUInt;
begin
  Result := PCefBinaryValue(FData)^.get_size(PCefBinaryValue(FData));
end;

function TCefBinaryValueRef.IsEqual(const that: ICefBinaryValue): Boolean;
begin
  Result := PCefBinaryValue(FData)^.is_equal(PCefBinaryValue(FData), CefGetData(that)) <> 0;
end;

function TCefBinaryValueRef.IsOwned: Boolean;
begin
  Result := PCefBinaryValue(FData)^.is_owned(PCefBinaryValue(FData)) <> 0;
end;

function TCefBinaryValueRef.IsSame(const that: ICefBinaryValue): Boolean;
begin
  Result := PCefBinaryValue(FData)^.is_same(PCefBinaryValue(FData), CefGetData(that)) <> 0;
end;

function TCefBinaryValueRef.IsValid: Boolean;
begin
  Result := PCefBinaryValue(FData)^.is_valid(PCefBinaryValue(FData)) <> 0;
end;

class function TCefBinaryValueRef.New(const data: Pointer; dataSize: NativeUInt): ICefBinaryValue;
begin
  Result := UnWrap(cef_binary_value_create(data, dataSize));
end;

class function TCefBinaryValueRef.UnWrap(data: Pointer): ICefBinaryValue;
begin
  if (data <> nil) then
    Result := Create(data) as ICefBinaryValue
   else
    Result := nil;
end;


// **********************************************
// **********  TCefBinaryValueOwn  **************
// **********************************************

function cef_binary_value_is_valid(self: PCefBinaryValue): Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(False);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBinaryValueOwn) then
    Result := Ord(TCefBinaryValueOwn(TempObject).IsValid);
end;

function cef_binary_value_is_owned(self: PCefBinaryValue): Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(False);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBinaryValueOwn) then
    Result := Ord(TCefBinaryValueOwn(TempObject).IsOwned);
end;

function cef_binary_value_is_same(self, that: PCefBinaryValue):Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(False);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBinaryValueOwn) then
    Result := Ord(TCefBinaryValueOwn(TempObject).IsSame(TCefBinaryValueRef.UnWrap(that)));
end;

function cef_binary_value_is_equal(self, that: PCefBinaryValue): Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(False);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBinaryValueOwn) then
    Result := Ord(TCefBinaryValueOwn(TempObject).IsEqual(TCefBinaryValueRef.UnWrap(that)));
end;

function cef_binary_value_copy(self: PCefBinaryValue): PCefBinaryValue; stdcall;
var
  TempObject  : TObject;
begin
  Result      := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBinaryValueOwn) then
    Result := CefGetData(TCefBinaryValueOwn(TempObject).Copy);
end;

function cef_binary_value_get_raw_data(self: PCefBinaryValue): Pointer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBinaryValueOwn) then
    Result := TCefBinaryValueOwn(TempObject).GetRawData;
end;

function cef_binary_value_get_size(self: PCefBinaryValue): NativeUInt; stdcall;
var
  TempObject  : TObject;
begin
  Result      := 0;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBinaryValueOwn) then
    Result := TCefBinaryValueOwn(TempObject).GetSize;
end;

function cef_binary_value_get_data(self: PCefBinaryValue; buffer: Pointer; buffer_size, data_offset: NativeUInt): NativeUInt; stdcall;
var
  TempObject  : TObject;
begin
  Result      := 0;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBinaryValueOwn) then
    Result := TCefBinaryValueOwn(TempObject).GetData(buffer, buffer_size, data_offset);
end;

constructor TCefBinaryValueOwn.Create;
begin
  inherited CreateData(SizeOf(TCefBinaryValue));

  with PCefBinaryValue(FData)^ do
    begin
      is_valid     := {$IFDEF FPC}@{$ENDIF}cef_binary_value_is_valid;
      is_owned     := {$IFDEF FPC}@{$ENDIF}cef_binary_value_is_owned;
      is_same      := {$IFDEF FPC}@{$ENDIF}cef_binary_value_is_same;
      is_equal     := {$IFDEF FPC}@{$ENDIF}cef_binary_value_is_equal;
      copy         := {$IFDEF FPC}@{$ENDIF}cef_binary_value_copy;
      get_raw_data := {$IFDEF FPC}@{$ENDIF}cef_binary_value_get_raw_data;
      get_size     := {$IFDEF FPC}@{$ENDIF}cef_binary_value_get_size;
      get_data     := {$IFDEF FPC}@{$ENDIF}cef_binary_value_get_data;
    end;
end;

function TCefBinaryValueOwn.IsValid: Boolean;
begin
  Result := False;
end;

function TCefBinaryValueOwn.IsOwned: Boolean;
begin
  Result := False;
end;

function TCefBinaryValueOwn.IsSame(const that: ICefBinaryValue): Boolean;
begin
  Result := False;
end;

function TCefBinaryValueOwn.IsEqual(const that: ICefBinaryValue): Boolean;
begin
  Result := False;
end;

function TCefBinaryValueOwn.Copy: ICefBinaryValue;
begin
  Result := nil;
end;

function TCefBinaryValueOwn.GetRawData: Pointer;
begin
  Result := nil;
end;

function TCefBinaryValueOwn.GetSize: NativeUInt;
begin
  Result := 0;
end;

function TCefBinaryValueOwn.GetData(buffer: Pointer; bufferSize, dataOffset: NativeUInt): NativeUInt;
begin
  Result := 0;
end;


end.
