unit uCEFStringMap;

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
  /// <summary>
  /// CEF string maps are a set of key/value string pairs.
  /// </summary>
  TCefCustomStringMap = class(TInterfacedObject, ICefStringMap)
    protected
      FHandle : TCefStringMap;

      function  GetHandle: TCefStringMap; virtual;
      /// <summary>
      /// Return the number of elements in the string map.
      /// </summary>
      function  GetSize: NativeUInt; virtual;
      /// <summary>
      /// Return the value assigned to the specified key.
      /// </summary>
      function  Find(const key: ustring): ustring; virtual;
      /// <summary>
      /// Return the key at the specified zero-based string map index.
      /// </summary>
      function  GetKey(index: NativeUInt): ustring; virtual;
      /// <summary>
      /// Return the value at the specified zero-based string map index.
      /// </summary>
      function  GetValue(index: NativeUInt): ustring; virtual;
      /// <summary>
      /// Append a new key/value pair at the end of the string map. If the key exists,
      /// overwrite the existing value with a new value w/o changing the pair order.
      /// </summary>
      function  Append(const key, value: ustring) : boolean; virtual;
      /// <summary>
      /// Clear the string map.
      /// </summary>
      procedure Clear; virtual;

    public
      constructor Create; virtual;
  end;

  TCefStringMapOwn = class(TCefCustomStringMap)
    public
      /// <summary>
      /// Allocate a new string map.
      /// </summary>
      constructor Create; override;
      /// <summary>
      /// Free the string map.
      /// </summary>
      destructor  Destroy; override;
  end;

  TCefStringMapRef = class(TCefCustomStringMap)
    public
      constructor Create(aHandle : TCefStringMap); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


// ****************************************
// ********* TCefCustomStringMap **********
// ****************************************


constructor TCefCustomStringMap.Create;
begin
  inherited Create;

  FHandle := nil;
end;

function TCefCustomStringMap.Append(const key, value: ustring) : boolean;
var
  TempKey, TempValue : TCefString;
begin
  if (FHandle <> nil) then
    begin
      TempKey   := CefString(key);
      TempValue := CefString(value);
      Result    := cef_string_map_append(FHandle, @TempKey, @TempValue) <> 0;
    end
   else
    Result := False;
end;

procedure TCefCustomStringMap.Clear;
begin
  if (FHandle <> nil) then cef_string_map_clear(FHandle);
end;

function TCefCustomStringMap.Find(const key: ustring): ustring;
var
  TempKey, TempValue : TCefString;
begin
  Result := '';

  if (FHandle <> nil) then
    begin
      CefStringInitialize(@TempValue);

      TempKey := CefString(key);

      if (cef_string_map_find(FHandle, @TempKey, @TempValue) <> 0) then
        Result := CefStringClearAndGet(@TempValue);
    end;
end;

function TCefCustomStringMap.GetHandle: TCefStringMap;
begin
  Result := FHandle;
end;

function TCefCustomStringMap.GetKey(index: NativeUInt): ustring;
var
  TempKey : TCefString;
begin
  Result := '';

  if (FHandle <> nil) then
    begin
      CefStringInitialize(@TempKey);

      if (cef_string_map_key(FHandle, index, @TempKey) <> 0) then
        Result := CefStringClearAndGet(@TempKey);
    end;
end;

function TCefCustomStringMap.GetSize: NativeUInt;
begin
  if (FHandle <> nil) then
    Result := cef_string_map_size(FHandle)
   else
    Result := 0;
end;

function TCefCustomStringMap.GetValue(index: NativeUInt): ustring;
var
  TempValue : TCefString;
begin
  Result := '';

  if (FHandle <> nil) then
    begin
      CefStringInitialize(@TempValue);

      if (cef_string_map_value(FHandle, index, @TempValue) <> 0) then
        Result := CefStringClearAndGet(@TempValue);
    end;
end;


// **************************************
// ********* TCefStringMapOwn ***********
// **************************************


constructor TCefStringMapOwn.Create;
begin
  inherited Create;

  FHandle := cef_string_map_alloc();
end;

destructor TCefStringMapOwn.Destroy;
begin
  if (FHandle <> nil) then cef_string_map_free(FHandle);

  inherited Destroy;
end;


// **************************************
// ********* TCefStringMapRef ***********
// **************************************


constructor TCefStringMapRef.Create(aHandle : TCefStringMap);
begin
  inherited Create;

  FHandle := aHandle;
end;

end.
