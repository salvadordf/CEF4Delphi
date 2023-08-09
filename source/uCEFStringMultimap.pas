unit uCEFStringMultimap;

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
  /// CEF string multimaps are a set of key/value string pairs.
  /// More than one value can be assigned to a single key.
  /// </summary>
  TCefCustomStringMultimap = class(TInterfacedObject, ICefStringMultimap)
    protected
      FHandle : TCefStringMultimap;

      function  GetHandle: TCefStringMultimap; virtual;
      /// <summary>
      /// Return the number of elements in the string multimap.
      /// </summary>
      function  GetSize: NativeUInt; virtual;
      /// <summary>
      /// Return the number of values with the specified key.
      /// </summary>
      function  FindCount(const Key: ustring): NativeUInt; virtual;
      /// <summary>
      /// Return the value_index-th value with the specified key.
      /// </summary>
      function  GetEnumerate(const Key: ustring; ValueIndex: NativeUInt): ustring; virtual;
      /// <summary>
      /// Return the key at the specified zero-based string multimap index.
      /// </summary>
      function  GetKey(Index: NativeUInt): ustring; virtual;
      /// <summary>
      /// Return the value at the specified zero-based string multimap index.
      /// </summary>
      function  GetValue(Index: NativeUInt): ustring; virtual;
      /// <summary>
      /// Append a new key/value pair at the end of the string multimap.
      /// </summary>
      function  Append(const Key, Value: ustring) : boolean; virtual;
      /// <summary>
      /// Clear the string multimap.
      /// </summary>
      procedure Clear; virtual;

    public
      constructor Create; virtual;
  end;

  TCefStringMultimapOwn = class(TCefCustomStringMultimap)
    public
      /// <summary>
      /// Allocate a new string multimap.
      /// </summary>
      constructor Create; override;
      /// <summary>
      /// Free the string multimap.
      /// </summary>
      destructor  Destroy; override;
  end;

  TCefStringMultimapRef = class(TCefCustomStringMultimap)
    public
      constructor Create(aHandle : TCefStringMultimap); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


// *********************************************
// ********* TCefCustomStringMultimap **********
// *********************************************


constructor TCefCustomStringMultimap.Create;
begin
  inherited Create;

  FHandle := nil;
end;

function TCefCustomStringMultimap.Append(const Key, Value: ustring) : boolean;
var
  TempKey, TempValue : TCefString;
begin
  if (FHandle <> nil) then
    begin
      TempKey   := CefString(key);
      TempValue := CefString(value);
      Result    := (cef_string_multimap_append(FHandle, @TempKey, @TempValue) <> 0);
    end
   else
    Result := False;
end;

procedure TCefCustomStringMultimap.Clear;
begin
  if (FHandle <> nil) then cef_string_multimap_clear(FHandle);
end;

function TCefCustomStringMultimap.FindCount(const Key: ustring): NativeUInt;
var
  TempKey : TCefString;
begin
  if (FHandle <> nil) then
    begin
      TempKey := CefString(Key);
      Result  := cef_string_multimap_find_count(FHandle, @TempKey);
    end
   else
    Result := 0;
end;

function TCefCustomStringMultimap.GetEnumerate(const Key: ustring; ValueIndex: NativeUInt): ustring;
var
  TempKey, TempValue : TCefString;
begin
  Result := '';

  if (FHandle <> nil) then
    begin
      CefStringInitialize(@TempValue);

      TempKey := CefString(Key);

      if (cef_string_multimap_enumerate(FHandle, @TempKey, ValueIndex, @TempValue) <> 0) then
        Result := CefStringClearAndGet(@TempValue);
    end;
end;

function TCefCustomStringMultimap.GetHandle: TCefStringMultimap;
begin
  Result := FHandle;
end;

function TCefCustomStringMultimap.GetKey(Index: NativeUInt): ustring;
var
  TempKey : TCefString;
begin
  Result := '';

  if (FHandle <> nil) then
    begin
      CefStringInitialize(@TempKey);

      if (cef_string_multimap_key(FHandle, index, @TempKey) <> 0) then
        Result := CefStringClearAndGet(@TempKey);
    end;
end;

function TCefCustomStringMultimap.GetSize: NativeUInt;
begin
  if (FHandle <> nil) then
    Result := cef_string_multimap_size(FHandle)
   else
    Result := 0;
end;

function TCefCustomStringMultimap.GetValue(Index: NativeUInt): ustring;
var
  TempValue : TCefString;
begin
  Result := '';

  if (FHandle <> nil) then
    begin
      CefStringInitialize(@TempValue);

      if (cef_string_multimap_value(FHandle, index, @TempValue) <> 0) then
        Result := CefStringClearAndGet(@TempValue);
    end;
end;


// ******************************************
// ********* TCefStringMultimapOwn **********
// ******************************************


constructor TCefStringMultimapOwn.Create;
begin
  inherited Create;

  FHandle := cef_string_multimap_alloc();
end;

destructor TCefStringMultimapOwn.Destroy;
begin
  if (FHandle <> nil) then cef_string_multimap_free(FHandle);

  inherited Destroy;
end;


// ******************************************
// ********* TCefStringMultimapRef **********
// ******************************************


constructor TCefStringMultimapRef.Create(aHandle : TCefStringMultimap);
begin
  inherited Create;

  FHandle := aHandle;
end;

end.
