unit uCEFStringList;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  /// <summary>
  /// CEF string maps are a set of key/value string pairs.
  /// </summary>
  TCefCustomStringList = class(TInterfacedObject, ICefStringList)
    protected
      FHandle : TCefStringList;

      function  GetHandle: TCefStringList; virtual;
      /// <summary>
      /// Return the number of elements in the string list.
      /// </summary>
      function  GetSize: NativeUInt; virtual;
      /// <summary>
      /// Retrieve the value at the specified zero-based string list index. Returns
      /// true (1) if the value was successfully retrieved.
      /// </summary>
      function  GetValue(index: NativeUInt): ustring; virtual;
      /// <summary>
      /// Append a new value at the end of the string list.
      /// </summary>
      procedure Append(const value: ustring); virtual;
      /// <summary>
      /// Clear the string list.
      /// </summary>
      procedure Clear; virtual;
      /// <summary>
      /// Creates a copy of an existing string list.
      /// </summary>
      function  Copy : TCefStringList; virtual;
      procedure CopyToStrings(const aStrings : TStrings); virtual;
      procedure AddStrings(const aStrings : TStrings); virtual;

    public
      constructor Create; virtual;
  end;

  TCefStringListOwn = class(TCefCustomStringList)
    public
      /// <summary>
      /// Allocate a new string map.
      /// </summary>
      constructor Create; override;
      /// <summary>
      /// Free the string list.
      /// </summary>
      destructor  Destroy; override;
  end;

  TCefStringListRef = class(TCefCustomStringList)
    public
      constructor Create(aHandle : TCefStringList); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


// *****************************************
// ********* TCefCustomStringList **********
// *****************************************


constructor TCefCustomStringList.Create;
begin
  inherited Create;

  FHandle := nil;
end;

procedure TCefCustomStringList.Append(const value: ustring);
var
  TempValue : TCefString;
begin
  if (FHandle <> nil) then
    begin
      TempValue := CefString(value);
      cef_string_list_append(FHandle, @TempValue);
    end;
end;

procedure TCefCustomStringList.Clear;
begin
  if (FHandle <> nil) then cef_string_list_clear(FHandle);
end;

function TCefCustomStringList.GetHandle: TCefStringList;
begin
  Result := FHandle;
end;

function TCefCustomStringList.GetSize: NativeUInt;
begin
  if (FHandle <> nil) then
    Result := cef_string_list_size(FHandle)
   else
    Result := 0;
end;

function TCefCustomStringList.Copy : TCefStringList;
begin
  if (FHandle <> nil) then
    Result := cef_string_list_copy(FHandle)
   else
    Result := nil;
end;

function TCefCustomStringList.GetValue(index: NativeUInt): ustring;
var
  TempValue : TCefString;
begin
  Result := '';

  if (FHandle <> nil) then
    begin
      CefStringInitialize(@TempValue);

      if (cef_string_list_value(FHandle, index, @TempValue) <> 0) then
        Result := CefStringClearAndGet(@TempValue);
    end;
end;

procedure TCefCustomStringList.CopyToStrings(const aStrings : TStrings);
var
  i, j : NativeUInt;
  TempValue : TCefString;
begin
  if (aStrings <> nil) and (FHandle <> nil) then
    begin
      i := 0;
      j := GetSize;

      while (i < j) do
        begin
          CefStringInitialize(@TempValue);

          if (cef_string_list_value(FHandle, i, @TempValue) <> 0) then
            aStrings.Add(CefStringClearAndGet(@TempValue));

          inc(i);
        end;
    end;
end;

procedure TCefCustomStringList.AddStrings(const aStrings : TStrings);
var
  i : integer;
begin
  if (FHandle <> nil) and (aStrings <> nil) and (aStrings.Count > 0) then
    for i := 0 to aStrings.Count - 1 do Append(aStrings[i]);
end;



// *****************************************
// *********** TCefStringListOwn ***********
// *****************************************


constructor TCefStringListOwn.Create;
begin
  inherited Create;

  FHandle := cef_string_list_alloc();
end;

destructor TCefStringListOwn.Destroy;
begin
  if (FHandle <> nil) then cef_string_list_free(FHandle);

  inherited Destroy;
end;


// *****************************************
// *********** TCefStringListRef ***********
// *****************************************


constructor TCefStringListRef.Create(aHandle : TCefStringList);
begin
  inherited Create;

  FHandle := aHandle;
end;

end.
