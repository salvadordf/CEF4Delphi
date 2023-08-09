unit uCEFProcessMessage;

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
  TCefProcessMessageRef = class(TCefBaseRefCountedRef, ICefProcessMessage)
    protected
      function IsValid: Boolean;
      function IsReadOnly: Boolean;
      function Copy: ICefProcessMessage;
      function GetName: ustring;
      function GetArgumentList: ICefListValue;
      function GetSharedMemoryRegion: ICefSharedMemoryRegion;

    public
      class function UnWrap(data: Pointer): ICefProcessMessage;
      class function New(const name: ustring): ICefProcessMessage;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFListValue, uCEFSharedMemoryRegion;

function TCefProcessMessageRef.Copy: ICefProcessMessage;
begin
  Result := UnWrap(PCefProcessMessage(FData)^.copy(PCefProcessMessage(FData)));
end;

function TCefProcessMessageRef.GetArgumentList: ICefListValue;
begin
  Result := TCefListValueRef.UnWrap(PCefProcessMessage(FData)^.get_argument_list(PCefProcessMessage(FData)));
end;

function TCefProcessMessageRef.GetSharedMemoryRegion: ICefSharedMemoryRegion;
begin
  Result := TCefSharedMemoryRegionRef.UnWrap(PCefProcessMessage(FData)^.get_shared_memory_region(PCefProcessMessage(FData)));
end;

function TCefProcessMessageRef.GetName: ustring;
begin
  Result := CefStringFreeAndGet(PCefProcessMessage(FData)^.get_name(PCefProcessMessage(FData)));
end;

function TCefProcessMessageRef.IsReadOnly: Boolean;
begin
  Result := PCefProcessMessage(FData)^.is_read_only(PCefProcessMessage(FData)) <> 0;
end;

function TCefProcessMessageRef.IsValid: Boolean;
begin
  Result := PCefProcessMessage(FData)^.is_valid(PCefProcessMessage(FData)) <> 0;
end;

class function TCefProcessMessageRef.New(const name: ustring): ICefProcessMessage;
var
  TempString : TCefString;
begin
  TempString := CefString(name);
  Result     := UnWrap(cef_process_message_create(@TempString));
end;

class function TCefProcessMessageRef.UnWrap(data: Pointer): ICefProcessMessage;
begin
  if (data <> nil) then
    Result := Create(data) as ICefProcessMessage
   else
    Result := nil;
end;

end.
