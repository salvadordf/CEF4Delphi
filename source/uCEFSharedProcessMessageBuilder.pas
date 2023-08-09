unit uCEFSharedProcessMessageBuilder;

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
  TCefSharedProcessMessageBuilderRef = class(TCefBaseRefCountedRef, ICefSharedProcessMessageBuilder)
    protected
      function IsValid: boolean;
      function Size: NativeUInt;
      function Memory: pointer;
      function Build: ICefProcessMessage;
    public
      class function UnWrap(data: Pointer): ICefSharedProcessMessageBuilder;
      class function CreateBuilder(const name: ustring; byte_size: NativeUInt): ICefSharedProcessMessageBuilder;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFProcessMessage;

class function TCefSharedProcessMessageBuilderRef.CreateBuilder(const name: ustring; byte_size: NativeUInt): ICefSharedProcessMessageBuilder;
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  Result   := UnWrap(cef_shared_process_message_builder_create(@TempName, byte_size));
end;

class function TCefSharedProcessMessageBuilderRef.UnWrap(data: Pointer): ICefSharedProcessMessageBuilder;
begin
  if (data <> nil) then
    Result := Create(data) as ICefSharedProcessMessageBuilder
   else
    Result := nil;
end;

function TCefSharedProcessMessageBuilderRef.IsValid: boolean;
begin
  Result := PCefSharedProcessMessageBuilder(FData)^.is_valid(PCefSharedProcessMessageBuilder(FData)) <> 0;
end;

function TCefSharedProcessMessageBuilderRef.Size: NativeUInt;
begin
  Result := PCefSharedProcessMessageBuilder(FData)^.size(PCefSharedProcessMessageBuilder(FData));
end;

function TCefSharedProcessMessageBuilderRef.Memory: pointer;
begin
  Result := PCefSharedProcessMessageBuilder(FData)^.memory(PCefSharedProcessMessageBuilder(FData));
end;

function TCefSharedProcessMessageBuilderRef.Build: ICefProcessMessage;
begin
  Result := TCefProcessMessageRef.UnWrap(PCefSharedProcessMessageBuilder(FData)^.build(PCefSharedProcessMessageBuilder(FData)));
end;

end.
