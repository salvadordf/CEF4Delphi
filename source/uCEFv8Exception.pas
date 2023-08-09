unit uCEFv8Exception;

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
  TCefV8ExceptionRef = class(TCefBaseRefCountedRef, ICefV8Exception)
    protected
      function GetMessage: ustring;
      function GetSourceLine: ustring;
      function GetScriptResourceName: ustring;
      function GetLineNumber: Integer;
      function GetStartPosition: Integer;
      function GetEndPosition: Integer;
      function GetStartColumn: Integer;
      function GetEndColumn: Integer;

    public
      class function UnWrap(data: Pointer): ICefV8Exception;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


function TCefV8ExceptionRef.GetEndColumn: Integer;
begin
  Result := PCefV8Exception(FData)^.get_end_column(PCefV8Exception(FData));
end;

function TCefV8ExceptionRef.GetEndPosition: Integer;
begin
  Result := PCefV8Exception(FData)^.get_end_position(PCefV8Exception(FData));
end;

function TCefV8ExceptionRef.GetLineNumber: Integer;
begin
  Result := PCefV8Exception(FData)^.get_line_number(PCefV8Exception(FData));
end;

function TCefV8ExceptionRef.GetMessage: ustring;
begin
  Result := CefStringFreeAndGet(PCefV8Exception(FData)^.get_message(PCefV8Exception(FData)));
end;

function TCefV8ExceptionRef.GetScriptResourceName: ustring;
begin
  Result := CefStringFreeAndGet(PCefV8Exception(FData)^.get_script_resource_name(PCefV8Exception(FData)));
end;

function TCefV8ExceptionRef.GetSourceLine: ustring;
begin
  Result := CefStringFreeAndGet(PCefV8Exception(FData)^.get_source_line(PCefV8Exception(FData)));
end;

function TCefV8ExceptionRef.GetStartColumn: Integer;
begin
  Result := PCefV8Exception(FData)^.get_start_column(PCefV8Exception(FData));
end;

function TCefV8ExceptionRef.GetStartPosition: Integer;
begin
  Result := PCefV8Exception(FData)^.get_start_position(PCefV8Exception(FData));
end;

class function TCefV8ExceptionRef.UnWrap(data: Pointer): ICefV8Exception;
begin
  if (data <> nil) then
    Result := Create(data) as ICefV8Exception
   else
    Result := nil;
end;

end.
