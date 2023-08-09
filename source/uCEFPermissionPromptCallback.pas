unit uCEFPermissionPromptCallback;

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
  TCefPermissionPromptCallbackRef = class(TCefBaseRefCountedRef, ICefPermissionPromptCallback)
    protected
      procedure cont(result: TCefPermissionRequestResult);

    public
      class function UnWrap(data: Pointer): ICefPermissionPromptCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefPermissionPromptCallbackRef.cont(result: TCefPermissionRequestResult);
begin
  PCefPermissionPromptCallback(FData)^.cont(PCefPermissionPromptCallback(FData), result);
end;

class function TCefPermissionPromptCallbackRef.UnWrap(data: Pointer): ICefPermissionPromptCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefPermissionPromptCallback
   else
    Result := nil;
end;

end.
