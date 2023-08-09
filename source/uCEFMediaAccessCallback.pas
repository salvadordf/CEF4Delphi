unit uCEFMediaAccessCallback;

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
  TCefMediaAccessCallbackRef = class(TCefBaseRefCountedRef, ICefMediaAccessCallback)
    protected
      procedure cont(allowed_permissions: TCefMediaAccessPermissionTypes);
      procedure cancel;

    public
      class function UnWrap(data: Pointer): ICefMediaAccessCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefMediaAccessCallbackRef.cont(allowed_permissions: TCefMediaAccessPermissionTypes);
begin
  PCefMediaAccessCallback(FData)^.cont(PCefMediaAccessCallback(FData), allowed_permissions);
end;

procedure TCefMediaAccessCallbackRef.cancel;
begin
  PCefMediaAccessCallback(FData)^.cancel(PCefMediaAccessCallback(FData));
end;

class function TCefMediaAccessCallbackRef.UnWrap(data: Pointer): ICefMediaAccessCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefMediaAccessCallback
   else
    Result := nil;
end;

end.
