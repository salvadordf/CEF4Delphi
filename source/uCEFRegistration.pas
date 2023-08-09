unit uCEFRegistration;

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
  TCefRegistrationRef = class(TCefBaseRefCountedRef, ICefRegistration)
  public
    class function UnWrap(data: Pointer): ICefRegistration;
  end;

implementation

class function TCefRegistrationRef.UnWrap(data: Pointer): ICefRegistration;
begin
  if (data <> nil) then
    Result := Create(data) as ICefRegistration
   else
    Result := nil;
end;

end.
