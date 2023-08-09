unit uCEFBaseScopedWrapper;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

type
  TCEFBaseScopedWrapperRef = class
    protected
      FData: Pointer;

    public
      constructor Create(data: Pointer); virtual;
      function    Wrap: Pointer;
  end;

implementation

constructor TCEFBaseScopedWrapperRef.Create(data: Pointer);
begin
  inherited Create;

  FData := data;
end;

function TCEFBaseScopedWrapperRef.Wrap: Pointer;
begin
  Result := FData;
end;

end.
