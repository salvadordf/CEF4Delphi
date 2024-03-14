unit ucustommessage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TMsgInfo = record
    Msg      : integer;
    StrParam : string;
    IntParam : integer;
  end;

  TCustomMessage = class
    protected
      FValue : TMsgInfo;

    public
      constructor Create(const aValue : TMsgInfo);

      property Value : TMsgInfo read FValue;
  end;

implementation

constructor TCustomMessage.Create(const aValue : TMsgInfo);
begin
  inherited Create;

  FValue := aValue;
end;

end.

