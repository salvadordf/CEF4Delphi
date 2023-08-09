unit uCEFMediaRoute;

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
  TCefMediaRouteRef = class(TCefBaseRefCountedRef, ICefMediaRoute)
  protected
    function  GetId: ustring;
    function  GetSource: ICefMediaSource;
    function  GetSink: ICefMediaSink;
    procedure SendRouteMessage(const message_: ustring);
    procedure Terminate;
  public
    class function UnWrap(data: Pointer): ICefMediaRoute;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFMediaSource, uCEFMediaSink;

function TCefMediaRouteRef.GetId: ustring;
begin
  Result := CefStringFreeAndGet(PCefMediaRoute(FData)^.get_id(PCefMediaRoute(FData)));
end;

function TCefMediaRouteRef.GetSource: ICefMediaSource;
begin
  Result := TCefMediaSourceRef.UnWrap(PCefMediaRoute(FData)^.get_source(PCefMediaRoute(FData)));
end;

function TCefMediaRouteRef.GetSink: ICefMediaSink;
begin
  Result := TCefMediaSinkRef.UnWrap(PCefMediaRoute(FData)^.get_sink(PCefMediaRoute(FData)));
end;

procedure TCefMediaRouteRef.SendRouteMessage(const message_: ustring);
var
  TempMsgPtr : pointer;
  TempMsg    : AnsiString;
  TempSize   : NativeUInt;
begin
  TempSize := length(message_);

  if (TempSize > 0) then
    begin
      TempMsg    := Utf8Encode(message_);
      TempMsgPtr := @TempMsg[1];
    end
   else
    TempMsgPtr := nil;

  PCefMediaRoute(FData)^.send_route_message(PCefMediaRoute(FData),
                                            TempMsgPtr,
                                            TempSize);
end;

procedure TCefMediaRouteRef.Terminate;
begin
  PCefMediaRoute(FData)^.terminate(PCefMediaRoute(FData));
end;

class function TCefMediaRouteRef.UnWrap(data: Pointer): ICefMediaRoute;
begin
  if (data <> nil) then
    Result := Create(data) as ICefMediaRoute
   else
    Result := nil;
end;

end.
