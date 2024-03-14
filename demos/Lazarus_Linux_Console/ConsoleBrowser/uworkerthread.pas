unit uworkerthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Contnrs,
  ucustommessage;

const
  WORKERTHREADMSG_QUIT = 1;

type
  TWorkerThread = class(TThread)
    protected
      FCritSect        : TCriticalSection;
      FEvent           : TEvent;
      FWaiting         : boolean;
      FStop            : boolean;
      FMsgQueue        : TObjectQueue;

      function  Lock : boolean;
      procedure Unlock;
      function  CanContinue : boolean;
      procedure ReadAllPendingMessages;
      procedure ProcessValue(const aInfo : TMsgInfo); virtual;
      function  ReadPendingMessage(var aMsgInfo : TMsgInfo) : boolean;
      procedure StopThread;
      procedure DestroyQueue;                      
      procedure EnqueueMessage(const aMsgInfo : TMsgInfo); overload;  
      procedure EnqueueMessage(aMsg: integer; aIntParam : integer = 0; const aStrParam : string = ''); overload;

      procedure Execute; override;

    public
      constructor Create;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
  end;

implementation


constructor TWorkerThread.Create;
begin
  FCritSect        := nil;
  FWaiting         := False;
  FStop            := False;
  FEvent           := nil;
  FMsgQueue        := nil;

  inherited Create(True);

  FreeOnTerminate := False;
end;

destructor TWorkerThread.Destroy;
begin
  if (FEvent    <> nil) then FreeAndNil(FEvent);
  if (FCritSect <> nil) then FreeAndNil(FCritSect);

  DestroyQueue;

  inherited Destroy;
end;

procedure TWorkerThread.DestroyQueue;
begin
  if (FMsgQueue <> nil) then
    begin
      while (FMsgQueue.Count > 0) do
        FMsgQueue.Pop.Free;

      FreeAndNil(FMsgQueue);
    end;
end;

procedure TWorkerThread.AfterConstruction;
begin
  inherited AfterConstruction;

  FEvent    := TEvent.Create(nil, False, False, '');
  FCritSect := TCriticalSection.Create;
  FMsgQueue := TObjectQueue.Create;
end;

function TWorkerThread.Lock : boolean;
begin
  if (FCritSect <> nil) then
    begin
      FCritSect.Acquire;
      Result := True;
    end
   else
    Result := False;
end;

procedure TWorkerThread.Unlock;
begin
  if (FCritSect <> nil) then FCritSect.Release;
end;

procedure TWorkerThread.StopThread;
begin
  if Lock then
    begin
      FStop := True;
      Unlock;
    end;
end;         

procedure TWorkerThread.EnqueueMessage(aMsg, aIntParam : integer; const aStrParam : string);
var
  TempMsgInfo : TMsgInfo;
begin
  TempMsgInfo.Msg      := aMsg;
  TempMsgInfo.StrParam := aStrParam;
  TempMsgInfo.IntParam := aIntParam;
  EnqueueMessage(TempMsgInfo);
end;

procedure TWorkerThread.EnqueueMessage(const aMsgInfo : TMsgInfo);
begin
  if Lock then
    try
      if (FMsgQueue <> nil) then
        FMsgQueue.Push(TCustomMessage.Create(aMsgInfo));

      if FWaiting then
        begin
          FWaiting := False;
          FEvent.SetEvent;
        end;
    finally
      Unlock;
    end;
end;

function TWorkerThread.ReadPendingMessage(var aMsgInfo : TMsgInfo) : boolean;
var
  TempMessage : TCustomMessage;
begin
  Result := False;

  if Lock then
    try
      FWaiting := False;

      if (FMsgQueue <> nil) and (FMsgQueue.Count > 0) then
        begin
          TempMessage := TCustomMessage(FMsgQueue.Pop);
          aMsgInfo    := TempMessage.Value;
          Result      := True;
          TempMessage.Free;
        end;
    finally
      Unlock;
    end;
end;

procedure TWorkerThread.ReadAllPendingMessages;
var
  TempInfo : TMsgInfo;
begin
  while ReadPendingMessage(TempInfo) do
    case TempInfo.Msg of
      WORKERTHREADMSG_QUIT :
        begin
          StopThread;
          exit;
        end;

      else ProcessValue(TempInfo);
    end;
end;

procedure TWorkerThread.ProcessValue(const aInfo : TMsgInfo);
begin
  //
end;

function TWorkerThread.CanContinue : boolean;
begin
  Result := False;

  if Lock then
    try
      if not(Terminated) and not(FStop) then
        begin
          Result   := True;
          FWaiting := True;
          FEvent.ResetEvent;
        end;
    finally
      Unlock;
    end;
end;

procedure TWorkerThread.Execute;
begin
  while CanContinue do
    begin
      FEvent.WaitFor(INFINITE);
      ReadAllPendingMessages;
    end;
end;

end.

