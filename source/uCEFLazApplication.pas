unit uCEFLazApplication;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  {$ELSE}
    Forms, LclProc, Classes, SysUtils,
  {$ENDIF}
  uCEFTypes, uCEFApplication;

type

  { TCefLazApplication }

  TCefLazApplication = class(TCefApplication)
    protected
      FContextInitializedHandlers: TMethodList;
      FContextInitializedDone: Boolean;

      procedure CallContextInitializedHandlers(Data: PtrInt);
    public
      constructor Create;
      destructor  Destroy; override;
      procedure Internal_OnContextInitialized; override; // In UI thread

      Procedure AddContextInitializedHandler(AHandler: TNotifyEvent);
      Procedure RemoveContextInitializedHandler(AHandler: TNotifyEvent);
  end;

implementation

{ TCefLazApplication }

procedure TCefLazApplication.Internal_OnContextInitialized;
begin
  inherited Internal_OnContextInitialized;
  Application.QueueAsyncCall(@CallContextInitializedHandlers, 0);
  //TThread.Queue(@CallContextInitializedHandlers);
end;

procedure TCefLazApplication.CallContextInitializedHandlers(Data: PtrInt);
begin
  FContextInitializedHandlers.CallNotifyEvents(Self);
  FContextInitializedDone := True;
end;

constructor TCefLazApplication.Create;
begin
  FContextInitializedHandlers := TMethodList.Create;
  inherited Create;
end;

destructor TCefLazApplication.Destroy;
begin
  inherited Destroy;
  FContextInitializedHandlers.Free;
end;

procedure TCefLazApplication.AddContextInitializedHandler(AHandler: TNotifyEvent);
begin
  FContextInitializedHandlers.Add(TMethod(AHandler));
  if FContextInitializedDone then
    AHandler(Self);
end;

procedure TCefLazApplication.RemoveContextInitializedHandler(
  AHandler: TNotifyEvent);
begin
  FContextInitializedHandlers.Remove(TMethod(AHandler));
end;

end.

