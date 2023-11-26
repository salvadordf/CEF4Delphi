unit uFMXApplicationService;

{$I ..\..\..\source\cef.inc}

// This unit is based in the TFMXApplicationService class created by Takashi Yamamoto
// https://www.gesource.jp/weblog/?p=7367

interface

uses
  FMX.Platform;

type
  TFMXApplicationService = class(TInterfacedObject, IFMXApplicationService)
    protected
      class var OldFMXApplicationService: IFMXApplicationService;
      class var NewFMXApplicationService: IFMXApplicationService;

    public
      procedure Run;
      function  HandleMessage: Boolean;
      procedure WaitMessage;
      function  GetDefaultTitle: string;
      function  GetTitle: string;
      procedure SetTitle(const Value: string);
      function  GetVersionString: string;
      procedure Terminate;
      function  Terminating: Boolean;
      function  Running: Boolean;

      class procedure AddPlatformService;

      property  DefaultTitle  : string read GetDefaultTitle;
      property  Title         : string read GetTitle          write SetTitle;
      property  AppVersion    : string read GetVersionString;
  end;

implementation

uses
  FMX.Forms, {$IFDEF MSWINDOWS}Winapi.Messages, Winapi.Windows,{$ENDIF}
  uFMXVirtualUIBrowser, uCEFFMXWorkScheduler, uCEFApplication, uCEFConstants;

class procedure TFMXApplicationService.AddPlatformService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationService, IInterface(OldFMXApplicationService)) then
    begin
      TPlatformServices.Current.RemovePlatformService(IFMXApplicationService);

      NewFMXApplicationService := TFMXApplicationService.Create;
      TPlatformServices.Current.AddPlatformService(IFMXApplicationService, NewFMXApplicationService);
    end;
end;

function TFMXApplicationService.GetDefaultTitle: string;
begin
  Result := OldFMXApplicationService.GetDefaultTitle;
end;

function TFMXApplicationService.GetTitle: string;
begin
  Result := OldFMXApplicationService.GetTitle;
end;

function TFMXApplicationService.GetVersionString: string;
begin
  {$IFDEF DELPHI22_UP}
  Result := OldFMXApplicationService.GetVersionString;
  {$ELSE DELPHI22_UP}
  Result := 'unsupported yet';
  {$ENDIF DELPHI22_UP}
end;

procedure TFMXApplicationService.Run;
begin
  OldFMXApplicationService.Run;
end;

procedure TFMXApplicationService.SetTitle(const Value: string);
begin
  OldFMXApplicationService.SetTitle(Value);
end;

procedure TFMXApplicationService.Terminate;
begin
  OldFMXApplicationService.Terminate;
end;

function TFMXApplicationService.Terminating: Boolean;
begin
  Result := OldFMXApplicationService.Terminating;
end;

procedure TFMXApplicationService.WaitMessage;
begin
  OldFMXApplicationService.WaitMessage;
end;

function TFMXApplicationService.Running: Boolean;
begin
  {$IFDEF DELPHI24_UP}
  Result := OldFMXApplicationService.Running;
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

function TFMXApplicationService.HandleMessage: Boolean;
{$IFDEF MSWINDOWS}
var
  TempMsg : TMsg;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if PeekMessage(TempMsg, 0, 0, 0, PM_NOREMOVE) then
    case TempMsg.Message of
      WM_MOVE,
      WM_MOVING :
        if not(Application.Terminated) and
           (Application.MainForm <> nil) and
           (Application.MainForm is TMainForm) then
          TMainForm(Application.MainForm).NotifyMoveOrResizeStarted;

      WM_ENTERMENULOOP :
        if (TempMsg.wParam = 0) and
           (GlobalCEFApp <> nil) then
          GlobalCEFApp.OsmodalLoop := True;

      WM_EXITMENULOOP :
        if (TempMsg.wParam = 0) and
           (GlobalCEFApp <> nil) then
          GlobalCEFApp.OsmodalLoop := False;

      WM_CAPTURECHANGED,
      WM_CANCELMODE :
        if not(Application.Terminated) and
           (Application.MainForm <> nil) and
           (Application.MainForm is TMainForm) then
          TMainForm(Application.MainForm).SendCaptureLostEvent;

      WM_SYSCHAR :
        if not(Application.Terminated) and
           (Application.MainForm <> nil) and
           (Application.MainForm is TMainForm) then
          TMainForm(Application.MainForm).HandleSYSCHAR(TempMsg);

      WM_SYSKEYDOWN :
        if not(Application.Terminated) and
           (Application.MainForm <> nil) and
           (Application.MainForm is TMainForm) then
          TMainForm(Application.MainForm).HandleSYSKEYDOWN(TempMsg);

      WM_SYSKEYUP :
        if not(Application.Terminated) and
           (Application.MainForm <> nil) and
           (Application.MainForm is TMainForm) then
          TMainForm(Application.MainForm).HandleSYSKEYUP(TempMsg);

      WM_KEYDOWN :
        if not(Application.Terminated) and
           (Application.MainForm <> nil) and
           (Application.MainForm is TMainForm) then
          TMainForm(Application.MainForm).HandleKEYDOWN(TempMsg);

      WM_KEYUP :
        if not(Application.Terminated) and
           (Application.MainForm <> nil) and
           (Application.MainForm is TMainForm) then
          TMainForm(Application.MainForm).HandleKEYUP(TempMsg);

      WM_POINTERDOWN,
      WM_POINTERUPDATE,
      WM_POINTERUP :
        if not(Application.Terminated) and
           (Application.MainForm <> nil) and
           (Application.MainForm is TMainForm) then
          TMainForm(Application.MainForm).HandlePOINTER(TempMsg);

      CEF_PENDINGRESIZE :
        if not(Application.Terminated) and
           (Application.MainForm <> nil) and
           (Application.MainForm is TMainForm) then
          TMainForm(Application.MainForm).DoResize;
    end;
  {$ENDIF}

  Result := OldFMXApplicationService.HandleMessage;
end;

end.
