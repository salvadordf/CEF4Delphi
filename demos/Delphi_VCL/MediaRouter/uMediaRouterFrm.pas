// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2020 Salvador Diaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit uMediaRouterFrm;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Menus,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Types, Vcl.ComCtrls, Vcl.ClipBrd,
  System.UITypes, Vcl.AppEvnts, Winapi.ActiveX, Winapi.ShlObj, System.SyncObjs,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Menus,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Types, ComCtrls, ClipBrd,
  AppEvnts, ActiveX, ShlObj, SyncObjs,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes, uCEFConstants,
  uCEFWinControl, uCEFSentinel, uCEFChromiumCore;

const
  MEDIA_ROUTER_PENDING_LOG_LINES    = WM_APP + $B00;
  MEDIA_ROUTER_REFRESH_SINKS        = WM_APP + $B01;
  MEDIA_ROUTER_REFRESH_ROUTES       = WM_APP + $B02;
  MEDIA_ROUTER_UPDATE_BUTTONS       = WM_APP + $B03;

type
  TMediaRouterFrm = class(TForm)
    Chromium1: TChromium;
    CEFWindowParent1: TCEFWindowParent;
    Timer1: TTimer;
    MainPnl: TPanel;
    SinksGbx: TGroupBox;
    SinksLbx: TListBox;
    SinksButtonsPnl: TPanel;
    CreateRouteBtn: TButton;
    CentralPnl: TPanel;
    SourcePnl: TPanel;
    SourceURNEdt: TEdit;
    SourceLblPnl: TPanel;
    SourceURNLbl: TLabel;
    RoutesGbx: TGroupBox;
    RoutesLbx: TListBox;
    RoutesButtonPnl: TPanel;
    TerminateRouteBtn: TButton;
    MessageGbx: TGroupBox;
    SendMessagePnl: TPanel;
    SendMsgBtn: TButton;
    MessageMem: TMemo;
    LogGbx: TGroupBox;
    LogMem: TMemo;
    SpacerPnl: TPanel;
    NotifySinksBtn: TButton;
    NotifyRoutesBtn: TButton;
    ClearLogPnl: TPanel;
    ClearLogBtn: TButton;

    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
    procedure Chromium1Sinks(Sender: TObject; const sinks: TCefMediaSinkArray);
    procedure Chromium1Routes(Sender: TObject; const routes: TCefMediaRouteArray);
    procedure Chromium1RouteStateChanged(Sender: TObject; const route: ICefMediaRoute; state: TCefMediaRouteConnectionState);
    procedure Chromium1RouteMessageReceived(Sender: TObject; const route: ICefMediaRoute; const message_: ustring);
    procedure Chromium1MediaRouteCreateFinished(Sender: TObject; result: Integer; const error: ustring; const route: ICefMediaRoute);

    procedure Timer1Timer(Sender: TObject);
    procedure SourceURNEdtChange(Sender: TObject);
    procedure SinksLbxClick(Sender: TObject);
    procedure CreateRouteBtnClick(Sender: TObject);
    procedure TerminateRouteBtnClick(Sender: TObject);
    procedure SendMsgBtnClick(Sender: TObject);
    procedure NotifySinksBtnClick(Sender: TObject);
    procedure NotifyRoutesBtnClick(Sender: TObject);
    procedure ClearLogBtnClick(Sender: TObject);

  protected
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    FMediaCS  : TCriticalSection;
    FLog      : TStringList;
    FSinks    : TCefMediaSinkInfoArray;
    FRoutes   : TCefMediaRouteInfoArray;

    procedure BrowserDestroyMsg(var aMessage : TMessage); message CEF_DESTROY;
    procedure PendingLogLinesMsg(var aMessage : TMessage); message MEDIA_ROUTER_PENDING_LOG_LINES;
    procedure RefreshSinksMsg(var aMessage : TMessage); message MEDIA_ROUTER_REFRESH_SINKS;
    procedure RefreshRoutesMsg(var aMessage : TMessage); message MEDIA_ROUTER_REFRESH_ROUTES;
    procedure UpdateButtonsMsg(var aMessage : TMessage); message MEDIA_ROUTER_UPDATE_BUTTONS;

    procedure DestroySinksArray;
    procedure DestroyRoutesArray;
    procedure DestroyAllArrays;
    procedure CopySinksArray(const aSinks : TCefMediaSinkArray);
    procedure CopyRoutesArray(const aRoutes : TCefMediaRouteArray);
    procedure UpdateAvailableSinks;
    procedure UpdateAvailableRoutes;
    procedure UpdateButtons;
    procedure AddLogEntry(const aMessage1, aMessage2 : string; aRec : boolean); overload;
    procedure AddLogEntry(const aMessage1 : string; const aMessage2 : string = ''); overload;
  end;

var
  MediaRouterFrm: TMediaRouterFrm;

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}

uses
  {$IFDEF DELPHI16_UP}
  System.Math,
  {$ELSE}
  Math,
  {$ENDIF}
  uCEFMiscFunctions;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp             := TCefApplication.Create;
  GlobalCEFApp.LogFile     := 'debug.log';
  GlobalCEFApp.LogSeverity := LOGSEVERITY_INFO; //LOGSEVERITY_VERBOSE;
  GlobalCEFApp.FrameworkDirPath     := 'c:\cef';
  GlobalCEFApp.ResourcesDirPath     := 'c:\cef';
  GlobalCEFApp.LocalesDirPath       := 'c:\cef\locales';
end;

procedure TMediaRouterFrm.Chromium1AfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  AddLogEntry('Browser initialized.');
end;

procedure TMediaRouterFrm.Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TMediaRouterFrm.Chromium1Close(Sender: TObject;
  const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
begin
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  aAction := cbaDelay;
end;

procedure TMediaRouterFrm.Chromium1MediaRouteCreateFinished(Sender: TObject;
  result: Integer; const error: ustring; const route: ICefMediaRoute);
var
  TempMsg, TempID : string;
begin
  TempMsg := '';

  try
    FMediaCS.Acquire;

    if (result = CEF_MRCR_OK) then
      begin
        TempMsg := 'Route created';
        if (route <> nil) then TempID := route.ID;
      end
     else
      TempMsg := error;
  finally
    PostMessage(Handle, MEDIA_ROUTER_UPDATE_BUTTONS, 0, 0);
    FMediaCS.Release;
    if (length(TempMsg) > 0) then AddLogEntry(TempID, TempMsg);
  end;
end;

procedure TMediaRouterFrm.Chromium1RouteMessageReceived(Sender: TObject;
  const route: ICefMediaRoute; const message_: ustring);
var
  TempID : string;
begin
  if (route <> nil) then
    TempID := route.ID;

  AddLogEntry(TempID, message_, True);
end;

procedure TMediaRouterFrm.Chromium1Routes(Sender: TObject;
  const routes: TCefMediaRouteArray);
begin
  CopyRoutesArray(routes);
end;

procedure TMediaRouterFrm.Chromium1RouteStateChanged(Sender: TObject;
  const route: ICefMediaRoute; state: TCefMediaRouteConnectionState);
var
  TempMsg, TempID : string;
begin
  if (route <> nil) then TempID := route.ID;

  case state of
    CEF_MRCS_CONNECTING : TempMsg := 'State : Connecting.';
    CEF_MRCS_CONNECTED  : TempMsg := 'State : Connected.';
    CEF_MRCS_CLOSED     : TempMsg := 'State : Closed.';
    CEF_MRCS_TERMINATED : TempMsg := 'State : Terminated.';
    else                  TempMsg := 'State : Unknown.';
  end;

  TempMsg := TempMsg + ' ' + dateTimeToStr(now);

  AddLogEntry(TempID, TempMsg);
end;

procedure TMediaRouterFrm.Chromium1Sinks(Sender: TObject;
  const sinks: TCefMediaSinkArray);
begin
  CopySinksArray(sinks);
end;

procedure TMediaRouterFrm.ClearLogBtnClick(Sender: TObject);
begin
  LogMem.Lines.Clear;
end;

procedure TMediaRouterFrm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      DestroyAllArrays;
      Chromium1.CloseBrowser(True);
    end;
end;

procedure TMediaRouterFrm.FormCreate(Sender: TObject);
begin
  FCanClose := False;
  FClosing  := False;
  FSinks    := nil;
  FRoutes   := nil;
  FMediaCS  := TCriticalSection.Create;
  FLog      := TStringList.Create;
end;

procedure TMediaRouterFrm.FormDestroy(Sender: TObject);
begin
  FLog.Free;
  FMediaCS.Free;
end;

procedure TMediaRouterFrm.FormShow(Sender: TObject);
begin
  UpdateButtons;

  if not(Chromium1.CreateBrowser(CEFWindowParent1)) then
    Timer1.Enabled := True;
end;

procedure TMediaRouterFrm.NotifyRoutesBtnClick(Sender: TObject);
begin
  Chromium1.NotifyCurrentRoutes;
end;

procedure TMediaRouterFrm.NotifySinksBtnClick(Sender: TObject);
begin
  Chromium1.NotifyCurrentSinks;
end;

procedure TMediaRouterFrm.SendMsgBtnClick(Sender: TObject);
var
  TempMsg, TempID : string;
begin
  TempMsg := '';

  try
    FMediaCS.Acquire;

    if (RoutesLbx.Items.Count > 0) and
       (FRoutes <> nil) and
       (RoutesLbx.ItemIndex >= 0) and
       (RoutesLbx.ItemIndex < length(FRoutes)) and
       (MessageMem.Lines.Count > 0) then
      begin
        TempMsg := trim(MessageMem.Lines.Text);

        if (length(TempMsg) > 0) and
           (FRoutes[RoutesLbx.ItemIndex].RouteIntf <> nil) then
          try
            TempID := FRoutes[RoutesLbx.ItemIndex].RouteIntf.ID;
            FRoutes[RoutesLbx.ItemIndex].RouteIntf.SendRouteMessage(TempMsg);
          except
            on e : exception do
              if CustomExceptionHandler('TMediaRouterFrm.SendMsgBtnClick', e) then raise;
          end;
      end;
  finally
    FMediaCS.Release;
    if (length(TempMsg) > 0) then AddLogEntry(TempID, TempMsg, False);
  end;
end;

procedure TMediaRouterFrm.SinksLbxClick(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TMediaRouterFrm.SourceURNEdtChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TMediaRouterFrm.TerminateRouteBtnClick(Sender: TObject);
begin
  try
    FMediaCS.Acquire;

    if (RoutesLbx.Items.Count > 0) and
       (FRoutes <> nil) and
       (RoutesLbx.ItemIndex >= 0) and
       (RoutesLbx.ItemIndex < length(FRoutes)) and
       (FRoutes[RoutesLbx.ItemIndex].RouteIntf <> nil) then
      try
        FRoutes[RoutesLbx.ItemIndex].RouteIntf.Terminate;
      except
        on e : exception do
          if CustomExceptionHandler('TMediaRouterFrm.TerminateRouteBtnClick', e) then raise;
      end;
  finally
    FMediaCS.Release;
    UpdateButtons;
  end;
end;

procedure TMediaRouterFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TMediaRouterFrm.BrowserDestroyMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.Free;
end;

procedure TMediaRouterFrm.PendingLogLinesMsg(var aMessage : TMessage);
begin
  if FClosing then exit;

  try
    FMediaCS.Acquire;

    if (FLog <> nil) and (FLog.Count > 0) then
      begin
        LogMem.Lines.AddStrings(FLog);
        FLog.Clear;
      end;
  finally
    FMediaCS.Release;
  end;
end;

procedure TMediaRouterFrm.RefreshSinksMsg(var aMessage : TMessage);
begin
  if FClosing then exit;

  UpdateAvailableSinks;
  UpdateButtons;
end;

procedure TMediaRouterFrm.RefreshRoutesMsg(var aMessage : TMessage);
begin
  if FClosing then exit;

  UpdateAvailableRoutes;
  UpdateButtons;
end;

procedure TMediaRouterFrm.UpdateButtonsMsg(var aMessage : TMessage);
begin
  if FClosing then exit;

  UpdateButtons;
end;

procedure TMediaRouterFrm.DestroyAllArrays;
begin
  try
    FMediaCS.Acquire;
    DestroyRoutesArray;
    DestroySinksArray;
  finally
    FMediaCS.Release;
  end;
end;

procedure TMediaRouterFrm.DestroySinksArray;
var
  i : integer;
begin
  if (FSinks <> nil) then
    begin
      i := pred(length(FSinks));
      while (i >= 0) do
        begin
          FSinks[i].SinkIntf := nil;
          dec(i);
        end;

      Finalize(FSinks);
      FSinks := nil;
    end;
end;

procedure TMediaRouterFrm.DestroyRoutesArray;
var
  i : integer;
begin
  if (FRoutes <> nil) then
    begin
      i := pred(length(FRoutes));
      while (i >= 0) do
        begin
          FRoutes[i].RouteIntf := nil;
          dec(i);
        end;

      Finalize(FRoutes);
      FRoutes := nil;
    end;
end;

procedure TMediaRouterFrm.CopySinksArray(const aSinks : TCefMediaSinkArray);
var
  i, TempLen : integer;
begin
  try
    FMediaCS.Acquire;

    FLog.Add('Sinks available : ' + inttostr(length(aSinks)));
    FLog.Add('------------------------------------------');

    DestroySinksArray;

    if (aSinks <> nil) then
      begin
        TempLen := length(aSinks);
        SetLength(FSinks, TempLen);

        i := 0;
        while (i < TempLen) do
          begin
            FSinks[i].ID          := aSinks[i].ID;
            FSinks[i].Name        := aSinks[i].Name;
            FSinks[i].Description := aSinks[i].Description;
            FSinks[i].Valid       := aSinks[i].IsValid;
            FSinks[i].SinkIntf    := aSinks[i];

            if aSinks[i].IsCastSink then
              FSinks[i].SinkType := mtCast
             else
              if aSinks[i].IsDialSink then
                FSinks[i].SinkType := mtDial
               else
                FSinks[i].SinkType := mtUnknown;

            inc(i);
          end;
      end;
  finally
    PostMessage(Handle, MEDIA_ROUTER_REFRESH_SINKS, 0, 0);
    FMediaCS.Release;
  end;
end;

procedure TMediaRouterFrm.CopyRoutesArray(const aRoutes : TCefMediaRouteArray);
var
  i, TempLen : integer;
begin
  try
    FMediaCS.Acquire;

    FLog.Add('Routes available : ' + inttostr(length(aRoutes)));
    FLog.Add('------------------------------------------');

    DestroyRoutesArray;

    if (aRoutes <> nil) then
      begin
        TempLen := length(aRoutes);
        SetLength(FRoutes, TempLen);

        i := 0;
        while (i < TempLen) do
          begin
            FRoutes[i].ID        := aRoutes[i].ID;
            FRoutes[i].SourceID  := aRoutes[i].GetSource.ID;
            FRoutes[i].SinkID    := aRoutes[i].GetSink.ID;
            FRoutes[i].RouteIntf := aRoutes[i];
            inc(i);
          end;
      end;
  finally
    PostMessage(Handle, MEDIA_ROUTER_REFRESH_ROUTES, 0, 0);
    FMediaCS.Release;
  end;
end;

procedure TMediaRouterFrm.CreateRouteBtnClick(Sender: TObject);
var
  TempURN, TempErrorMsg : string;
  TempSource : ICefMediaSource;
begin
  TempURN := trim(SourceURNEdt.Text);

  if (length(TempURN) = 0) then
    begin
      AddLogEntry('Invalid URN');
      exit;
    end;

  TempErrorMsg := '';

  try
    try
      FMediaCS.Acquire;

      if (FSinks <> nil) and
         (SinksLbx.Items.Count > 0) and
         (SinksLbx.ItemIndex >= 0) and
         (SinksLbx.ItemIndex < length(FSinks)) then
        begin
          TempSource := Chromium1.GetSource(TempURN);

          if (TempSource <> nil) and TempSource.IsValid then
            begin
              if (FSinks[SinksLbx.ItemIndex].SinkIntf <> nil) and
                 FSinks[SinksLbx.ItemIndex].SinkIntf.IsValid then
                begin
                  if FSinks[SinksLbx.ItemIndex].SinkIntf.IsCompatibleWith(TempSource) then
                    Chromium1.CreateRoute(TempSource, FSinks[SinksLbx.ItemIndex].SinkIntf)
                   else
                    TempErrorMsg := 'The selected Sink is not compatible with the Media Source.';
                end
               else
                TempErrorMsg := 'The selected Sink is not valid.';
            end
           else
            TempErrorMsg := 'The Media Source is not valid.';
        end
       else
        TempErrorMsg := 'The sinks list is outdated.';
    except
      on e : exception do
        begin
          TempErrorMsg := e.Message;
          if CustomExceptionHandler('TMediaRouterFrm.CreateRouteBtnClick', e) then raise;
        end;
    end;
  finally
    FMediaCS.Release;
    if (length(TempErrorMsg) > 0) then AddLogEntry(TempErrorMsg);
  end;
end;

procedure TMediaRouterFrm.UpdateAvailableSinks;
var
  i : integer;
begin
  try
    FMediaCS.Acquire;
    SinksLbx.Items.Clear;

    if (FSinks <> nil) then
      begin
        i := 0;
        while (i < length(FSinks)) do
          begin
            case FSinks[i].SinkType of
              mtCast : SinksLbx.Items.Add(FSinks[i].Name + ' (CAST)');
              mtDial : SinksLbx.Items.Add(FSinks[i].Name + ' (DIAL)');
              else     SinksLbx.Items.Add(FSinks[i].Name + ' (UNKNOWN)');
            end;

            inc(i);
          end;
      end;
  finally
    FMediaCS.Release;
  end;
end;

procedure TMediaRouterFrm.UpdateAvailableRoutes;
var
  i : integer;
begin
  try
    FMediaCS.Acquire;
    RoutesLbx.Items.Clear;

    if (FRoutes <> nil) then
      begin
        i := 0;
        while (i < length(FRoutes)) do
          begin
            RoutesLbx.Items.Add('ID : ' + quotedstr(FRoutes[i].ID));
            inc(i);
          end;
      end;
  finally
    FMediaCS.Release;
  end;
end;

procedure TMediaRouterFrm.UpdateButtons;
begin
  TerminateRouteBtn.Enabled := (RoutesLbx.ItemIndex >= 0) and
                               (RoutesLbx.Items.Count > 0);

  SendMsgBtn.Enabled := TerminateRouteBtn.Enabled and
                        (length(trim(MessageMem.Lines.Text)) > 0);

  CreateRouteBtn.Enabled := not(TerminateRouteBtn.Enabled) and
                            (length(trim(SourceURNEdt.Text)) > 0) and
                            (SinksLbx.ItemIndex >= 0) and
                            (SinksLbx.Items.Count > 0);
end;

procedure TMediaRouterFrm.AddLogEntry(const aMessage1, aMessage2 : string; aRec : boolean);
begin
  try
    FMediaCS.Acquire;

    if aRec then
      FLog.Add('Message received ' + dateTimeToStr(now))
     else
      FLog.Add('Message sent ' + dateTimeToStr(now));

    FLog.Add(aMessage1);
    if (length(aMessage2) > 0) then FLog.Add(aMessage2);
    FLog.Add('------------------------------------------');
  finally
    PostMessage(Handle, MEDIA_ROUTER_PENDING_LOG_LINES, 0, 0);
    FMediaCS.Release;
  end;
end;

procedure TMediaRouterFrm.AddLogEntry(const aMessage1, aMessage2 : string);
begin
  try
    FMediaCS.Acquire;
    FLog.Add(aMessage1);
    if (length(aMessage2) > 0) then FLog.Add(aMessage2);
    FLog.Add('------------------------------------------');
  finally
    PostMessage(Handle, MEDIA_ROUTER_PENDING_LOG_LINES, 0, 0);
    FMediaCS.Release;
  end;
end;

end.
