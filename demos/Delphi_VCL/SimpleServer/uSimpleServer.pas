unit uSimpleServer;

{$I ..\..\..\source\cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ExtCtrls, System.Math,
  Winapi.ShellAPI,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Spin, ExtCtrls, Math,
  ShellAPI,
  {$ENDIF}
  uCEFInterfaces, uCEFServerComponent, uCEFTypes, uCEFMiscFunctions, uCEFStringMultimap;

type
  TSimpleServerFrm = class(TForm)
    CEFServerComponent1: TCEFServerComponent;
    ButtonPnl: TPanel;
    ConnectionLogMem: TMemo;
    AddressLbl: TLabel;
    AddressEdt: TEdit;
    PortLbl: TLabel;
    PortEdt: TSpinEdit;
    BacklogLbl: TLabel;
    BacklogEdt: TSpinEdit;
    StartBtn: TButton;
    StopBtn: TButton;
    OpenBtn: TButton;

    procedure StartBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure AddressEdtChange(Sender: TObject);

    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);

    procedure CEFServerComponent1ServerCreated(Sender: TObject; const server: ICefServer);
    procedure CEFServerComponent1ServerDestroyed(Sender: TObject; const server: ICefServer);
    procedure CEFServerComponent1ClientConnected(Sender: TObject; const server: ICefServer; connection_id: Integer);
    procedure CEFServerComponent1ClientDisconnected(Sender: TObject; const server: ICefServer; connection_id: Integer);
    procedure CEFServerComponent1HttpRequest(Sender: TObject; const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest);
    procedure CEFServerComponent1WebSocketConnected(Sender: TObject; const server: ICefServer; connection_id: Integer);
    procedure CEFServerComponent1WebSocketMessage(Sender: TObject; const server: ICefServer; connection_id: Integer; const data: Pointer; data_size: NativeUInt);
    procedure CEFServerComponent1WebSocketRequest(Sender: TObject; const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest; const callback: ICefCallback);

  protected
    FClosing : boolean;

    function  BufferToString(const aBuffer : TBytes) : string;
    procedure ShowRequestInfo(const aRequest : ICefRequest);
    procedure ShowPostDataInfo(const aPostData : ICefPostData);
  public
    { Public declarations }
  end;

var
  SimpleServerFrm: TSimpleServerFrm;

implementation

{$R *.dfm}

// Server capacity is limited and is intended to handle only a small number of
// simultaneous connections (e.g. for communicating between applications on localhost).

// To test the HTTP server follow these steps :
// 1- Build and run this demo.
// 2- Click on the Start button.
// 3- Open your web browser and visit this address http://127.0.0.1:8099
// 4- You should see some connection details in the server log and a "Hellow world" text in your web browser.

// To test the websockets server follow these steps :
// 1- Build and run this demo.
// 2- Click on the "Start" button.
// 3- Open your web browser and visit this address https://www.websocket.org/echo.html
// 4- Type this in the "Location" field ws://127.0.0.1:8099
// 5- Click the "Connect" button.
// 6- Click the "Send" button.
// 7- You should see some connection details in the server log and the default text message "Rock it with HTML5 WebSocket"

procedure TSimpleServerFrm.AddressEdtChange(Sender: TObject);
begin
  if not(CEFServerComponent1.IsRunning) then
    StartBtn.Enabled := (length(trim(AddressEdt.Text)) > 0);
end;

procedure TSimpleServerFrm.StartBtnClick(Sender: TObject);
begin
  if (length(trim(AddressEdt.Text)) > 0) then
    CEFServerComponent1.CreateServer(AddressEdt.Text, PortEdt.Value, BacklogEdt.Value);
end;

procedure TSimpleServerFrm.StopBtnClick(Sender: TObject);
begin
  CEFServerComponent1.Shutdown;
end;

procedure TSimpleServerFrm.CEFServerComponent1ClientConnected(
  Sender: TObject; const server: ICefServer; connection_id: Integer);
begin
  ConnectionLogMem.Lines.Add('Client connected : ' + inttostr(connection_id));
end;

procedure TSimpleServerFrm.CEFServerComponent1ClientDisconnected(
  Sender: TObject; const server: ICefServer; connection_id: Integer);
begin
  ConnectionLogMem.Lines.Add('Client disconnected : ' + inttostr(connection_id));
end;

procedure TSimpleServerFrm.CEFServerComponent1HttpRequest(Sender: TObject;
  const server: ICefServer; connection_id: Integer;
  const client_address: ustring; const request: ICefRequest);
var
  TempData : AnsiString;
  TempParts : TUrlParts;
begin
  ConnectionLogMem.Lines.Add('---------------------------------------');
  ConnectionLogMem.Lines.Add('HTTP request received from connection ' + inttostr(connection_id));
  ConnectionLogMem.Lines.Add('Client address : ' + client_address);
  ShowRequestInfo(request);
  ConnectionLogMem.Lines.Add('---------------------------------------');

  if (request <> nil) and CefParseUrl(Request.URL, TempParts) then
    begin
      if (TempParts.path = '') or (TempParts.path = '/') then
        begin
          TempData := UTF8Encode('<html><body><img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAUAAAAFCAYAAACNbyblAAAAHElEQVQI12P4//8/w38GIAXDIBKE0DHxgljNBAAO9TXL0Y4OHwAAAABJRU5ErkJggg==" alt="Red dot" />&nbsp;HTML response from SimpleServer.</body></html>');
          CEFServerComponent1.SendHttp200response(connection_id, 'text/html; charset=utf-8', @TempData[1], length(TempData) * SizeOf(AnsiChar));
        end
       else
        CEFServerComponent1.SendHttp404response(connection_id);
    end
   else
    CEFServerComponent1.SendHttp404response(connection_id);
end;

procedure TSimpleServerFrm.ShowRequestInfo(const aRequest : ICefRequest);
var
  TempHeaderMap : ICefStringMultimap;
  i : NativeUInt;
begin
  if (aRequest = nil) then exit;

  ConnectionLogMem.Lines.Add('Request URL : ' + aRequest.URL);
  ConnectionLogMem.Lines.Add('Request Method : ' + aRequest.Method);

  if (length(aRequest.ReferrerUrl) > 0) then
    ConnectionLogMem.Lines.Add('Request Referrer : ' + aRequest.ReferrerUrl);

  TempHeaderMap := TCefStringMultimapOwn.Create;
  aRequest.GetHeaderMap(TempHeaderMap);
  i := 0;
  while (i < TempHeaderMap.Size) do
    begin
      ConnectionLogMem.Lines.Add('HTTP header : ' + TempHeaderMap.Key[i] + ':' + TempHeaderMap.Value[i]);
      inc(i);
    end;

  ShowPostDataInfo(aRequest.PostData);
end;

procedure TSimpleServerFrm.ShowPostDataInfo(const aPostData : ICefPostData);
var
  i : integer;
  TempLen : NativeUInt;
  TempBytes : TBytes;
  TempArray : TCefPostDataElementArray;
begin
  TempArray := nil;

  try
    try
      if (aPostData <> nil) and (aPostData.GetElementCount > 0) then
        begin
          ConnectionLogMem.Lines.Add('Post element count : ' + inttostr(aPostData.GetElementCount));
          aPostData.GetElements(aPostData.GetElementCount, TempArray);

          i := 0;
          while (i < length(TempArray)) do
            begin
              case TempArray[i].GetType of
                PDE_TYPE_EMPTY : ConnectionLogMem.Lines.Add('Post element ' + inttostr(i) + ' type : empty');
                PDE_TYPE_FILE  : ConnectionLogMem.Lines.Add('Post element ' + inttostr(i) + ' type : file. ' + TempArray[i].GetFile);
                PDE_TYPE_BYTES :
                  begin
                    ConnectionLogMem.Lines.Add('Post element ' + inttostr(i) + ' type : bytes');
                    if (TempArray[i].GetBytesCount > 0) then
                      begin
                        SetLength(TempBytes, TempArray[i].GetBytesCount);
                        TempLen := TempArray[i].GetBytes(TempArray[i].GetBytesCount, @TempBytes[0]);

                        if (TempLen > 0) then
                          begin
                            ConnectionLogMem.Lines.Add('Post element ' + inttostr(i) + ' length : ' + inttostr(TempLen));
                            ConnectionLogMem.Lines.Add('Post element ' + inttostr(i) + ' contents : ' + BufferToString(TempBytes));
                          end;
                      end;
                  end;

                else
                 ConnectionLogMem.Lines.Add('Post element ' + inttostr(i) + ' type : unknown');
              end;
              inc(i);
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TSimpleServerFrm.ShowPostDataInfo', e) then raise;
    end;
  finally
    if (TempArray <> nil) then
      begin
        i := 0;
        while (i < length(TempArray)) do
          begin
            TempArray[i] := nil;
            inc(i);
          end;
        Finalize(TempArray);
        TempArray := nil;
      end;
  end;
end;

function TSimpleServerFrm.BufferToString(const aBuffer : TBytes) : string;
var
  i, j : integer;
  TempStream : TStringStream;
begin
  Result := '';

  TempStream := TStringStream.Create(aBuffer);
  try
    Result := TempStream.DataString;
  finally
    TempStream.Free;
  end;

  if (length(Result) = 0) then
    begin
      i := 0;
      j := length(aBuffer);

      while (i < j) do
        begin
          Result := Result + IntToHex(aBuffer[i], 2);
          inc(i);
        end;
    end;
end;

procedure TSimpleServerFrm.OpenBtnClick(Sender: TObject);
var
  TempURL : string;
begin
  TempURL := 'http://' + AddressEdt.Text + ':' + inttostr(PortEdt.Value);
  ShellExecute(0, 'Open', PChar(TempURL), nil, nil, SW_SHOW);
end;

procedure TSimpleServerFrm.CEFServerComponent1ServerCreated(Sender: TObject; const server: ICefServer);
begin
  if CEFServerComponent1.Initialized then
    begin
      ConnectionLogMem.Lines.Add('Server created');
      StartBtn.Enabled   := False;
      StopBtn.Enabled    := not(StartBtn.Enabled);
      OpenBtn.Enabled    := not(StartBtn.Enabled);
      AddressEdt.Enabled := StartBtn.Enabled;
      PortEdt.Enabled    := StartBtn.Enabled;
      BacklogEdt.Enabled := StartBtn.Enabled;
    end
   else
    ConnectionLogMem.Lines.Add('Server creation error!');
end;

procedure TSimpleServerFrm.CEFServerComponent1ServerDestroyed(Sender: TObject; const server: ICefServer);
begin
  if FClosing then
    PostMessage(Handle, WM_CLOSE, 0, 0)
   else
    begin
      ConnectionLogMem.Lines.Add('Server destroyed');
      StartBtn.Enabled   := True;
      StopBtn.Enabled    := not(StartBtn.Enabled);
      OpenBtn.Enabled    := not(StartBtn.Enabled);
      AddressEdt.Enabled := StartBtn.Enabled;
      PortEdt.Enabled    := StartBtn.Enabled;
      BacklogEdt.Enabled := StartBtn.Enabled;
    end;
end;

procedure TSimpleServerFrm.CEFServerComponent1WebSocketConnected(
  Sender: TObject; const server: ICefServer; connection_id: Integer);
begin
  ConnectionLogMem.Lines.Add('Client connected : ' + inttostr(connection_id));
end;

procedure TSimpleServerFrm.CEFServerComponent1WebSocketMessage(
  Sender: TObject; const server: ICefServer; connection_id: Integer;
  const data: Pointer; data_size: NativeUInt);
var
  TempStream : TStringStream;
begin
  TempStream := nil;

  try
    if (data_size > 0) and (data <> nil) then
      begin
        TempStream := TStringStream.Create;
        TempStream.WriteBuffer(data^, data_size);
        ConnectionLogMem.Lines.Add('Client message received : ' + quotedstr(TempStream.DataString));
      end;
  finally
    if (TempStream <> nil) then FreeAndNil(TempStream);
  end;
end;

procedure TSimpleServerFrm.CEFServerComponent1WebSocketRequest(
  Sender: TObject; const server: ICefServer; connection_id: Integer;
  const client_address: ustring; const request: ICefRequest;
  const callback: ICefCallback);
begin
  if (callback <> nil) then callback.cont;
end;

procedure TSimpleServerFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CEFServerComponent1.Initialized then
    begin
      CanClose := False;
      FClosing := True;
      Visible  := False;
      CEFServerComponent1.Shutdown;
    end
   else
    CanClose := True;
end;

procedure TSimpleServerFrm.FormCreate(Sender: TObject);
begin
  FClosing := False;
end;

end.
