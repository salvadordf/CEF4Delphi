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
//        Copyright © 2021 Salvador Diaz Fau. All rights reserved.
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

unit uOAuth2TesterFrm;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, WinApi.ShellApi,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ShellApi,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes,
  uCEFWinControl, uCEFSentinel, uCEFChromiumCore, uCEFServerComponent,
  uCEFUrlRequestClientComponent, uCEFRequest, uCEFUrlRequest, uCEFOAuth2Helper;


const
  URLREQUEST_SUCCESS            = WM_APP + $101;
  URLREQUEST_ERROR              = WM_APP + $102;
  AUTHCODE_ERROR                = WM_APP + $103;

type
  TOAuthTesterStatus = (tsIdle, tsLogin, tsRefresh, tsRequest);

  TOAuth2TesterFrm = class(TForm)
    CEFServerComponent1: TCEFServerComponent;
    CEFUrlRequestClientComponent1: TCEFUrlRequestClientComponent;
    LoginGrp: TGroupBox;
    ClientIDEdt: TEdit;
    ClientIDLbl: TLabel;
    ClientSecretLbl: TLabel;
    ClientSecretEdt: TEdit;
    ScopeLbl: TLabel;
    ScopeEdt: TEdit;
    AccessTokenLbl: TLabel;
    AccessTokenEdt: TEdit;
    RefreshTokenLbl: TLabel;
    RefreshTokenEdt: TEdit;
    LoginBtn: TButton;
    RefreshBtn: TButton;
    LogGrp: TGroupBox;
    LogMem: TMemo;
    ApiPnl: TPanel;
    ApiGrp: TGroupBox;
    EndpointLbl: TLabel;
    EndpointEdt: TEdit;
    RequestBtn: TButton;

    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure LoginBtnClick(Sender: TObject);
    procedure RefreshBtnClick(Sender: TObject);
    procedure RequestBtnClick(Sender: TObject);

    procedure CEFUrlRequestClientComponent1CreateURLRequest(Sender: TObject);
    procedure CEFUrlRequestClientComponent1DownloadData(Sender: TObject; const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt);
    procedure CEFUrlRequestClientComponent1DownloadProgress( Sender: TObject; const request: ICefUrlRequest; current, total: Int64);
    procedure CEFUrlRequestClientComponent1RequestComplete(Sender: TObject; const request: ICefUrlRequest);

    procedure CEFServerComponent1HttpRequest(Sender: TObject; const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest);
    procedure CEFServerComponent1ServerDestroyed(Sender: TObject; const server: ICefServer);

  protected
    FCanClose      : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing       : boolean;  // Set to True in the CloseQuery event.

    FMemStream     : TMemoryStream;
    FBusy          : boolean;
    FOAuthHelper   : TCEFOAuth2Helper;
    FStatus        : TOAuthTesterStatus;

    procedure CreateLoginRequest;
    procedure CreateTokenRefreshRequest;
    procedure CreateAPIRequest;
    procedure AppendAuthorizationHeader(const aRequest : ICefRequest);

    procedure URLRequestSuccessMsg(var aMessage : TMessage); message URLREQUEST_SUCCESS;
    procedure URLRequestErrorMsg(var aMessage : TMessage); message URLREQUEST_ERROR;
    procedure AuthCodeErrorMsg(var aMessage : TMessage); message AUTHCODE_ERROR;
  end;

var
  OAuth2TesterFrm: TOAuth2TesterFrm;

implementation

{$R *.dfm}

// This demo shows how to authenticate users using OAuth 2.0 and how
// to make requests to REST APIs that requiere authenticated users.

// Before you begin, please read the code comments in the URLRequest and SimpleServer
// demos because this demo uses the TCEFUrlRequestClientComponent and TCEFServerComponent
// components too.

// This demo was tested with the Google API only. If you need to access other REST APIs you
// may need to override some TCEFOAuth2Helper functions to send the right URL parameters.

// This is an alternative to the "REST Client Library" found in the
// latest Delphi but this time using CEF classes and functions like
// TCEFUrlRequestClientComponent, TCEFServerComponent, etc.

// As you can see in the REFERENCES, you need to follow a few steps
// to implement OAuth 2.0 in windows applications.

// STEP 1 :
// ========
// Obtain OAuth 2.0 client credentials from the API server.

// In this case, open the Google API Console at https://console.developers.google.com/
// and go to the "Credentials" page at https://console.developers.google.com/apis/credentials
// Then click "Create credentials -> OAuth client ID" and select "other" to create
// credentials for "native" applications.

// When it finishes you will see a "Client ID" and a "Client Secret" that you will need
// to run this demo.

// STEP 2 :
// ========
// Run this demo and fill the "Client ID" and "Client Secret" edit boxes with the credentials
// you got from Google.

// STEP 3 :
// ========
// Click on the "User login" button to launch the system browser and login to google with
// another user account. The URL in that login page was created by TCEFOAuth2Helper and it
// includes the credentials and other safety parameters.

// Once the user authenticated and he/she has granted permissions the browser will be
// redirected to a local URL served by TCEFServerComponent but Google has added some
// parameters to the redirected URL that will be used in later steps.

// STEP 4 :
// ========
// This demo uses TCEFOAuth2Helper to parse the parameters in the redirected URL which includes an
// "authentication code" and then it uses TCEFUrlRequestClientComponent to send a POST request
// to exchange that "authentication code" for an "access token".

// STEP 5 :
// ========
// TCEFUrlRequestClientComponent receives the "access token" and a "refresh token" that we'll use to
// request whatever we need to the REST API.

// STEP 6 :
// ========
// Click on the "Request" button to request some user information to the REST API using a
// GET request with TCEFUrlRequestClientComponent. This request includes the "access token" in the
// "Authorization" HTTP header.

// STEP 7 :
// ========
// The "access token" is only valid for some time and we'll need to click the "Request new token"
// button to receive a new token when it expires. This function uses TCEFOAuth2Helper and
// TCEFUrlRequestClientComponent to generate the parameters in a POST request needed to
// refresh the token.

// Read the TCEFOAuth2Helper.TokenExpiry property to know the amount of seconds that the
// "access token" will be valid.

// Destruction steps
// =================
// 1- Set CanClose to FALSE in the TForm.OnCloseQuery event, set FClosing to TRUE and call TCEFServerComponent.shutdown.
// 2- If there are pending URLRequests and the TCEFUrlRequestClientComponent.OnDownloadProgress
//    event is executed then call request.Cancel, which triggers the TCEFUrlRequestClientComponent.OnRequestComplete event.
// 3- In the TCEFUrlRequestClientComponent.OnRequestComplete event set FCanClose := True and send WM_CLOSE to the form.
// 4- If the TCEFServerComponent was initialized it will trigger TCEFServerComponent.OnServerDestroyed that sets
//    FCanClose := True and sends WM_CLOSE to the form.


// REFERENCES :
// ============
// https://tools.ietf.org/html/rfc6749
// https://tools.ietf.org/html/rfc6750
// https://tools.ietf.org/html/rfc8252
// https://tools.ietf.org/html/rfc6819
// https://tools.ietf.org/html/rfc7636
// https://tools.ietf.org/html/draft-ietf-oauth-native-apps-12
// https://tools.ietf.org/html/draft-ietf-oauth-security-topics-13
// https://developers.google.com/identity/protocols/OAuth2
// https://developers.google.com/identity/protocols/OAuth2InstalledApp
// https://developers.google.com/identity/protocols/googlescopes
// https://developers.google.com/identity/protocols/OpenIDConnect
// https://aaronparecki.com/oauth-2-simplified/
// https://example-app.com/pkce

uses
  uCEFApplication, uCEFMiscFunctions, uCEFPostData, uCEFPostDataElement, uCEFStringMultimap;

procedure TOAuth2TesterFrm.CEFServerComponent1HttpRequest(Sender: TObject;
  const server: ICefServer; connection_id: Integer;
  const client_address: ustring; const request: ICefRequest);
var
  TempData  : ustring;
begin
  TempData := '<html><head><title>User authentication successfull</title></head>' +
              '<body><p>User authentication successfull.</p><p>You can close this window now.</p></body></html>';

  CEFServerComponent1.SendHttp200response(connection_id, 'text/html', @TempData[1], length(TempData) * SizeOf(char));

  if (request <> nil) then
    begin
      if FOAuthHelper.ParseCodeRequestResponse(request.url) then
        CEFUrlRequestClientComponent1.AddURLRequest
       else
        PostMessage(Handle, AUTHCODE_ERROR, 0, 0);
    end;
end;

procedure TOAuth2TesterFrm.CEFServerComponent1ServerDestroyed(Sender: TObject; const server: ICefServer);
begin
  if FClosing then
    begin
      FCanClose := True;
      PostMessage(Handle, WM_CLOSE, 0, 0);
    end;
end;

procedure TOAuth2TesterFrm.CEFUrlRequestClientComponent1DownloadData(
  Sender: TObject; const request: ICefUrlRequest; data: Pointer;
  dataLength: NativeUInt);
begin
  try
    if FClosing then
      request.Cancel
     else
      if (data <> nil) and (dataLength > 0) then
        FMemStream.WriteBuffer(data^, dataLength);
  except
    on e : exception do
      if CustomExceptionHandler('TOAuth2BrowserFrm.CEFUrlRequestClientComponent1DownloadData', e) then raise;
  end;
end;

procedure TOAuth2TesterFrm.CEFUrlRequestClientComponent1DownloadProgress(
  Sender: TObject; const request: ICefUrlRequest; current, total: Int64);
begin
  if FClosing then request.Cancel;
end;

procedure TOAuth2TesterFrm.CEFUrlRequestClientComponent1RequestComplete(
  Sender: TObject; const request: ICefUrlRequest);
begin
  FBusy := False;

  if FClosing then
    begin
      FCanClose := True;
      PostMessage(Handle, WM_CLOSE, 0, 0);
    end
   else
    if (request <> nil) then
      begin
        if (request.response <> nil) and
           (request.RequestStatus = UR_SUCCESS) and
           (pos('application/json', request.response.MimeType) > 0) then
          PostMessage(Handle, URLREQUEST_SUCCESS, 0, 0)
         else
          PostMessage(Handle, URLREQUEST_ERROR, 0, request.RequestError);
      end
     else
      PostMessage(Handle, URLREQUEST_ERROR, 0, 0);
end;

procedure TOAuth2TesterFrm.CEFUrlRequestClientComponent1CreateURLRequest(Sender: TObject);
begin
  case FStatus of
    tsLogin   : CreateLoginRequest;
    tsRefresh : CreateTokenRefreshRequest;
    tsRequest : CreateAPIRequest;
  end;
end;

procedure TOAuth2TesterFrm.CreateLoginRequest;
var
  TempRequest     : ICefRequest;
  TempPostData    : ICefPostData;
  TempElement     : ICefPostDataElement;
  TempElementData : AnsiString;
begin
  try
    FBusy              := True;
    TempRequest        := TCefRequestRef.New;
    TempRequest.URL    := FOAuthHelper.TokenEndpoint;
    TempRequest.Method := 'POST';
    TempRequest.Flags  := UR_FLAG_ALLOW_STORED_CREDENTIALS;
    TempElementData    := AnsiString(FOAuthHelper.TokeExchangeParams);

    TempElement := TCefPostDataElementRef.New;
    TempElement.SetToBytes(length(TempElementData), @TempElementData[1]);

    TempPostData := TCefPostDataRef.New;
    TempPostData.AddElement(TempElement);

    TempRequest.PostData := TempPostData;

    TCefUrlRequestRef.New(TempRequest, CEFUrlRequestClientComponent1.Client, nil);
  finally
    TempElement  := nil;
    TempPostData := nil;
    TempRequest  := nil;
  end;
end;

procedure TOAuth2TesterFrm.CreateTokenRefreshRequest;
var
  TempRequest     : ICefRequest;
  TempPostData    : ICefPostData;
  TempElement     : ICefPostDataElement;
  TempElementData : AnsiString;
begin
  try
    FBusy              := True;
    TempRequest        := TCefRequestRef.New;
    TempRequest.URL    := FOAuthHelper.TokenEndpoint;
    TempRequest.Method := 'POST';
    TempRequest.Flags  := UR_FLAG_ALLOW_STORED_CREDENTIALS;
    TempElementData    := AnsiString(FOAuthHelper.RefreshParams);

    TempElement := TCefPostDataElementRef.New;
    TempElement.SetToBytes(length(TempElementData), @TempElementData[1]);

    TempPostData := TCefPostDataRef.New;
    TempPostData.AddElement(TempElement);

    TempRequest.PostData := TempPostData;

    TCefUrlRequestRef.New(TempRequest, CEFUrlRequestClientComponent1.Client, nil);
  finally
    TempElement  := nil;
    TempPostData := nil;
    TempRequest  := nil;
  end;
end;

procedure TOAuth2TesterFrm.CreateAPIRequest;
var
  TempRequest  : ICefRequest;
begin
  try
    FBusy              := True;
    TempRequest        := TCefRequestRef.New;
    TempRequest.URL    := EndpointEdt.Text;
    TempRequest.Method := 'GET';
    TempRequest.Flags  := UR_FLAG_ALLOW_STORED_CREDENTIALS;

    AppendAuthorizationHeader(TempRequest);

    TCefUrlRequestRef.New(TempRequest, CEFUrlRequestClientComponent1.Client, nil);
  finally
    TempRequest := nil;
  end;
end;

procedure TOAuth2TesterFrm.AppendAuthorizationHeader(const aRequest : ICefRequest);
var
  TempOldMap, TempNewMap : ICefStringMultimap;
  i : NativeUInt;
begin
  try
    TempNewMap := TCefStringMultimapOwn.Create;
    TempOldMap := TCefStringMultimapOwn.Create;

    aRequest.GetHeaderMap(TempOldMap);

    i := 0;
    while (i < TempOldMap.Size) do
      begin
        TempNewMap.Append(TempOldMap.Key[i], TempOldMap.Value[i]);
        inc(i);
      end;

    TempNewMap.Append('Authorization', FOAuthHelper.TokenType + ' ' + FOAuthHelper.AccessToken);

    aRequest.SetHeaderMap(TempNewMap);
  finally
    TempNewMap := nil;
    TempOldMap := nil;
  end;
end;

procedure TOAuth2TesterFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose and not(FBusy) and not(CEFServerComponent1.Initialized);

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;

      if CEFServerComponent1.Initialized then CEFServerComponent1.Shutdown;
    end;
end;

procedure TOAuth2TesterFrm.FormCreate(Sender: TObject);
begin
  FMemStream   := TMemoryStream.Create;
  FCanClose    := False;
  FClosing     := False;
  FBusy        := False;
  FStatus      := tsIdle;

  FOAuthHelper := TCEFOAuth2Helper.Create;
  {$IFDEF DELPHI22_UP}
  // SHA256 is not implemented in TCEFOAuth2Helper for older Delphi versions
  FOAuthHelper.ChallengeMethod := cmSHA256;
  {$ENDIF}

  CEFServerComponent1.CreateServer(FOAuthHelper.RedirectHost, FOAuthHelper.RedirectPort, 10);
end;

procedure TOAuth2TesterFrm.FormDestroy(Sender: TObject);
begin
  if (FMemStream   <> nil) then FreeAndNil(FMemStream);
  if (FOAuthHelper <> nil) then FreeAndNil(FOAuthHelper);
end;

procedure TOAuth2TesterFrm.LoginBtnClick(Sender: TObject);
var
  TempAuthURI : string;
begin
  if (FStatus <> tsIdle)                or
     (length(ClientIDEdt.Text)     = 0) or
     (length(ClientSecretEdt.Text) = 0) or
     (length(ScopeEdt.Text)        = 0) then
    exit;

  FStatus       := tsLogin;
  screen.cursor := crAppStart;

  FOAuthHelper.ClientID     := ClientIDEdt.Text;
  FOAuthHelper.ClientSecret := ClientSecretEdt.Text;
  FOAuthHelper.Scope        := ScopeEdt.Text;
  TempAuthURI               := FOAuthHelper.AuthCodeURI;

  LogMem.Lines.Add('-----------------------------------------------------------------');
  LogMem.Lines.Add('Opening authorization request in the system browser : ' + TempAuthURI);

  ShellExecute(Handle, 'open', PWideChar(TempAuthURI + #0), nil, nil, SW_SHOWNORMAL);
end;

procedure TOAuth2TesterFrm.RefreshBtnClick(Sender: TObject);
begin
  if (FStatus <> tsIdle)                or
     (length(ClientIDEdt.Text)     = 0) or
     (length(ClientSecretEdt.Text) = 0) or
     (length(ScopeEdt.Text)        = 0) or
     (length(RefreshTokenEdt.Text) = 0) then
    exit;

  FStatus       := tsRefresh;
  screen.cursor := crAppStart;

  LogMem.Lines.Add('-----------------------------------------------------------------');
  LogMem.Lines.Add('Requesting a new token.');

  CEFUrlRequestClientComponent1.AddURLRequest;
end;

procedure TOAuth2TesterFrm.RequestBtnClick(Sender: TObject);
begin
  if (FStatus <> tsIdle)                or
     (length(ClientIDEdt.Text)     = 0) or
     (length(ClientSecretEdt.Text) = 0) or
     (length(ScopeEdt.Text)        = 0) or
     (length(EndpointEdt.Text)     = 0) then
    exit;

  FStatus       := tsRequest;
  screen.cursor := crAppStart;

  LogMem.Lines.Add('-----------------------------------------------------------------');
  LogMem.Lines.Add('Requesting information to the API endpoint.');

  CEFUrlRequestClientComponent1.AddURLRequest;
end;

procedure TOAuth2TesterFrm.URLRequestSuccessMsg(var aMessage : TMessage);
var
  TempStream : TStringStream;
  TempBuffer : TBytes;
begin
  if (FMemStream = nil) or (FMemStream.Size = 0) then exit;

  try
    TempStream          := nil;
    FMemStream.position := 0;
    SetLength(TempBuffer, FMemStream.Size);

    if (FMemStream.Read(TempBuffer[0], FMemStream.Size) > 0) then
      begin
        TempStream := TStringStream.Create(TempBuffer);

        case FStatus of
          tsLogin :
            begin
              LogMem.Lines.Add('Token exchange response : ' + TempStream.DataString);

              if FOAuthHelper.ParseTokenExchangeResponse(TempStream.DataString) then
                begin
                  AccessTokenEdt.Text  := FOAuthHelper.AccessToken;
                  RefreshTokenEdt.Text := FOAuthHelper.RefreshToken;
                end
               else
                begin
                  AccessTokenEdt.Text  := '';
                  RefreshTokenEdt.Text := '';

                  showmessage('Login error : ' + FOAuthHelper.Error + #13 + #10 + FOAuthHelper.ErrorDescription);
                end;
            end;

          tsRefresh :
            begin
              LogMem.Lines.Add('Token refresh response : ' + TempStream.DataString);

              if FOAuthHelper.ParseRefreshTokenResponse(TempStream.DataString) then
                AccessTokenEdt.Text := FOAuthHelper.AccessToken
               else
                showmessage('Token refresh error : ' + FOAuthHelper.Error + #13 + #10 + FOAuthHelper.ErrorDescription);
            end;

          tsRequest :
            LogMem.Lines.Add('API request response : ' + TempStream.DataString);
        end;
      end;
  finally
    FMemStream.Clear;
    if (TempStream <> nil) then FreeAndNil(TempStream);
    SetLength(TempBuffer, 0);
    FStatus       := tsIdle;
    screen.cursor := crDefault;
  end;
end;

procedure TOAuth2TesterFrm.URLRequestErrorMsg(var aMessage : TMessage);
begin
  case FStatus of
    tsLogin   : showmessage('Login error code : ' + inttostr(aMessage.lParam));
    tsRefresh : showmessage('Token refresh error code : ' + inttostr(aMessage.lParam));
    tsRequest : showmessage('API request error code : ' + inttostr(aMessage.lParam));
  end;

  FMemStream.Clear;
  FStatus       := tsIdle;
  screen.cursor := crDefault;
end;

procedure TOAuth2TesterFrm.AuthCodeErrorMsg(var aMessage : TMessage);
var
  TempMessage : string;
begin
  TempMessage := 'Authentication error' + #13 + #10;

  if not(FOAuthHelper.ValidState) then
    TempMessage := TempMessage + #13 + #10 + 'Received request with invalid state';

  if (length(FOAuthHelper.Error) > 0) then
    TempMessage := TempMessage + #13 + #10 + FOAuthHelper.Error + #13 + #10 + FOAuthHelper.ErrorDescription;

  showmessage(TempMessage);
end;

end.
