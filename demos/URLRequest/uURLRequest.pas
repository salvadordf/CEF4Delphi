// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2018 Salvador Díaz Fau. All rights reserved.
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

unit uURLRequest;

interface

uses

  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls, StdCtrls,
  {$ENDIF}
  uCEFInterfaces, uCEFUrlRequestClientComponent;

const
  URLREQUEST_SUCCESS    = WM_APP + $101;
  URLREQUEST_ERROR      = WM_APP + $102;

type
  TURLRequestFrm = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    DownloadBtn: TButton;
    StatusBar1: TStatusBar;
    SaveDialog1: TSaveDialog;
    CEFUrlRequestClientComponent1: TCEFUrlRequestClientComponent;

    procedure DownloadBtnClick(Sender: TObject);

    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure CEFUrlRequestClientComponent1DownloadData(Sender: TObject; const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt);
    procedure CEFUrlRequestClientComponent1DownloadProgress(Sender: TObject; const request: ICefUrlRequest; current, total: Int64);
    procedure CEFUrlRequestClientComponent1RequestComplete(Sender: TObject; const request: ICefUrlRequest);
    procedure CEFUrlRequestClientComponent1CreateURLRequest(Sender: TObject);
  private
    FStream         : TMemoryStream;
    FCanClose       : boolean;
    FClosing        : boolean;
    FDownloading    : boolean;
    FPendingURL     : string;

    procedure URLRequestSuccessMsg(var aMessage : TMessage); message URLREQUEST_SUCCESS;
    procedure URLRequestErrorMsg(var aMessage : TMessage); message URLREQUEST_ERROR;

    procedure SaveStreamToFile;
  end;

var
  URLRequestFrm: TURLRequestFrm;

implementation

{$R *.dfm}

// This is a simple URL request example to download small files using TCEFUrlRequestClientComponent.
// WARNING : If you try to download big files you may get an "Out of memory" exception. Replace TMemoryStream in that case.

// All TCEFUrlRequestClientComponent events are executed in a different thread. Don't create or destroy VCL componets
// inside them.

// To keep this demo as simple as possible, it's only allowed to download one file at a time. You can add as many requests
// as you want but then you would have to use a different way to store the data with synchronization objects.

// It's much safer to cancel all requests before closing the app.
// This demo follows this destruction sequence in case there is a file download running :
// --------------------------------------------------------------------------------------
// 1- Set CanClose to FALSE in the TForm.OnCloseQuery event and set FClosing to TRUE.
// 2- The next time TCEFUrlRequestClientComponent.OnDownloadProgress is executed we call request.Cancel, which triggers the
//    TCEFUrlRequestClientComponent.OnRequestComplete event.
// 3- in the TCEFUrlRequestClientComponent.OnRequestComplete event we set FCanClose to TRUE and send WM_CLOSE to the form.

uses
  uCEFMiscFunctions, uCEFTypes, uCEFUrlRequest, uCEFRequest, uCEFPostData, uCEFPostDataElement;

procedure TURLRequestFrm.DownloadBtnClick(Sender: TObject);
var
  TempURL, TempPath, TempName : string;
  TempParts : TUrlParts;
  i : integer;
begin
  TempURL := trim(Edit1.Text);

  if (length(TempURL) > 0) then
    begin
      CefParseUrl(TempURL, TempParts);
      TempPath := trim(TempParts.path);
      TempName := '';

      if (length(TempPath) > 0) then
        begin
          i := LastDelimiter('/', TempPath);

          if (i > 0) then
            TempName := trim(copy(TempPath, succ(i), length(TempPath)))
           else
            TempName := TempPath;
        end;

      if (length(TempName) > 0) then
        SaveDialog1.FileName := TempName  // This name should be decoded and sanitized before using it in Windows
       else
        SaveDialog1.FileName := 'UnknownFileName';

      if SaveDialog1.Execute and
         (length(SaveDialog1.FileName) > 0) then
        begin
          FPendingURL               := TempURL;
          DownloadBtn.Enabled       := False;
          StatusBar1.Panels[0].Text := 'Downloading...';
          FStream.Clear;

          // TCEFUrlRequestClientComponent.AddURLRequest will trigger the
          // TCEFUrlRequestClientComponent.OnCreateURLRequest event in the right
          // thread where you can create your custom requests.
          CEFUrlRequestClientComponent1.AddURLRequest;
        end;
    end;
end;

procedure TURLRequestFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose or not(FDownloading);
  FClosing := True;
end;

procedure TURLRequestFrm.FormCreate(Sender: TObject);
begin
  FStream      := TMemoryStream.Create;
  FCanClose    := False;
  FClosing     := False;
  FDownloading := False;
end;

procedure TURLRequestFrm.FormDestroy(Sender: TObject);
begin
  if (FStream <> nil) then FreeAndNil(FStream);
end;

procedure TURLRequestFrm.CEFUrlRequestClientComponent1CreateURLRequest(Sender: TObject);
var
  TempRequest  : ICefRequest;
  // TempPostData : ICefPostData;
  // TempElement  : ICefPostDataElement;
begin
  try
    if (length(FPendingURL) > 0) then
      begin
        FDownloading       := True;

        // GET request example
        // -------------------
        TempRequest        := TCefRequestRef.New;
        TempRequest.URL    := FPendingURL;
        TempRequest.Method := 'GET';

        // POST request example
        // --------------------
        // TempElement             := TCefPostDataElementOwn.Create(True);
        // TempElement.SetToFile('c:\myfile.txt');
        //
        // TempPostData            := TCefPostDataRef.New;
        // TempPostData.AddElement := TempElement;
        //
        // TempRequest             := TCefRequestRef.New;
        // TempRequest.URL         := FPendingURL;
        // TempRequest.Method      := 'POST';
        // TempRequest.PostData    := TempPostData;

        // Set the "client" parameter to the TCEFUrlRequestClientComponent.Client property
        // to use the TCEFUrlRequestClientComponent events.
        // The "requestContext" parameter can be nil to use the global request context.
        TCefUrlRequestRef.New(TempRequest, CEFUrlRequestClientComponent1.Client, nil);
      end;
  finally
    TempRequest := nil;
  end;
end;

procedure TURLRequestFrm.CEFUrlRequestClientComponent1DownloadData(Sender: TObject; const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt);
begin
  try
    if FClosing then
      request.Cancel
     else
      if (data <> nil) and (dataLength > 0) then
        FStream.WriteBuffer(data^, dataLength);
  except
    on e : exception do
      if CustomExceptionHandler('TURLRequestFrm.CEFUrlRequestClientComponent1DownloadData', e) then raise;
  end;
end;

procedure TURLRequestFrm.CEFUrlRequestClientComponent1DownloadProgress(Sender: TObject; const request: ICefUrlRequest; current, total: Int64);
begin
  if FClosing then
    request.Cancel
   else
    if (total > 0) then
      StatusBar1.Panels[0].Text := 'Downloading : ' + inttostr(round((current / total) * 100)) + ' %'
     else
      StatusBar1.Panels[0].Text := 'Downloading : ' + inttostr(current) + ' bytes';
end;

procedure TURLRequestFrm.CEFUrlRequestClientComponent1RequestComplete(Sender: TObject; const request: ICefUrlRequest);
begin
  FDownloading := False;

  // Use request.response here to get a ICefResponse interface with all the response headers, status, error code, etc.

  if FClosing then
    begin
      FCanClose := True;
      PostMessage(Handle, WM_CLOSE, 0, 0);
    end
   else
    if (request <> nil) and (request.RequestStatus = UR_SUCCESS) then
      PostMessage(Handle, URLREQUEST_SUCCESS, 0, 0)
     else
      PostMessage(Handle, URLREQUEST_ERROR, 0, request.RequestError);
end;

procedure TURLRequestFrm.URLRequestSuccessMsg(var aMessage : TMessage);
begin
  DownloadBtn.Enabled       := True;
  StatusBar1.Panels[0].Text := 'Download complete!';
  SaveStreamToFile;
end;

procedure TURLRequestFrm.URLRequestErrorMsg(var aMessage : TMessage);
begin
  DownloadBtn.Enabled       := True;
  StatusBar1.Panels[0].Text := 'Download error : ' + inttostr(aMessage.lParam);
end;

procedure TURLRequestFrm.SaveStreamToFile;
begin
  try
    FStream.SaveToFile(SaveDialog1.FileName);
    FStream.Clear;
  except
    on e : exception do
      if CustomExceptionHandler('TURLRequestFrm.SaveStreamToFile', e) then raise;
  end;
end;

end.
