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
//        Copyright ?2018 Salvador Diaz Fau. All rights reserved.
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

unit uResponseFilterBrowser;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.SyncObjs,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, SyncObjs,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes, uCEFResponseFilter,
  Vcl.ComCtrls;

const
  STREAM_COPY_COMPLETE    = WM_APP + $B00;

type
  TResponseFilterBrowserFrm = class(TForm)
    AddressPnl: TPanel;
    AddressEdt: TEdit;
    Timer1: TTimer;
    Chromium1: TChromium;
    CEFWindowParent1: TCEFWindowParent;
    Splitter1: TSplitter;
    Panel1: TPanel;
    GoBtn: TButton;
    Label1: TLabel;
    RscNameEdt: TEdit;
    Panel2: TPanel;
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    procedure GoBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1GetResourceResponseFilter(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; out Result: ICefResponseFilter);
    procedure Chromium1ResourceLoadComplete(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus; receivedContentLength: Int64);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Chromium1BeforePopup(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
      targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
      const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser;
      out Result: Boolean);
    procedure Chromium1BeforeClose(Sender: TObject;
      const browser: ICefBrowser);
  protected
    FFilter        : ICefResponseFilter; // CEF Filter interface that receives the resource contents
    FStream        : TMemoryStream;      // TMemoryStream to hold the resource contents
    FStreamCS      : TCriticalSection;   // Critical section used to protect the memory stream
    FRscSize       : int64;              // size of the resource if the server sends the Content-Length header
    FRscCompleted  : boolean;            // This variable will be used to handle the results only once.
    FRscEncoding   : TEncoding;          // The resource response Encoding. When encoding is unicode. The response data may be sent by multi chains, will cause encoding parsing problem.
    FRscMimeType   : String;
    FFilterInit    : boolean;
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    procedure WMMove(var aMessage: TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage: TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure BrowserDestroyMsg(var aMessage: TMessage); message CEF_DESTROY;
    procedure StreamCopyCompleteMsg(var aMessage : TMessage); message STREAM_COPY_COMPLETE;

    procedure Filter_OnFilter(Sender: TObject; data_in: Pointer; data_in_size: NativeUInt; var data_in_read: NativeUInt; data_out: Pointer; data_out_size : NativeUInt; var data_out_written: NativeUInt; var aResult : TCefResponseFilterStatus);

    procedure UpdateRscEncoding(const aMimeType, aContentType : string);
    function  IsMyResource(const aRequest : ICefRequest) : boolean;
    procedure GetResponseEncoding(const aContentType: string);
  public
    { Public declarations }
  end;

var
  ResponseFilterBrowserFrm: TResponseFilterBrowserFrm;

implementation

{$R *.dfm}

uses
  {$IFDEF DELPHI16_UP}
  System.Math,
  {$ELSE}
  Math,
  {$ENDIF}
  uCEFApplication, uCEFMiscFunctions;

// This demo uses a TCustomResponseFilter to read the contents from a JavaScript file in wikipedia.org into a TMemoryStream.
// The stream is shown in the TMemo when it's finished.

// For more information read the CEF3 code comments here :
//      https://github.com/chromiumembedded/cef/blob/master/include/capi/cef_response_filter_capi.h

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE calls TChromium.CloseBrowser which triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy CEFWindowParent1 in the main thread, which triggers the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure TResponseFilterBrowserFrm.Filter_OnFilter(Sender: TObject;
                                                        data_in          : Pointer;
                                                        data_in_size     : NativeUInt;
                                                    var data_in_read     : NativeUInt;
                                                        data_out         : Pointer;
                                                        data_out_size    : NativeUInt;
                                                    var data_out_written : NativeUInt;
                                                    var aResult          : TCefResponseFilterStatus);
begin
  try
    try
      FStreamCS.Acquire;

      if FFilterInit then
        begin
          // This event will be called repeatedly until the input buffer has been fully read.
          // When there's no more data then data_in is nil and you can show the stream contents.

          if (data_in = nil) then
            begin
              data_in_read     := 0;
              data_out_written := 0;
              aResult          := RESPONSE_FILTER_DONE;

              // Send the message only if this is the first time that this event has data_in = nil
              if not(FRscCompleted) and
                 (FStream.Size > 0) and
                 ((FRscSize = -1) or (FRscSize = FStream.Size)) then
                FRscCompleted := PostMessage(Handle, STREAM_COPY_COMPLETE, 0, 0);
            end
           else
            begin
              if (data_out <> nil) then
                begin
                  data_out_written := min(data_in_size, data_out_size);

                  if (data_out_written > 0) then
                    Move(data_in^, data_out^, data_out_written);
                end;

                if (data_in_size > 0) then
                  data_in_read := FStream.Write(data_in^, data_in_size);

                // Send the STREAM_COPY_COMPLETE message only if the server sent the data size in
                // a Content-Length header and we can compare it with the stream size
                if not(FRscCompleted) and (FRscSize <> -1) and (FRscSize = FStream.Size) then
                  FRscCompleted := PostMessage(Handle, STREAM_COPY_COMPLETE, 0, 0);

                aResult := RESPONSE_FILTER_NEED_MORE_DATA;
              end;
        end;
    except
      on e : exception do
        begin
          aResult := RESPONSE_FILTER_ERROR;
          if CustomExceptionHandler('TResponseFilterBrowserFrm.Filter_OnFilter', e) then raise;
        end;
    end;
  finally
    FStreamCS.Release;
  end;
end;

function TResponseFilterBrowserFrm.IsMyResource(const aRequest : ICefRequest) : boolean;
var
  TempName : string;
begin
  TempName := trim(RscNameEdt.Text);

  if (aRequest <> nil) and (length(TempName) > 0) then
    Result := (pos(TempName, aRequest.URL) > 0)
   else
    Result := False;
end;

procedure TResponseFilterBrowserFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
    end;
end;

procedure TResponseFilterBrowserFrm.FormCreate(Sender: TObject);
begin
  FFilterInit    := False;
  FRscCompleted  := False;
  FRscSize       := -1;
  FStream        := TMemoryStream.Create;
  FStreamCS      := TCriticalSection.Create;
  FFilter        := TCustomResponseFilter.Create;
  FRscEncoding   := TEncoding.Default;

  FCanClose := False;
  FClosing  := False;

  // This event will receive the data
  TCustomResponseFilter(FFilter).OnFilter := Filter_OnFilter;
end;

procedure TResponseFilterBrowserFrm.FormDestroy(Sender: TObject);
begin
  FFilter := nil;
  FStream.Free;
  FStreamCS.Free;
end;

procedure TResponseFilterBrowserFrm.FormShow(Sender: TObject);
begin
  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) then Timer1.Enabled := True;
end;

procedure TResponseFilterBrowserFrm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can send a message to the main form to load the initial web page.
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TResponseFilterBrowserFrm.Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TResponseFilterBrowserFrm.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var noJavascriptAccess: Boolean;
  var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TResponseFilterBrowserFrm.Chromium1Close(Sender: TObject; const browser: ICefBrowser; out Result: Boolean);
begin
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  Result := True;
end;

procedure TResponseFilterBrowserFrm.Chromium1GetResourceResponseFilter(Sender : TObject;
                                                                       const browser   : ICefBrowser;
                                                                       const frame     : ICefFrame;
                                                                       const request   : ICefRequest;
                                                                       const response  : ICefResponse;
                                                                       out   Result    : ICefResponseFilter);
var
  TempContentLength, TempContentEncoding : string;
  TempLen : integer;
begin
  if not(FRscCompleted) and (response <> nil) and IsMyResource(request) then
    begin
      try
        FStreamCS.Acquire;

        Result              := FFilter;
        FFilterInit         := True;
        TempContentEncoding := trim(lowercase(response.GetHeader('Content-Encoding')));

        if (length(TempContentEncoding) > 0) and (TempContentEncoding <> 'identity') then
          begin
            // We can't use this information because Content-Length has the
            // compressed length but the OnFilter event has uncompressed data.
            FRscSize := -1;
            StatusBar1.Panels[0].Text := 'Content-Length : compressed';
          end
         else
          begin
            TempContentLength := trim(response.GetHeader('Content-Length'));

            if TryStrToInt(TempContentLength, TempLen) and (TempLen > 0) then
              begin
                FRscSize := TempLen;
                StatusBar1.Panels[0].Text := 'Content-Length : ' + inttostr(FRscSize);
              end
             else
              begin
                FRscSize := -1;
                StatusBar1.Panels[0].Text := 'Content-Length : not available';
              end;
          end;

        UpdateRscEncoding(response.MimeType, response.GetHeader('Content-Type'));
      finally
        FStreamCS.Release;
      end;
    end
   else
    Result := nil;
end;

procedure TResponseFilterBrowserFrm.UpdateRscEncoding(const aMimeType, aContentType : string);
begin
  FRscMimeType := aMimeType;

  if (aMimeType = 'application/json') or
     (aMimeType = 'text/json')        or
     (aMimeType = 'text/javascript')  or
     (aMimeType = 'application/javascript') then
    GetResponseEncoding(aContentType);
end;

procedure TResponseFilterBrowserFrm.Chromium1ResourceLoadComplete(Sender : TObject;
                                                                  const browser               : ICefBrowser;
                                                                  const frame                 : ICefFrame;
                                                                  const request               : ICefRequest;
                                                                  const response              : ICefResponse;
                                                                        status                : TCefUrlRequestStatus;
                                                                        receivedContentLength : Int64);
begin
  try
    FStreamCS.Acquire;

    // In case the server didn't send a Content-Length header
    // and CEF didn't send a data_in = nil in Filter_OnFilter
    // we still can use this event to know when the resource is complete
    if IsMyResource(request) then
      begin
        UpdateRscEncoding(response.MimeType, response.GetHeader('Content-Type'));

        // Only send the message if this event is triggered before we have a OnFilter event with data_in = nil
        if not(FRscCompleted) then
          FRscCompleted := PostMessage(Handle, STREAM_COPY_COMPLETE, 0, 0);
      end;
  finally
    FStreamCS.Release;
  end;
end;

procedure TResponseFilterBrowserFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  Caption            := 'Response Filter Browser';
  AddressPnl.Enabled := True;
  GoBtn.Click;
end;

procedure TResponseFilterBrowserFrm.BrowserDestroyMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.Free;
end;

// This procedure handles the stream contents after it's fully downloaded
procedure TResponseFilterBrowserFrm.StreamCopyCompleteMsg(var aMessage : TMessage);
var
  LAS: AnsiString;
  LS: String;
begin
  try
    FStreamCS.Acquire;

    if (FStream.Size > 0) then
      begin
        FStream.Seek(0, soBeginning);

        Memo1.Lines.Clear;

        if (FRscMimeType = 'application/json') or
           (FRscMimeType = 'text/json')        or
           (FRscMimeType = 'text/javascript')  or
           (FRscMimeType = 'application/javascript') then
          begin
            StatusBar1.Panels[1].Text := 'Stream size : ' + inttostr(FStream.Size);

            SetLength(LAS, FStream.Size);
            FStream.Read(LAS[Low(LAS)], FStream.Size);

            if (FRscEncoding = TEncoding.UTF8) then
              LS := UTF8Decode(LAS) // UTF8 Here
             else
              LS := string(LAS); // Others encoding text

            Memo1.Lines.Add(LS);

            StatusBar1.Panels[2].Text := 'Decoded size : ' + inttostr(length(LS));
          end
         else
          Memo1.Lines.LoadFromStream(FStream); // Image or others

        StatusBar1.Panels[3].Text := 'Memo size : ' + inttostr(length(trim(Memo1.Lines.Text)));

        // There might be a small difference in sizes because of the text decoding

        FStream.Clear;
      end
     else
      Memo1.Lines.Clear;
  finally
    FStreamCS.Release;
  end;
end;

procedure TResponseFilterBrowserFrm.GoBtnClick(Sender: TObject);
begin
  try
    FStreamCS.Acquire;

    FFilterInit               := False;
    FRscCompleted             := False;
    FRscSize                  := -1;
    StatusBar1.Panels[0].Text := '';
    StatusBar1.Panels[1].Text := '';
    StatusBar1.Panels[2].Text := '';
    StatusBar1.Panels[3].Text := '';
    Memo1.Lines.Clear;
    FStream.Clear;
  finally
    FStreamCS.Release;
  end;

  Chromium1.LoadURL(AddressEdt.Text);
end;

procedure TResponseFilterBrowserFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TResponseFilterBrowserFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TResponseFilterBrowserFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TResponseFilterBrowserFrm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TResponseFilterBrowserFrm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

procedure TResponseFilterBrowserFrm.GetResponseEncoding(const aContentType: string);
var
  LEncoding: String;
begin
  {
    (Name: 'Latin-US (DOS)'; CodePage: 437),
    (Name: 'Western (DOS Latin 1)'; CodePage: 850),
    (Name: 'Thai (Windows, DOS)'; CodePage: 874),
    (Name: 'Japanese (Windows, DOS)'; CodePage: 932),
    (Name: 'Simplified Chinese (Windows, DOS)'; CodePage: 936),
    (Name: 'Korean (Windows, DOS)'; CodePage: 949),
    (Name: 'Traditional Chinese (Windows, DOS)'; CodePage: 950),
    (Name: 'Unicode (UTF-16)'; CodePage: 1200),
    (Name: 'Unicode (UTF-16LE)'; CodePage: 1200),
    (Name: 'Unicode (UTF-16BE)'; CodePage: 1201),
    (Name: 'Central European (Windows Latin 2)'; CodePage: 1250),
    (Name: 'Cyrillic (Windows)'; CodePage: 1251),
    (Name: 'Western (Windows Latin 1)'; CodePage: 1252),
    (Name: 'Greek (Windows)'; CodePage: 1253),
    (Name: 'Turkish (Windows Latin 5)'; CodePage: 1254),
    (Name: 'Hebrew (Windows)'; CodePage: 1255),
    (Name: 'Arabic (Windows)'; CodePage: 1256),
    (Name: 'Baltic (Windows)'; CodePage: 1257),
    (Name: 'Vietnamese (Windows)'; CodePage: 1258),
    (Name: 'Western (ASCII)'; CodePage: 20127),
    (Name: 'Unicode (UTF-7)'; CodePage: CP_UTF7),
    (Name: 'Unicode (UTF-8)'; CodePage: CP_UTF8),
    // Windows code pages...
    (Name: 'Windows-1252'; CodePage: 1252),
    (Name: 'US-ASCII'; CodePage: 20127),
    (Name: 'UTF-7'; CodePage: CP_UTF7),
    (Name: 'UTF-8'; CodePage: CP_UTF8),
    (Name: 'UTF-16'; CodePage: 1200),
    (Name: 'UTF-16BE'; CodePage: 1201),
    (Name: 'UTF-16LE'; CodePage: 1200),
    (Name: 'SHIFT-JIS'; CodePage: 932),
    (Name: 'ISO-8859-1'; CodePage: 28591),
    (Name: 'iso-8859-1'; CodePage: 28591),
    (Name: 'MACCROATIAN'; CodePage: 10082),
    (Name: 'ASCII'; CodePage: 20127),
    (Name: ''; CodePage: 0)
  }
  LEncoding := Trim(Copy(UpperCase(aContentType), Pos('CHARSET=',
    UpperCase(aContentType)) + Length('CHARSET='), MaxInt));

  if LEncoding = 'ANSI' then
  begin
    FRscEncoding := TEncoding.ANSI;
  end
  else if LEncoding = 'ASCII' then
  begin
    FRscEncoding := TEncoding.ASCII;
  end
  else if LEncoding = 'UTF-8' then
  begin
    FRscEncoding := TEncoding.UTF8;
  end
  else if LEncoding = 'UTF-7' then
  begin
    FRscEncoding := TEncoding.UTF7;
  end
  else if LEncoding = 'UTF-16' then
  begin
    FRscEncoding := TEncoding.Unicode;
  end
  else if LEncoding = 'UNICODEFFFE' then
  begin
    FRscEncoding := TEncoding.GetEncoding(1201);
  end
  else if LEncoding = 'UNICODE' then
  begin
    FRscEncoding := TEncoding.Unicode;
  end
  else if LEncoding = 'GB2312' then
  begin
    FRscEncoding := TEncoding.GetEncoding(936);
  end
  else if LEncoding = 'GBK' then
  begin
    FRscEncoding := TEncoding.GetEncoding(936);
  end
  else if LEncoding = 'GB18030' then
  begin
    FRscEncoding := TEncoding.GetEncoding(54936);
  end
  else if LEncoding = 'ISO-8859-1' then
  begin
    FRscEncoding := TEncoding.GetEncoding(28591);
  end
  else if LEncoding = 'BIG5' then
  begin
    FRscEncoding := TEncoding.GetEncoding(950);
  end
  else
  begin
    FRscEncoding := TEncoding.Default;
  end;
end;

end.
