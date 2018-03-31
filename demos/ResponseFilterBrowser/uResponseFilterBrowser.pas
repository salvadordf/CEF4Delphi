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
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes, uCEFResponseFilter;

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
    Memo1: TMemo;
    Panel1: TPanel;
    GoBtn: TButton;
    Label1: TLabel;
    RscNameEdt: TEdit;
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
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure BrowserDestroyMsg(var aMessage : TMessage); message CEF_DESTROY;
    procedure StreamCopyCompleteMsg(var aMessage : TMessage); message STREAM_COPY_COMPLETE;

    procedure Filter_OnFilter(Sender: TObject; data_in: Pointer; data_in_size: NativeUInt; var data_in_read: NativeUInt; data_out: Pointer; data_out_size : NativeUInt; var data_out_written: NativeUInt; var aResult : TCefResponseFilterStatus);

    function  IsMyResource(const aRequest : ICefRequest) : boolean;
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
      // This event will be called repeatedly until the input buffer has been fully read.
      // When there's no more data then data_in is nil and you can show the stream contents.

      FStreamCS.Acquire;

      if (data_in = nil) then
        begin
          data_in_read     := 0;
          data_out_written := 0;
          aResult          := RESPONSE_FILTER_DONE;

          if not(FRscCompleted) then
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
  FRscCompleted  := False;
  FRscSize       := -1;
  FStream        := TMemoryStream.Create;
  FStreamCS      := TCriticalSection.Create;
  FFilter        := TCustomResponseFilter.Create;

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

procedure TResponseFilterBrowserFrm.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
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

procedure TResponseFilterBrowserFrm.Chromium1Close(Sender: TObject;
  const browser: ICefBrowser; out Result: Boolean);
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
  TempHeader : string;
  TempLen : integer;
begin
  if (response <> nil) and IsMyResource(request) then
    begin
      Result     := FFilter;
      TempHeader := trim(response.GetHeader('Content-Length'));

      if TryStrToInt(TempHeader, TempLen) and (TempLen > 0) then
        FRscSize := TempLen
       else
        FRscSize := -1;
    end
   else
    Result := nil;
end;

procedure TResponseFilterBrowserFrm.Chromium1ResourceLoadComplete(Sender : TObject;
                                                                  const browser               : ICefBrowser;
                                                                  const frame                 : ICefFrame;
                                                                  const request               : ICefRequest;
                                                                  const response              : ICefResponse;
                                                                        status                : TCefUrlRequestStatus;
                                                                        receivedContentLength : Int64);
begin
  // In case the server didn't send a Content-Length header
  // and CEF didn't send a data_in = nil in Filter_OnFilter
  // we still can use this event to know when the resource is complete
  if not(FRscCompleted) and IsMyResource(request) then
    FRscCompleted := PostMessage(Handle, STREAM_COPY_COMPLETE, 0, 0);
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
begin
  try
    FStreamCS.Acquire;

    if (FStream.Size > 0) then
      begin
        FStream.Seek(0, soBeginning);

        Memo1.Lines.Clear;
        Memo1.Lines.LoadFromStream(FStream);

        FStream.Clear;
      end
     else
      Memo1.Lines.Clear;

    FRscSize      := -1;
    FRscCompleted := False;
  finally
    FStreamCS.Release;
  end;
end;

procedure TResponseFilterBrowserFrm.GoBtnClick(Sender: TObject);
begin
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

end.
