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

unit uJSEval;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Menus,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Types, Vcl.ComCtrls, Vcl.ClipBrd,
  System.UITypes, Soap.EncdDecd,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Menus,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Types, ComCtrls, ClipBrd, EncdDecd,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes, uCEFConstants,
  uCEFWinControl, uCEFSentinel, uCEFChromiumCore;

const
  MINIBROWSER_SHOWTEXTVIEWER = WM_APP + $101;
  MINIBROWSER_EVALJSCODE     = WM_APP + $102;
  MINIBROWSER_JSBINPARAM     = WM_APP + $103;
  MINIBROWSER_GETSCROLLPOS   = WM_APP + $104;

  MINIBROWSER_CONTEXTMENU_EVALJSCODE   = MENU_ID_USER_FIRST + 1;
  MINIBROWSER_CONTEXTMENU_JSBINPARAM   = MENU_ID_USER_FIRST + 2;
  MINIBROWSER_CONTEXTMENU_GETSCROLLPOS = MENU_ID_USER_FIRST + 3;

  EVAL_JS         = 'JSContextEvalDemo';
  BINARY_PARAM_JS = 'JSBinaryParameter';

type
  TJSEvalFrm = class(TForm)
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    AddressBarPnl: TPanel;
    GoBtn: TButton;
    AddressEdt: TEdit;
    Timer1: TTimer;
    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure GoBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure Chromium1ProcessMessageReceived(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
    procedure Chromium1BeforeContextMenu(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure Chromium1ContextMenuCommand(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer;
      eventFlags: Cardinal; out Result: Boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure Chromium1BeforePopup(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
      targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
      const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var extra_info: ICefDictionaryValue;
      var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser;
      var aAction : TCefCloseBrowserAction);
    procedure Chromium1BeforeClose(Sender: TObject;
      const browser: ICefBrowser);

  private
    { Private declarations }

  protected
    FText : string;
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure BrowserDestroyMsg(var aMessage : TMessage); message CEF_DESTROY;
    procedure ShowTextViewerMsg(var aMessage : TMessage); message MINIBROWSER_SHOWTEXTVIEWER;
    procedure EvalJSCodeMsg(var aMessage : TMessage); message MINIBROWSER_EVALJSCODE;
    procedure GetScrollPosMsg(var aMessage : TMessage); message MINIBROWSER_GETSCROLLPOS;
    procedure EvalJSBinParamMsg(var aMessage : TMessage); message MINIBROWSER_JSBINPARAM;
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
  end;

var
  JSEvalFrm: TJSEvalFrm;

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}

uses
  uSimpleTextViewer, uCefProcessMessage, uCefBinaryValue, uCefMiscFunctions;

// 99.9% of the code in this demo was created by xpert13 and shared in the CEF4Delphi forum.

// Steps to evaluate some JavaScript code using the V8Context
// ----------------------------------------------------------
// 1. Set GlobalCEFApp.OnProcessMessageReceived to RenderProcessHandler_OnProcessMessageReceivedEvent.
// 2. To get the Javascript code in this demo we use a context menu that sends a MINIBROWSER_EVALJSCODE to the form.
// 3. The EvalJSCodeMsg asks for the Javascript code and sends it to the renderer using a process message.
// 4. RenderProcessHandler_OnProcessMessageReceivedEvent receives the process message and calls ParseEvalJsAnswer
//    to evaluate the code.
// 5. ParseEvalJsAnswer evaluates the code and sends a message with the results to the browser process using a
//    process message.
// 6. Chromium1ProcessMessageReceived receives the message, stores the results and sends a
//    MINIBROWSER_SHOWTEXTVIEWER message to the form.
// 7. ShowTextViewerMsg shows the results safely using a SimpleTextViewer.


// This demo also has an example of binary parameters in process messages
// ----------------------------------------------------------------------
// 1. Set GlobalCEFApp.OnProcessMessageReceived to RenderProcessHandler_OnProcessMessageReceivedEvent.
// 2. The context menu has a 'Send JPEG image' option that sends a MINIBROWSER_JSBINPARAM message to the form.
// 3. EvalJSBinParamMsg asks for a JPEG image and sends a process message with a ICefBinaryValue parameter to the
//    renderer process.
// 4. The renderer process parses the binary parameter in the ParseBinaryValue function and sends back the image
//    size and encoded image data to the browser process.
// 5. Chromium1ProcessMessageReceived receives the message, stores the results and sends a
//    MINIBROWSER_SHOWTEXTVIEWER message to the form.
// 6. ShowTextViewerMsg shows the results safely using a SimpleTextViewer.


// About binary parameters
// -----------------------
// There is a size limit in the binary parameters of only a few kilobytes.
// For more info and alternatives read this thread in the official CEF forum :
// http://www.magpcss.org/ceforum/viewtopic.php?f=6&t=10590
//
// Compress the binary data if necessary!


// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE calls TChromium.CloseBrowser which triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy CEFWindowParent1 in the main thread, which triggers the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure TJSEvalFrm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TJSEvalFrm.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TJSEvalFrm.Chromium1BeforeContextMenu(      Sender  : TObject;
                                                const browser : ICefBrowser;
                                                const frame   : ICefFrame;
                                                const params  : ICefContextMenuParams;
                                                const model   : ICefMenuModel);
begin
  model.AddItem(MINIBROWSER_CONTEXTMENU_EVALJSCODE,   'Evaluate JavaScript code...');
  model.AddItem(MINIBROWSER_CONTEXTMENU_GETSCROLLPOS, 'Get vertical scroll position...');
  model.AddItem(MINIBROWSER_CONTEXTMENU_JSBINPARAM,   'Send JPEG image...');
end;

procedure TJSEvalFrm.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings;
  var extra_info: ICefDictionaryValue;
  var noJavascriptAccess: Boolean;
  var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TJSEvalFrm.Chromium1Close(Sender: TObject;
  const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
begin
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  aAction := cbaDelay;
end;

procedure TJSEvalFrm.Chromium1ContextMenuCommand(      Sender     : TObject;
                                                 const browser    : ICefBrowser;
                                                 const frame      : ICefFrame;
                                                 const params     : ICefContextMenuParams;
                                                       commandId  : Integer;
                                                       eventFlags : Cardinal;
                                                 out   Result     : Boolean);
begin
  Result := False;

  case commandId of
    MINIBROWSER_CONTEXTMENU_EVALJSCODE   : PostMessage(Handle, MINIBROWSER_EVALJSCODE, 0, 0);
    MINIBROWSER_CONTEXTMENU_JSBINPARAM   : PostMessage(Handle, MINIBROWSER_JSBINPARAM, 0, 0);
    MINIBROWSER_CONTEXTMENU_GETSCROLLPOS : PostMessage(Handle, MINIBROWSER_GETSCROLLPOS, 0, 0);
  end;
end;

procedure TJSEvalFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
    end;
end;

procedure TJSEvalFrm.FormShow(Sender: TObject);
begin
  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) then Timer1.Enabled := True;
end;

procedure TJSEvalFrm.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(AddressEdt.Text);
end;

procedure TJSEvalFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.UpdateSize;
  AddressBarPnl.Enabled := True;
  GoBtn.Click;
end;

procedure TJSEvalFrm.BrowserDestroyMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.Free;
end;

procedure TJSEvalFrm.ShowTextViewerMsg(var aMessage : TMessage);
begin
  SimpleTextViewerFrm.Memo1.Lines.Text := FText;
  SimpleTextViewerFrm.ShowModal;
end;

procedure TJSEvalFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TJSEvalFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TJSEvalFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TJSEvalFrm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TJSEvalFrm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

procedure TJSEvalFrm.EvalJSCodeMsg(var aMessage : TMessage);
var
  TempMsg    : ICefProcessMessage;
  TempScript : string;
begin
  TempScript := InputBox('JSEval demo', 'Please type some JavaScript code', 'document.title;');

  if (length(TempScript) > 0) then
    begin
      TempMsg := TCefProcessMessageRef.New(EVAL_JS);

      if TempMsg.ArgumentList.SetString(0, TempScript) then
        Chromium1.SendProcessMessage(PID_RENDERER, TempMsg);
    end;
end;

procedure TJSEvalFrm.GetScrollPosMsg(var aMessage : TMessage);
var
  TempMsg : ICefProcessMessage;
begin
  TempMsg := TCefProcessMessageRef.New(EVAL_JS);

  if TempMsg.ArgumentList.SetString(0, 'window.pageYOffset') then
    Chromium1.SendProcessMessage(PID_RENDERER, TempMsg);
end;

procedure TJSEvalFrm.EvalJSBinParamMsg(var aMessage : TMessage);
var
  TempMsg        : ICefProcessMessage;
  TempOpenDialog : TOpenDialog;
  TempStream     : TFileStream;
  TempBinValue   : ICefBinaryValue;
  TempBuffer     : TBytes;
  TempSize       : NativeUInt;
  TempPointer    : pointer;
begin
  TempOpenDialog := nil;
  TempStream     := nil;

  try
    try
      TempOpenDialog         := TOpenDialog.Create(nil);
      TempOpenDialog.Options := TempOpenDialog.Options + [ofFileMustExist];
      TempOpenDialog.Filter  := 'JPEG files (*.jpg)|*.JPG';

      if TempOpenDialog.Execute then
        begin
          TempStream := TFileStream.Create(TempOpenDialog.FileName, fmOpenRead);
          TempSize   := TempStream.Size;

          if (TempSize > 0) then
            begin
              SetLength(TempBuffer, TempSize);
              TempSize := TempStream.Read(TempBuffer[0], TempSize);

              if (TempSize > 0) then
                begin
                  TempPointer  := @TempBuffer[0];
                  TempBinValue := TCefBinaryValueRef.New(TempPointer, TempSize);
                  TempMsg      := TCefProcessMessageRef.New(BINARY_PARAM_JS);

                  if TempMsg.ArgumentList.SetBinary(0, TempBinValue) then
                    Chromium1.SendProcessMessage(PID_RENDERER, TempMsg);
                end;
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TJSEvalFrm.EvalJSBinParamMsg', e) then raise;
    end;
  finally
    if (TempOpenDialog <> nil) then FreeAndNil(TempOpenDialog);
    if (TempStream     <> nil) then FreeAndNil(TempStream);
    SetLength(TempBuffer, 0);
  end;
end;

procedure ParseEvalJsAnswer(const pMessage     : ICefProcessMessage;
                            const pBrowser     : ICefBrowser;
                            const pFrame       : ICefFrame;
                            const pReturnValue : ICefv8Value;
                            const pException   : ICefV8Exception);
var
  pAnswer       : ICefProcessMessage;
  strResult     : String;
  bGoodDataType : Boolean;
begin
  pAnswer := TCefProcessMessageRef.New(EVAL_JS);

  if (pReturnValue = nil) or not(pReturnValue.IsValid) then
    begin
      pAnswer.ArgumentList.SetBool(0, false);
      pAnswer.ArgumentList.SetString(1, pException.Message);
    end
   else
    begin
      bGoodDataType := True;

      if      pReturnValue.IsString then strResult := pReturnValue.GetStringValue
      else if pReturnValue.IsBool   then strResult := BoolToStr(pReturnValue.GetBoolValue)
      else if pReturnValue.IsInt    then strResult := IntToStr(pReturnValue.GetIntValue)
      else if pReturnValue.IsUInt   then strResult := IntToStr(pReturnValue.GetUIntValue)
      else if pReturnValue.IsDouble then strResult := FloatToStr(pReturnValue.GetDoubleValue)
      else bGoodDataType := False;

      if bGoodDataType then
        begin
          pAnswer.ArgumentList.SetBool(0, true);
          pAnswer.ArgumentList.SetString(1, strResult);
        end
       else
        begin
          pAnswer.ArgumentList.SetBool(0, false);
          pAnswer.ArgumentList.SetString(1, 'Result data type need to be string, int, uint or double!');
        end;
    end;

  if (pFrame <> nil) and pFrame.IsValid then
    pFrame.SendProcessMessage(PID_BROWSER, pAnswer);
end;

procedure ParseBinaryValue(const pBrowser : ICefBrowser; const pFrame : ICefFrame; const aBinaryValue : ICefBinaryValue);
var
  pAnswer     : ICefProcessMessage;
  TempBuffer  : TBytes;
  TempPointer : pointer;
  TempSize    : NativeUInt;
  TempString  : string;
begin
  if (aBinaryValue = nil) then exit;

  try
    try
      TempSize := aBinaryValue.GetSize;

      if (TempSize > 0) then
        begin
          SetLength(TempBuffer, TempSize);
          TempPointer := @TempBuffer[0];
          TempSize    := aBinaryValue.GetData(TempPointer, TempSize, 0);

          if (TempSize > 0) then
            begin
              pAnswer    := TCefProcessMessageRef.New(BINARY_PARAM_JS);
              TempString := 'Image size : ' + inttostr(TempSize) + #13 + #10 +
                            'Encoded image : ' + EncodeBase64(TempPointer, TempSize);

              if (pFrame <> nil) and pFrame.IsValid and pAnswer.ArgumentList.SetString(0, TempString) then
                pFrame.SendProcessMessage(PID_BROWSER, pAnswer);
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('ParseBinaryValue', e) then raise;
    end;
  finally
    SetLength(TempBuffer, 0);
  end;
end;

procedure RenderProcessHandler_OnProcessMessageReceivedEvent(const pBrowser       : ICefBrowser;
                                                             const pFrame         : ICefFrame;
                                                                   uSourceProcess : TCefProcessId;
                                                             const pMessage       : ICefProcessMessage;
                                                             var   aHandled       : boolean);
var
  pV8Context   : ICefv8Context;
  pReturnValue : ICefv8Value;
  pException   : ICefV8Exception;
  TempScript   : string;
  TempBinValue : ICefBinaryValue;
begin
  aHandled := False;

  if (pMessage = nil) or (pMessage.ArgumentList = nil) then exit;

  if (pMessage.Name = EVAL_JS) then
    begin
      TempScript := pMessage.ArgumentList.GetString(0);

      if (length(TempScript) > 0) and (pFrame <> nil) and pFrame.IsValid then
        begin
          pV8Context := pFrame.GetV8Context;

          if pV8Context.Enter then
            begin
              pV8Context.Eval(TempScript, '', 1, pReturnValue, pException);
              ParseEvalJsAnswer(pMessage, pBrowser, pFrame, pReturnValue, pException);
              pV8Context.Exit;
            end;
        end;

      aHandled := True;
    end
   else
    if (pMessage.Name = BINARY_PARAM_JS) then
      begin
        TempBinValue := pMessage.ArgumentList.GetBinary(0);
        ParseBinaryValue(pBrowser, pFrame, TempBinValue);
        aHandled := True;
      end;
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                          := TCefApplication.Create;
  GlobalCEFApp.OnProcessMessageReceived := RenderProcessHandler_OnProcessMessageReceivedEvent;
end;

procedure TJSEvalFrm.Chromium1ProcessMessageReceived(      Sender        : TObject;
                                                     const browser       : ICefBrowser;
                                                     const frame         : ICefFrame;
                                                           sourceProcess : TCefProcessId;
                                                     const message       : ICefProcessMessage;
                                                     out   Result        : Boolean);
begin
  Result := False;

  if (message = nil) or (message.ArgumentList = nil) then exit;

  if (message.Name = EVAL_JS) then
    begin
      FText := message.ArgumentList.GetString(1);
      PostMessage(Handle, MINIBROWSER_SHOWTEXTVIEWER, 0, 0);
      Result := True;
    end
   else
    if (message.Name = BINARY_PARAM_JS) then
      begin
        FText := message.ArgumentList.GetString(0);
        PostMessage(Handle, MINIBROWSER_SHOWTEXTVIEWER, 0, 0);
        Result := True;
      end;
end;

end.
