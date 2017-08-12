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
//        Copyright © 2017 Salvador Díaz Fau. All rights reserved.
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
  System.UITypes,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Menus,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Types, ComCtrls, ClipBrd,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes, uCEFConstants;

const
  MINIBROWSER_CREATED        = WM_APP + $100;
  MINIBROWSER_SHOWTEXTVIEWER = WM_APP + $101;
  MINIBROWSER_EVALJSCODE     = WM_APP + $102;

  MINIBROWSER_CONTEXTMENU_EVALJSCODE     = MENU_ID_USER_FIRST + 1;

  EVAL_JS = 'JSContextEvalDemo';

type
  TJSEvalFrm = class(TForm)
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    AddressBarPnl: TPanel;
    GoBtn: TButton;
    AddressEdt: TEdit;
    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure GoBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure Chromium1ProcessMessageReceived(Sender: TObject;
      const browser: ICefBrowser; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
    procedure Chromium1BeforeContextMenu(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure Chromium1ContextMenuCommand(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer;
      eventFlags: Cardinal; out Result: Boolean);

  private
    { Private declarations }

  protected
    FText : string;

    procedure BrowserCreatedMsg(var aMessage : TMessage); message MINIBROWSER_CREATED;
    procedure ShowTextViewerMsg(var aMessage : TMessage); message MINIBROWSER_SHOWTEXTVIEWER;
    procedure EvalJSCodeMsg(var aMessage : TMessage); message MINIBROWSER_EVALJSCODE;
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;

    procedure ParseEvalJsAnswer(const pMessage: ICefProcessMessage; pBrowser: ICefBrowser; pReturnValue : ICefv8Value; pException : ICefV8Exception);

  public
    procedure RenderProcessHandler_OnCustomMessage(const pBrowser: ICefBrowser; uSourceProcess: TCefProcessId; const pMessage: ICefProcessMessage);
  end;

var
  JSEvalFrm: TJSEvalFrm;

implementation

{$R *.dfm}

uses
  uSimpleTextViewer, uCefProcessMessage;

// 99.9% of the code in this demo was created by xpert13 and shared in the CEF4Delphi forum.

// Steps to evaluate some JavaScript code using the V8Context
// ----------------------------------------------------------
// 1. Create a TCefCustomRenderProcessHandler in the DPR file, set a message name and the OnCustomMessage event.
// 2. Set the TCefCustomRenderProcessHandler in the GlobalCEFApp.RenderProcessHandler property.
// 3. To get the Javascript code in this demo we use a context menu that sends a MINIBROWSER_EVALJSCODE to the form.
// 4. The EvalJSCodeMsg asks for the Javascript code and sends it to the renderer using a process message.
// 5. RenderProcessHandler_OnCustomMessage receives the process message and calls ParseEvalJsAnswer to evaluate the code
// 6. ParseEvalJsAnswer evaluates the code and sends a message with the results to the browser process using a process message.
// 7. Chromium1ProcessMessageReceived receives the message, stores the results and sends a MINIBROWSER_SHOWTEXTVIEWER
//    message to the form.
// 8. ShowTextViewerMsg shows the results safely using a SimpleTextViewer.

procedure TJSEvalFrm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, MINIBROWSER_CREATED, 0, 0);
end;

procedure TJSEvalFrm.Chromium1BeforeContextMenu(Sender : TObject;
                                                const browser : ICefBrowser;
                                                const frame   : ICefFrame;
                                                const params  : ICefContextMenuParams;
                                                const model   : ICefMenuModel);
begin
  model.AddItem(MINIBROWSER_CONTEXTMENU_EVALJSCODE, 'Evaluate JavaScript code...');
end;

procedure TJSEvalFrm.Chromium1ContextMenuCommand(Sender : TObject;
                                                 const browser    : ICefBrowser;
                                                 const frame      : ICefFrame;
                                                 const params     : ICefContextMenuParams;
                                                       commandId  : Integer;
                                                       eventFlags : Cardinal;
                                                 out   Result     : Boolean);
begin
  Result := False;

  case commandId of
    MINIBROWSER_CONTEXTMENU_EVALJSCODE : PostMessage(Handle, MINIBROWSER_EVALJSCODE, 0, 0);
  end;
end;

procedure TJSEvalFrm.FormShow(Sender: TObject);
begin
  Chromium1.CreateBrowser(CEFWindowParent1, '');
end;

procedure TJSEvalFrm.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(AddressEdt.Text);
end;

procedure TJSEvalFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  AddressBarPnl.Enabled := True;
  GoBtn.Click;
end;

procedure TJSEvalFrm.ShowTextViewerMsg(var aMessage : TMessage);
begin
  SimpleTextViewerFrm.Memo1.Lines.Text := FText;
  SimpleTextViewerFrm.ShowModal;
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

procedure TJSEvalFrm.EvalJSCodeMsg(var aMessage : TMessage);
var
  TempMsg    : ICefProcessMessage;
  TempScript : string;
begin
  TempScript := InputBox('JSEval demo', 'Please type some JavaScript code', 'document.title;');

  if (length(TempScript) > 0) then
    begin
      TempMsg := TCefProcessMessageRef.New(EVAL_JS);
      TempMsg.ArgumentList.SetString(0, TempScript);
      Chromium1.SendProcessMessage(PID_RENDERER, TempMsg);
    end;
end;

procedure TJSEvalFrm.RenderProcessHandler_OnCustomMessage(const pBrowser       : ICefBrowser;
                                                                uSourceProcess : TCefProcessId;
                                                          const pMessage       : ICefProcessMessage);
var
  pV8Context   : ICefv8Context;
  pReturnValue : ICefv8Value;
  pException   : ICefV8Exception;
  TempScript   : string;
begin
  if (pMessage = nil) or (pMessage.ArgumentList = nil) then exit;

  if (pMessage.Name = EVAL_JS) then
  begin
    TempScript := pMessage.ArgumentList.GetString(0);

    if (length(TempScript) > 0) then
      begin
        pV8Context := pBrowser.MainFrame.GetV8Context;

        if pV8Context.Enter then
          begin
            pV8Context.Eval(TempScript, '', 1, pReturnValue, pException);
            ParseEvalJsAnswer(pMessage, pBrowser, pReturnValue, pException);
            pV8Context.Exit;
          end;
      end;
  end;
end;

procedure TJSEvalFrm.ParseEvalJsAnswer(const pMessage     : ICefProcessMessage;
                                             pBrowser     : ICefBrowser;
                                             pReturnValue : ICefv8Value;
                                             pException   : ICefV8Exception);
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

  pBrowser.SendProcessMessage(PID_BROWSER, pAnswer);
end;

procedure TJSEvalFrm.Chromium1ProcessMessageReceived(Sender : TObject;
                                                     const browser       : ICefBrowser;
                                                           sourceProcess : TCefProcessId;
                                                     const message       : ICefProcessMessage;
                                                     out   Result        : Boolean);
begin
  if (message = nil) or (message.ArgumentList = nil) then exit;

  if (message.Name = EVAL_JS) then
    begin
      FText := message.ArgumentList.GetString(1);
      PostMessage(Handle, MINIBROWSER_SHOWTEXTVIEWER, 0, 0);
      Result := True;
    end
   else
    Result := False;
end;

end.
