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

unit uEditorBrowser;

{$MODE Delphi}

{$I cef.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ToolWin, ComCtrls,
  ImgList,
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes,
  uCEFWinControl, uCEFSentinel;

type

  { TForm1 }

  TForm1 = class(TForm)
    Timer1: TTimer;
    Chromium1: TChromium;
    CEFWindowParent1: TCEFWindowParent;
    ToolBar1: TToolBar;
    SaveBtn: TToolButton;
    ImageList1: TImageList;
    Separator1: TToolButton;
    BoldBtn: TToolButton;
    ItalicBtn: TToolButton;
    UnderlineBtn: TToolButton;
    StrikethroughBtn: TToolButton;
    Separator2: TToolButton;
    AlignLeftBtn: TToolButton;
    AlignCenterBtn: TToolButton;
    AlignRightBtn: TToolButton;
    OpenBtn: TToolButton;
    NewBtn: TToolButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Separator3: TToolButton;
    LinkBtn: TToolButton;
    ImageBtn: TToolButton;
    AlignJustifyBtn: TToolButton;
    Separator4: TToolButton;
    UnorderedListBtn: TToolButton;
    OrderedListBtn: TToolButton;
    ColorDialog1: TColorDialog;
    Separator5: TToolButton;
    IndentBtn: TToolButton;
    TextColorBtn: TToolButton;
    FillColorBtn: TToolButton;
    Separator6: TToolButton;
    RemoveFormatBtn: TToolButton;
    OutdentBtn: TToolButton;
    Separator7: TToolButton;

    procedure CEFSentinel1Close(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1LoadEnd(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
    procedure Chromium1TextResultAvailable(Sender: TObject;  const aText: ustring);

    procedure BoldBtnClick(Sender: TObject);
    procedure ItalicBtnClick(Sender: TObject);
    procedure UnderlineBtnClick(Sender: TObject);
    procedure StrikethroughBtnClick(Sender: TObject);
    procedure AlignLeftBtnClick(Sender: TObject);
    procedure AlignCenterBtnClick(Sender: TObject);
    procedure AlignRightBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure NewBtnClick(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure LinkBtnClick(Sender: TObject);
    procedure ImageBtnClick(Sender: TObject);
    procedure AlignJustifyBtnClick(Sender: TObject);
    procedure UnorderedListBtnClick(Sender: TObject);
    procedure OrderedListBtnClick(Sender: TObject);
    procedure IndentBtnClick(Sender: TObject);
    procedure TextColorBtnClick(Sender: TObject);
    procedure FillColorBtnClick(Sender: TObject);
    procedure RemoveFormatBtnClick(Sender: TObject);
    procedure OutdentBtnClick(Sender: TObject);

  protected
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    procedure EnableDesignMode;

    // You have to handle this two messages to call NotifyMoveOrResizeStarted or some page elements will be misaligned.
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    // You also have to handle these two messages to set GlobalCEFApp.OsmodalLoop
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;

    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure BrowserDestroyMsg(var aMessage : TMessage); message CEF_DESTROY;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

procedure CreateGlobalCEFApp;

implementation

{$R *.lfm}

uses
  uCEFApplication, uCEFMiscFunctions;

// This demo shows how to create a simple editor using a browser.

// It's possible to add many more editor commands available with the JavaScript function called 'execCommand'
// https://developer.mozilla.org/en-US/docs/Web/API/Document/execCommand

// There are several TODO comments with some missing features that all editors should have

// This demo includes some icons from "Material Design Icons", made by Google ( https://github.com/google/material-design-icons )

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE calls TChromium.CloseBrowser which triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy CEFWindowParent1 in the main thread, which triggers the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                      := TCefApplication.Create;
  //GlobalCEFApp.LogFile          := 'cef.log';
  //GlobalCEFApp.LogSeverity      := LOGSEVERITY_VERBOSE;
end;

procedure TForm1.FillColorBtnClick(Sender: TObject);
var
  TempCode, TempHexColor : string;
begin
  if ColorDialog1.execute then
    begin
      TempHexColor := '#' + IntToHex(GetRValue(ColorDialog1.Color), 2) +
                            IntToHex(GetGValue(ColorDialog1.Color), 2) +
                            IntToHex(GetBValue(ColorDialog1.Color), 2);

      TempCode     := 'document.execCommand("backColor", false, "' + TempHexColor + '");';

      Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
    end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCanClose := False;
  FClosing  := False;

  Chromium1.DefaultURL := 'file:///EditorBrowser.html';
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // You *MUST* call CreateBrowser to create and initialize the browser.
  // This will trigger the AfterCreated event when the browser is fully
  // initialized and ready to receive commands.

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) then Timer1.Enabled := True;
end;

procedure TForm1.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can send a message to the main form to load the initial web page.
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TForm1.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TForm1.Chromium1Close(Sender: TObject;
  const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
begin
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  aAction := cbaDelay;
end;

procedure TForm1.Chromium1LoadEnd(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  httpStatusCode: Integer);
begin
  if (frame <> nil) and (not(frame.IsValid) or not(frame.isMain)) then exit;

  // Enable the "designMode" for all loaded files to edit them
  EnableDesignMode;
end;

procedure TForm1.Chromium1TextResultAvailable(Sender: TObject; const aText: ustring);
var
  TempLines : TStringList;
begin
  // TODO: This function should notify the user if an existing file is replaced

  TempLines              := nil;
  SaveDialog1.DefaultExt := '.html';
  SaveDialog1.Filter     := 'HTML Files (*.html)|*.HTML';

  if SaveDialog1.Execute then
    try
      try
        TempLines      := TStringList.Create;
        TempLines.Text := aText;
        TempLines.SaveToFile(SaveDialog1.FileName);
      except
        on e : exception do
          if CustomExceptionHandler('TForm1.Chromium1TextResultAvailable', e) then raise;
      end;
    finally
      if (TempLines <> nil) then FreeAndNil(TempLines);
    end;
end;

procedure TForm1.TextColorBtnClick(Sender: TObject);
var
  TempCode, TempHexColor : string;
begin
  if ColorDialog1.execute then
    begin
      TempHexColor := '#' + IntToHex(GetRValue(ColorDialog1.Color), 2) +
                            IntToHex(GetGValue(ColorDialog1.Color), 2) +
                            IntToHex(GetBValue(ColorDialog1.Color), 2);

      TempCode     := 'document.execCommand("foreColor", false, "' + TempHexColor + '");';

      Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
    end;
end;

procedure TForm1.EnableDesignMode;
var
  TempCode : string;
begin
  TempCode := 'document.designMode = "on";';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TForm1.AlignCenterBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("justifyCenter", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TForm1.AlignJustifyBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("justifyFull", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TForm1.AlignLeftBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("justifyLeft", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TForm1.AlignRightBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("justifyRight", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TForm1.BoldBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("bold", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TForm1.ImageBtnClick(Sender: TObject);
var
  TempCode, TempURL : string;
begin
  // TODO: Replace InputBox
  TempURL  := inputbox('Type the URL used in the image', 'URL : ', 'https://www.briskbard.com/images/logo5.png');
  TempCode := 'document.execCommand("insertImage", false, "' + TempURL + '");';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TForm1.IndentBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("indent", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TForm1.ItalicBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("italic", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TForm1.LinkBtnClick(Sender: TObject);
var
  TempCode, TempURL : string;
begin
  // TODO: Replace InputBox
  TempURL  := inputbox('Type the URL used in the link', 'URL : ', 'https://www.briskbard.com');
  TempCode := 'document.execCommand("createLink", false, "' + TempURL + '");';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TForm1.SaveBtnClick(Sender: TObject);
begin
  Chromium1.RetrieveHTML;
end;

procedure TForm1.StrikethroughBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("strikeThrough", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TForm1.UnderlineBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("underline", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TForm1.UnorderedListBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("insertUnorderedList", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TForm1.BrowserCreatedMsg(var aMessage : TMessage);
begin
  Caption := 'Editor Browser';
end;

procedure TForm1.BrowserDestroyMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TForm1.CEFSentinel1Close(Sender: TObject);
begin

end;

procedure TForm1.OpenBtnClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'HTML Files (*.html)|*.HTML';

  if OpenDialog1.Execute then
    Chromium1.LoadURL('file:///' + OpenDialog1.FileName); // TODO: The URL should be encoded
end;

procedure TForm1.OrderedListBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("insertOrderedList", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TForm1.OutdentBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("outdent", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TForm1.RemoveFormatBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("removeFormat", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TForm1.NewBtnClick(Sender: TObject);
begin
  // TODO: Before clearing the document we should notify the user if the document has unsaved changes
  Chromium1.LoadURL('about:blank');
  EnableDesignMode;
end;

procedure TForm1.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TForm1.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

end.
