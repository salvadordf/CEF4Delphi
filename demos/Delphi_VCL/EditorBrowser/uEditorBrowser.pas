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

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ToolWin, Vcl.ComCtrls,
  System.ImageList, Vcl.ImgList,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ToolWin, ComCtrls,
  ImageList, ImgList,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes,
  uCEFWinControl, uCEFSentinel, uCEFChromiumCore;

const
  MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS = MENU_ID_USER_FIRST + 1;

type
  TEditorBrowserFrm = class(TForm)
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

    procedure Timer1Timer(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1LoadEnd(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
    procedure Chromium1TextResultAvailable(Sender: TObject;  const aText: ustring);
    procedure Chromium1BeforeContextMenu(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure Chromium1ContextMenuCommand(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;const params: ICefContextMenuParams; commandId: Integer; eventFlags: Cardinal; out Result: Boolean);

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
  EditorBrowserFrm: TEditorBrowserFrm;

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}

uses
  uCEFApplication, uCefMiscFunctions, uImageSelection;

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
  GlobalCEFApp                  := TCefApplication.Create;
  //GlobalCEFApp.LogFile          := 'cef.log';
  //GlobalCEFApp.LogSeverity      := LOGSEVERITY_VERBOSE;
end;

procedure TEditorBrowserFrm.FillColorBtnClick(Sender: TObject);
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

procedure TEditorBrowserFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
    end;
end;

procedure TEditorBrowserFrm.FormCreate(Sender: TObject);
begin
  FCanClose := False;
  FClosing  := False;

  Chromium1.DefaultURL := 'file:///EditorBrowser.html';
end;

procedure TEditorBrowserFrm.FormShow(Sender: TObject);
begin
  // You *MUST* call CreateBrowser to create and initialize the browser.
  // This will trigger the AfterCreated event when the browser is fully
  // initialized and ready to receive commands.

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) then Timer1.Enabled := True;
end;

procedure TEditorBrowserFrm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can send a message to the main form to load the initial web page.
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TEditorBrowserFrm.Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TEditorBrowserFrm.Chromium1BeforeContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  model.AddItem(MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS, 'Show DevTools');
end;

procedure TEditorBrowserFrm.Chromium1Close(Sender: TObject;
  const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
begin
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  aAction := cbaDelay;
end;

procedure TEditorBrowserFrm.Chromium1ContextMenuCommand(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: Cardinal; out Result: Boolean);
var
  TempPoint : TPoint;
begin
  Result := False;

  case commandId of
    MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS :
      begin
        TempPoint.x := params.XCoord;
        TempPoint.y := params.YCoord;
        Chromium1.ShowDevTools(TempPoint, nil);
      end;
  end;
end;

procedure TEditorBrowserFrm.Chromium1LoadEnd(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  httpStatusCode: Integer);
begin
  if (frame <> nil) and (not(frame.IsValid) or not(frame.isMain)) then exit;

  // Enable the "designMode" for all loaded files to edit them
  EnableDesignMode;
end;

procedure TEditorBrowserFrm.Chromium1TextResultAvailable(Sender: TObject; const aText: ustring);
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

procedure TEditorBrowserFrm.TextColorBtnClick(Sender: TObject);
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

procedure TEditorBrowserFrm.EnableDesignMode;
var
  TempCode : string;
begin
  TempCode := 'document.designMode = "on";';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TEditorBrowserFrm.AlignCenterBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("justifyCenter", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TEditorBrowserFrm.AlignJustifyBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("justifyFull", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TEditorBrowserFrm.AlignLeftBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("justifyLeft", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TEditorBrowserFrm.AlignRightBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("justifyRight", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TEditorBrowserFrm.BoldBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("bold", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TEditorBrowserFrm.ImageBtnClick(Sender: TObject);
var
  TempCode   : string;
  TempHTML   : string;
  TempURL    : string;
  TempPath   : string;
  TempStream : TFileStream;
  TempBuffer : TBytes;
  TempSize   : NativeUInt;
begin
  TempStream := nil;
  TempBuffer := nil;

  ImageSelectionFrm.ShowModal;

  if (ImageSelectionFrm.ModalResult <> mrOk) then exit;

  if ImageSelectionFrm.RemoteRb.Checked then
    begin
      TempURL := trim(ImageSelectionFrm.URLEdt.Text);

      if (length(TempURL) > 0) then
        begin
          TempCode := 'document.execCommand("insertImage", false, "' + TempURL + '");';
          Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
        end;
    end
   else
    begin
      TempPath := trim(ImageSelectionFrm.FileEdt.Text);

      if (length(TempPath) > 0) and FileExists(TempPath) then
        try
          try
            TempStream := TFileStream.Create(TempPath, fmOpenRead);
            TempSize   := TempStream.Size;

            if (TempSize > 0) then
              begin
                SetLength(TempBuffer, TempSize);
                TempSize := TempStream.Read(TempBuffer[0], TempSize);

                if (TempSize > 0) then
                  begin
                    TempHTML := '<img src=' + quotedstr(CefGetDataURI(@TempBuffer[0], TempSize, 'image/png')) + '>';
                    TempCode := 'document.execCommand("insertHTML", false, "' + TempHTML + '");';
                    Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
                  end;
              end;
          except
            on e : exception do
              if CustomExceptionHandler('TEditorBrowserFrm.ImageBtnClick', e) then raise;
          end;
        finally
          if (TempStream <> nil) then FreeAndNil(TempStream);
          SetLength(TempBuffer, 0);
        end;
    end;
end;

procedure TEditorBrowserFrm.IndentBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("indent", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TEditorBrowserFrm.ItalicBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("italic", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TEditorBrowserFrm.LinkBtnClick(Sender: TObject);
var
  TempCode, TempURL : string;
begin
  // TODO: Replace InputBox
  TempURL  := inputbox('Type the URL used in the link', 'URL : ', 'https://www.briskbard.com');
  TempCode := 'document.execCommand("createLink", false, "' + TempURL + '");';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TEditorBrowserFrm.SaveBtnClick(Sender: TObject);
begin
  Chromium1.RetrieveHTML;
end;

procedure TEditorBrowserFrm.StrikethroughBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("strikeThrough", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TEditorBrowserFrm.UnderlineBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("underline", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TEditorBrowserFrm.UnorderedListBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("insertUnorderedList", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TEditorBrowserFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  Caption := 'Editor Browser';
end;

procedure TEditorBrowserFrm.BrowserDestroyMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.Free;
end;

procedure TEditorBrowserFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TEditorBrowserFrm.OpenBtnClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'HTML Files (*.html)|*.HTML';

  if OpenDialog1.Execute then
    Chromium1.LoadURL('file:///' + OpenDialog1.FileName); // TODO: The URL should be encoded
end;

procedure TEditorBrowserFrm.OrderedListBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("insertOrderedList", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TEditorBrowserFrm.OutdentBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("outdent", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TEditorBrowserFrm.RemoveFormatBtnClick(Sender: TObject);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("removeFormat", false, null);';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TEditorBrowserFrm.NewBtnClick(Sender: TObject);
begin
  // TODO: Before clearing the document we should notify the user if the document has unsaved changes
  Chromium1.LoadURL('about:blank');
  EnableDesignMode;
end;

procedure TEditorBrowserFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TEditorBrowserFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TEditorBrowserFrm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TEditorBrowserFrm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

end.
