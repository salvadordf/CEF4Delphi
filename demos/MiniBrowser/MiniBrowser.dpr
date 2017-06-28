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


program MiniBrowser;

{$I cef.inc}

uses
  {$IFDEF DELPHI16_UP}
  Vcl.Forms, System.SysUtils,
  {$ELSE}
  Forms, SysUtils,
  {$ENDIF}
  uCEFApplication,
  uCEFMiscFunctions,
  uCEFSchemeRegistrar,
  uCEFRenderProcessHandler,
  uCEFv8Handler,
  uCEFInterfaces,
  uCEFDomVisitor,
  uCEFDomNode,
  uCEFConstants,
  uCEFTypes,
  uCEFTask,
  uMiniBrowser in 'uMiniBrowser.pas' {MiniBrowserFrm},
  uTestExtension in 'uTestExtension.pas',
  uHelloScheme in 'uHelloScheme.pas',
  uPreferences in 'uPreferences.pas' {PreferencesFrm};

{$R *.res}

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

var
  TempProcessHandler : TCefCustomRenderProcessHandler;

procedure SimpleDOMIteration(const aDocument: ICefDomDocument);
var
  TempHead, TempChild : ICefDomNode;
begin
  try
    if (aDocument <> nil) then
      begin
        TempHead := aDocument.Head;

        if (TempHead <> nil) then
          begin
            TempChild := TempHead.FirstChild;

            while (TempChild <> nil) do
              begin
                CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, 'Head child element : ' + TempChild.Name);
                TempChild := TempChild.NextSibling;
              end;
          end;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('SimpleDOMIteration', e) then raise;
  end;
end;

procedure SimpleNodeSearch(const aDocument: ICefDomDocument);
const
  NODE_ID = 'lst-ib';
var
  TempNode : ICefDomNode;
begin
  try
    if (aDocument <> nil) then
      begin
        TempNode := aDocument.GetElementById(NODE_ID);

        if (TempNode <> nil) then
          CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, NODE_ID + ' element name : ' + TempNode.Name);

        TempNode := aDocument.GetFocusedNode;

        if (TempNode <> nil) then
          CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, 'Focused element name : ' + TempNode.Name);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('SimpleNodeSearch', e) then raise;
  end;
end;

procedure DOMVisitor_OnDocAvailable(const document: ICefDomDocument);
begin
  // This function is called from a different process.
  // document is only valid inside this function.
  // As an example, this function only writes the document title to the 'debug.log' file.
  CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, 'document.Title : ' + document.Title);

  // Simple DOM iteration example
  SimpleDOMIteration(document);

  // Simple DOM searches
  SimpleNodeSearch(document);
end;

procedure ProcessHandler_OnCustomMessage(const browser: ICefBrowser; sourceProcess: TCefProcessId; const message: ICefProcessMessage);
var
  TempFrame : ICefFrame;
  TempVisitor : TCefFastDomVisitor;
begin
  if (browser <> nil) then
    begin
      TempFrame := browser.MainFrame;

      if (TempFrame <> nil) then
        begin
          TempVisitor := TCefFastDomVisitor.Create(DOMVisitor_OnDocAvailable);
          TempFrame.VisitDom(TempVisitor);
        end;
    end;
end;

procedure ProcessHandler_OnWebKitReady;
begin
{$IFDEF DELPHI14_UP}
  // Registering the extension. Read this document for more details :
  // https://bitbucket.org/chromiumembedded/cef/wiki/JavaScriptIntegration.md
  TCefRTTIExtension.Register('myextension', TTestExtension);
{$ENDIF}
end;

procedure GlobalCEFApp_OnRegCustomSchemes(const registrar: TCefSchemeRegistrarRef);
begin
  registrar.AddCustomScheme('hello', True, True, False, False, False, False);
end;

begin
  // This ProcessHandler is used for the extension and the DOM visitor demos.
  // It can be removed if you don't want those features.
  TempProcessHandler                 := TCefCustomRenderProcessHandler.Create;
  TempProcessHandler.MessageName     := 'retrievedom';   // same message name than TMiniBrowserFrm.VisitDOMMsg
  TempProcessHandler.OnCustomMessage := ProcessHandler_OnCustomMessage;
  TempProcessHandler.OnWebKitReady   := ProcessHandler_OnWebKitReady;

  GlobalCEFApp                      := TCefApplication.Create;
  GlobalCEFApp.RemoteDebuggingPort  := 9000;
  GlobalCEFApp.RenderProcessHandler := TempProcessHandler as ICefRenderProcessHandler;
  GlobalCEFApp.OnRegCustomSchemes   := GlobalCEFApp_OnRegCustomSchemes;

  // In case you want to use custom directories for the CEF3 binaries, cache, cookies and user data.
{
  GlobalCEFApp.FrameworkDirPath     := 'cef';
  GlobalCEFApp.ResourcesDirPath     := 'cef';
  GlobalCEFApp.LocalesDirPath       := 'cef\locales';
  GlobalCEFApp.cache                := 'cef\cache';
  GlobalCEFApp.cookies              := 'cef\cookies';
  GlobalCEFApp.UserDataPath         := 'cef\User Data';
}

  // Enabling the debug log file for then DOM visitor demo.
  // This adds lots of warnings to the console, specially if you run this inside VirtualBox.
  // Remove it if you don't want to use the DOM visitor
  GlobalCEFApp.LogFile              := 'debug.log';
  GlobalCEFApp.LogSeverity          := LOGSEVERITY_ERROR;

  // Examples of command line switches.
  // **********************************
  //
  // Uncomment the following line to see an FPS counter in the browser.
  //GlobalCEFApp.AddCustomCommandLine('--show-fps-counter');
  //
  // Uncomment the following line to change the user agent string.
  //GlobalCEFApp.AddCustomCommandLine('--user-agent', 'MiniBrowser');

  if GlobalCEFApp.StartMainProcess then
    begin
      // You can register the Scheme Handler Factory here or later, for example in a context menu command.
      CefRegisterSchemeHandlerFactory('hello', '', THelloScheme);

      Application.Initialize;
      {$IFDEF DELPHI11_UP}
      Application.MainFormOnTaskbar := True;
      {$ENDIF}
      Application.CreateForm(TMiniBrowserFrm, MiniBrowserFrm);
      Application.CreateForm(TPreferencesFrm, PreferencesFrm);
      Application.Run;
    end;

  GlobalCEFApp.Free;
end.
