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
  Vcl.Forms,
  {$ELSE}
  Forms,
  {$ENDIF }
  uCEFApplication,
  uCEFMiscFunctions,
  uCEFSchemeRegistrar,
  uMiniBrowser in 'uMiniBrowser.pas' {MiniBrowserFrm},
  uTestExtension in 'uTestExtension.pas',
  uHelloScheme in 'uHelloScheme.pas',
  uPreferences in 'uPreferences.pas' {PreferencesFrm};

{$R *.res}

procedure GlobalCEFApp_OnRegCustomSchemes(const registrar: TCefSchemeRegistrarRef);
begin
  registrar.AddCustomScheme('hello', True, True, False, False, False);
end;

begin
  GlobalCEFApp                      := TCefApplication.Create;
  GlobalCEFApp.RemoteDebuggingPort  := 9000;
  GlobalCEFApp.RenderProcessHandler := TCustomRenderProcessHandler.Create;
  GlobalCEFApp.OnRegCustomSchemes   := GlobalCEFApp_OnRegCustomSchemes;

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
      CefRegisterSchemeHandlerFactory('hello', '', THelloScheme);

      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TMiniBrowserFrm, MiniBrowserFrm);
      Application.CreateForm(TPreferencesFrm, PreferencesFrm);
      Application.Run;
    end;

  GlobalCEFApp.Free;
end.
