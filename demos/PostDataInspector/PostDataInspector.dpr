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

program PostDataInspector;

{$I cef.inc}

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows,
  Vcl.Forms,
  System.SysUtils,
  {$ELSE}
  Forms,
  Windows,
  SysUtils,
  {$ENDIF }
  uCEFApplication,
  uCEFRenderProcessHandler,
  uCEFInterfaces,
  uCEFProcessMessage,
  uCEFTypes,
  uPostDataInspector in 'uPostDataInspector.pas' {PostDataInspectorFrm};

{$R *.res}

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

var
  TempProcessHandler : TCefCustomRenderProcessHandler;

procedure ProcessHandler_OnBeforeNavigationEvent(const browser        : ICefBrowser;
                                                 const frame          : ICefFrame;
                                                 const request        : ICefRequest;
                                                       navigationType : TCefNavigationType;
                                                       isRedirect     : Boolean;
                                                 var   aResult        : boolean);
var
  msg: ICefProcessMessage;
  TempString : string;
begin
  aResult := False;

  if (request = nil) then
    TempString := 'no request'
   else
    if (request.postdata = nil) then
      TempString := 'no postdata'
     else
      TempString := 'postdata elements : ' + inttostr(request.postdata.GetCount);

  msg := TCefProcessMessageRef.New(POSTDATA_MSGNAME);
  msg.ArgumentList.SetString(0, TempString);
  browser.SendProcessMessage(PID_BROWSER, msg);
end;

begin
  TempProcessHandler                         := TCefCustomRenderProcessHandler.Create;
  TempProcessHandler.OnBeforeNavigationEvent := ProcessHandler_OnBeforeNavigationEvent;

  GlobalCEFApp                      := TCefApplication.Create;
  GlobalCEFApp.RenderProcessHandler := TempProcessHandler as ICefRenderProcessHandler;

  // The directories are optional.
  {
  GlobalCEFApp.FrameworkDirPath     := 'cef';
  GlobalCEFApp.ResourcesDirPath     := 'cef';
  GlobalCEFApp.LocalesDirPath       := 'cef\locales';
  GlobalCEFApp.cache                := 'cef\cache';
  GlobalCEFApp.cookies              := 'cef\cookies';
  GlobalCEFApp.UserDataPath         := 'cef\User Data';
  }

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      {$IFDEF DELPHI11_UP}
      Application.MainFormOnTaskbar := True;
      {$ENDIF}
      Application.CreateForm(TPostDataInspectorFrm, PostDataInspectorFrm);
      Application.Run;
    end;

  GlobalCEFApp.Free;
end.
