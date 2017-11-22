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

program JSWindowBindingWithObject;

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
  uCEFInterfaces,
  uCEFv8Value,
  uCEFConstants,
  uJSWindowBindingWithObject in 'uJSWindowBindingWithObject.pas' {JSWindowBindingWithObjectFrm},
  uMyV8Accessor in 'uMyV8Accessor.pas';

{$R *.res}

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

procedure GlobalCEFApp_OnContextCreated(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
var
  TempAccessor : ICefV8Accessor;
  TempObject   : ICefv8Value;
begin
  // This is the first JS Window Binding example in the "JavaScript Integration" wiki page at
  // https://bitbucket.org/chromiumembedded/cef/wiki/JavaScriptIntegration.md

  TempAccessor := TMyV8Accessor.Create;
  TempObject   := TCefv8ValueRef.NewObject(TempAccessor, nil);
  TempObject.SetValueByKey('myval', TCefv8ValueRef.NewString('My Value!'), V8_PROPERTY_ATTRIBUTE_NONE);

  context.Global.SetValueByKey('myobj', TempObject, V8_PROPERTY_ATTRIBUTE_NONE);
end;

begin
  GlobalCEFApp                  := TCefApplication.Create;
  GlobalCEFApp.OnContextCreated := GlobalCEFApp_OnContextCreated;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      {$IFDEF DELPHI11_UP}
      Application.MainFormOnTaskbar := True;
      {$ENDIF}
      Application.CreateForm(TJSWindowBindingWithObjectFrm, JSWindowBindingWithObjectFrm);
      Application.Run;
    end;

  GlobalCEFApp.Free;
end.
