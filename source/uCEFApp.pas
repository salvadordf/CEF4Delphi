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

unit uCEFApp;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows, System.Classes, System.UITypes,
  {$ELSE}
  Windows, Classes,
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFBaseRefCounted, uCEFSchemeRegistrar, uCEFApplication;

type
  TCefAppOwn = class(TCefBaseRefCountedOwn, ICefApp)
    protected
      procedure OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine); virtual; abstract;
      procedure OnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef); virtual; abstract;
      procedure GetResourceBundleHandler(var aHandler : ICefResourceBundleHandler); virtual; abstract;
      procedure GetBrowserProcessHandler(var aHandler : ICefBrowserProcessHandler); virtual; abstract;
      procedure GetRenderProcessHandler(var aHandler : ICefRenderProcessHandler); virtual; abstract;

    public
      constructor Create; virtual;
  end;

  TCustomCefApp = class(TCefAppOwn)
    protected
      FCefApp : TCefApplication;

      procedure OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine); override;
      procedure OnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef); override;
      procedure GetResourceBundleHandler(var aHandler : ICefResourceBundleHandler); override;
      procedure GetBrowserProcessHandler(var aHandler : ICefBrowserProcessHandler); override;
      procedure GetRenderProcessHandler(var aHandler : ICefRenderProcessHandler); override;

    public
      constructor Create(const aCefApp : TCefApplication); reintroduce;
      destructor  Destroy; override;
  end;


implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFLibFunctions, uCEFMiscFunctions, uCEFCommandLine, uCEFConstants;


// TCefAppOwn

procedure cef_app_on_before_command_line_processing(self: PCefApp;
                                                    const process_type: PCefString;
                                                          command_line: PCefCommandLine); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefAppOwn) then
    TCefAppOwn(TempObject).OnBeforeCommandLineProcessing(CefString(process_type), TCefCommandLineRef.UnWrap(command_line));
end;

procedure cef_app_on_register_custom_schemes(self: PCefApp; registrar: PCefSchemeRegistrar); stdcall;
var
  TempWrapper : TCefSchemeRegistrarRef;
  TempObject  : TObject;
begin
  TempWrapper := nil;

  try
    try
      TempWrapper := TCefSchemeRegistrarRef.Create(registrar);
      TempObject  := CefGetObject(self);

      if (TempObject <> nil) and (TempObject is TCefAppOwn) then
        TCefAppOwn(TempObject).OnRegisterCustomSchemes(TempWrapper);
    except
      on e : exception do
        if CustomExceptionHandler('cef_app_on_register_custom_schemes', e) then raise;
    end;
  finally
    if (TempWrapper <> nil) then FreeAndNil(TempWrapper);
  end;
end;

function cef_app_get_resource_bundle_handler(self: PCefApp): PCefResourceBundleHandler; stdcall;
var
  TempObject  : TObject;
  TempHandler : ICefResourceBundleHandler;
begin
  Result      := nil;
  TempHandler := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefAppOwn) then
    begin
      TCefAppOwn(TempObject).GetResourceBundleHandler(TempHandler);
      Result := CefGetData(TempHandler);
    end;
end;

function cef_app_get_browser_process_handler(self: PCefApp): PCefBrowserProcessHandler; stdcall;
var
  TempObject  : TObject;
  TempHandler : ICefBrowserProcessHandler;
begin
  Result      := nil;
  TempHandler := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefAppOwn) then
    begin
      TCefAppOwn(TempObject).GetBrowserProcessHandler(TempHandler);
      Result := CefGetData(TempHandler);
    end;
end;

function cef_app_get_render_process_handler(self: PCefApp): PCefRenderProcessHandler; stdcall;
var
  TempObject  : TObject;
  TempHandler : ICefRenderProcessHandler;
begin
  Result      := nil;
  TempHandler := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefAppOwn) then
    begin
      TCefAppOwn(TempObject).GetRenderProcessHandler(TempHandler);
      Result := CefGetData(TempHandler);
    end;
end;

constructor TCefAppOwn.Create;
begin
  inherited CreateData(SizeOf(TCefApp));

  with PCefApp(FData)^ do
    begin
      on_before_command_line_processing := cef_app_on_before_command_line_processing;
      on_register_custom_schemes        := cef_app_on_register_custom_schemes;
      get_resource_bundle_handler       := cef_app_get_resource_bundle_handler;
      get_browser_process_handler       := cef_app_get_browser_process_handler;
      get_render_process_handler        := cef_app_get_render_process_handler;
    end;
end;


// TCustomCefApp


constructor TCustomCefApp.Create(const aCefApp : TCefApplication);
begin
  inherited Create;

  FCefApp := aCefApp;
end;

destructor TCustomCefApp.Destroy;
begin
  FCefApp := nil;

  inherited Destroy;
end;

procedure TCustomCefApp.OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine);
begin
  if (FCefApp <> nil) then FCefApp.Internal_OnBeforeCommandLineProcessing(processType, commandLine);
end;

procedure TCustomCefApp.OnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef);
begin
  if (FCefApp <> nil) then FCefApp.Internal_OnRegisterCustomSchemes(registrar);
end;

procedure TCustomCefApp.GetResourceBundleHandler(var aHandler : ICefResourceBundleHandler);
begin
  if (FCefApp <> nil) then
    FCefApp.Internal_CopyResourceBundleHandler(aHandler)
   else
    aHandler := nil;
end;

procedure TCustomCefApp.GetBrowserProcessHandler(var aHandler : ICefBrowserProcessHandler);
begin
  if (FCefApp <> nil) then
    FCefApp.Internal_CopyBrowserProcessHandler(aHandler)
   else
    aHandler := nil;
end;

procedure TCustomCefApp.GetRenderProcessHandler(var aHandler : ICefRenderProcessHandler);
begin
  if (FCefApp <> nil) then
    FCefApp.Internal_CopyRenderProcessHandler(aHandler)
   else
    aHandler := nil;
end;

end.
