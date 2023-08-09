unit uCEFApp;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.UITypes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFBaseRefCounted, uCEFSchemeRegistrar, uCEFApplicationCore;

type
  /// <summary>
  /// Implement this interface to provide handler implementations. Methods will be
  /// called by the process and/or thread indicated.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_app_capi.h">CEF source file: /include/capi/cef_app_capi.h (cef_app_t)</see></para>
  /// </remarks>
  TCefAppOwn = class(TCefBaseRefCountedOwn, ICefApp)
    protected
      procedure OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine); virtual; abstract;
      procedure OnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef); virtual; abstract;
      procedure GetResourceBundleHandler(var aHandler : ICefResourceBundleHandler); virtual; abstract;
      procedure GetBrowserProcessHandler(var aHandler : ICefBrowserProcessHandler); virtual; abstract;
      procedure GetRenderProcessHandler(var aHandler : ICefRenderProcessHandler); virtual; abstract;

      procedure RemoveReferences; virtual; abstract;

    public
      constructor Create; virtual;
  end;

  TCustomCefApp = class(TCefAppOwn)
    protected
      FCefApp                : TCefApplicationCore;
      FResourceBundleHandler : ICefResourceBundleHandler;
      FBrowserProcessHandler : ICefBrowserProcessHandler;
      FRenderProcessHandler  : ICefRenderProcessHandler;

      procedure OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine); override;
      procedure OnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef); override;
      procedure GetResourceBundleHandler(var aHandler : ICefResourceBundleHandler); override;
      procedure GetBrowserProcessHandler(var aHandler : ICefBrowserProcessHandler); override;
      procedure GetRenderProcessHandler(var aHandler : ICefRenderProcessHandler); override;

      procedure InitializeVars;
      procedure RemoveReferences; override;

    public
      constructor Create(const aCefApp : TCefApplicationCore); reintroduce;
      destructor  Destroy; override;
  end;


implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFCommandLine, uCEFConstants,
  uCEFBrowserProcessHandler, uCEFResourceBundleHandler, uCEFRenderProcessHandler;


// TCefAppOwn

procedure cef_app_on_before_command_line_processing(      self         : PCefApp;
                                                    const process_type : PCefString;
                                                          command_line : PCefCommandLine); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefAppOwn) then
    TCefAppOwn(TempObject).OnBeforeCommandLineProcessing(CefString(process_type),
                                                         TCefCommandLineRef.UnWrap(command_line));
end;

procedure cef_app_on_register_custom_schemes(self      : PCefApp;
                                             registrar : PCefSchemeRegistrar); stdcall;
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
    try
      TCefAppOwn(TempObject).GetResourceBundleHandler(TempHandler);
      if (TempHandler <> nil) then Result := TempHandler.Wrap;
    finally
      TempHandler := nil;
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
    try
      TCefAppOwn(TempObject).GetBrowserProcessHandler(TempHandler);
      if (TempHandler <> nil) then Result := TempHandler.Wrap;
    finally
      TempHandler := nil;
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
    try
      TCefAppOwn(TempObject).GetRenderProcessHandler(TempHandler);
      if (TempHandler <> nil) then Result := TempHandler.Wrap;
    finally
      TempHandler := nil;
    end;
end;

constructor TCefAppOwn.Create;
begin
  inherited CreateData(SizeOf(TCefApp));

  with PCefApp(FData)^ do
    begin
      on_before_command_line_processing := {$IFDEF FPC}@{$ENDIF}cef_app_on_before_command_line_processing;
      on_register_custom_schemes        := {$IFDEF FPC}@{$ENDIF}cef_app_on_register_custom_schemes;
      get_resource_bundle_handler       := {$IFDEF FPC}@{$ENDIF}cef_app_get_resource_bundle_handler;
      get_browser_process_handler       := {$IFDEF FPC}@{$ENDIF}cef_app_get_browser_process_handler;
      get_render_process_handler        := {$IFDEF FPC}@{$ENDIF}cef_app_get_render_process_handler;
    end;
end;


// TCustomCefApp


constructor TCustomCefApp.Create(const aCefApp : TCefApplicationCore);
begin
  inherited Create;

  InitializeVars;

  FCefApp := aCefApp;

  if (FCefApp <> nil) then
    begin
      if FCefApp.MustCreateBrowserProcessHandler then
        FBrowserProcessHandler := TCefCustomBrowserProcessHandler.Create(FCefApp);

      if FCefApp.MustCreateResourceBundleHandler then
        FResourceBundleHandler := TCefCustomResourceBundleHandler.Create(FCefApp);

      if FCefApp.MustCreateRenderProcessHandler then
        FRenderProcessHandler  := TCefCustomRenderProcessHandler.Create(FCefApp);
    end;
end;

destructor TCustomCefApp.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomCefApp.InitializeVars;
begin
  FCefApp                := nil;
  FResourceBundleHandler := nil;
  FBrowserProcessHandler := nil;
  FRenderProcessHandler  := nil;
end;

procedure TCustomCefApp.RemoveReferences;
begin
  if (FResourceBundleHandler <> nil) then
    FResourceBundleHandler.RemoveReferences;

  if (FBrowserProcessHandler <> nil) then
    FBrowserProcessHandler.RemoveReferences;

  if (FRenderProcessHandler <> nil) then
    FRenderProcessHandler.RemoveReferences;

  InitializeVars;
end;

procedure TCustomCefApp.OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine);
begin
  try
    if (FCefApp <> nil) then
      IApplicationCoreEvents(FCefApp).doOnBeforeCommandLineProcessing(processType, commandLine);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomCefApp.OnBeforeCommandLineProcessing', e) then raise;
  end;
end;

procedure TCustomCefApp.OnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef);
begin
  try
    if (FCefApp <> nil) then
      IApplicationCoreEvents(FCefApp).doOnRegisterCustomSchemes(registrar);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomCefApp.OnRegisterCustomSchemes', e) then raise;
  end;
end;

procedure TCustomCefApp.GetResourceBundleHandler(var aHandler : ICefResourceBundleHandler);
begin
  if (FResourceBundleHandler <> nil) then
    aHandler := FResourceBundleHandler
   else
    aHandler := nil;
end;

procedure TCustomCefApp.GetBrowserProcessHandler(var aHandler : ICefBrowserProcessHandler);
begin
  if (FBrowserProcessHandler <> nil) then
    aHandler := FBrowserProcessHandler
   else
    aHandler := nil;
end;

procedure TCustomCefApp.GetRenderProcessHandler(var aHandler : ICefRenderProcessHandler);
begin
  if (FRenderProcessHandler <> nil) then
    aHandler := FRenderProcessHandler
   else
    aHandler := nil;
end;

end.
