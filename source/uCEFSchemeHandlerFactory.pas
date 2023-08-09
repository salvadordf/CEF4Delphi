unit uCEFSchemeHandlerFactory;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFResourceHandler;

type
  TCefSchemeHandlerFactoryOwn = class(TCefBaseRefCountedOwn, ICefSchemeHandlerFactory)
    protected
      FClass : TCefResourceHandlerClass;

      function New(const browser: ICefBrowser; const frame: ICefFrame; const schemeName: ustring; const request: ICefRequest): ICefResourceHandler; virtual;

    public
      constructor Create(const AClass: TCefResourceHandlerClass); virtual;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFrame, uCEFRequest;

function cef_scheme_handler_factory_create(      self        : PCefSchemeHandlerFactory;
                                                 browser     : PCefBrowser;
                                                 frame       : PCefFrame;
                                           const scheme_name : PCefString;
                                                 request     : PCefRequest): PCefResourceHandler; stdcall;
var
  TempObject : TObject;
begin
  Result     := nil;
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefSchemeHandlerFactoryOwn) then
    Result := CefGetData(TCefSchemeHandlerFactoryOwn(TempObject).New(TCefBrowserRef.UnWrap(browser),
                                                                     TCefFrameRef.UnWrap(frame),
                                                                     CefString(scheme_name),
                                                                     TCefRequestRef.UnWrap(request)));
end;

constructor TCefSchemeHandlerFactoryOwn.Create(const AClass: TCefResourceHandlerClass);
begin
  inherited CreateData(SizeOf(TCefSchemeHandlerFactory));

  FClass := AClass;

  PCefSchemeHandlerFactory(FData)^.create := {$IFDEF FPC}@{$ENDIF}cef_scheme_handler_factory_create;
end;

function TCefSchemeHandlerFactoryOwn.New(const browser    : ICefBrowser;
                                         const frame      : ICefFrame;
                                         const schemeName : ustring;
                                         const request    : ICefRequest): ICefResourceHandler;
begin
  if (FClass <> nil) then
    Result := FClass.Create(browser, frame, schemeName, request)
   else
    Result := nil;
end;


end.
