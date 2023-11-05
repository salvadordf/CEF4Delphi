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
  /// <summary>
  /// Class that creates ICefResourceHandler instances for handling scheme
  /// requests.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_scheme_capi.h">CEF source file: /include/capi/cef_scheme_capi.h (cef_scheme_handler_factory_t)</see></para>
  /// </remarks>
  TCefSchemeHandlerFactoryOwn = class(TCefBaseRefCountedOwn, ICefSchemeHandlerFactory)
    protected
      FClass : TCefResourceHandlerClass;

      /// <summary>
      /// Return a new resource handler instance to handle the request or an NULL
      /// reference to allow default handling of the request. |browser| and |frame|
      /// will be the browser window and frame respectively that originated the
      /// request or NULL if the request did not originate from a browser window
      /// (for example, if the request came from ICefUrlRequest). The |request|
      /// object passed to this function cannot be modified.
      /// </summary>
      function New(const browser: ICefBrowser; const frame: ICefFrame; const schemeName: ustring; const request: ICefRequest): ICefResourceHandler; virtual;

    public
      /// <summary>
      /// Constructor of the scheme handler factory.
      /// </summary>
      /// <param name="AClass">Class of the custom resource handler used to handle custom scheme requests.</param>
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
