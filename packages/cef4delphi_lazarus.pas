{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CEF4Delphi_Lazarus;

{$warn 5023 off : no warning about unused units}
interface

uses
  uCEFAccessibilityHandler, uCEFApp, uCEFApplication, uCEFAuthCallback, 
  uCEFBaseRefCounted, uCEFBaseScopedWrapper, uCEFBeforeDownloadCallback, 
  uCEFBinaryValue, uCEFBrowser, uCEFBrowserProcessHandler, uCEFCallback, 
  uCEFChromiumEvents, uCEFChromiumFontOptions, uCEFChromiumOptions, 
  uCEFChromiumWindow, uCEFClient, uCEFCommandLine, uCEFCompletionCallback, 
  uCEFConstants, uCEFContextMenuHandler, uCEFContextMenuParams, 
  uCEFCookieManager, uCEFCookieVisitor, uCEFCustomStreamReader, 
  uCEFDeleteCookiesCallback, uCEFDialogHandler, uCEFDictionaryValue, 
  uCEFDisplayHandler, uCEFDomDocument, uCEFDomNode, uCEFDomVisitor, 
  uCEFDownloadHandler, uCEFDownloadImageCallBack, uCEFDownLoadItem, 
  uCEFDownloadItemCallback, uCEFDragData, uCEFDragHandler, 
  uCEFEndTracingCallback, uCEFExtension, uCEFExtensionHandler, 
  uCEFFileDialogCallback, uCEFFindHandler, uCEFFocusHandler, uCEFFrame, 
  uCEFGetExtensionResourceCallback, uCEFImage, uCEFInterfaces, 
  uCEFJsDialogCallback, uCEFJsDialogHandler, uCEFKeyboardHandler, 
  uCEFLibFunctions, uCEFLifeSpanHandler, uCEFListValue, uCEFLoadHandler, 
  uCEFMenuModel, uCEFMenuModelDelegate, uCEFMiscFunctions, 
  uCEFNavigationEntry, uCEFNavigationEntryVisitor, uCEFPDFPrintCallback, 
  uCEFPDFPrintOptions, uCEFPostData, uCEFPostDataElement, uCEFPrintSettings, 
  uCEFProcessMessage, uCEFRegisterCDMCallback, uCEFRenderHandler, 
  uCEFRenderProcessHandler, uCEFRequest, uCEFRequestCallback, 
  uCEFRequestContext, uCEFRequestContextHandler, uCEFRequestHandler, 
  uCEFResolveCallback, uCEFResourceBundle, uCEFResourceBundleHandler, 
  uCEFResourceHandler, uCEFResponse, uCEFResponseFilter, 
  uCEFRunContextMenuCallback, uCEFRunFileDialogCallback, 
  uCEFSchemeHandlerFactory, uCEFSchemeRegistrar, 
  uCEFSelectClientCertificateCallback, uCEFSetCookieCallback, uCEFSslInfo, 
  uCEFSSLStatus, uCEFStreamReader, uCEFStreamWriter, uCEFStringList, 
  uCEFStringMap, uCEFStringMultimap, uCEFStringVisitor, uCEFTask, 
  uCEFTaskRunner, uCEFThread, uCEFTypes, uCEFUrlRequest, uCEFUrlrequestClient, 
  uCEFv8Accessor, uCEFv8ArrayBufferReleaseCallback, uCEFv8Context, 
  uCEFv8Exception, uCEFv8Handler, uCEFv8Interceptor, uCEFv8StackFrame, 
  uCEFv8StackTrace, uCEFv8Value, uCEFValue, uCEFWaitableEvent, 
  uCEFWebPluginInfo, uCEFWebPluginInfoVisitor, uCEFWebPluginUnstableCallback, 
  uCEFWindowParent, uCEFWorkScheduler, uCEFWorkSchedulerThread, 
  uCEFWriteHandler, uCEFX509Certificate, uCEFX509CertPrincipal, uCEFXmlReader, 
  uCEFZipReader, uCEFChromiumCore, uCEFChromium, uCEFBufferPanel, uCEFServer, 
  uCEFServerComponent, uCEFServerEvents, uCEFServerHandler, uCEFWinControl, 
  uCEFLinkedWindowParent, uCEFUrlRequestClientEvents, 
  uCEFUrlRequestClientComponent, uCEFOSRIMEHandler, uCEFCookieAccessFilter, 
  uCEFResourceReadCallback, uCEFResourceRequestHandler, 
  uCEFResourceSkipCallback, uCEFSentinel, uCEFApplicationCore, 
  uCEFOAuth2Helper, uCEFMediaObserver, uCEFMediaRoute, 
  uCEFMediaRouteCreateCallback, uCEFMediaRouter, uCEFMediaSink, 
  uCEFMediaSource, uCEFRegistration, uCEFWindowDelegate, uCEFWindow, 
  uCEFMenuButtonDelegate, uCEFMenuButtonPressedLock, uCEFMenuButton, 
  uCEFLabelButton, uCEFButtonDelegate, uCEFButton, uCEFBrowserViewDelegate, 
  uCEFBrowserView, uCEFPanelDelegate, uCEFPanel, uCEFScrollView, 
  uCEFTextfieldDelegate, uCEFTextfield, uCEFViewDelegate, uCEFView, 
  uCEFFillLayout, uCEFBoxLayout, uCEFLayout, uCEFDisplay, 
  uCEFMenuButtonComponent, uCEFLabelButtonComponent, uCEFButtonComponent, 
  uCEFBrowserViewComponent, uCEFWindowComponent, uCEFPanelComponent, 
  uCEFScrollViewComponent, uCEFTextfieldComponent, uCEFViewComponent, 
  uCEFViewsFrameworkEvents, uCEFAudioHandler, uCEFDevToolsMessageObserver, 
  uCEFMediaSinkDeviceInfoCallback, uCEFJson, uCEFBitmapBitBuffer, 
  uCEFPrintDialogCallback, uCEFPrintHandler, uCEFPrintJobCallback, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uCEFChromiumWindow', @uCEFChromiumWindow.Register);
  RegisterUnit('uCEFWindowParent', @uCEFWindowParent.Register);
  RegisterUnit('uCEFWorkScheduler', @uCEFWorkScheduler.Register);
  RegisterUnit('uCEFChromium', @uCEFChromium.Register);
  RegisterUnit('uCEFBufferPanel', @uCEFBufferPanel.Register);
  RegisterUnit('uCEFServerComponent', @uCEFServerComponent.Register);
  RegisterUnit('uCEFLinkedWindowParent', @uCEFLinkedWindowParent.Register);
  RegisterUnit('uCEFUrlRequestClientComponent', 
    @uCEFUrlRequestClientComponent.Register);
  RegisterUnit('uCEFSentinel', @uCEFSentinel.Register);
  RegisterUnit('uCEFMenuButtonComponent', @uCEFMenuButtonComponent.Register);
  RegisterUnit('uCEFLabelButtonComponent', @uCEFLabelButtonComponent.Register);
  RegisterUnit('uCEFBrowserViewComponent', @uCEFBrowserViewComponent.Register);
  RegisterUnit('uCEFWindowComponent', @uCEFWindowComponent.Register);
  RegisterUnit('uCEFPanelComponent', @uCEFPanelComponent.Register);
  RegisterUnit('uCEFScrollViewComponent', @uCEFScrollViewComponent.Register);
  RegisterUnit('uCEFTextfieldComponent', @uCEFTextfieldComponent.Register);
end;

initialization
  RegisterPackage('CEF4Delphi_Lazarus', @Register);
end.
