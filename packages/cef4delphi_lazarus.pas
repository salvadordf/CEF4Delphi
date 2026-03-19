{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CEF4Delphi_Lazarus;

{$warn 5023 off : no warning about unused units}
interface

uses
  uCEFAccessibilityHandler, uCEFApp, uCEFApplication, uCEFApplicationCore, 
  uCEFApplicationEvents, uCEFArgCopy, uCEFAudioHandler, uCEFAuthCallback, 
  uCEFBaseRefCounted, uCEFBaseScopedWrapper, uCEFBeforeDownloadCallback, 
  uCEFBinaryValue, uCEFBitmapBitBuffer, uCEFBoxLayout, uCEFBrowser, 
  uCEFBrowserBitmap, uCEFBrowserProcessHandler, uCEFBrowserView, 
  uCEFBrowserViewComponent, uCEFBrowserViewDelegate, uCEFBrowserWindow, 
  uCEFBufferPanel, uCEFButton, uCEFButtonComponent, uCEFButtonDelegate, 
  uCEFCallback, uCEFChromium, uCEFChromiumCore, uCEFChromiumEvents, 
  uCEFChromiumFontOptions, uCEFChromiumOptions, uCEFChromiumWindow, 
  uCEFClient, uCEFCommandHandler, uCEFCommandLine, uCEFCompletionCallback, 
  uCEFComponentIdList, uCEFConstants, uCEFContextMenuHandler, 
  uCEFContextMenuParams, uCEFCookieAccessFilter, uCEFCookieManager, 
  uCEFCookieVisitor, uCEFCustomStreamReader, uCEFDeleteCookiesCallback, 
  uCEFDevToolsMessageObserver, uCEFDialogHandler, uCEFDictionaryValue, 
  uCEFDisplay, uCEFDisplayHandler, uCEFDomDocument, uCEFDomNode, 
  uCEFDomVisitor, uCEFDownloadHandler, uCEFDownloadImageCallBack, 
  uCEFDownloadItem, uCEFDownloadItemCallback, uCEFDragData, uCEFDragHandler, 
  uCEFEndTracingCallback, uCEFFileDialogCallback, uCEFFileDialogInfo, 
  uCEFFillLayout, uCEFFindHandler, uCEFFocusHandler, uCEFFrame, 
  uCEFFrameHandler, uCEFImage, uCEFInterfaces, uCEFJsDialogCallback, 
  uCEFJsDialogHandler, uCEFJson, uCEFKeyboardHandler, uCEFLabelButton, 
  uCEFLabelButtonComponent, uCEFLayout, uCEFLazarusCocoa, uCEFLibFunctions, 
  uCEFLifeSpanHandler, uCEFLinkedWinControlBase, uCEFLinkedWindowParent, 
  uCEFLinuxConstants, uCEFLinuxEventPipe, uCEFLinuxFunctions, 
  uCEFLinuxOSRIMEHandler, uCEFLinuxTypes, uCEFListValue, uCEFLoadHandler, 
  uCEFMediaAccessCallback, uCEFMediaObserver, uCEFMediaRoute, 
  uCEFMediaRouteCreateCallback, uCEFMediaRouter, uCEFMediaSink, 
  uCEFMediaSinkDeviceInfoCallback, uCEFMediaSource, uCEFMenuButton, 
  uCEFMenuButtonComponent, uCEFMenuButtonDelegate, uCEFMenuButtonPressedLock, 
  uCEFMenuModel, uCEFMenuModelDelegate, uCEFMiscFunctions, 
  uCEFNavigationEntry, uCEFNavigationEntryVisitor, uCEFOAuth2Helper, 
  uCEFOsrBrowserWindow, uCEFOSRIMEHandler, uCEFOverlayController, uCEFPanel, 
  uCEFPanelComponent, uCEFPanelDelegate, uCEFPDFPrintCallback, 
  uCEFPDFPrintOptions, uCEFPermissionHandler, uCEFPermissionPromptCallback, 
  uCEFPostData, uCEFPostDataElement, uCEFPreferenceManager, 
  uCEFPreferenceObserver, uCEFPreferenceRegistrar, uCEFPrintDialogCallback, 
  uCEFPrintHandler, uCEFPrintJobCallback, uCEFPrintSettings, 
  uCEFProcessMessage, uCEFRegistration, uCEFRenderHandler, 
  uCEFRenderProcessHandler, uCEFRequest, uCEFRequestContext, 
  uCEFRequestContextHandler, uCEFRequestHandler, uCEFResolveCallback, 
  uCEFResourceBundle, uCEFResourceBundleHandler, uCEFResourceHandler, 
  uCEFResourceReadCallback, uCEFResourceRequestHandler, 
  uCEFResourceSkipCallback, uCEFResponse, uCEFResponseFilter, 
  uCEFRunContextMenuCallback, uCEFRunFileDialogCallback, 
  uCEFRunQuickMenuCallback, uCEFSchemeHandlerFactory, uCEFSchemeRegistrar, 
  uCEFScrollView, uCEFScrollViewComponent, 
  uCEFSelectClientCertificateCallback, uCEFSentinel, uCEFServer, 
  uCEFServerComponent, uCEFServerEvents, uCEFServerHandler, 
  uCEFSetCookieCallback, uCEFSettingObserver, uCEFSharedMemoryRegion, 
  uCEFSharedProcessMessageBuilder, uCEFSslInfo, uCEFSSLStatus, 
  uCEFStreamReader, uCEFStreamWriter, uCEFStringList, uCEFStringMap, 
  uCEFStringMultimap, uCEFStringVisitor, uCEFTask, uCEFTaskManager, 
  uCEFTaskRunner, uCEFTextfield, uCEFTextfieldComponent, 
  uCEFTextfieldDelegate, uCEFThread, uCEFTimerWorkScheduler, uCEFTypes, 
  uCEFUrlRequest, uCEFUrlrequestClient, uCEFUrlRequestClientComponent, 
  uCEFUrlRequestClientEvents, uCEFv8Accessor, 
  uCEFv8ArrayBufferReleaseCallback, uCEFv8Context, uCEFv8Exception, 
  uCEFv8Handler, uCEFv8Interceptor, uCEFv8StackFrame, uCEFv8StackTrace, 
  uCEFv8Value, uCEFValue, uCEFView, uCEFViewComponent, uCEFViewDelegate, 
  uCEFViewsFrameworkEvents, uCEFWaitableEvent, uCEFWinControl, uCEFWindow, 
  uCEFWindowComponent, uCEFWindowDelegate, uCEFWindowInfoWrapper, 
  uCEFWindowParent, uCEFWorkScheduler, uCEFWorkSchedulerQueueThread, 
  uCEFWorkSchedulerThread, uCEFWriteHandler, uCEFX509Certificate, 
  uCEFX509CertPrincipal, uCEFXmlReader, uCEFZipReader, uCEFComponentUpdater, 
  uCEFComponentUpdateCallback, uCEFComponent, uCEFv8BackingStore, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uCEFBrowserViewComponent', @uCEFBrowserViewComponent.Register);
  RegisterUnit('uCEFBrowserWindow', @uCEFBrowserWindow.Register);
  RegisterUnit('uCEFBufferPanel', @uCEFBufferPanel.Register);
  RegisterUnit('uCEFChromium', @uCEFChromium.Register);
  RegisterUnit('uCEFChromiumWindow', @uCEFChromiumWindow.Register);
  RegisterUnit('uCEFLabelButtonComponent', @uCEFLabelButtonComponent.Register);
  RegisterUnit('uCEFLinkedWindowParent', @uCEFLinkedWindowParent.Register);
  RegisterUnit('uCEFMenuButtonComponent', @uCEFMenuButtonComponent.Register);
  RegisterUnit('uCEFOsrBrowserWindow', @uCEFOsrBrowserWindow.Register);
  RegisterUnit('uCEFPanelComponent', @uCEFPanelComponent.Register);
  RegisterUnit('uCEFScrollViewComponent', @uCEFScrollViewComponent.Register);
  RegisterUnit('uCEFSentinel', @uCEFSentinel.Register);
  RegisterUnit('uCEFServerComponent', @uCEFServerComponent.Register);
  RegisterUnit('uCEFTextfieldComponent', @uCEFTextfieldComponent.Register);
  RegisterUnit('uCEFUrlRequestClientComponent', 
    @uCEFUrlRequestClientComponent.Register);
  RegisterUnit('uCEFWindowComponent', @uCEFWindowComponent.Register);
  RegisterUnit('uCEFWindowParent', @uCEFWindowParent.Register);
  RegisterUnit('uCEFWorkScheduler', @uCEFWorkScheduler.Register);
end;

initialization
  RegisterPackage('CEF4Delphi_Lazarus', @Register);
end.
