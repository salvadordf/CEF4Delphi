unit uCEFChromiumOptions;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFTypes, uCEFConstants;

type
  /// <summary>
  /// The TChromiumOptions properties used to fill the TCefBrowserSettings record which is used during the browser creation.
  /// </summary>
  TChromiumOptions = class(TPersistent)
    protected
      FWindowlessFrameRate         : Integer;
      FJavascript                  : TCefState;
      FJavascriptCloseWindows      : TCefState;
      FJavascriptAccessClipboard   : TCefState;
      FJavascriptDomPaste          : TCefState;
      FImageLoading                : TCefState;
      FImageShrinkStandaloneToFit  : TCefState;
      FTextAreaResize              : TCefState;
      FTabToLinks                  : TCefState;
      FLocalStorage                : TCefState;
      FDatabases                   : TCefState;
      FWebgl                       : TCefState;
      FBackgroundColor             : TCefColor;
      FChromeStatusBubble          : TCefState;
      FChromeZoomBubble            : TCefState;

    public
      /// <summary>
      /// Constructor of TChromiumOptions
      /// </summary>
      constructor Create; virtual;

    published
      /// <summary>
      /// Controls whether JavaScript can be executed. Also configurable using the
      /// "disable-javascript" command-line switch.
      /// </summary>
      property Javascript                  : TCefState read FJavascript                   write FJavascript                  default STATE_DEFAULT;
      /// <summary>
      /// Controls whether JavaScript can be used to close windows that were not
      /// opened via JavaScript. JavaScript can still be used to close windows that
      /// were opened via JavaScript or that have no back/forward history. Also
      /// configurable using the "disable-javascript-close-windows" command-line
      /// switch.
      /// </summary>
      property JavascriptCloseWindows      : TCefState read FJavascriptCloseWindows       write FJavascriptCloseWindows      default STATE_DEFAULT;
      /// <summary>
      /// Controls whether JavaScript can access the clipboard. Also configurable
      /// using the "disable-javascript-access-clipboard" command-line switch.
      /// </summary>
      property JavascriptAccessClipboard   : TCefState read FJavascriptAccessClipboard    write FJavascriptAccessClipboard   default STATE_DEFAULT;
      /// <summary>
      /// Controls whether DOM pasting is supported in the editor via
      /// execCommand("paste"). The |javascript_access_clipboard| setting must also
      /// be enabled. Also configurable using the "disable-javascript-dom-paste"
      /// command-line switch.
      /// </summary>
      property JavascriptDomPaste          : TCefState read FJavascriptDomPaste           write FJavascriptDomPaste          default STATE_DEFAULT;
      /// <summary>
      /// Controls whether image URLs will be loaded from the network. A cached
      /// image will still be rendered if requested. Also configurable using the
      /// "disable-image-loading" command-line switch.
      /// </summary>
      property ImageLoading                : TCefState read FImageLoading                 write FImageLoading                default STATE_DEFAULT;
      /// <summary>
      /// Controls whether standalone images will be shrunk to fit the page. Also
      /// configurable using the "image-shrink-standalone-to-fit" command-line
      /// switch.
      /// </summary>
      property ImageShrinkStandaloneToFit  : TCefState read FImageShrinkStandaloneToFit   write FImageShrinkStandaloneToFit  default STATE_DEFAULT;
      /// <summary>
      /// Controls whether text areas can be resized. Also configurable using the
      /// "disable-text-area-resize" command-line switch.
      /// </summary>
      property TextAreaResize              : TCefState read FTextAreaResize               write FTextAreaResize              default STATE_DEFAULT;
      /// <summary>
      /// Controls whether the tab key can advance focus to links. Also configurable
      /// using the "disable-tab-to-links" command-line switch.
      /// </summary>
      property TabToLinks                  : TCefState read FTabToLinks                   write FTabToLinks                  default STATE_DEFAULT;
      /// <summary>
      /// Controls whether local storage can be used. Also configurable using the
      /// "disable-local-storage" command-line switch.
      /// </summary>
      property LocalStorage                : TCefState read FLocalStorage                 write FLocalStorage                default STATE_DEFAULT;
      /// <summary>
      /// Controls whether databases can be used. Also configurable using the
      /// "disable-databases" command-line switch.
      /// </summary>
      property Databases                   : TCefState read FDatabases                    write FDatabases                   default STATE_DEFAULT;
      /// <summary>
      /// Controls whether WebGL can be used. Note that WebGL requires hardware
      /// support and may not work on all systems even when enabled. Also
      /// configurable using the "disable-webgl" command-line switch.
      /// </summary>
      property Webgl                       : TCefState read FWebgl                        write FWebgl                       default STATE_DEFAULT;
      /// <summary>
      /// Background color used for the browser before a document is loaded and when
      /// no document color is specified. The alpha component must be either fully
      /// opaque (0xFF) or fully transparent (0x00). If the alpha component is fully
      /// opaque then the RGB components will be used as the background color. If
      /// the alpha component is fully transparent for a windowed browser then the
      /// TCefSettings.background_color value will be used. If the alpha component is
      /// fully transparent for a windowless (off-screen) browser then transparent
      /// painting will be enabled.
      /// </summary>
      property BackgroundColor             : TCefColor read FBackgroundColor              write FBackgroundColor             default 0;
      /// <summary>
      /// The maximum rate in frames per second (fps) that ICefRenderHandler.OnPaint
      /// will be called for a windowless browser. The actual fps may be lower if
      /// the browser cannot generate frames at the requested rate. The minimum
      /// value is 1 and the maximum value is 60 (default 30). This value can also
      /// be changed dynamically via ICefBrowserHost.SetWindowlessFrameRate.
      /// </summary>
      /// <remarks>
      /// <para>Use CEF_OSR_SHARED_TEXTURES_FRAMERATE_DEFAULT as default value if the shared textures are enabled.</para>
      /// <para>Use CEF_OSR_FRAMERATE_DEFAULT as default value if the shared textures are disabled.</para>
      /// </remarks>
      property WindowlessFrameRate         : Integer   read FWindowlessFrameRate          write FWindowlessFrameRate         default CEF_OSR_FRAMERATE_DEFAULT;
      /// <summary>
      /// Controls whether the Chrome status bubble will be used. Only supported
      /// with Chrome style. For details about the status bubble see
      /// https://www.chromium.org/user-experience/status-bubble/
      /// </summary>
      property ChromeStatusBubble          : TCefState read FChromeStatusBubble           write FChromeStatusBubble          default STATE_DEFAULT;
      /// <summary>
      /// Controls whether the Chrome zoom bubble will be shown when zooming. Only
      /// supported with Chrome style.
      /// </summary>
      property ChromeZoomBubble            : TCefState read FChromeZoomBubble             write FChromeZoomBubble            default STATE_DEFAULT;
  end;

implementation

constructor TChromiumOptions.Create;
begin
  FWindowlessFrameRate         := CEF_OSR_FRAMERATE_DEFAULT;
  FJavascript                  := STATE_DEFAULT;
  FJavascriptCloseWindows      := STATE_DEFAULT;
  FJavascriptAccessClipboard   := STATE_DEFAULT;
  FJavascriptDomPaste          := STATE_DEFAULT;
  FImageLoading                := STATE_DEFAULT;
  FImageShrinkStandaloneToFit  := STATE_DEFAULT;
  FTextAreaResize              := STATE_DEFAULT;
  FTabToLinks                  := STATE_DEFAULT;
  FLocalStorage                := STATE_DEFAULT;
  FDatabases                   := STATE_DEFAULT;
  FWebgl                       := STATE_DEFAULT;
  FBackgroundColor             := 0;
  FChromeStatusBubble          := STATE_DEFAULT;
  FChromeZoomBubble            := STATE_DEFAULT;
end;

end.
