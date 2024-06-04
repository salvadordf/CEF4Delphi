unit uCEFWindowInfoWrapper;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF}System.Classes, System.Types,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF}Classes, Types,
  {$ENDIF}
  uCEFTypes;

type
  /// <summary>
  /// Wrapper class used to initialize a TCEFWindowInfo record.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_win.h">CEF source file: /include/internal/cef_win.h (CefWindowInfo)</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_mac.h">CEF source file: /include/internal/cef_mac.h (CefWindowInfo)</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_linux.h">CEF source file: /include/internal/cef_linux.h (CefWindowInfo)</see></para>
  /// </remarks>
  TCEFWindowInfoWrapper = class
    protected
      FWindowInfo : TCEFWindowInfo;

      function  GetWindowName : ustring;
      function  GetBounds : TCefRect;
      function  GetParentWindow : TCefWindowHandle;
      function  GetWindowlessRenderingEnabled : boolean;
      function  GetSharedTextureEnabled : boolean;
      function  GetExternalBeginFrameEnabled : boolean;
      function  GetWindow : TCefWindowHandle;
      function  GetRuntimeStyle : TCefRuntimeStyle;
      {$IFDEF MSWINDOWS}
      function  GetExStyle : DWORD;
      function  GetStyle : DWORD;
      function  GetMenu : HMENU;
      {$ENDIF}
      {$IFDEF MACOSX}
      function  GetHidden : boolean;
      function  GetParentView : TCefWindowHandle;
      function  GetView : TCefWindowHandle;
      {$ENDIF}

      procedure SetWindowName(const aValue : ustring);
      procedure SetBounds(const aValue : TCefRect);
      procedure SetParentWindow(aValue : TCefWindowHandle);
      procedure SetWindowlessRenderingEnabled(aValue : boolean);
      procedure SetSharedTextureEnabled(aValue : boolean);
      procedure SetExternalBeginFrameEnabled(aValue : boolean);
      procedure SetWindow(aValue : TCefWindowHandle);
      procedure SetRuntimeStyle(aValue : TCefRuntimeStyle);
      {$IFDEF MSWINDOWS}
      procedure SetExStyle(aValue : DWORD);
      procedure SetStyle(aValue : DWORD);
      procedure SetMenu(aValue : HMENU);
      {$ENDIF}
      {$IFDEF MACOSX}
      procedure SetHidden(aValue : boolean);
      procedure SetParentView(aValue : TCefWindowHandle);
      procedure SetView(aValue : TCefWindowHandle);
      {$ENDIF}

    public
      constructor Create;
      /// <summary>
      /// Copy the information from an external TCEFWindowInfo instance
      /// </summary>
      procedure CopyFromWindowInfo(const aSrcWindowInfo: TCEFWindowInfo);
      /// <summary>
      /// Create the browser as a child window.
      /// </summary>
      class procedure AsChild(var aWindowInfo: TCEFWindowInfo; aParent : TCefWindowHandle; aWindowBounds : TRect);
      /// <summary>
      /// Create the browser as a child window.
      /// </summary>
      procedure SetAsChild(aParent : TCefWindowHandle; aWindowBounds : TRect);
      {$IFDEF MSWINDOWS}
      /// <summary>
      /// Create the browser as a popup window.
      /// </summary>
      class procedure AsPopup(var aWindowInfo: TCEFWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring);
      /// <summary>
      /// Create the browser as a popup window.
      /// </summary>
      procedure SetAsPopup(aParent : TCefWindowHandle; const aWindowName : ustring);
      {$ENDIF}
      /// <summary>
      /// Create the browser using windowless (off-screen) rendering. No window
      /// will be created for the browser and all rendering will occur via the
      /// ICefRenderHandler interface. The |parent| value will be used to identify
      /// monitor info and to act as the parent window for dialogs, context menus,
      /// etc. If |parent| is not provided then the main screen monitor will be used
      /// and some functionality that requires a parent window may not function
      /// correctly. In order to create windowless browsers the
      /// TCefSettings.windowless_rendering_enabled value must be set to true.
      /// Transparent painting is enabled by default but can be disabled by setting
      /// ICefBrowserSettings.background_color to an opaque value.
      /// </summary>
      class procedure AsWindowless(var aWindowInfo: TCEFWindowInfo; aParent : TCefWindowHandle);
      /// <summary>
      /// Create the browser using windowless (off-screen) rendering. No window
      /// will be created for the browser and all rendering will occur via the
      /// ICefRenderHandler interface. The |parent| value will be used to identify
      /// monitor info and to act as the parent window for dialogs, context menus,
      /// etc. If |parent| is not provided then the main screen monitor will be used
      /// and some functionality that requires a parent window may not function
      /// correctly. In order to create windowless browsers the
      /// TCefSettings.windowless_rendering_enabled value must be set to true.
      /// Transparent painting is enabled by default but can be disabled by setting
      /// ICefBrowserSettings.background_color to an opaque value.
      /// </summary>
      procedure SetAsWindowless(aParent : TCefWindowHandle);

      /// <summary>
      /// Internal TCEFWindowInfo instance
      /// </summary>
      property WindowInfoRecord              : TCEFWindowInfo      read FWindowInfo                     write FWindowInfo;
      /// <summary>
      /// The initial title of the window, to be set when the window is created.
      /// Some layout managers (e.g., Compiz) can look at the window title
      /// in order to decide where to place the window when it is
      /// created. When this attribute is not empty, the window title will
      /// be set before the window is mapped to the dispay. Otherwise the
      /// title will be initially empty.
      /// </summary>
      property WindowName                    : ustring             read GetWindowName                   write SetWindowName;
      /// <summary>
      /// Initial window bounds.
      /// </summary>
      property Bounds                        : TCefRect            read GetBounds                       write SetBounds;
      /// <summary>
      /// Set to true (1) to create the browser using windowless (off-screen)
      /// rendering. No window will be created for the browser and all rendering
      /// will occur via the ICefRenderHandler interface. The |parent_window| value
      /// will be used to identify monitor info and to act as the parent window for
      /// dialogs, context menus, etc. If |parent_window| is not provided then the
      /// main screen monitor will be used and some functionality that requires a
      /// parent window may not function correctly. In order to create windowless
      /// browsers the TCefSettings.windowless_rendering_enabled value must be set to
      /// true. Transparent painting is enabled by default but can be disabled by
      /// setting TCefBrowserSettings.background_color to an opaque value.
      /// </summary>
      property WindowlessRenderingEnabled    : boolean             read GetWindowlessRenderingEnabled   write SetWindowlessRenderingEnabled;
      /// <summary>
      /// Set to true (1) to enable shared textures for windowless rendering. Only
      /// valid if windowless_rendering_enabled above is also set to true. Currently
      /// only supported on Windows (D3D11).
      /// </summary>
      property SharedTextureEnabled          : boolean             read GetSharedTextureEnabled         write SetSharedTextureEnabled;
      /// <summary>
      /// Set to true (1) to enable the ability to issue BeginFrame requests from
      /// the client application by calling ICefBrowserHost.SendExternalBeginFrame.
      /// </summary>
      property ExternalBeginFrameEnabled     : boolean             read GetExternalBeginFrameEnabled    write SetExternalBeginFrameEnabled;
      /// <summary>
      /// Optionally change the runtime style. Alloy style will always be used if
      /// |windowless_rendering_enabled| is true. See TCefRuntimeStyle
      /// documentation for details.
      /// </summary>
      property RuntimeStyle                  : TCefRuntimeStyle    read GetRuntimeStyle                 write SetRuntimeStyle;
      {$IFDEF MSWINDOWS}
      /// <summary>
      /// Standard parameters required by CreateWindowEx().
      /// Used as the dwExStyle parameter in CreateWindowEx.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-createwindowexw">Read the CreateWindowExW article.</see></para>
      /// </remarks>
      property ExStyle                       : DWORD               read GetExStyle                      write SetExStyle;
      /// <summary>
      /// Used as the dwStyle parameter in CreateWindowEx.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-createwindowexw">Read the CreateWindowExW article.</see></para>
      /// </remarks>
      property Style                         : DWORD               read GetStyle                        write SetStyle;
      /// <summary>
      /// Used as the hMenu parameter in CreateWindowEx.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-createwindowexw">Read the CreateWindowExW article.</see></para>
      /// </remarks>
      property Menu                          : HMENU               read GetMenu                         write SetMenu;
      /// <summary>
      /// Used as the hWndParent parameter in CreateWindowEx.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-createwindowexw">Read the CreateWindowExW article.</see></para>
      /// </remarks>
      property ParentWindow                  : TCefWindowHandle    read GetParentWindow                 write SetParentWindow;
      /// <summary>
      /// Handle for the new browser window. Only used with windowed rendering.
      /// </summary>
      property Window                        : TCefWindowHandle    read GetWindow                       write SetWindow;
      {$ENDIF}
      {$IFDEF MACOSX}
      /// <summary>
      /// Set to true (1) to create the view initially hidden.
      /// </summary>
      property Hidden                        : boolean             read GetHidden                       write SetHidden;
      /// <summary>
      /// NSView pointer for the parent view.
      /// </summary>
      property ParentView                    : TCefWindowHandle    read GetParentView                   write SetParentView;
      /// <summary>
      /// NSView pointer for the new browser view. Only used with windowed
      /// rendering.
      /// </summary>
      property View                          : TCefWindowHandle    read GetView                         write SetView;
      {$ENDIF}
      {$IFDEF LINUX}
      /// <summary>
      /// Pointer for the parent window.
      /// </summary>
      property ParentWindow                  : TCefWindowHandle    read GetParentWindow                 write SetParentWindow;
      /// <summary>
      /// Pointer for the new browser window. Only used with windowed rendering.
      /// </summary>
      property Window                        : TCefWindowHandle    read GetWindow                       write SetWindow;
      {$ENDIF}
  end;

implementation

uses
  {$IFDEF LINUX}{$IFDEF FPC}
    ctypes, keysym, xf86keysym, x, xlib,
    {$IFDEF LCLGTK2}gtk2, glib2, gdk2, gtk2proc, gtk2int, Gtk2Def, gdk2x, Gtk2Extra,{$ENDIF}
    {$IFDEF LCLGTK3}LazGdk3, LazGtk3, LazGLib2, gtk3widgets,{$ENDIF}
    uCEFLinuxFunctions,
  {$ENDIF}{$ENDIF}
  uCEFMiscFunctions;

constructor TCEFWindowInfoWrapper.Create;
begin
  inherited Create;

  FillChar(FWindowInfo, SizeOf(TCefWindowInfo), 0);
end;

{$IFDEF MSWINDOWS}
function TCEFWindowInfoWrapper.GetExStyle : DWORD;
begin
  Result := FWindowInfo.ex_style;
end;

function TCEFWindowInfoWrapper.GetStyle : DWORD;
begin
  Result := FWindowInfo.style;
end;

function TCEFWindowInfoWrapper.GetMenu : HMENU;
begin
  Result := FWindowInfo.menu;
end;
{$ENDIF}

function TCEFWindowInfoWrapper.GetWindowName : ustring;
begin
  Result := CefString(@FWindowInfo.window_name);
end;

function TCEFWindowInfoWrapper.GetBounds : TCefRect;
begin
  Result := FWindowInfo.bounds;
end;

function TCEFWindowInfoWrapper.GetParentWindow : TCefWindowHandle;
begin
  {$IFDEF MSWINDOWS}
  Result := FWindowInfo.parent_window;
  {$ENDIF}
  {$IFDEF LINUX}
  Result := FWindowInfo.parent_window;
  {$ENDIF}
end;

function TCEFWindowInfoWrapper.GetWindowlessRenderingEnabled : boolean;
begin
  Result := FWindowInfo.windowless_rendering_enabled <> 0;
end;

function TCEFWindowInfoWrapper.GetSharedTextureEnabled : boolean;
begin
  Result := FWindowInfo.shared_texture_enabled <> 0;
end;

function TCEFWindowInfoWrapper.GetExternalBeginFrameEnabled : boolean;
begin
  Result := FWindowInfo.external_begin_frame_enabled <> 0;
end;

function TCEFWindowInfoWrapper.GetWindow : TCefWindowHandle;
begin
  {$IFDEF MSWINDOWS}
  Result := FWindowInfo.window;
  {$ENDIF}
  {$IFDEF LINUX}
  Result := FWindowInfo.window;
  {$ENDIF}
end;

function TCEFWindowInfoWrapper.GetRuntimeStyle : TCefRuntimeStyle;
begin
  {$IFDEF MSWINDOWS}
  Result := FWindowInfo.runtime_style;
  {$ENDIF}
  {$IFDEF MACOSX}
  Result := FWindowInfo.runtime_style;
  {$ENDIF}
  {$IFDEF LINUX}
  Result := FWindowInfo.runtime_style;
  {$ENDIF}
end;

{$IFDEF MACOSX}
function TCEFWindowInfoWrapper.GetHidden : boolean;
begin
  Result := FWindowInfo.hidden <> 0;
end;

function TCEFWindowInfoWrapper.GetParentView : TCefWindowHandle;
begin
  Result := FWindowInfo.parent_view;
end;

function TCEFWindowInfoWrapper.GetView : TCefWindowHandle;
begin
  Result := FWindowInfo.view;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure TCEFWindowInfoWrapper.SetExStyle(aValue : DWORD);
begin
  FWindowInfo.ex_style := aValue;
end;

procedure TCEFWindowInfoWrapper.SetStyle(aValue : DWORD);
begin
  FWindowInfo.style := aValue;
end;

procedure TCEFWindowInfoWrapper.SetMenu(aValue : HMENU);
begin
  FWindowInfo.menu := aValue;
end;
{$ENDIF}

procedure TCEFWindowInfoWrapper.SetWindowName(const aValue : ustring);
begin
  FWindowInfo.window_name := CefString(aValue);
end;

procedure TCEFWindowInfoWrapper.SetBounds(const aValue : TCefRect);
begin
  FWindowInfo.bounds := aValue;
end;

procedure TCEFWindowInfoWrapper.SetParentWindow(aValue : TCefWindowHandle);
begin
  {$IFDEF MSWINDOWS}
  FWindowInfo.parent_window := aValue;
  {$ENDIF}
  {$IFDEF LINUX}
  FWindowInfo.parent_window := aValue;
  {$ENDIF}
end;

procedure TCEFWindowInfoWrapper.SetWindowlessRenderingEnabled(aValue : boolean);
begin
  FWindowInfo.windowless_rendering_enabled := ord(aValue);
end;

procedure TCEFWindowInfoWrapper.SetSharedTextureEnabled(aValue : boolean);
begin
  FWindowInfo.shared_texture_enabled := ord(aValue);
end;

procedure TCEFWindowInfoWrapper.SetExternalBeginFrameEnabled(aValue : boolean);
begin
  FWindowInfo.external_begin_frame_enabled := ord(aValue);
end;

procedure TCEFWindowInfoWrapper.SetWindow(aValue : TCefWindowHandle);
begin
  {$IFDEF MSWINDOWS}
  FWindowInfo.window := aValue;
  {$ENDIF}
  {$IFDEF LINUX}
  FWindowInfo.window := aValue;
  {$ENDIF}
end;

procedure TCEFWindowInfoWrapper.SetRuntimeStyle(aValue : TCefRuntimeStyle);
begin
  {$IFDEF MSWINDOWS}
  FWindowInfo.runtime_style := aValue;
  {$ENDIF}
  {$IFDEF MACOSX}
  FWindowInfo.runtime_style := aValue;
  {$ENDIF}
  {$IFDEF LINUX}
  FWindowInfo.runtime_style := aValue;
  {$ENDIF}
end;

{$IFDEF MACOSX}
procedure TCEFWindowInfoWrapper.SetHidden(aValue : boolean);
begin
  FWindowInfo.hidden := ord(aValue);
end;

procedure TCEFWindowInfoWrapper.SetParentView(aValue : TCefWindowHandle);
begin
  FWindowInfo.parent_view := aValue;
end;

procedure TCEFWindowInfoWrapper.SetView(aValue : TCefWindowHandle);
begin
  FWindowInfo.view := aValue;
end;
{$ENDIF}

procedure TCEFWindowInfoWrapper.CopyFromWindowInfo(const aSrcWindowInfo: TCEFWindowInfo);
begin
  {$IFDEF MSWINDOWS}
  FWindowInfo.ex_style                      := aSrcWindowInfo.ex_style;
  CefStringSet(FWindowInfo.window_name, aSrcWindowInfo.window_name);
  FWindowInfo.style                         := aSrcWindowInfo.style;
  FWindowInfo.bounds                        := aSrcWindowInfo.bounds;
  FWindowInfo.parent_window                 := aSrcWindowInfo.parent_window;
  FWindowInfo.menu                          := aSrcWindowInfo.menu;
  FWindowInfo.windowless_rendering_enabled  := aSrcWindowInfo.windowless_rendering_enabled;
  FWindowInfo.shared_texture_enabled        := aSrcWindowInfo.shared_texture_enabled;
  FWindowInfo.external_begin_frame_enabled  := aSrcWindowInfo.external_begin_frame_enabled;
  FWindowInfo.window                        := aSrcWindowInfo.window;
  FWindowInfo.runtime_style                 := aSrcWindowInfo.runtime_style;
  {$ENDIF}
  {$IFDEF MACOSX}
  CefStringSet(FWindowInfo.window_name, aSrcWindowInfo.window_name);
  FWindowInfo.bounds                        := aSrcWindowInfo.bounds;
  FWindowInfo.hidden                        := aSrcWindowInfo.hidden;
  FWindowInfo.parent_view                   := aSrcWindowInfo.parent_view;
  FWindowInfo.windowless_rendering_enabled  := aSrcWindowInfo.windowless_rendering_enabled;
  FWindowInfo.shared_texture_enabled        := aSrcWindowInfo.shared_texture_enabled;
  FWindowInfo.external_begin_frame_enabled  := aSrcWindowInfo.external_begin_frame_enabled;
  FWindowInfo.view                          := aSrcWindowInfo.view;
  FWindowInfo.runtime_style                 := aSrcWindowInfo.runtime_style;
  {$ENDIF}
  {$IFDEF LINUX}
  CefStringSet(FWindowInfo.window_name, aSrcWindowInfo.window_name);
  FWindowInfo.bounds                        := aSrcWindowInfo.bounds;
  FWindowInfo.parent_window                 := aSrcWindowInfo.parent_window;
  FWindowInfo.windowless_rendering_enabled  := aSrcWindowInfo.windowless_rendering_enabled;
  FWindowInfo.shared_texture_enabled        := aSrcWindowInfo.shared_texture_enabled;
  FWindowInfo.external_begin_frame_enabled  := aSrcWindowInfo.external_begin_frame_enabled;
  FWindowInfo.window                        := aSrcWindowInfo.window;
  FWindowInfo.runtime_style                 := aSrcWindowInfo.runtime_style;
  {$ENDIF}
end;

class procedure TCEFWindowInfoWrapper.AsChild(var aWindowInfo: TCEFWindowInfo; aParent : TCefWindowHandle; aWindowBounds : TRect);
{$IFDEF LINUX}
var
  TempParent : TCefWindowHandle;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  aWindowInfo.style         := WS_CHILD or WS_VISIBLE or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or WS_TABSTOP;
  aWindowInfo.parent_window := aParent;
  aWindowInfo.bounds.x      := aWindowBounds.left;
  aWindowInfo.bounds.y      := aWindowBounds.top;
  aWindowInfo.bounds.width  := aWindowBounds.right  - aWindowBounds.left;
  aWindowInfo.bounds.height := aWindowBounds.bottom - aWindowBounds.top;
  {$ENDIF}
  {$IFDEF MACOSX}
  aWindowInfo.parent_view   := aParent;
  aWindowInfo.bounds.x      := aWindowBounds.left;
  aWindowInfo.bounds.y      := aWindowBounds.top;
  aWindowInfo.bounds.width  := aWindowBounds.right  - aWindowBounds.left;
  aWindowInfo.bounds.height := aWindowBounds.bottom - aWindowBounds.top;
  aWindowInfo.hidden        := ord(False);
  {$ENDIF}
  {$IFDEF LINUX}
  TempParent := aParent;

  {$IFDEF FPC}
    {$IFDEF LCLGTK2}
    if ValidCefWindowHandle(aParent) and (PGtkWidget(aParent)^.window <> nil) then
      TempParent := gdk_window_xwindow(PGtkWidget(aParent)^.window);
    {$ENDIF}
    {$IFDEF LCLGTK3}
    if ValidCefWindowHandle(aParent) and (TGtk3Widget(aParent).Widget <> nil) then
      begin
        // cefclient creates the main window with this code in root_window_gtk.cc
        //   window_ = gtk_window_new(GTK_WINDOW_TOPLEVEL);
        // Then if populates window_info with :
        //   window_info.SetAsChild(GetXWindowForWidget(parent_handle), rect);
        // GetXWindowForWidget returns this :
        //   GDK_WINDOW_XID(gtk_widget_get_window(widget));
        // GDK_WINDOW_XID is a macro equivalent to gdk_x11_drawable_get_xid in gtk2 but
        // in gtk3 we use gdk_x11_window_get_xid instead.
        // LCL sets TGtk3Widget.Widget to gtk_window_new(GTK_WINDOW_TOPLEVEL) for the main form.
        // When we call TChromium.CreateBrowser with the main form as parent we get this error in the console (not in the log) :
        //   [19140:19166:0604/174851.690766:ERROR:x11_software_bitmap_presenter.cc(144)] XGetWindowAttributes failed for window XXXXXXX
        TempParent := gdk_x11_window_get_xid(gtk_widget_get_window(TGtk3Widget(aParent).Widget));
      end;
    {$ENDIF}
  {$ENDIF}

  aWindowInfo.parent_window := TempParent;
  aWindowInfo.bounds.x      := aWindowBounds.left;
  aWindowInfo.bounds.y      := aWindowBounds.top;
  aWindowInfo.bounds.width  := aWindowBounds.right  - aWindowBounds.left;
  aWindowInfo.bounds.height := aWindowBounds.bottom - aWindowBounds.top;
  {$ENDIF}
end;

procedure TCEFWindowInfoWrapper.SetAsChild(aParent : TCefWindowHandle; aWindowBounds : TRect);
begin
  AsChild(FWindowInfo, aParent, aWindowBounds);
end;

{$IFDEF MSWINDOWS}
class procedure TCEFWindowInfoWrapper.AsPopup(var aWindowInfo: TCEFWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring);
begin
  aWindowInfo.window_name   := CefString(aWindowName);
  aWindowInfo.style         := WS_OVERLAPPEDWINDOW or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or WS_VISIBLE;
  aWindowInfo.bounds.x      := integer(CW_USEDEFAULT);
  aWindowInfo.bounds.y      := integer(CW_USEDEFAULT);
  aWindowInfo.bounds.width  := integer(CW_USEDEFAULT);
  aWindowInfo.bounds.height := integer(CW_USEDEFAULT);
  aWindowInfo.parent_window := aParent;
end;

procedure TCEFWindowInfoWrapper.SetAsPopup(aParent : TCefWindowHandle; const aWindowName : ustring);
begin
  AsPopup(FWindowInfo, aParent, aWindowName);
end;
{$ENDIF}

class procedure TCEFWindowInfoWrapper.AsWindowless(var aWindowInfo: TCEFWindowInfo; aParent : TCefWindowHandle);
begin
  {$IFDEF MSWINDOWS}
  aWindowInfo.windowless_rendering_enabled := ord(True);
  aWindowInfo.parent_window                := aParent;
  aWindowInfo.runtime_style                := CEF_RUNTIME_STYLE_ALLOY;
  {$ENDIF}
  {$IFDEF MACOSX}
  aWindowInfo.windowless_rendering_enabled := ord(True);
  aWindowInfo.parent_view                  := aParent;
  aWindowInfo.runtime_style                := CEF_RUNTIME_STYLE_ALLOY;
  {$ENDIF}
  {$IFDEF LINUX}
  aWindowInfo.windowless_rendering_enabled := ord(True);
  aWindowInfo.parent_window                := aParent;
  aWindowInfo.runtime_style                := CEF_RUNTIME_STYLE_ALLOY;
  {$ENDIF}
end;

procedure TCEFWindowInfoWrapper.SetAsWindowless(aParent : TCefWindowHandle);
begin
  AsWindowless(FWindowInfo, aParent);
end;

end.
