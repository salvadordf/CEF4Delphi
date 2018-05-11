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
//        Copyright � 2018 Salvador D�az Fau. All rights reserved.
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

unit uCEFBrowser;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefBrowserRef = class(TCefBaseRefCountedRef, ICefBrowser)
    protected
      function  GetHost: ICefBrowserHost;
      function  CanGoBack: Boolean;
      procedure GoBack;
      function  CanGoForward: Boolean;
      procedure GoForward;
      function  IsLoading: Boolean;
      procedure Reload;
      procedure ReloadIgnoreCache;
      procedure StopLoad;
      function  GetIdentifier: Integer;
      function  IsSame(const that: ICefBrowser): Boolean;
      function  IsPopup: Boolean;
      function  HasDocument: Boolean;
      function  GetMainFrame: ICefFrame;
      function  GetFocusedFrame: ICefFrame;
      function  GetFrameByident(const identifier: Int64): ICefFrame;
      function  GetFrame(const name: ustring): ICefFrame;
      function  GetFrameCount: NativeUInt;
      function  GetFrameIdentifiers(var aFrameCount : NativeUInt; var aFrameIdentifierArray : TCefFrameIdentifierArray) : boolean;
      function  GetFrameNames(var aFrameNames : TStrings) : boolean;
      function  SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage): Boolean;

    public
      class function UnWrap(data: Pointer): ICefBrowser;
  end;

  TCefBrowserHostRef = class(TCefBaseRefCountedRef, ICefBrowserHost)
    protected
      function  GetBrowser: ICefBrowser;
      procedure CloseBrowser(forceClose: Boolean);
      function  TryCloseBrowser: Boolean;
      procedure SetFocus(focus: Boolean);
      function  GetWindowHandle: TCefWindowHandle;
      function  GetOpenerWindowHandle: TCefWindowHandle;
      function  HasView: Boolean;
      function  GetRequestContext: ICefRequestContext;
      function  GetZoomLevel: Double;
      procedure SetZoomLevel(const zoomLevel: Double);
      procedure RunFileDialog(mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: ICefRunFileDialogCallback);
      procedure RunFileDialogProc(mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: TCefRunFileDialogCallbackProc);
      procedure StartDownload(const url: ustring);
      procedure DownloadImage(const imageUrl: ustring; isFavicon: Boolean; maxImageSize: Cardinal; bypassCache: Boolean; const callback: ICefDownloadImageCallback);
      procedure DownloadImageProc(const imageUrl: ustring; isFavicon: Boolean; maxImageSize: Cardinal; bypassCache: Boolean; const callback: TOnDownloadImageFinishedProc);
      procedure Print;
      procedure PrintToPdf(const path: ustring; settings: PCefPdfPrintSettings; const callback: ICefPdfPrintCallback);
      procedure PrintToPdfProc(const path: ustring; settings: PCefPdfPrintSettings; const callback: TOnPdfPrintFinishedProc);
      procedure Find(identifier: Integer; const searchText: ustring; forward, matchCase, findNext: Boolean);
      procedure StopFinding(clearSelection: Boolean);
      procedure ShowDevTools(const windowInfo: PCefWindowInfo; const client: ICefClient; const settings: PCefBrowserSettings; inspectElementAt: PCefPoint);
      procedure CloseDevTools;
      function  HasDevTools: Boolean;
      procedure GetNavigationEntries(const visitor: ICefNavigationEntryVisitor; currentOnly: Boolean);
      procedure GetNavigationEntriesProc(const proc: TCefNavigationEntryVisitorProc; currentOnly: Boolean);
      procedure SetMouseCursorChangeDisabled(disabled: Boolean);
      function  IsMouseCursorChangeDisabled: Boolean;
      procedure ReplaceMisspelling(const word: ustring);
      procedure AddWordToDictionary(const word: ustring);
      function  IsWindowRenderingDisabled: Boolean;
      procedure WasResized;
      procedure NotifyScreenInfoChanged;
      procedure WasHidden(hidden: Boolean);
      procedure Invalidate(kind: TCefPaintElementType);
      procedure SendKeyEvent(const event: PCefKeyEvent);
      procedure SendMouseClickEvent(const event: PCefMouseEvent; kind: TCefMouseButtonType; mouseUp: Boolean; clickCount: Integer);
      procedure SendMouseMoveEvent(const event: PCefMouseEvent; mouseLeave: Boolean);
      procedure SendMouseWheelEvent(const event: PCefMouseEvent; deltaX, deltaY: Integer);
      procedure SendFocusEvent(setFocus: Boolean);
      procedure SendCaptureLostEvent;
      procedure NotifyMoveOrResizeStarted;
      function  GetWindowlessFrameRate : Integer;
      procedure SetWindowlessFrameRate(frameRate: Integer);
      procedure IMESetComposition(const text: ustring; underlinesCount : NativeUInt; const underlines : PCefCompositionUnderline; const replacement_range, selection_range : PCefRange);
      procedure IMECommitText(const text: ustring; const replacement_range : PCefRange; relative_cursor_pos : integer);
      procedure IMEFinishComposingText(keep_selection : boolean);
      procedure IMECancelComposition;
      procedure DragTargetDragEnter(const dragData: ICefDragData; const event: PCefMouseEvent; allowedOps: TCefDragOperations);
      procedure DragTargetDragOver(const event: PCefMouseEvent; allowedOps: TCefDragOperations);
      procedure DragTargetDragLeave;
      procedure DragTargetDrop(event: PCefMouseEvent);
      procedure DragSourceEndedAt(x, y: Integer; op: TCefDragOperation);
      procedure DragSourceSystemDragEnded;
      function  GetVisibleNavigationEntry : ICefNavigationEntry;
      procedure SetAccessibilityState(accessibilityState: TCefState);
      procedure SetAutoResizeEnabled(enabled: boolean; const min_size, max_size: PCefSize);
      function  GetExtension : ICefExtension;
      function  IsBackgroundHost : boolean;

    public
      class function UnWrap(data: Pointer): ICefBrowserHost;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFDownloadImageCallBack, uCEFFrame, uCEFPDFPrintCallback,
  uCEFRunFileDialogCallback, uCEFRequestContext, uCEFNavigationEntryVisitor, uCEFNavigationEntry,
  uCEFExtension, uCEFStringList;

function TCefBrowserRef.GetHost: ICefBrowserHost;
begin
  Result := TCefBrowserHostRef.UnWrap(PCefBrowser(FData)^.get_host(PCefBrowser(FData)));
end;

function TCefBrowserRef.CanGoBack: Boolean;
begin
  Result := PCefBrowser(FData)^.can_go_back(PCefBrowser(FData)) <> 0;
end;

function TCefBrowserRef.CanGoForward: Boolean;
begin
  Result := PCefBrowser(FData)^.can_go_forward(PCefBrowser(FData)) <> 0;
end;

function TCefBrowserRef.GetFocusedFrame: ICefFrame;
begin
  Result := TCefFrameRef.UnWrap(PCefBrowser(FData)^.get_focused_frame(PCefBrowser(FData)));
end;

function TCefBrowserRef.GetFrameByident(const identifier: Int64): ICefFrame;
begin
  Result := TCefFrameRef.UnWrap(PCefBrowser(FData)^.get_frame_byident(PCefBrowser(FData), identifier));
end;

function TCefBrowserRef.GetFrame(const name: ustring): ICefFrame;
var
  n: TCefString;
begin
  n := CefString(name);
  Result := TCefFrameRef.UnWrap(PCefBrowser(FData)^.get_frame(PCefBrowser(FData), @n));
end;

function TCefBrowserRef.GetFrameCount: NativeUInt;
begin
  Result := PCefBrowser(FData)^.get_frame_count(PCefBrowser(FData));
end;

function TCefBrowserRef.GetFrameIdentifiers(var aFrameCount : NativeUInt; var aFrameIdentifierArray : TCefFrameIdentifierArray) : boolean;
var
  i : NativeUInt;
begin
  Result := False;

  try
    if (aFrameCount > 0) then
      begin
        SetLength(aFrameIdentifierArray, aFrameCount);
        i := 0;
        while (i < aFrameCount) do
          begin
            aFrameIdentifierArray[i] := 0;
            inc(i);
          end;

        PCefBrowser(FData)^.get_frame_identifiers(PCefBrowser(FData), aFrameCount, aFrameIdentifierArray[0]);

        Result := True;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TCefBrowserRef.GetFrameIdentifiers', e) then raise;
  end;
end;

function TCefBrowserRef.GetFrameNames(var aFrameNames : TStrings) : boolean;
var
  TempSL : ICefStringList;
begin
  Result := False;

  if (aFrameNames <> nil) then
    begin
      TempSL := TCefStringListOwn.Create;
      PCefBrowser(FData)^.get_frame_names(PCefBrowser(FData), TempSL.Handle);
      TempSL.CopyToStrings(aFrameNames);
      Result := True;
    end;
end;

function TCefBrowserRef.SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage): Boolean;
begin
  Result := PCefBrowser(FData)^.send_process_message(PCefBrowser(FData), targetProcess, CefGetData(ProcMessage)) <> 0;
end;

function TCefBrowserRef.GetMainFrame: ICefFrame;
begin
  Result := TCefFrameRef.UnWrap(PCefBrowser(FData)^.get_main_frame(PCefBrowser(FData)))
end;

procedure TCefBrowserRef.GoBack;
begin
  PCefBrowser(FData)^.go_back(PCefBrowser(FData));
end;

procedure TCefBrowserRef.GoForward;
begin
  PCefBrowser(FData)^.go_forward(PCefBrowser(FData));
end;

function TCefBrowserRef.IsLoading: Boolean;
begin
  Result := PCefBrowser(FData)^.is_loading(PCefBrowser(FData)) <> 0;
end;

function TCefBrowserRef.HasDocument: Boolean;
begin
  Result := PCefBrowser(FData)^.has_document(PCefBrowser(FData)) <> 0;
end;

function TCefBrowserRef.IsPopup: Boolean;
begin
  Result := PCefBrowser(FData)^.is_popup(PCefBrowser(FData)) <> 0;
end;

function TCefBrowserRef.IsSame(const that: ICefBrowser): Boolean;
begin
  Result := PCefBrowser(FData)^.is_same(PCefBrowser(FData), CefGetData(that)) <> 0;
end;

procedure TCefBrowserRef.Reload;
begin
  PCefBrowser(FData)^.reload(PCefBrowser(FData));
end;

procedure TCefBrowserRef.ReloadIgnoreCache;
begin
  PCefBrowser(FData)^.reload_ignore_cache(PCefBrowser(FData));
end;

procedure TCefBrowserRef.StopLoad;
begin
  PCefBrowser(FData)^.stop_load(PCefBrowser(FData));
end;

function TCefBrowserRef.GetIdentifier: Integer;
begin
  Result := PCefBrowser(FData)^.get_identifier(PCefBrowser(FData));
end;

class function TCefBrowserRef.UnWrap(data: Pointer): ICefBrowser;
begin
  if (data <> nil) then
    Result := Create(data) as ICefBrowser
   else
    Result := nil;
end;

// TCefBrowserHostRef

procedure TCefBrowserHostRef.CloseDevTools;
begin
  PCefBrowserHost(FData).close_dev_tools(FData);
end;

procedure TCefBrowserHostRef.DownloadImage(const imageUrl     : ustring;
                                                 isFavicon    : Boolean;
                                                 maxImageSize : Cardinal;
                                                 bypassCache  : Boolean;
                                           const callback     : ICefDownloadImageCallback);
var
  url: TCefString;
begin
  url := CefString(imageUrl);
  PCefBrowserHost(FData).download_image(FData, @url, Ord(isFavicon), maxImageSize, Ord(bypassCache), CefGetData(callback));
end;

procedure TCefBrowserHostRef.DownloadImageProc(const imageUrl     : ustring;
                                                     isFavicon    : Boolean;
                                                     maxImageSize : Cardinal;
                                                     bypassCache  : Boolean;
                                               const callback     : TOnDownloadImageFinishedProc);
begin
  DownloadImage(imageUrl, isFavicon, maxImageSize, bypassCache, TCefFastDownloadImageCallback.Create(callback));
end;

procedure TCefBrowserHostRef.DragSourceEndedAt(x, y: Integer; op: TCefDragOperation);
begin
  PCefBrowserHost(FData).drag_source_ended_at(FData, x, y, op);
end;

procedure TCefBrowserHostRef.DragSourceSystemDragEnded;
begin
  PCefBrowserHost(FData).drag_source_system_drag_ended(FData);
end;

function TCefBrowserHostRef.GetVisibleNavigationEntry : ICefNavigationEntry;
begin
  Result := TCefNavigationEntryRef.UnWrap(PCefBrowserHost(FData).get_visible_navigation_entry(PCefBrowserHost(FData)));
end;

procedure TCefBrowserHostRef.SetAccessibilityState(accessibilityState: TCefState);
begin
  PCefBrowserHost(FData).set_accessibility_state(FData, accessibilityState);
end;

procedure TCefBrowserHostRef.SetAutoResizeEnabled(enabled: boolean; const min_size, max_size: PCefSize);
begin
  PCefBrowserHost(FData).set_auto_resize_enabled(FData, Ord(enabled), min_size, max_size);
end;

function TCefBrowserHostRef.GetExtension : ICefExtension;
begin
  Result := TCefExtensionRef.UnWrap(PCefBrowserHost(FData).get_extension(FData));
end;

function TCefBrowserHostRef.IsBackgroundHost : boolean;
begin
  Result := PCefBrowserHost(FData).is_background_host(FData) <> 0;
end;

procedure TCefBrowserHostRef.DragTargetDragEnter(const dragData: ICefDragData;
  const event: PCefMouseEvent; allowedOps: TCefDragOperations);
begin
  PCefBrowserHost(FData).drag_target_drag_enter(FData, CefGetData(dragData), event, allowedOps);
end;

procedure TCefBrowserHostRef.DragTargetDragLeave;
begin
  PCefBrowserHost(FData).drag_target_drag_leave(FData);
end;

procedure TCefBrowserHostRef.DragTargetDragOver(const event: PCefMouseEvent;
  allowedOps: TCefDragOperations);
begin
  PCefBrowserHost(FData).drag_target_drag_over(FData, event, allowedOps);
end;

procedure TCefBrowserHostRef.DragTargetDrop(event: PCefMouseEvent);
begin
  PCefBrowserHost(FData).drag_target_drop(FData, event);
end;

procedure TCefBrowserHostRef.Find(identifier: Integer; const searchText: ustring; forward, matchCase, findNext: Boolean);
var
  s: TCefString;
begin
  s := CefString(searchText);
  PCefBrowserHost(FData).find(FData, identifier, @s, Ord(forward), Ord(matchCase), Ord(findNext));
end;

function TCefBrowserHostRef.GetBrowser: ICefBrowser;
begin
  Result := TCefBrowserRef.UnWrap(PCefBrowserHost(FData).get_browser(PCefBrowserHost(FData)));
end;

procedure TCefBrowserHostRef.Print;
begin
  PCefBrowserHost(FData).print(FData);
end;

procedure TCefBrowserHostRef.PrintToPdf(const path     : ustring;
                                              settings : PCefPdfPrintSettings;
                                        const callback : ICefPdfPrintCallback);
var
  str: TCefString;
begin
  str := CefString(path);
  PCefBrowserHost(FData).print_to_pdf(FData, @str, settings, CefGetData(callback));
end;

procedure TCefBrowserHostRef.PrintToPdfProc(const path     : ustring;
                                                  settings : PCefPdfPrintSettings;
                                            const callback : TOnPdfPrintFinishedProc);
begin
  PrintToPdf(path, settings, TCefFastPdfPrintCallback.Create(callback));
end;

procedure TCefBrowserHostRef.ReplaceMisspelling(const word: ustring);
var
  str: TCefString;
begin
  str := CefString(word);
  PCefBrowserHost(FData).replace_misspelling(FData, @str);
end;

procedure TCefBrowserHostRef.RunFileDialog(      mode                 : TCefFileDialogMode;
                                           const title                : ustring;
                                           const defaultFilePath      : ustring;
                                           const acceptFilters        : TStrings;
                                                 selectedAcceptFilter : Integer;
                                           const callback             : ICefRunFileDialogCallback);
var
  TempTitle, TempPath : TCefString;
  TempAcceptFilters : ICefStringList;
begin
  try
    TempTitle := CefString(title);
    TempPath  := CefString(defaultFilePath);

    TempAcceptFilters := TCefStringListOwn.Create;
    TempAcceptFilters.AddStrings(acceptFilters);

    PCefBrowserHost(FData).run_file_dialog(PCefBrowserHost(FData),
                                           mode,
                                           @TempTitle,
                                           @TempPath,
                                           TempAcceptFilters.Handle,
                                           selectedAcceptFilter,
                                           CefGetData(callback));
  finally
    TempAcceptFilters := nil;
  end;
end;

procedure TCefBrowserHostRef.RunFileDialogProc(      mode                 : TCefFileDialogMode;
                                               const title                : ustring;
                                               const defaultFilePath      : ustring;
                                               const acceptFilters        : TStrings;
                                                     selectedAcceptFilter : Integer;
                                               const callback             : TCefRunFileDialogCallbackProc);
begin
  RunFileDialog(mode, title, defaultFilePath, acceptFilters, selectedAcceptFilter, TCefFastRunFileDialogCallback.Create(callback));
end;

procedure TCefBrowserHostRef.AddWordToDictionary(const word: ustring);
var
  str: TCefString;
begin
  str := CefString(word);
  PCefBrowserHost(FData).add_word_to_dictionary(FData, @str);
end;

procedure TCefBrowserHostRef.CloseBrowser(forceClose: Boolean);
begin
  PCefBrowserHost(FData).close_browser(PCefBrowserHost(FData), Ord(forceClose));
end;

procedure TCefBrowserHostRef.SendCaptureLostEvent;
begin
  PCefBrowserHost(FData).send_capture_lost_event(FData);
end;

procedure TCefBrowserHostRef.SendFocusEvent(setFocus: Boolean);
begin
  PCefBrowserHost(FData).send_focus_event(FData, Ord(setFocus));
end;

procedure TCefBrowserHostRef.SendKeyEvent(const event: PCefKeyEvent);
begin
  PCefBrowserHost(FData).send_key_event(FData, event);
end;

procedure TCefBrowserHostRef.SendMouseClickEvent(const event      : PCefMouseEvent;
                                                       kind       : TCefMouseButtonType;
                                                       mouseUp    : Boolean;
                                                       clickCount : Integer);
begin
  PCefBrowserHost(FData).send_mouse_click_event(FData, event, kind, Ord(mouseUp), clickCount);
end;

procedure TCefBrowserHostRef.SendMouseMoveEvent(const event: PCefMouseEvent; mouseLeave: Boolean);
begin
  PCefBrowserHost(FData).send_mouse_move_event(FData, event, Ord(mouseLeave));
end;

procedure TCefBrowserHostRef.SendMouseWheelEvent(const event: PCefMouseEvent; deltaX, deltaY: Integer);
begin
  PCefBrowserHost(FData).send_mouse_wheel_event(FData, event, deltaX, deltaY);
end;

procedure TCefBrowserHostRef.SetFocus(focus: Boolean);
begin
  PCefBrowserHost(FData).set_focus(PCefBrowserHost(FData), Ord(focus));
end;

procedure TCefBrowserHostRef.SetMouseCursorChangeDisabled(disabled: Boolean);
begin
  PCefBrowserHost(FData).set_mouse_cursor_change_disabled(PCefBrowserHost(FData), Ord(disabled));
end;

procedure TCefBrowserHostRef.SetWindowlessFrameRate(frameRate: Integer);
begin
  PCefBrowserHost(FData).set_windowless_frame_rate(PCefBrowserHost(FData), frameRate);
end;

function TCefBrowserHostRef.GetWindowHandle: TCefWindowHandle;
begin
  Result := PCefBrowserHost(FData).get_window_handle(PCefBrowserHost(FData))
end;

function TCefBrowserHostRef.GetWindowlessFrameRate: Integer;
begin
  Result := PCefBrowserHost(FData).get_windowless_frame_rate(PCefBrowserHost(FData));
end;

function TCefBrowserHostRef.GetOpenerWindowHandle: TCefWindowHandle;
begin
  Result := PCefBrowserHost(FData).get_opener_window_handle(PCefBrowserHost(FData));
end;

function TCefBrowserHostRef.GetRequestContext: ICefRequestContext;
begin
  Result := TCefRequestContextRef.UnWrap(PCefBrowserHost(FData).get_request_context(FData));
end;

procedure TCefBrowserHostRef.GetNavigationEntries(const visitor: ICefNavigationEntryVisitor; currentOnly: Boolean);
begin
  PCefBrowserHost(FData).get_navigation_entries(FData, CefGetData(visitor), Ord(currentOnly));
end;

procedure TCefBrowserHostRef.GetNavigationEntriesProc(const proc: TCefNavigationEntryVisitorProc; currentOnly: Boolean);
begin
  GetNavigationEntries(TCefFastNavigationEntryVisitor.Create(proc), currentOnly);
end;

function TCefBrowserHostRef.GetZoomLevel: Double;
begin
  Result := PCefBrowserHost(FData).get_zoom_level(PCefBrowserHost(FData));
end;

procedure TCefBrowserHostRef.IMESetComposition(const text              : ustring;
                                                     underlinesCount   : NativeUInt;
                                               const underlines        : PCefCompositionUnderline;
                                               const replacement_range : PCefRange;
                                               const selection_range   : PCefRange);
var
  TempString : TCefString;
begin
  TempString := CefString(text);
  PCefBrowserHost(FData).ime_set_composition(PCefBrowserHost(FData), @TempString, underlinesCount, underlines, replacement_range, selection_range);
end;

procedure TCefBrowserHostRef.IMECommitText(const text: ustring; const replacement_range : PCefRange; relative_cursor_pos : integer);
var
  TempString : TCefString;
begin
  TempString := CefString(text);
  PCefBrowserHost(FData).ime_commit_text(PCefBrowserHost(FData), @TempString, replacement_range, relative_cursor_pos);
end;

procedure TCefBrowserHostRef.IMEFinishComposingText(keep_selection : boolean);
begin
  PCefBrowserHost(FData).ime_finish_composing_text(PCefBrowserHost(FData), ord(keep_selection));
end;

procedure TCefBrowserHostRef.IMECancelComposition;
begin
  PCefBrowserHost(FData).ime_cancel_composition(PCefBrowserHost(FData));
end;

function TCefBrowserHostRef.HasDevTools: Boolean;
begin
  Result := PCefBrowserHost(FData).has_dev_tools(FData) <> 0;
end;

function TCefBrowserHostRef.HasView: Boolean;
begin
  Result := PCefBrowserHost(FData).has_view(FData) <> 0;
end;

procedure TCefBrowserHostRef.Invalidate(kind: TCefPaintElementType);
begin
  PCefBrowserHost(FData).invalidate(FData, kind);
end;

function TCefBrowserHostRef.IsMouseCursorChangeDisabled: Boolean;
begin
  Result := PCefBrowserHost(FData).is_mouse_cursor_change_disabled(FData) <> 0
end;

function TCefBrowserHostRef.IsWindowRenderingDisabled: Boolean;
begin
  Result := PCefBrowserHost(FData).is_window_rendering_disabled(FData) <> 0
end;

procedure TCefBrowserHostRef.NotifyMoveOrResizeStarted;
begin
  PCefBrowserHost(FData).notify_move_or_resize_started(PCefBrowserHost(FData));
end;

procedure TCefBrowserHostRef.NotifyScreenInfoChanged;
begin
  PCefBrowserHost(FData).notify_screen_info_changed(PCefBrowserHost(FData));
end;

procedure TCefBrowserHostRef.SetZoomLevel(const zoomLevel: Double);
begin
  PCefBrowserHost(FData).set_zoom_level(PCefBrowserHost(FData), zoomLevel);
end;

procedure TCefBrowserHostRef.ShowDevTools(const windowInfo       : PCefWindowInfo;
                                          const client           : ICefClient;
                                          const settings         : PCefBrowserSettings;
                                                inspectElementAt : PCefPoint);
begin
  PCefBrowserHost(FData).show_dev_tools(FData, windowInfo, CefGetData(client), settings, inspectElementAt);
end;

procedure TCefBrowserHostRef.StartDownload(const url: ustring);
var
  u: TCefString;
begin
  u := CefString(url);
  PCefBrowserHost(FData).start_download(PCefBrowserHost(FData), @u);
end;

procedure TCefBrowserHostRef.StopFinding(clearSelection: Boolean);
begin
  PCefBrowserHost(FData).stop_finding(FData, Ord(clearSelection));
end;

function TCefBrowserHostRef.TryCloseBrowser: Boolean;
begin
  Result := PCefBrowserHost(FData).try_close_browser(FData) <> 0;
end;

class function TCefBrowserHostRef.UnWrap(data: Pointer): ICefBrowserHost;
begin
  if data <> nil then
    Result := Create(data) as ICefBrowserHost else
    Result := nil;
end;

procedure TCefBrowserHostRef.WasHidden(hidden: Boolean);
begin
  PCefBrowserHost(FData).was_hidden(FData, Ord(hidden));
end;

procedure TCefBrowserHostRef.WasResized;
begin
  PCefBrowserHost(FData).was_resized(FData);
end;


end.
