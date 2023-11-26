unit uBrowserFrame;

{$I ..\..\..\source\cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls, System.SyncObjs,
  {$ELSE}
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, SyncObjs,
  {$ENDIF}
  uCEFWinControl, uCEFWindowParent, uCEFChromiumCore, uCEFChromium,
  uCEFInterfaces, uCEFTypes, uCEFConstants, uCEFBufferPanel;

const
  CEF_UPDATECAPTION    = WM_APP + $A55;
  CEF_UPDATEADDRESS    = WM_APP + $A56;
  CEF_UPDATESTATE      = WM_APP + $A57;
  CEF_UPDATESTATUSTEXT = WM_APP + $A58;


type
  TBrowserTitleEvent = procedure(Sender: TObject; const aTitle : string) of object;

  TBrowserFrame = class(TFrame)
      NavControlPnl: TPanel;
      NavButtonPnl: TPanel;
      BackBtn: TButton;
      ForwardBtn: TButton;
      ReloadBtn: TButton;
      StopBtn: TButton;
      URLEditPnl: TPanel;
      URLCbx: TComboBox;
      ConfigPnl: TPanel;
      GoBtn: TButton;
      StatusBar1: TStatusBar;
      chrmosr: TChromium;
      Panel1: TBufferPanel;

      procedure Panel1Enter(Sender: TObject);
      procedure Panel1Exit(Sender: TObject);
      procedure Panel1Resize(Sender: TObject);
      procedure Panel1Click(Sender: TObject);
      procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure Panel1MouseLeave(Sender: TObject);
      procedure Panel1IMECancelComposition(Sender: TObject);
      procedure Panel1IMECommitText(Sender: TObject; const aText: ustring; const replacement_range: PCefRange; relative_cursor_pos: Integer);
      procedure Panel1IMESetComposition(Sender: TObject; const aText: ustring; const underlines: TCefCompositionUnderlineDynArray; const replacement_range, selection_range: TCefRange);

      procedure chrmosrPaint(Sender: TObject; const browser: ICefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
      procedure chrmosrCursorChange(Sender: TObject; const browser: ICefBrowser; cursor_: TCefCursorHandle; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult: Boolean);
      procedure chrmosrGetViewRect(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect);
      procedure chrmosrGetScreenPoint(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer; out Result: Boolean);
      procedure chrmosrGetScreenInfo(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean);
      procedure chrmosrPopupShow(Sender: TObject; const browser: ICefBrowser; show: Boolean);
      procedure chrmosrPopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
      procedure chrmosrAfterCreated(Sender: TObject; const browser: ICefBrowser);
      procedure chrmosrAddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
      procedure chrmosrBeforeClose(Sender: TObject; const browser: ICefBrowser);
      procedure chrmosrLoadError(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring);
      procedure chrmosrLoadingStateChange(Sender: TObject; const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
      procedure chrmosrStatusMessage(Sender: TObject; const browser: ICefBrowser; const value: ustring);
      procedure chrmosrTitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);
      procedure chrmosrBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess, Result: Boolean);
      procedure chrmosrOpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);
      procedure chrmosrTooltip(Sender: TObject; const browser: ICefBrowser; var text: ustring; out Result: Boolean);
      procedure chrmosrIMECompositionRangeChanged(Sender: TObject; const browser: ICefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect);
      procedure chrmosrRenderProcessTerminated(Sender: TObject; const browser: ICefBrowser; status: TCefTerminationStatus);
      procedure chrmosrCanFocus(Sender: TObject);

      procedure BackBtnClick(Sender: TObject);
      procedure ForwardBtnClick(Sender: TObject);
      procedure ReloadBtnClick(Sender: TObject);
      procedure StopBtnClick(Sender: TObject);
      procedure GoBtnClick(Sender: TObject);

    protected
      FHomepage          : string;
      FPopUpBitmap       : TBitmap;
      FPopUpRect         : TRect;
      FShowPopUp         : boolean;
      FResizing          : boolean;
      FPendingResize     : boolean;
      FClosing           : boolean;
      FResizeCS          : TCriticalSection;
      FTabVisible        : boolean;
      FClientInitialized : boolean;

      FLastClickCount  : integer;
      FLastClickTime   : integer;
      FLastClickPoint  : TPoint;
      FLastClickButton : TMouseButton;

      FOnBrowserCreated     : TNotifyEvent;
      FOnBrowserDestroyed   : TNotifyEvent;
      FOnBrowserTitleChange : TBrowserTitleEvent;

      function  GetInitialized : boolean;

      function  getModifiers(Shift: TShiftState): TCefEventFlags;
      function  GetButton(Button: TMouseButton): TCefMouseButtonType;
      procedure DoResize;
      procedure InitializeLastClick;
      function  CancelPreviousClick(x, y : integer; var aCurrentTime : integer) : boolean;

      procedure PendingResizeMsg(var aMessage : TMessage); message CEF_PENDINGRESIZE;
      procedure FocusEnabledMsg(var aMessage : TMessage); message CEF_FOCUSENABLED;

    public
      constructor Create(AOwner : TComponent); override;
      destructor  Destroy; override;
      procedure   NotifyMoveOrResizeStarted;
      procedure   CreateBrowser;
      procedure   CloseBrowser;
      procedure   ShowBrowser;
      procedure   HideBrowser;
      function    CreateClientHandler(var windowInfo : TCefWindowInfo; var client : ICefClient; const targetFrameName : string; const popupFeatures : TCefPopupFeatures) : boolean;
      procedure   HandleBrowserMessage(var Msg: tagMSG; var Handled: Boolean);

      property    Initialized          : boolean             read GetInitialized;
      property    ClientInitialized    : boolean             read FClientInitialized;
      property    Closing              : boolean             read FClosing;
      property    Homepage             : string              read FHomepage              write FHomepage;
      property    OnBrowserCreated     : TNotifyEvent        read FOnBrowserCreated      write FOnBrowserCreated;
      property    OnBrowserDestroyed   : TNotifyEvent        read FOnBrowserDestroyed    write FOnBrowserDestroyed;
      property    OnBrowserTitleChange : TBrowserTitleEvent  read FOnBrowserTitleChange  write FOnBrowserTitleChange;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF DELPHI16_UP}
  System.Math,
  {$ELSE}
  Math,
  {$ENDIF}
  uCEFMiscFunctions, uCEFApplication, uBrowserTab;

// The TChromium events are executed in a CEF thread and we should only update the
// GUI controls in the main application thread.

// This demo saves all the information in those events using a synchronization
// object and sends a custom message to update the GUI in the main application thread.

// Destruction steps
// =================
// 1. TBrowserFrame.CloseBrowser calls TChromium.CloseBrowser which triggers the
//    TChromium.OnClose event and the internal browser is destroyed immediately.
// 2. TChromium.OnBeforeClose is triggered because the internal browser was destroyed
//    and we send a CEF_DESTROYTAB message with the TabID to the main form.

constructor TBrowserFrame.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FPopUpBitmap       := nil;
  FPopUpRect         := rect(0, 0, 0, 0);
  FShowPopUp         := False;
  FResizing          := False;
  FPendingResize     := False;
  FClosing           := False;
  FTabVisible        := True;
  FClientInitialized := False;
  FResizeCS          := TCriticalSection.Create;

  InitializeLastClick;

  FHomepage              := '';
  FOnBrowserDestroyed    := nil;
  FOnBrowserTitleChange  := nil;
  FOnBrowserCreated      := nil;
end;

destructor TBrowserFrame.Destroy;
begin
  chrmosr.ShutdownDragAndDrop;

  if (FPopUpBitmap <> nil) then FreeAndNil(FPopUpBitmap);
  if (FResizeCS    <> nil) then FreeAndNil(FResizeCS);

  inherited Destroy;
end;

procedure TBrowserFrame.HandleBrowserMessage(var Msg: tagMSG; var Handled: Boolean);
var
  TempKeyEvent   : TCefKeyEvent;
  TempMouseEvent : TCefMouseEvent;
  TempPoint      : TPoint;
begin
  case Msg.message of
    WM_SYSCHAR :
      if Panel1.Focused then
        begin
          TempKeyEvent.kind                    := KEYEVENT_CHAR;
          TempKeyEvent.modifiers               := GetCefKeyboardModifiers(Msg.wParam, Msg.lParam);
          TempKeyEvent.windows_key_code        := Msg.wParam;
          TempKeyEvent.native_key_code         := Msg.lParam;
          TempKeyEvent.is_system_key           := ord(True);
          TempKeyEvent.character               := #0;
          TempKeyEvent.unmodified_character    := #0;
          TempKeyEvent.focus_on_editable_field := ord(False);

          CefCheckAltGrPressed(Msg.wParam, TempKeyEvent);
          chrmosr.SendKeyEvent(@TempKeyEvent);
        end;

    WM_SYSKEYDOWN :
      if Panel1.Focused then
        begin
          TempKeyEvent.kind                    := KEYEVENT_RAWKEYDOWN;
          TempKeyEvent.modifiers               := GetCefKeyboardModifiers(Msg.wParam, Msg.lParam);
          TempKeyEvent.windows_key_code        := Msg.wParam;
          TempKeyEvent.native_key_code         := Msg.lParam;
          TempKeyEvent.is_system_key           := ord(True);
          TempKeyEvent.character               := #0;
          TempKeyEvent.unmodified_character    := #0;
          TempKeyEvent.focus_on_editable_field := ord(False);

          chrmosr.SendKeyEvent(@TempKeyEvent);
        end;

    WM_SYSKEYUP :
      if Panel1.Focused then
        begin
          TempKeyEvent.kind                    := KEYEVENT_KEYUP;
          TempKeyEvent.modifiers               := GetCefKeyboardModifiers(Msg.wParam, Msg.lParam);
          TempKeyEvent.windows_key_code        := Msg.wParam;
          TempKeyEvent.native_key_code         := Msg.lParam;
          TempKeyEvent.is_system_key           := ord(True);
          TempKeyEvent.character               := #0;
          TempKeyEvent.unmodified_character    := #0;
          TempKeyEvent.focus_on_editable_field := ord(False);

          chrmosr.SendKeyEvent(@TempKeyEvent);
        end;

    WM_KEYDOWN :
      if Panel1.Focused then
        begin
          TempKeyEvent.kind                    := KEYEVENT_RAWKEYDOWN;
          TempKeyEvent.modifiers               := GetCefKeyboardModifiers(Msg.wParam, Msg.lParam);
          TempKeyEvent.windows_key_code        := Msg.wParam;
          TempKeyEvent.native_key_code         := Msg.lParam;
          TempKeyEvent.is_system_key           := ord(False);
          TempKeyEvent.character               := #0;
          TempKeyEvent.unmodified_character    := #0;
          TempKeyEvent.focus_on_editable_field := ord(False);

          chrmosr.SendKeyEvent(@TempKeyEvent);
          Handled := (Msg.wParam in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_TAB]);
        end;

    WM_KEYUP :
      if Panel1.Focused then
        begin
          TempKeyEvent.kind                    := KEYEVENT_KEYUP;
          TempKeyEvent.modifiers               := GetCefKeyboardModifiers(Msg.wParam, Msg.lParam);
          TempKeyEvent.windows_key_code        := Msg.wParam;
          TempKeyEvent.native_key_code         := Msg.lParam;
          TempKeyEvent.is_system_key           := ord(False);
          TempKeyEvent.character               := #0;
          TempKeyEvent.unmodified_character    := #0;
          TempKeyEvent.focus_on_editable_field := ord(False);

          chrmosr.SendKeyEvent(@TempKeyEvent);
          Handled := (Msg.wParam <> VK_MENU);
        end;

    WM_CHAR :
      if Panel1.Focused then
        begin
          TempKeyEvent.kind                    := KEYEVENT_CHAR;
          TempKeyEvent.modifiers               := GetCefKeyboardModifiers(Msg.wParam, Msg.lParam);
          TempKeyEvent.windows_key_code        := Msg.wParam;
          TempKeyEvent.native_key_code         := Msg.lParam;
          TempKeyEvent.is_system_key           := ord(False);
          TempKeyEvent.character               := #0;
          TempKeyEvent.unmodified_character    := #0;
          TempKeyEvent.focus_on_editable_field := ord(False);

          CefCheckAltGrPressed(Msg.wParam, TempKeyEvent);
          chrmosr.SendKeyEvent(@TempKeyEvent);
          Handled := True;
        end;

    WM_MOUSEWHEEL :
      if Panel1.Focused then
        begin
          GetCursorPos(TempPoint);
          TempPoint                := Panel1.ScreenToclient(TempPoint);
          TempMouseEvent.x         := TempPoint.x;
          TempMouseEvent.y         := TempPoint.y;
          TempMouseEvent.modifiers := GetCefMouseModifiers(Msg.wParam);

          DeviceToLogical(TempMouseEvent, Panel1.ScreenScale);

          if CefIsKeyDown(VK_SHIFT) then
            chrmosr.SendMouseWheelEvent(@TempMouseEvent, smallint(Msg.wParam shr 16), 0)
           else
            chrmosr.SendMouseWheelEvent(@TempMouseEvent, 0, smallint(Msg.wParam shr 16));
        end;
  end;
end;

function TBrowserFrame.GetInitialized : boolean;
begin
  Result := chrmosr.Initialized;
end;

procedure TBrowserFrame.NotifyMoveOrResizeStarted;
begin
  chrmosr.NotifyMoveOrResizeStarted;
end;

procedure TBrowserFrame.Panel1Click(Sender: TObject);
begin
  Panel1.SetFocus;
end;

procedure TBrowserFrame.Panel1Enter(Sender: TObject);
begin
  chrmosr.SetFocus(True);
end;

procedure TBrowserFrame.Panel1Exit(Sender: TObject);
begin
  chrmosr.SetFocus(False);
end;

procedure TBrowserFrame.Panel1IMECancelComposition(Sender: TObject);
begin
  chrmosr.IMECancelComposition;
end;

procedure TBrowserFrame.Panel1IMECommitText(Sender: TObject;
  const aText: ustring; const replacement_range: PCefRange;
  relative_cursor_pos: Integer);
begin
  chrmosr.IMECommitText(aText, replacement_range, relative_cursor_pos);
end;

procedure TBrowserFrame.Panel1IMESetComposition(Sender: TObject;
  const aText: ustring; const underlines: TCefCompositionUnderlineDynArray;
  const replacement_range, selection_range: TCefRange);
begin
  chrmosr.IMESetComposition(aText, underlines, @replacement_range, @selection_range);
end;

procedure TBrowserFrame.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
  TempTime  : integer;
begin
  {$IFDEF DELPHI14_UP}
  if (ssTouch in Shift) then exit;
  {$ENDIF}

  Panel1.SetFocus;

  if not(CancelPreviousClick(x, y, TempTime)) and (Button = FLastClickButton) then
    inc(FLastClickCount)
   else
    begin
      FLastClickPoint.x := x;
      FLastClickPoint.y := y;
      FLastClickCount   := 1;
    end;

  FLastClickTime      := TempTime;
  FLastClickButton    := Button;

  TempEvent.x         := X;
  TempEvent.y         := Y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  chrmosr.SendMouseClickEvent(@TempEvent, GetButton(Button), False, FLastClickCount);
end;

procedure TBrowserFrame.Panel1MouseLeave(Sender: TObject);
var
  TempEvent : TCefMouseEvent;
  TempPoint : TPoint;
  TempTime  : integer;
begin
  GetCursorPos(TempPoint);
  TempPoint := Panel1.ScreenToclient(TempPoint);

  if CancelPreviousClick(TempPoint.x, TempPoint.y, TempTime) then InitializeLastClick;

  TempEvent.x         := TempPoint.x;
  TempEvent.y         := TempPoint.y;
  TempEvent.modifiers := GetCefMouseModifiers;
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  chrmosr.SendMouseMoveEvent(@TempEvent, True);
end;

procedure TBrowserFrame.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  TempEvent : TCefMouseEvent;
  TempTime  : integer;
begin
  {$IFDEF DELPHI14_UP}
  if (ssTouch in Shift) then exit;
  {$ENDIF}

  if CancelPreviousClick(x, y, TempTime) then InitializeLastClick;

  TempEvent.x         := X;
  TempEvent.y         := Y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  chrmosr.SendMouseMoveEvent(@TempEvent, False);
end;

procedure TBrowserFrame.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
begin
  {$IFDEF DELPHI14_UP}
  if (ssTouch in Shift) then exit;
  {$ENDIF}

  TempEvent.x         := X;
  TempEvent.y         := Y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  chrmosr.SendMouseClickEvent(@TempEvent, GetButton(Button), True, FLastClickCount);
end;

procedure TBrowserFrame.Panel1Resize(Sender: TObject);
begin
  DoResize;
end;

procedure TBrowserFrame.ReloadBtnClick(Sender: TObject);
begin
  chrmosr.Reload;
end;

procedure TBrowserFrame.StopBtnClick(Sender: TObject);
begin
  chrmosr.StopLoad;
end;

procedure TBrowserFrame.CreateBrowser;
begin
  chrmosr.DefaultURL := FHomepage;

  // opaque white background color
  chrmosr.Options.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);

  Panel1.CreateIMEHandler;

  if chrmosr.CreateBrowser(nil, '') then
    chrmosr.InitializeDragAndDrop(Panel1);
end;

procedure TBrowserFrame.CloseBrowser;
begin
  if not(FClosing) then
    begin
      FClosing              := True;
      NavControlPnl.Enabled := False;
      chrmosr.CloseBrowser(True);
    end;
end;

procedure TBrowserFrame.ShowBrowser;
begin
  if chrmosr.Initialized and not(FTabVisible) then
    begin
      chrmosr.WasHidden(False);
      chrmosr.WasResized;
      chrmosr.SetFocus(True);
      FTabVisible := True;
    end;
end;

procedure TBrowserFrame.HideBrowser;
begin
  if chrmosr.Initialized and FTabVisible then
    begin
      chrmosr.SetFocus(False);
      chrmosr.WasHidden(True);
      FTabVisible := False;
    end;
end;

procedure TBrowserFrame.ForwardBtnClick(Sender: TObject);
begin
  chrmosr.GoForward;
end;

procedure TBrowserFrame.GoBtnClick(Sender: TObject);
begin
  chrmosr.LoadURL(URLCbx.Text);
end;

procedure TBrowserFrame.chrmosrAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  NavControlPnl.Enabled := True;
  if assigned(FOnBrowserCreated) then
    FOnBrowserCreated(self);
end;

procedure TBrowserFrame.BackBtnClick(Sender: TObject);
begin
  chrmosr.GoBack;
end;

procedure TBrowserFrame.chrmosrAddressChange(      Sender  : TObject;
                                             const browser : ICefBrowser;
                                             const frame   : ICefFrame;
                                             const url     : ustring);
begin
  if (URLCbx.Items.IndexOf(url) < 0) then
    URLCbx.Items.Add(url);

  URLCbx.Text := url;
end;

procedure TBrowserFrame.chrmosrBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  if assigned(FOnBrowserDestroyed) then
    FOnBrowserDestroyed(self);
end;

procedure TBrowserFrame.chrmosrBeforePopup(      Sender             : TObject;
                                           const browser            : ICefBrowser;
                                           const frame              : ICefFrame;
                                           const targetUrl          : ustring;
                                           const targetFrameName    : ustring;
                                                 targetDisposition  : TCefWindowOpenDisposition;
                                                 userGesture        : Boolean;
                                           const popupFeatures      : TCefPopupFeatures;
                                           var   windowInfo         : TCefWindowInfo;
                                           var   client             : ICefClient;
                                           var   settings           : TCefBrowserSettings;
                                           var   extra_info         : ICefDictionaryValue;
                                           var   noJavascriptAccess : Boolean;
                                           var   Result             : Boolean);
begin
  Result := not(assigned(Parent) and
                (Parent is TBrowserTab) and
                TBrowserTab(Parent).DoOnBeforePopup(windowInfo, client, targetFrameName, popupFeatures, targetDisposition));
end;

procedure TBrowserFrame.chrmosrOpenUrlFromTab(      Sender            : TObject;
                                              const browser           : ICefBrowser;
                                              const frame             : ICefFrame;
                                              const targetUrl         : ustring;
                                                    targetDisposition : TCefWindowOpenDisposition;
                                                    userGesture       : Boolean;
                                              out   Result            : Boolean);
begin
  Result := assigned(Parent) and
            (Parent is TBrowserTab) and
            TBrowserTab(Parent).DoOpenUrlFromTab(targetUrl, targetDisposition);
end;

procedure TBrowserFrame.chrmosrPaint(Sender: TObject;
  const browser: ICefBrowser; type_: TCefPaintElementType;
  dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray;
  const buffer: Pointer; width, height: Integer);
var
  src, dst: PByte;
  i, j, TempLineSize, TempSrcOffset, TempDstOffset, SrcStride, DstStride : Integer;
  n : NativeUInt;
  TempWidth, TempHeight, TempScanlineSize : integer;
  TempBufferBits : Pointer;
  TempForcedResize : boolean;
  TempSrcRect : TRect;
begin
  try
    FResizeCS.Acquire;
    TempForcedResize := False;

    if Panel1.BeginBufferDraw then
      begin
        if (type_ = PET_POPUP) then
          begin
            if (FPopUpBitmap = nil) or
               (width  <> FPopUpBitmap.Width) or
               (height <> FPopUpBitmap.Height) then
              begin
                if (FPopUpBitmap <> nil) then FPopUpBitmap.Free;

                FPopUpBitmap             := TBitmap.Create;
                FPopUpBitmap.PixelFormat := pf32bit;
                FPopUpBitmap.HandleType  := bmDIB;
                FPopUpBitmap.Width       := width;
                FPopUpBitmap.Height      := height;
              end;

            TempWidth        := FPopUpBitmap.Width;
            TempHeight       := FPopUpBitmap.Height;
            TempScanlineSize := FPopUpBitmap.Width * SizeOf(TRGBQuad);
            TempBufferBits   := FPopUpBitmap.Scanline[pred(FPopUpBitmap.Height)];
          end
         else
          begin
            TempForcedResize := Panel1.UpdateBufferDimensions(Width, Height) or not(Panel1.BufferIsResized(False));
            TempWidth        := Panel1.BufferWidth;
            TempHeight       := Panel1.BufferHeight;
            TempScanlineSize := Panel1.ScanlineSize;
            TempBufferBits   := Panel1.BufferBits;
          end;

        if (TempBufferBits <> nil) then
          begin
            SrcStride := Width * SizeOf(TRGBQuad);
            DstStride := - TempScanlineSize;

            n := 0;

            while (n < dirtyRectsCount) do
              begin
                if (dirtyRects[n].x >= 0) and (dirtyRects[n].y >= 0) then
                  begin
                    TempLineSize := min(dirtyRects[n].width, TempWidth - dirtyRects[n].x) * SizeOf(TRGBQuad);

                    if (TempLineSize > 0) then
                      begin
                        TempSrcOffset := ((dirtyRects[n].y * Width) + dirtyRects[n].x) * SizeOf(TRGBQuad);
                        TempDstOffset := ((TempScanlineSize * pred(TempHeight)) - (dirtyRects[n].y * TempScanlineSize)) +
                                         (dirtyRects[n].x * SizeOf(TRGBQuad));

                        src := @PByte(buffer)[TempSrcOffset];
                        dst := @PByte(TempBufferBits)[TempDstOffset];

                        i := 0;
                        j := min(dirtyRects[n].height, TempHeight - dirtyRects[n].y);

                        while (i < j) do
                          begin
                            Move(src^, dst^, TempLineSize);

                            Inc(dst, DstStride);
                            Inc(src, SrcStride);
                            inc(i);
                          end;
                      end;
                  end;

                inc(n);
              end;

            if FShowPopup and (FPopUpBitmap <> nil) then
              begin
                TempSrcRect := Rect(0, 0,
                                    min(FPopUpRect.Right  - FPopUpRect.Left, FPopUpBitmap.Width),
                                    min(FPopUpRect.Bottom - FPopUpRect.Top,  FPopUpBitmap.Height));

                Panel1.BufferDraw(FPopUpBitmap, TempSrcRect, FPopUpRect);
              end;
          end;

        Panel1.EndBufferDraw;
        Panel1.InvalidatePanel;

        if (type_ = PET_VIEW) then
          begin
            if TempForcedResize or FPendingResize then
              PostMessage(Handle, CEF_PENDINGRESIZE, 0, 0);

            FResizing      := False;
            FPendingResize := False;
          end;
      end;
  finally
    FResizeCS.Release;
  end;
end;

procedure TBrowserFrame.chrmosrPopupShow(Sender: TObject;
  const browser: ICefBrowser; show: Boolean);
begin
  if show then
    FShowPopUp := True
   else
    begin
      FShowPopUp := False;
      FPopUpRect := rect(0, 0, 0, 0);

      if (chrmosr <> nil) then chrmosr.Invalidate(PET_VIEW);
    end;
end;

procedure TBrowserFrame.chrmosrPopupSize(Sender: TObject;
  const browser: ICefBrowser; const rect: PCefRect);
begin
  LogicalToDevice(rect^, Panel1.ScreenScale);

  FPopUpRect.Left   := rect.x;
  FPopUpRect.Top    := rect.y;
  FPopUpRect.Right  := rect.x + rect.width  - 1;
  FPopUpRect.Bottom := rect.y + rect.height - 1;
end;

procedure TBrowserFrame.chrmosrRenderProcessTerminated(Sender: TObject;
  const browser: ICefBrowser; status: TCefTerminationStatus);
begin
  StatusBar1.Panels[0].Text := 'The render process crashed!';
end;

procedure TBrowserFrame.chrmosrCanFocus(Sender: TObject);
begin
  // The browser required some time to create associated internal objects
  // before being able to accept the focus. Now we can set the focus on the
  // TBufferPanel control
  PostMessage(Handle, CEF_FOCUSENABLED, 0, 0);
end;

procedure TBrowserFrame.chrmosrCursorChange(Sender: TObject;
  const browser: ICefBrowser; cursor_: TCefCursorHandle;
  cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo;
  var aResult: Boolean);
begin
  Panel1.Cursor := CefCursorToWindowsCursor(cursorType);
  aResult       := True;
end;

procedure TBrowserFrame.chrmosrGetScreenInfo(Sender: TObject;
  const browser: ICefBrowser; var screenInfo: TCefScreenInfo;
  out Result: Boolean);
var
  TempRect  : TCEFRect;
  TempScale : single;
begin
  TempScale       := Panel1.ScreenScale;
  TempRect.x      := 0;
  TempRect.y      := 0;
  TempRect.width  := DeviceToLogical(Panel1.Width,  TempScale);
  TempRect.height := DeviceToLogical(Panel1.Height, TempScale);

  screenInfo.device_scale_factor := TempScale;
  screenInfo.depth               := 0;
  screenInfo.depth_per_component := 0;
  screenInfo.is_monochrome       := Ord(False);
  screenInfo.rect                := TempRect;
  screenInfo.available_rect      := TempRect;

  Result := True;
end;

procedure TBrowserFrame.chrmosrGetScreenPoint(Sender: TObject;
  const browser: ICefBrowser; viewX, viewY: Integer; var screenX,
  screenY: Integer; out Result: Boolean);
var
  TempScreenPt, TempViewPt : TPoint;
  TempScale : single;
begin
  TempScale    := Panel1.ScreenScale;
  TempViewPt.x := LogicalToDevice(viewX, TempScale);
  TempViewPt.y := LogicalToDevice(viewY, TempScale);
  TempScreenPt := Panel1.ClientToScreen(TempViewPt);
  screenX      := TempScreenPt.x;
  screenY      := TempScreenPt.y;
  Result       := True;
end;

procedure TBrowserFrame.chrmosrGetViewRect(Sender: TObject;
  const browser: ICefBrowser; var rect: TCefRect);
var
  TempScale : single;
begin
  TempScale   := Panel1.ScreenScale;
  rect.x      := 0;
  rect.y      := 0;
  rect.width  := DeviceToLogical(Panel1.Width,  TempScale);
  rect.height := DeviceToLogical(Panel1.Height, TempScale);
end;

procedure TBrowserFrame.chrmosrIMECompositionRangeChanged(Sender: TObject;
  const browser: ICefBrowser; const selected_range: PCefRange;
  character_boundsCount: NativeUInt; const character_bounds: PCefRect);
var
  TempDeviceBounds : TCefRectDynArray;
  TempPRect        : PCefRect;
  i                : NativeUInt;
  TempScale        : single;
begin
  TempDeviceBounds := nil;

  try
    if (character_boundsCount > 0) then
      begin
        SetLength(TempDeviceBounds, character_boundsCount);

        i         := 0;
        TempPRect := character_bounds;
        TempScale := Panel1.ScreenScale;

        while (i < character_boundsCount) do
          begin
            TempDeviceBounds[i] := TempPRect^;
            LogicalToDevice(TempDeviceBounds[i], TempScale);

            inc(TempPRect);
            inc(i);
          end;
      end;

    Panel1.ChangeCompositionRange(selected_range^, TempDeviceBounds);
  finally
    if (TempDeviceBounds <> nil) then
      begin
        Finalize(TempDeviceBounds);
        TempDeviceBounds := nil;
      end;
  end;
end;

procedure TBrowserFrame.chrmosrLoadError(      Sender    : TObject;
                                         const browser   : ICefBrowser;
                                         const frame     : ICefFrame;
                                               errorCode : TCefErrorCode;
                                         const errorText : ustring;
                                         const failedUrl : ustring);
var
  TempString : string;
begin
  if (errorCode = ERR_ABORTED) then exit;

  TempString := '<html><body bgcolor="white">' +
                '<h2>Failed to load URL ' + failedUrl +
                ' with error ' + errorText +
                ' (' + inttostr(errorCode) + ').</h2></body></html>';

  chrmosr.LoadString(TempString, frame);
end;

procedure TBrowserFrame.chrmosrLoadingStateChange(      Sender       : TObject;
                                                  const browser      : ICefBrowser;
                                                        isLoading    : Boolean;
                                                        canGoBack    : Boolean;
                                                        canGoForward : Boolean);
begin
  BackBtn.Enabled    := canGoBack;
  ForwardBtn.Enabled := canGoForward;

  if isLoading then
    begin
      ReloadBtn.Enabled := False;
      StopBtn.Enabled   := True;
    end
   else
    begin
      ReloadBtn.Enabled := True;
      StopBtn.Enabled   := False;
    end;
end;

procedure TBrowserFrame.chrmosrStatusMessage(      Sender  : TObject;
                                             const browser : ICefBrowser;
                                             const value   : ustring);
begin
  StatusBar1.Panels[0].Text := value;
end;

procedure TBrowserFrame.chrmosrTitleChange(      Sender  : TObject;
                                           const browser : ICefBrowser;
                                           const title   : ustring);
var
  TempTitle : string;
begin
  if (length(title) > 0) then
    TempTitle := title
   else
    TempTitle := chrmosr.DocumentURL;

  if assigned(FOnBrowserTitleChange) then
    FOnBrowserTitleChange(Sender, TempTitle);
end;

procedure TBrowserFrame.chrmosrTooltip(Sender: TObject;
  const browser: ICefBrowser; var text: ustring; out Result: Boolean);
begin
  Panel1.hint     := text;
  Panel1.ShowHint := (length(text) > 0);
  Result          := True;
end;

function TBrowserFrame.getModifiers(Shift: TShiftState): TCefEventFlags;
begin
  Result := EVENTFLAG_NONE;

  if (ssShift  in Shift) then Result := Result or EVENTFLAG_SHIFT_DOWN;
  if (ssAlt    in Shift) then Result := Result or EVENTFLAG_ALT_DOWN;
  if (ssCtrl   in Shift) then Result := Result or EVENTFLAG_CONTROL_DOWN;
  if (ssLeft   in Shift) then Result := Result or EVENTFLAG_LEFT_MOUSE_BUTTON;
  if (ssRight  in Shift) then Result := Result or EVENTFLAG_RIGHT_MOUSE_BUTTON;
  if (ssMiddle in Shift) then Result := Result or EVENTFLAG_MIDDLE_MOUSE_BUTTON;
end;

function TBrowserFrame.GetButton(Button: TMouseButton): TCefMouseButtonType;
begin
  case Button of
    TMouseButton.mbRight  : Result := MBT_RIGHT;
    TMouseButton.mbMiddle : Result := MBT_MIDDLE;
    else                    Result := MBT_LEFT;
  end;
end;

procedure TBrowserFrame.PendingResizeMsg(var aMessage : TMessage);
begin
  DoResize;
end;

procedure TBrowserFrame.FocusEnabledMsg(var aMessage : TMessage);
begin
  if Panel1.Focused then
    chrmosr.SetFocus(True)
   else
    Panel1.SetFocus;
end;

procedure TBrowserFrame.DoResize;
begin
  try
    FResizeCS.Acquire;

    if FResizing then
      FPendingResize := True
     else
      if Panel1.BufferIsResized then
        chrmosr.Invalidate(PET_VIEW)
       else
        begin
          FResizing := True;
          chrmosr.WasResized;
        end;
  finally
    FResizeCS.Release;
  end;
end;

procedure TBrowserFrame.InitializeLastClick;
begin
  FLastClickCount   := 1;
  FLastClickTime    := 0;
  FLastClickPoint.x := 0;
  FLastClickPoint.y := 0;
  FLastClickButton  := mbLeft;
end;

function TBrowserFrame.CancelPreviousClick(x, y : integer; var aCurrentTime : integer) : boolean;
begin
  aCurrentTime := GetMessageTime;

  Result := (abs(FLastClickPoint.x - x) > (GetSystemMetrics(SM_CXDOUBLECLK) div 2)) or
            (abs(FLastClickPoint.y - y) > (GetSystemMetrics(SM_CYDOUBLECLK) div 2)) or
            (cardinal(aCurrentTime - FLastClickTime) > GetDoubleClickTime);
end;

function TBrowserFrame.CreateClientHandler(var   windowInfo        : TCefWindowInfo;
                                           var   client            : ICefClient;
                                           const targetFrameName   : string;
                                           const popupFeatures     : TCefPopupFeatures) : boolean;
begin
  WindowInfoAsWindowless(windowInfo, 0, targetFrameName);
  FClientInitialized := chrmosr.CreateClientHandler(client);
  Result             := FClientInitialized;
end;

end.


