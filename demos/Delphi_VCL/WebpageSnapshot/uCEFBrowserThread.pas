// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2021 Salvador Diaz Fau. All rights reserved.
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

unit uCEFBrowserThread;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, System.SyncObjs, System.Math,
  {$ELSE}
  Windows, Messages, SysUtils, Classes, Graphics, SyncObjs, Math,
  {$ENDIF}
  uCEFChromium, uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFBufferPanel, uCEFChromiumCore, uCEFMiscFunctions;

type
  TCEFBrowserThread = class(TThread)
    protected
      FBrowser               : TChromium;
      FPanel                 : TBufferPanel;
      FPanelSize             : TSize;
      FScreenScale           : single;
      FPopUpBitmap           : TBitmap;
      FPopUpRect             : TRect;
      FResizeCS              : TCriticalSection;
      FBrowserInfoCS         : TCriticalSection;
      FShowPopUp             : boolean;
      FClosing               : boolean;
      FResizing              : boolean;
      FPendingResize         : boolean;
      FInitialized           : boolean;
      FDefaultURL            : ustring;
      FSnapshot              : TBitmap;
      FDelayMs               : integer;
      FOnSnapshotAvailable   : TNotifyEvent;
      FOnError               : TNotifyEvent;
      FErrorCode             : integer;
      FErrorText             : ustring;
      FFailedUrl             : ustring;
      FPendingUrl            : ustring;
      FSyncEvents            : boolean;

      function  GetErrorCode : integer;
      function  GetErrorText : ustring;
      function  GetFailedUrl : ustring;
      function  GetInitialized : boolean;

      procedure SetErrorText(const aValue : ustring);

      procedure Panel_OnResize(Sender: TObject);

      procedure Browser_OnAfterCreated(Sender: TObject; const browser: ICefBrowser);
      procedure Browser_OnPaint(Sender: TObject; const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; aWidth, aHeight: Integer);
      procedure Browser_OnGetViewRect(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect);
      procedure Browser_OnGetScreenPoint(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer; out Result: Boolean);
      procedure Browser_OnGetScreenInfo(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean);
      procedure Browser_OnPopupShow(Sender: TObject; const browser: ICefBrowser; show: Boolean);
      procedure Browser_OnPopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
      procedure Browser_OnBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
      procedure Browser_OnBeforeClose(Sender: TObject; const browser: ICefBrowser);
      procedure Browser_OnLoadError(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring);
      procedure Browser_OnLoadingStateChange(Sender: TObject; const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);

      procedure DoOnError;
      procedure DoOnSnapshotAvailable;
      procedure Resize;
      function  CreateBrowser : boolean;
      function  TakeSnapshot : boolean;
      procedure CloseBrowser;
      procedure InitError;
      procedure WebpagePostProcessing;
      procedure WebpageError;
      procedure LoadPendingURL;
      procedure Execute; override;

    public
      constructor Create(const aDefaultURL : ustring; aWidth, aHeight : integer; aDelayMs : integer = 500; const aScreenScale : single = 1);
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      function    TerminateBrowserThread : boolean;
      function    CopySnapshot(var aSnapshot : TBitmap) : boolean;
      function    SaveSnapshotToFile(const aPath : ustring) : boolean;
      procedure   LoadUrl(const aURL : ustring);

      property ErrorCode             : integer           read GetErrorCode;
      property ErrorText             : ustring           read GetErrorText             write SetErrorText;
      property FailedUrl             : ustring           read GetFailedUrl;
      property Initialized           : boolean           read GetInitialized;
      property Closing               : boolean           read FClosing;
      property SyncEvents            : boolean           read FSyncEvents              write FSyncEvents;

      property OnSnapshotAvailable   : TNotifyEvent      read FOnSnapshotAvailable     write FOnSnapshotAvailable;
      property OnError               : TNotifyEvent      read FOnError                 write FOnError;
  end;

implementation

const
  CEF_WEBPAGE_LOADED_MSG   = WM_APP + 1;
  CEF_WEBPAGE_ERROR_MSG    = WM_APP + 2;
  CEF_CLOSE_BROWSER_MSG    = WM_APP + 3;
  CEF_LOAD_PENDING_URL_MSG = WM_APP + 4;


  constructor TCEFBrowserThread.Create(const aDefaultURL : ustring; aWidth, aHeight, aDelayMs : integer; const aScreenScale : single);
begin
  inherited Create(True);

  FreeOnTerminate        := False;
  FInitialized           := False;
  FBrowser               := nil;
  FPanel                 := nil;
  FPanelSize.cx          := aWidth;
  FPanelSize.cy          := aHeight;
  FScreenScale           := aScreenScale;
  FDefaultURL            := aDefaultURL;
  FPopUpBitmap           := nil;
  FPopUpRect             := rect(0, 0, 0, 0);
  FShowPopUp             := False;
  FResizing              := False;
  FPendingResize         := False;
  FResizeCS              := nil;
  FBrowserInfoCS         := nil;
  FSnapshot              := nil;
  FDelayMs               := aDelayMs;
  FOnSnapshotAvailable   := nil;
  FOnError               := nil;
  FClosing               := False;
  FSyncEvents            := False;
end;

destructor TCEFBrowserThread.Destroy;
begin
  if (FBrowser <> nil) then
    FreeAndNil(FBrowser);

  if (FPanel <> nil) then
    FreeAndNil(FPanel);

  if (FPopUpBitmap <> nil) then
    FreeAndNil(FPopUpBitmap);

  if (FSnapshot <> nil) then
    FreeAndNil(FSnapshot);

  if (FResizeCS <> nil) then
    FreeAndNil(FResizeCS);

  if (FBrowserInfoCS <> nil) then
    FreeAndNil(FBrowserInfoCS);

  inherited Destroy;
end;

procedure TCEFBrowserThread.AfterConstruction;
begin
  inherited AfterConstruction;

  FResizeCS                        := TCriticalSection.Create;
  FBrowserInfoCS                   := TCriticalSection.Create;

  FPanel                           := TBufferPanel.Create(nil);
  FPanel.ForcedDeviceScaleFactor   := FScreenScale;
  FPanel.Width                     := FPanelSize.cx;
  FPanel.Height                    := FPanelSize.cy;
  FPanel.OnResize                  := Panel_OnResize;

  FBrowser                         := TChromium.Create(nil);
  FBrowser.DefaultURL              := FDefaultURL;
  FBrowser.Options.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);
  FBrowser.OnAfterCreated          := Browser_OnAfterCreated;
  FBrowser.OnPaint                 := Browser_OnPaint;
  FBrowser.OnGetViewRect           := Browser_OnGetViewRect;
  FBrowser.OnGetScreenPoint        := Browser_OnGetScreenPoint;
  FBrowser.OnGetScreenInfo         := Browser_OnGetScreenInfo;
  FBrowser.OnPopupShow             := Browser_OnPopupShow;
  FBrowser.OnPopupSize             := Browser_OnPopupSize;
  FBrowser.OnBeforePopup           := Browser_OnBeforePopup;
  FBrowser.OnBeforeClose           := Browser_OnBeforeClose;
  FBrowser.OnLoadError             := Browser_OnLoadError;
  FBrowser.OnLoadingStateChange    := Browser_OnLoadingStateChange;
end;

function TCEFBrowserThread.GetErrorCode : integer;
begin
  if assigned(FBrowserInfoCS) then
    try
      FBrowserInfoCS.Acquire;
      Result := FErrorCode;
    finally
      FBrowserInfoCS.Release;
    end
   else
    Result := 0;
end;

function TCEFBrowserThread.GetErrorText : ustring;
begin
  if assigned(FBrowserInfoCS) then
    try
      FBrowserInfoCS.Acquire;
      Result := FErrorText;
    finally
      FBrowserInfoCS.Release;
    end
   else
    Result := '';
end;

function TCEFBrowserThread.GetFailedUrl : ustring;
begin
  if assigned(FBrowserInfoCS) then
    try
      FBrowserInfoCS.Acquire;
      Result := FFailedUrl;
    finally
      FBrowserInfoCS.Release;
    end
   else
    Result := '';
end;

function TCEFBrowserThread.GetInitialized : boolean;
begin
  Result := False;

  if assigned(FBrowserInfoCS) and assigned(FBrowser) then
    try
      FBrowserInfoCS.Acquire;
      Result := FInitialized and FBrowser.Initialized;
    finally
      FBrowserInfoCS.Release;
    end;
end;

procedure TCEFBrowserThread.SetErrorText(const aValue : ustring);
begin
  if assigned(FBrowserInfoCS) then
    try
      FBrowserInfoCS.Acquire;
      FErrorText := aValue;
    finally
      FBrowserInfoCS.Release;
    end;
end;

function TCEFBrowserThread.CopySnapshot(var aSnapshot : TBitmap) : boolean;
begin
  Result := False;

  if FClosing or Terminated or not(Initialized) then exit;

  if assigned(FBrowserInfoCS) then
    try
      try
        FBrowserInfoCS.Acquire;

        if assigned(FSnapshot) and not(FSnapshot.Empty) then
          begin
            if (aSnapshot = nil) then
              begin
                aSnapshot             := TBitmap.Create;
                aSnapshot.PixelFormat := pf32bit;
                aSnapshot.HandleType  := bmDIB;
              end;

            if (aSnapshot.Width <> FSnapshot.Width) then
              aSnapshot.Width := FSnapshot.Width;

            if (aSnapshot.Height <> FSnapshot.Height) then
              aSnapshot.Height := FSnapshot.Height;

            aSnapshot.Canvas.Draw(0, 0, FSnapshot);
            Result := True;
          end;
      except
        on e : exception do
          if CustomExceptionHandler('TCEFBrowserThread.CopySnapshot', e) then raise;
      end;
    finally
      FBrowserInfoCS.Release;
    end;
end;

function TCEFBrowserThread.SaveSnapshotToFile(const aPath : ustring) : boolean;
begin
  Result := False;

  if FClosing or Terminated or not(Initialized) then exit;

  if assigned(FBrowserInfoCS) and (length(aPath) > 0) then
    try
      try
        FBrowserInfoCS.Acquire;

        if assigned(FSnapshot) and not(FSnapshot.Empty) then
          begin
            FSnapshot.SaveToFile(aPath);
            Result := True;
          end;
      except
        on e : exception do
          if CustomExceptionHandler('TCEFBrowserThread.SaveSnapshotToFile', e) then raise;
      end;
    finally
      FBrowserInfoCS.Release;
    end;
end;

procedure TCEFBrowserThread.LoadUrl(const aURL : ustring);
begin
  if FClosing or Terminated or not(Initialized) then exit;

  if assigned(FBrowserInfoCS) then
    try
      FBrowserInfoCS.Acquire;
      FPendingUrl := aURL;
      PostThreadMessage(ThreadID, CEF_LOAD_PENDING_URL_MSG, 0, 0);
    finally
      FBrowserInfoCS.Release;
    end;
end;

function TCEFBrowserThread.TerminateBrowserThread : boolean;
begin
  Result := Initialized and
            PostThreadMessage(ThreadID, CEF_CLOSE_BROWSER_MSG, 0, 0);
end;

procedure TCEFBrowserThread.Panel_OnResize(Sender: TObject);
begin
  Resize;
end;

procedure TCEFBrowserThread.Browser_OnAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  if assigned(FBrowserInfoCS) then
    try
      FBrowserInfoCS.Acquire;
      FInitialized := True;
    finally
      FBrowserInfoCS.Release;
    end;
end;

procedure TCEFBrowserThread.Browser_OnPaint(Sender: TObject; const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; aWidth, aHeight: Integer);
var
  src, dst: PByte;
  i, j, TempLineSize, TempSrcOffset, TempDstOffset, SrcStride, DstStride : Integer;
  n : NativeUInt;
  TempWidth, TempHeight, TempScanlineSize : integer;
  TempBufferBits : Pointer;
  TempForcedResize : boolean;
  TempSrcRect : TRect;
begin
  if assigned(FResizeCS) and assigned(FPanel) then
    try
      FResizeCS.Acquire;
      TempForcedResize := False;

      if FPanel.BeginBufferDraw then
        begin
          if (kind = PET_POPUP) then
            begin
              if (FPopUpBitmap = nil) or
                 (aWidth  <> FPopUpBitmap.Width) or
                 (aHeight <> FPopUpBitmap.Height) then
                begin
                  if (FPopUpBitmap <> nil) then FPopUpBitmap.Free;

                  FPopUpBitmap             := TBitmap.Create;
                  FPopUpBitmap.PixelFormat := pf32bit;
                  FPopUpBitmap.HandleType  := bmDIB;
                  FPopUpBitmap.Width       := aWidth;
                  FPopUpBitmap.Height      := aHeight;
                end;

              TempWidth        := FPopUpBitmap.Width;
              TempHeight       := FPopUpBitmap.Height;
              TempScanlineSize := FPopUpBitmap.Width * SizeOf(TRGBQuad);
              TempBufferBits   := FPopUpBitmap.Scanline[pred(FPopUpBitmap.Height)];
            end
           else
            begin
              TempForcedResize := FPanel.UpdateBufferDimensions(aWidth, aHeight) or not(FPanel.BufferIsResized(False));
              TempWidth        := FPanel.BufferWidth;
              TempHeight       := FPanel.BufferHeight;
              TempScanlineSize := FPanel.ScanlineSize;
              TempBufferBits   := FPanel.BufferBits;
            end;

          if (TempBufferBits <> nil) then
            begin
              SrcStride := aWidth * SizeOf(TRGBQuad);
              DstStride := - TempScanlineSize;

              n := 0;

              while (n < dirtyRectsCount) do
                begin
                  if (dirtyRects[n].x >= 0) and (dirtyRects[n].y >= 0) then
                    begin
                      TempLineSize := min(dirtyRects[n].width, TempWidth - dirtyRects[n].x) * SizeOf(TRGBQuad);

                      if (TempLineSize > 0) then
                        begin
                          TempSrcOffset := ((dirtyRects[n].y * aWidth) + dirtyRects[n].x) * SizeOf(TRGBQuad);
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

                  FPanel.BufferDraw(FPopUpBitmap, TempSrcRect, FPopUpRect);
                end;
            end;

          FPanel.EndBufferDraw;

          if (kind = PET_VIEW) then
            begin
              if TempForcedResize or FPendingResize then
                PostThreadMessage(ThreadID, CEF_PENDINGRESIZE, 0, 0);

              FResizing      := False;
              FPendingResize := False;
            end;
        end;
    finally
      FResizeCS.Release;
    end;
end;

procedure TCEFBrowserThread.Browser_OnGetViewRect(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect);
begin
  if assigned(FPanel) then
    begin
      rect.x      := 0;
      rect.y      := 0;
      rect.width  := DeviceToLogical(FPanel.Width,  FScreenScale);
      rect.height := DeviceToLogical(FPanel.Height, FScreenScale);
    end;
end;

procedure TCEFBrowserThread.Browser_OnGetScreenPoint(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer; out Result: Boolean);
begin
  screenX := LogicalToDevice(viewX, FScreenScale);
  screenY := LogicalToDevice(viewY, FScreenScale);
  Result  := True;
end;

procedure TCEFBrowserThread.Browser_OnGetScreenInfo(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean);
var
  TempRect : TCEFRect;
begin
  if assigned(FPanel) then
    begin
      TempRect.x      := 0;
      TempRect.y      := 0;
      TempRect.width  := DeviceToLogical(FPanel.Width,  FScreenScale);
      TempRect.height := DeviceToLogical(FPanel.Height, FScreenScale);

      screenInfo.device_scale_factor := FScreenScale;
      screenInfo.depth               := 0;
      screenInfo.depth_per_component := 0;
      screenInfo.is_monochrome       := Ord(False);
      screenInfo.rect                := TempRect;
      screenInfo.available_rect      := TempRect;

      Result := True;
    end;
end;

procedure TCEFBrowserThread.Browser_OnPopupShow(Sender: TObject; const browser: ICefBrowser; show: Boolean);
begin
  if show then
    FShowPopUp := True
   else
    begin
      FShowPopUp := False;
      FPopUpRect := rect(0, 0, 0, 0);

      if (FBrowser <> nil) then FBrowser.Invalidate(PET_VIEW);
    end;
end;

procedure TCEFBrowserThread.Browser_OnPopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
begin
  LogicalToDevice(rect^, FScreenScale);

  FPopUpRect.Left   := rect.x;
  FPopUpRect.Top    := rect.y;
  FPopUpRect.Right  := rect.x + rect.width  - 1;
  FPopUpRect.Bottom := rect.y + rect.height - 1;
end;

procedure TCEFBrowserThread.Browser_OnBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TCEFBrowserThread.Browser_OnBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  PostThreadMessage(ThreadID, WM_QUIT, 0, 0);
end;

procedure TCEFBrowserThread.Browser_OnLoadError(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring);
begin
  if not(FClosing) and not(Terminated) and (frame <> nil) and frame.IsValid and frame.IsMain and assigned(FBrowserInfoCS) then
    try
      FBrowserInfoCS.Acquire;

      FErrorCode := errorCode;
      FErrorText := errorText;
      FFailedUrl := failedUrl;

      PostThreadMessage(ThreadID, CEF_WEBPAGE_ERROR_MSG, 0, 0);
    finally
      FBrowserInfoCS.Release;
    end;
end;

procedure TCEFBrowserThread.Browser_OnLoadingStateChange(Sender: TObject; const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if not(FClosing) and not(Terminated) and not(isLoading) then
    PostThreadMessage(ThreadID, CEF_WEBPAGE_LOADED_MSG, 0, 0);
end;

procedure TCEFBrowserThread.Resize;
begin
  if FClosing or Terminated or not(Initialized) then exit;

  if assigned(FResizeCS) and assigned(FPanel) then
    try
      FResizeCS.Acquire;

      if FResizing then
        FPendingResize := True
       else
        if FPanel.BufferIsResized then
          FBrowser.Invalidate(PET_VIEW)
         else
          begin
            FResizing := True;
            FBrowser.WasResized;
          end;
    finally
      FResizeCS.Release;
    end;
end;

function TCEFBrowserThread.CreateBrowser : boolean;
begin
  Result := (FBrowser <> nil) and FBrowser.CreateBrowser;
end;

procedure TCEFBrowserThread.LoadPendingURL;
begin
  if FClosing or Terminated or not(Initialized) then exit;

  if assigned(FBrowserInfoCS) then
    try
      FBrowserInfoCS.Acquire;

      if (length(FPendingURL) > 0) then
        begin
          FBrowser.LoadURL(FPendingURL);
          FPendingURL := '';
        end;
    finally
      FBrowserInfoCS.Release;
    end;
end;

procedure TCEFBrowserThread.WebpagePostProcessing;
begin
  if FClosing or Terminated then
    exit;

  if (FDelayMs > 0) then
    sleep(FDelayMs);

  if TakeSnapshot and assigned(FOnSnapshotAvailable) then
    begin
      if FSyncEvents then
        Synchronize(DoOnSnapshotAvailable)
       else
        DoOnSnapshotAvailable;
    end;
end;

procedure TCEFBrowserThread.WebpageError;
begin
  if not(FClosing) and not(Terminated) and assigned(FOnError) then
    begin
      if FSyncEvents then
        Synchronize(DoOnError)
       else
        DoOnError;
    end;
end;

function TCEFBrowserThread.TakeSnapshot : boolean;
begin
  Result := False;

  if FClosing or Terminated or not(Initialized) then exit;

  if assigned(FBrowserInfoCS) and assigned(FPanel) and FPanel.BeginBufferDraw then
    try
      FBrowserInfoCS.Acquire;

      if assigned(FPanel.Buffer) and not(FPanel.Buffer.Empty) then
        begin
          if (FSnapshot = nil) then
            begin
              FSnapshot             := TBitmap.Create;
              FSnapshot.PixelFormat := pf32bit;
              FSnapshot.HandleType  := bmDIB;
            end;

          if (FSnapshot.Width <> FPanel.BufferWidth) then
            FSnapshot.Width := FPanel.BufferWidth;

          if (FSnapshot.Height <> FPanel.BufferHeight) then
            FSnapshot.Height := FPanel.BufferHeight;

          FSnapshot.Canvas.Draw(0, 0, FPanel.Buffer);
          Result := True;
        end;
    finally
      FBrowserInfoCS.Release;
      FPanel.EndBufferDraw;
    end;
end;

procedure TCEFBrowserThread.CloseBrowser;
begin
  if not(FClosing) and assigned(FBrowser) then
    begin
      FClosing := True;
      FBrowser.CloseBrowser(True);
    end;
end;

procedure TCEFBrowserThread.DoOnError;
begin
  FOnError(self);
end;

procedure TCEFBrowserThread.DoOnSnapshotAvailable;
begin
  FOnSnapshotAvailable(self);
end;

procedure TCEFBrowserThread.InitError;
begin
  ErrorText := 'There was an error initializing the CEF browser.';

  if FSyncEvents then
    Synchronize(DoOnError)
   else
    DoOnError;
end;

procedure TCEFBrowserThread.Execute;
var
  TempCont : boolean;
  TempMsg  : TMsg;
begin
  if CreateBrowser then
    begin
      TempCont := True;
      PeekMessage(TempMsg, 0, WM_USER, WM_USER, PM_NOREMOVE);

      while TempCont and GetMessage(TempMsg, 0, 0, 0) and not(Terminated) do
        begin
          case TempMsg.Message of
            CEF_PENDINGRESIZE        : Resize;
            CEF_CLOSE_BROWSER_MSG    : CloseBrowser;
            CEF_LOAD_PENDING_URL_MSG : LoadPendingURL;
            CEF_WEBPAGE_LOADED_MSG   : WebpagePostProcessing;
            CEF_WEBPAGE_ERROR_MSG    : WebpageError;
            WM_QUIT                  : TempCont := False;
          end;

          DispatchMessage(TempMsg);
        end;
    end
   else
    InitError;
end;

end.
