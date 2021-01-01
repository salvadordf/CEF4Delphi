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

unit uCEFTask;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefTaskOwn = class(TCefBaseRefCountedOwn, ICefTask)
    protected
      procedure Execute; virtual;

    public
      constructor Create; virtual;
  end;

  TCefTaskRef = class(TCefBaseRefCountedRef, ICefTask)
    protected
      procedure Execute; virtual;

    public
      class function UnWrap(data: Pointer): ICefTask;
  end;

  TCefFastTask = class(TCefTaskOwn)
    protected
      FMethod: TCefFastTaskProc;

      procedure Execute; override;

    public
      class procedure New(threadId: TCefThreadId; const method: TCefFastTaskProc);
      class procedure NewDelayed(threadId: TCefThreadId; Delay: Int64; const method: TCefFastTaskProc);
      constructor Create(const method: TCefFastTaskProc); reintroduce;
  end;

  TCefUpdatePrefsTask = class(TCefTaskOwn)
    protected
      FEvents : Pointer;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents); reintroduce;
      destructor  Destroy; override;
  end;

  TCefSavePrefsTask = class(TCefTaskOwn)
    protected
      FEvents : Pointer;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents); reintroduce;
      destructor  Destroy; override;
  end;

  TCefURLRequestTask = class(TCefTaskOwn)
    protected
      FEvents : Pointer;

      procedure Execute; override;

    public
      constructor Create(const aEvents : ICEFUrlRequestClientEvents); reintroduce;
      destructor  Destroy; override;
  end;

  TCefGenericTask = class(TCefTaskOwn)
    protected
      FEvents : Pointer;
      FTaskID : cardinal;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents; aTaskID : cardinal); reintroduce;
      destructor  Destroy; override;
  end;

  TCefUpdateZoomStepTask = class(TCefTaskOwn)
    protected
      FEvents : Pointer;
      FInc    : boolean;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents; aInc : boolean); reintroduce;
      destructor  Destroy; override;
  end;

  TCefUpdateZoomPctTask = class(TCefTaskOwn)
    protected
      FEvents : Pointer;
      FInc    : boolean;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents; aInc : boolean); reintroduce;
      destructor  Destroy; override;
  end;

  TCefReadZoomTask = class(TCefTaskOwn)
    protected
      FEvents : Pointer;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents); reintroduce;
      destructor  Destroy; override;
  end;

  TCefSetZoomLevelTask = class(TCefTaskOwn)
    protected
      FEvents : Pointer;
      FValue  : double;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents; const aValue : double); reintroduce;
      destructor  Destroy; override;
  end;

  TCefSetZoomPctTask = class(TCefTaskOwn)
    protected
      FEvents : Pointer;
      FValue  : double;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents; const aValue : double); reintroduce;
      destructor  Destroy; override;
  end;

  TCefSetZoomStepTask = class(TCefTaskOwn)
    protected
      FEvents : Pointer;
      FValue  : byte;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents; aValue : byte); reintroduce;
      destructor  Destroy; override;
  end;

  TCefCreateCustomViewTask = class(TCefTaskOwn)
    protected
      FEvents : Pointer;

      procedure Execute; override;

    public
      constructor Create(const aEvents : ICefViewDelegateEvents); reintroduce;
      destructor  Destroy; override;
  end;

  TCefBrowserNavigationTask = class(TCefTaskOwn)
    protected
      FEvents : Pointer;
      FTask   : TCefBrowserNavigation;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents; aTask : TCefBrowserNavigation); reintroduce;
      destructor  Destroy; override;
  end;

  TCefUpdateSizeTask = class(TCefTaskOwn)
    protected
      FEvents : Pointer;
      FLeft   : integer;
      FTop    : integer;
      FWidth  : integer;
      FHeight : integer;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents; aLeft, aTop, aWidth, aHeight : integer); reintroduce;
      destructor  Destroy; override;
  end;

  TCefSendCaptureLostEventTask = class(TCefTaskOwn)
    protected
      FEvents : Pointer;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents); reintroduce;
      destructor  Destroy; override;
  end;

  TCefUpdateXWindowVisibilityTask = class(TCefTaskOwn)
    protected
      FEvents  : pointer;
      FVisible : boolean;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents; aVisible : boolean); reintroduce;
      destructor  Destroy; override;
  end;


implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFCookieManager, uCEFUrlRequest;

procedure cef_task_execute(self: PCefTask); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefTaskOwn) then
    TCefTaskOwn(TempObject).Execute;
end;

constructor TCefTaskOwn.Create;
begin
  inherited CreateData(SizeOf(TCefTask));

  PCefTask(FData)^.execute := {$IFDEF FPC}@{$ENDIF}cef_task_execute;
end;

procedure TCefTaskOwn.Execute;
begin
  //
end;


// TCefTaskRef


procedure TCefTaskRef.Execute;
begin
  PCefTask(FData)^.execute(PCefTask(FData));
end;

class function TCefTaskRef.UnWrap(data: Pointer): ICefTask;
begin
  if (data <> nil) then
    Result := Create(data) as ICefTask
   else
    Result := nil;
end;


// TCefFastTask


constructor TCefFastTask.Create(const method: TCefFastTaskProc);
begin
  inherited Create;

  FMethod := method;
end;

procedure TCefFastTask.Execute;
begin
  FMethod();
end;

class procedure TCefFastTask.New(threadId: TCefThreadId; const method: TCefFastTaskProc);
begin
  CefPostTask(threadId, Create(method));
end;

class procedure TCefFastTask.NewDelayed(threadId: TCefThreadId; Delay: Int64; const method: TCefFastTaskProc);
begin
  CefPostDelayedTask(threadId, Create(method), Delay);
end;


// TCefUpdatePrefsTask


constructor TCefUpdatePrefsTask.Create(const aEvents : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
end;

destructor TCefUpdatePrefsTask.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

procedure TCefUpdatePrefsTask.Execute;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doUpdateOwnPreferences;
    except
      on e : exception do
        if CustomExceptionHandler('TCefUpdatePrefsTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;


// TCefSavePrefsTask


constructor TCefSavePrefsTask.Create(const aEvents : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
end;

destructor TCefSavePrefsTask.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

procedure TCefSavePrefsTask.Execute;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doSavePreferences;
    except
      on e : exception do
        if CustomExceptionHandler('TCefSavePrefsTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;


// TCefURLRequestTask

procedure TCefURLRequestTask.Execute;
begin
  try
    try
      if (FEvents <> nil) then ICEFUrlRequestClientEvents(FEvents).doOnCreateURLRequest;
    except
      on e : exception do
        if CustomExceptionHandler('TCefURLRequestTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefURLRequestTask.Create(const aEvents : ICEFUrlRequestClientEvents);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
end;

destructor TCefURLRequestTask.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;


// TCefGenericTask

procedure TCefGenericTask.Execute;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doOnExecuteTaskOnCefThread(FTaskID);
    except
      on e : exception do
        if CustomExceptionHandler('TCefGenericTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefGenericTask.Create(const aEvents : IChromiumEvents; aTaskID : cardinal);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
  FTaskID := aTaskID;
end;

destructor TCefGenericTask.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;



// TCefUpdateZoomStepTask

procedure TCefUpdateZoomStepTask.Execute;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doUpdateZoomStep(FInc);
    except
      on e : exception do
        if CustomExceptionHandler('TCefUpdateZoomStepTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefUpdateZoomStepTask.Create(const aEvents : IChromiumEvents; aInc : boolean);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
  FInc    := aInc;
end;

destructor TCefUpdateZoomStepTask.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;



// TCefUpdateZoomPctTask

procedure TCefUpdateZoomPctTask.Execute;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doUpdateZoomPct(FInc);
    except
      on e : exception do
        if CustomExceptionHandler('TCefUpdateZoomPctTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefUpdateZoomPctTask.Create(const aEvents : IChromiumEvents; aInc : boolean);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
  FInc    := aInc;
end;

destructor TCefUpdateZoomPctTask.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;



// TCefReadZoomTask

procedure TCefReadZoomTask.Execute;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doReadZoom;
    except
      on e : exception do
        if CustomExceptionHandler('TCefReadZoomTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefReadZoomTask.Create(const aEvents : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
end;

destructor TCefReadZoomTask.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;



// TCefSetZoomLevelTask

procedure TCefSetZoomLevelTask.Execute;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doSetZoomLevel(FValue);
    except
      on e : exception do
        if CustomExceptionHandler('TCefSetZoomLevelTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefSetZoomLevelTask.Create(const aEvents : IChromiumEvents; const aValue : double);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
  FValue  := aValue;
end;

destructor TCefSetZoomLevelTask.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;



// TCefSetZoomPctTask

procedure TCefSetZoomPctTask.Execute;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doSetZoomPct(FValue);
    except
      on e : exception do
        if CustomExceptionHandler('TCefSetZoomPctTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefSetZoomPctTask.Create(const aEvents : IChromiumEvents; const aValue : double);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
  FValue  := aValue;
end;

destructor TCefSetZoomPctTask.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;



// TCefSetZoomStepTask

procedure TCefSetZoomStepTask.Execute;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doSetZoomStep(FValue);
    except
      on e : exception do
        if CustomExceptionHandler('TCefSetZoomStepTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefSetZoomStepTask.Create(const aEvents : IChromiumEvents; aValue : byte);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
  FValue  := aValue;
end;

destructor TCefSetZoomStepTask.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;


// TCefCreateCustomViewTask

procedure TCefCreateCustomViewTask.Execute;
begin
  try
    try
      if (FEvents <> nil) then ICefViewDelegateEvents(FEvents).doCreateCustomView;
    except
      on e : exception do
        if CustomExceptionHandler('ICefViewDelegateEvents.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefCreateCustomViewTask.Create(const aEvents : ICefViewDelegateEvents);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
end;

destructor TCefCreateCustomViewTask.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;


// TCefBrowserNavigationTask

procedure TCefBrowserNavigationTask.Execute;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doBrowserNavigation(FTask);
    except
      on e : exception do
        if CustomExceptionHandler('TCefBrowserNavigationTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefBrowserNavigationTask.Create(const aEvents : IChromiumEvents; aTask : TCefBrowserNavigation);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
  FTask   := aTask;
end;

destructor TCefBrowserNavigationTask.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;


// TCefUpdateSizeTask

procedure TCefUpdateSizeTask.Execute;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doUpdateSize(FLeft, FTop, FWidth, FHeight);
    except
      on e : exception do
        if CustomExceptionHandler('TCefUpdateSizeTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefUpdateSizeTask.Create(const aEvents : IChromiumEvents; aLeft, aTop, aWidth, aHeight : integer);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
  FLeft   := aLeft;
  FTop    := aTop;
  FWidth  := aWidth;
  FHeight := aHeight;
end;

destructor TCefUpdateSizeTask.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;


// TCefSendCaptureLostEventTask

procedure TCefSendCaptureLostEventTask.Execute;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doSendCaptureLostEvent;
    except
      on e : exception do
        if CustomExceptionHandler('TCefSendCaptureLostEventTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefSendCaptureLostEventTask.Create(const aEvents : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
end;

destructor TCefSendCaptureLostEventTask.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;


// TCefUpdateXWindowVisibilityTask

procedure TCefUpdateXWindowVisibilityTask.Execute;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doUpdateXWindowVisibility(FVisible);
    except
      on e : exception do
        if CustomExceptionHandler('TCefUpdateXWindowVisibilityTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefUpdateXWindowVisibilityTask.Create(const aEvents : IChromiumEvents; aVisible : boolean);
begin
  inherited Create;

  FEvents  := Pointer(aEvents);
  FVisible := aVisible;
end;

destructor TCefUpdateXWindowVisibilityTask.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;


end.
