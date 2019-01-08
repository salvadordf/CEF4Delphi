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
//        Copyright © 2019 Salvador Diaz Fau. All rights reserved.
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

end.
