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

unit uCEFCompletionCallback;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFWaitableEvent;

type
  TCefCompletionCallbackOwn = class(TCefBaseRefCountedOwn, ICefCompletionCallback)
    protected
      procedure OnComplete; virtual;

    public
      constructor Create; virtual;
  end;

  TCefFastCompletionCallback = class(TCefCompletionCallbackOwn)
    protected
      FProc: TCefCompletionCallbackProc;

      procedure OnComplete; override;

    public
      constructor Create(const proc: TCefCompletionCallbackProc); reintroduce;
  end;

  TCefEventCompletionCallback = class(TCefCompletionCallbackOwn)
    protected
      FEvent : ICefWaitableEvent;

      procedure OnComplete; override;

    public
      constructor Create(const event : ICefWaitableEvent); reintroduce;
      destructor  Destroy; override;
  end;

  TCefCustomCompletionCallback = class(TCefCompletionCallbackOwn)
    protected
      FEvents : Pointer;

    public
      constructor Create(const aEvents : IChromiumEvents); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFTypes;

procedure cef_completion_callback_on_complete(self: PCefCompletionCallback); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefCompletionCallbackOwn) then
    TCefCompletionCallbackOwn(TempObject).OnComplete;
end;

// TCefCompletionHandlerOwn

constructor TCefCompletionCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefCompletionCallback));

  PCefCompletionCallback(FData)^.on_complete := {$IFDEF FPC}@{$ENDIF}cef_completion_callback_on_complete;
end;

procedure TCefCompletionCallbackOwn.OnComplete;
begin
  //
end;

// TCefFastCompletionHandler

constructor TCefFastCompletionCallback.Create(const proc: TCefCompletionCallbackProc);
begin
  inherited Create;

  FProc := proc;
end;

procedure TCefFastCompletionCallback.OnComplete;
begin
  FProc();
end;


// TCefEventCompletionCallback


constructor TCefEventCompletionCallback.Create(const event : ICefWaitableEvent);
begin
  inherited Create;

  FEvent := event;
end;

destructor TCefEventCompletionCallback.Destroy;
begin
  FEvent := nil;

  inherited Destroy;
end;

procedure TCefEventCompletionCallback.OnComplete;
begin
  if (FEvent <> nil) then FEvent.Signal;
end;


// TCefCustomCompletionCallback

constructor TCefCustomCompletionCallback.Create(const aEvents : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
end;

destructor TCefCustomCompletionCallback.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

end.
