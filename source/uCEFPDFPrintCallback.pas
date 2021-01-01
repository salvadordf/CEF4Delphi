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

unit uCEFPDFPrintCallback;

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
  TCefPdfPrintCallbackOwn = class(TCefBaseRefCountedOwn, ICefPdfPrintCallback)
    protected
      procedure OnPdfPrintFinished(const path: ustring; ok: Boolean); virtual; abstract;

    public
      constructor Create; virtual;
  end;

  TCefFastPdfPrintCallback = class(TCefPdfPrintCallbackOwn)
    protected
      FProc: TOnPdfPrintFinishedProc;

      procedure OnPdfPrintFinished(const path: ustring; ok: Boolean); override;

    public
      constructor Create(const proc: TOnPdfPrintFinishedProc); reintroduce;
      destructor  Destroy; override;
  end;

  TCefCustomPDFPrintCallBack = class(TCefPdfPrintCallbackOwn)
    protected
      FEvents : Pointer;

      procedure OnPdfPrintFinished(const path: ustring; aResultOK : Boolean); override;

    public
      constructor Create(const aEvents : IChromiumEvents); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions;

procedure cef_pdf_print_callback_on_pdf_print_finished(      self : PCefPdfPrintCallback;
                                                       const path : PCefString;
                                                             ok   : Integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefPdfPrintCallbackOwn) then
    TCefPdfPrintCallbackOwn(TempObject).OnPdfPrintFinished(CefString(path),
                                                           ok <> 0);
end;

constructor TCefPdfPrintCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefPdfPrintCallback));

  with PCefPdfPrintCallback(FData)^ do
    on_pdf_print_finished := {$IFDEF FPC}@{$ENDIF}cef_pdf_print_callback_on_pdf_print_finished;
end;

// TCefFastPdfPrintCallback

constructor TCefFastPdfPrintCallback.Create(const proc: TOnPdfPrintFinishedProc);
begin
  FProc := proc;

  inherited Create;
end;

procedure TCefFastPdfPrintCallback.OnPdfPrintFinished(const path: ustring; ok: Boolean);
begin
  if assigned(FProc) then FProc(path, ok);
end;

destructor TCefFastPdfPrintCallback.Destroy;
begin
  FProc := nil;

  inherited Destroy;
end;

// TCefCustomPDFPrintCallBack

constructor TCefCustomPDFPrintCallBack.Create(const aEvents : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
end;

destructor TCefCustomPDFPrintCallBack.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

procedure TCefCustomPDFPrintCallBack.OnPdfPrintFinished(const path: ustring; aResultOK : Boolean);
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doPdfPrintFinished(aResultOK);
    except
      on e : exception do
        if CustomExceptionHandler('TCefCustomPDFPrintCallBack.OnPdfPrintFinished', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

end.
