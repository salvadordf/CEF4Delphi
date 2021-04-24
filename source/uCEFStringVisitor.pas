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

unit uCEFStringVisitor;

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
  TCefStringVisitorOwn = class(TCefBaseRefCountedOwn, ICefStringVisitor)
    protected
      procedure Visit(const str: ustring); virtual;

    public
      constructor Create; virtual;
  end;

  TCefFastStringVisitor = class(TCefStringVisitorOwn)
    protected
      FVisit: TCefStringVisitorProc;

      procedure Visit(const str: ustring); override;

    public
      constructor Create(const callback: TCefStringVisitorProc); reintroduce;
  end;

  TCustomCefStringVisitor = class(TCefStringVisitorOwn)
    protected
      FEvents : Pointer;

      procedure Visit(const str: ustring); override;

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

procedure cef_string_visitor_visit(      self : PCefStringVisitor;
                                   const str  : PCefString); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefStringVisitorOwn) then
    TCefStringVisitorOwn(TempObject).Visit(CefString(str));
end;

// TCefStringVisitorOwn

constructor TCefStringVisitorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefStringVisitor));

  PCefStringVisitor(FData)^.visit := {$IFDEF FPC}@{$ENDIF}cef_string_visitor_visit;
end;

procedure TCefStringVisitorOwn.Visit(const str: ustring);
begin
  //
end;

// TCefFastStringVisitor

constructor TCefFastStringVisitor.Create(const callback: TCefStringVisitorProc);
begin
  inherited Create;

  FVisit := callback;
end;

procedure TCefFastStringVisitor.Visit(const str: ustring);
begin
  FVisit(str);
end;

// TCustomCefStringVisitor

constructor TCustomCefStringVisitor.Create(const aEvents : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
end;

destructor TCustomCefStringVisitor.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

procedure TCustomCefStringVisitor.Visit(const str: ustring);
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doTextResultAvailable(str);
    except
      on e : exception do
        if CustomExceptionHandler('TCustomCefStringVisitor.Visit', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

end.
