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

unit uCEFDomVisitor;

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
  TCefDomVisitorOwn = class(TCefBaseRefCountedOwn, ICefDomVisitor)
    protected
      procedure visit(const document: ICefDomDocument); virtual;

    public
      constructor Create; virtual;
  end;

  TCefFastDomVisitor = class(TCefDomVisitorOwn)
    protected
      FProc    : TCefDomVisitorProc;

      procedure visit(const document: ICefDomDocument); override;

    public
      constructor Create(const proc: TCefDomVisitorProc); reintroduce; virtual;
  end;

  TCefFastDomVisitor2 = class(TCefDomVisitorOwn)
    protected
      FProc    : TCefDomVisitorProc2;
      FBrowser : ICefBrowser;
      FFrame   : ICefFrame;

      procedure visit(const document: ICefDomDocument); override;

    public
      constructor Create(const browser: ICefBrowser; const frame: ICefFrame; const proc: TCefDomVisitorProc2); reintroduce; virtual;
      destructor  Destroy; override;
  end;

  TCefFastDomVisitor3 = class(TCefDomVisitorOwn)
    protected
      FProc    : TCefDomVisitorProc3;
      FBrowser : ICefBrowser;
      FFrame   : ICefFrame;
      FValue   : ustring;

      procedure visit(const document: ICefDomDocument); override;

    public
      constructor Create(const browser: ICefBrowser; const frame: ICefFrame; const proc: TCefDomVisitorProc3; const aValue : ustring); reintroduce; virtual;
      destructor  Destroy; override;
  end;


implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFDomDocument;

procedure cef_dom_visitor_visite(self: PCefDomVisitor; document: PCefDomDocument); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDomVisitorOwn) then
    TCefDomVisitorOwn(TempObject).visit(TCefDomDocumentRef.UnWrap(document));
end;

// TCefDomVisitorOwn

constructor TCefDomVisitorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDomVisitor));

  PCefDomVisitor(FData)^.visit := {$IFDEF FPC}@{$ENDIF}cef_dom_visitor_visite;
end;

procedure TCefDomVisitorOwn.visit(const document: ICefDomDocument);
begin

end;

// TCefFastDomVisitor

constructor TCefFastDomVisitor.Create(const proc: TCefDomVisitorProc);
begin
  inherited Create;

  FProc := proc;
end;

procedure TCefFastDomVisitor.visit(const document: ICefDomDocument);
begin
  FProc(document);
end;


// TCefFastDomVisitor2

constructor TCefFastDomVisitor2.Create(const browser: ICefBrowser; const frame: ICefFrame; const proc: TCefDomVisitorProc2);
begin
  inherited Create;

  FBrowser := browser;
  FFrame   := frame;
  FProc    := proc;
end;

destructor TCefFastDomVisitor2.Destroy;
begin
  FBrowser := nil;
  FFrame   := nil;

  inherited Destroy;
end;

procedure TCefFastDomVisitor2.visit(const document: ICefDomDocument);
begin
  FProc(FBrowser, FFrame, document);
end;


// TCefFastDomVisitor3

constructor TCefFastDomVisitor3.Create(const browser: ICefBrowser; const frame: ICefFrame; const proc: TCefDomVisitorProc3; const aValue : ustring);
begin
  inherited Create;

  FBrowser := browser;
  FFrame   := frame;
  FProc    := proc;
  FValue   := aValue;
end;

destructor TCefFastDomVisitor3.Destroy;
begin
  FBrowser := nil;
  FFrame   := nil;

  inherited Destroy;
end;

procedure TCefFastDomVisitor3.visit(const document: ICefDomDocument);
begin
  FProc(FBrowser, FFrame, document, FValue);
end;

end.
