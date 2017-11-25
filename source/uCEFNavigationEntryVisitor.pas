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
//        Copyright © 2017 Salvador Díaz Fau. All rights reserved.
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

unit uCEFNavigationEntryVisitor;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces;

type
  TCefNavigationEntryVisitorOwn = class(TCefBaseRefCountedOwn, ICefNavigationEntryVisitor)
    protected
      function Visit(const entry: ICefNavigationEntry; current: Boolean; index, total: Integer): Boolean; virtual;

    public
      constructor Create;
  end;

  TCefFastNavigationEntryVisitor = class(TCefNavigationEntryVisitorOwn)
    protected
      FVisitor: TCefNavigationEntryVisitorProc;

      function Visit(const entry: ICefNavigationEntry; current: Boolean; index, total: Integer): Boolean; override;

    public
      constructor Create(const proc: TCefNavigationEntryVisitorProc); reintroduce;
  end;

implementation

uses
  uCEFTypes, uCEFMiscFunctions, uCEFNavigationEntry;

function cef_navigation_entry_visitor_visit(self: PCefNavigationEntryVisitor; entry: PCefNavigationEntry; current, index, total: Integer): Integer; stdcall;
begin
  with TCefNavigationEntryVisitorOwn(CefGetObject(self)) do
    Result := Ord(Visit(TCefNavigationEntryRef.UnWrap(entry), current <> 0, index, total));
end;

// TCefNavigationEntryVisitorOwn

constructor TCefNavigationEntryVisitorOwn.Create;
begin
  CreateData(SizeOf(TCefNavigationEntryVisitor));
  with PCefNavigationEntryVisitor(FData)^ do
    visit := cef_navigation_entry_visitor_visit;
end;

function TCefNavigationEntryVisitorOwn.Visit(const entry: ICefNavigationEntry;
  current: Boolean; index, total: Integer): Boolean;
begin
  Result:= False;
end;

// TCefFastNavigationEntryVisitor

constructor TCefFastNavigationEntryVisitor.Create(
  const proc: TCefNavigationEntryVisitorProc);
begin
  FVisitor := proc;
  inherited Create;
end;

function TCefFastNavigationEntryVisitor.Visit(const entry: ICefNavigationEntry;
  current: Boolean; index, total: Integer): Boolean;
begin
  Result := FVisitor(entry, current, index, total);
end;

end.
