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

unit uCEFWebPluginInfoVisitor;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefWebPluginInfoVisitorOwn = class(TCefBaseRefCountedOwn, ICefWebPluginInfoVisitor)
    protected
      function Visit(const info: ICefWebPluginInfo; count, total: Integer): Boolean; virtual;

    public
      constructor Create; virtual;
  end;

  TCefWebPluginInfoVisitorProc = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(const info: ICefWebPluginInfo; count, total: Integer): Boolean;

  TCefFastWebPluginInfoVisitor = class(TCefWebPluginInfoVisitorOwn)
    protected
      FProc: TCefWebPluginInfoVisitorProc;

      function Visit(const info: ICefWebPluginInfo; count, total: Integer): Boolean; override;

    public
      constructor Create(const proc: TCefWebPluginInfoVisitorProc); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFWebPluginInfo;

function cef_web_plugin_info_visitor_visit(self: PCefWebPluginInfoVisitor; info: PCefWebPluginInfo; count, total: Integer): Integer; stdcall;
begin
  with TCefWebPluginInfoVisitorOwn(CefGetObject(self)) do
    Result := Ord(Visit(TCefWebPluginInfoRef.UnWrap(info), count, total));
end;

constructor TCefWebPluginInfoVisitorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefWebPluginInfoVisitor));
  PCefWebPluginInfoVisitor(FData).visit := cef_web_plugin_info_visitor_visit;
end;

function TCefWebPluginInfoVisitorOwn.Visit(const info: ICefWebPluginInfo; count,
  total: Integer): Boolean;
begin
  Result := False;
end;

// TCefFastWebPluginInfoVisitor

constructor TCefFastWebPluginInfoVisitor.Create(
  const proc: TCefWebPluginInfoVisitorProc);
begin
  inherited Create;
  FProc := proc;
end;

function TCefFastWebPluginInfoVisitor.Visit(const info: ICefWebPluginInfo;
  count, total: Integer): Boolean;
begin
  Result := FProc(info, count, total);
end;

end.
