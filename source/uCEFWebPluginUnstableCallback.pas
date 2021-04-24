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

unit uCEFWebPluginUnstableCallback;

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
  TCefWebPluginUnstableCallbackOwn = class(TCefBaseRefCountedOwn, ICefWebPluginUnstableCallback)
    protected
      procedure IsUnstable(const path: ustring; unstable: Boolean); virtual;

    public
      constructor Create; virtual;
  end;

  TCefFastWebPluginUnstableCallback = class(TCefWebPluginUnstableCallbackOwn)
    protected
      FCallback: TCefWebPluginIsUnstableProc;
      procedure IsUnstable(const path: ustring; unstable: Boolean); override;

    public
      constructor Create(const callback: TCefWebPluginIsUnstableProc); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure cef_web_plugin_unstable_callback_is_unstable(self: PCefWebPluginUnstableCallback;
                                                       const path: PCefString;
                                                       unstable: Integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefWebPluginUnstableCallbackOwn) then
    TCefWebPluginUnstableCallbackOwn(TempObject).IsUnstable(CefString(path),
                                                            unstable <> 0);
end;

// TCefWebPluginUnstableCallbackOwn

constructor TCefWebPluginUnstableCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefWebPluginUnstableCallback));

  PCefWebPluginUnstableCallback(FData)^.is_unstable := {$IFDEF FPC}@{$ENDIF}cef_web_plugin_unstable_callback_is_unstable;
end;

procedure TCefWebPluginUnstableCallbackOwn.IsUnstable(const path: ustring; unstable: Boolean);
begin
  //
end;

// TCefFastWebPluginUnstableCallback

constructor TCefFastWebPluginUnstableCallback.Create(const callback: TCefWebPluginIsUnstableProc);
begin
  inherited Create;

  FCallback := callback;
end;

procedure TCefFastWebPluginUnstableCallback.IsUnstable(const path: ustring; unstable: Boolean);
begin
  FCallback(path, unstable);
end;


end.
