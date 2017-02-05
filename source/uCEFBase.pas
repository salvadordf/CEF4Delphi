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

unit uCEFBase;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFInterfaces;

type
  TCefBaseOwn = class(TInterfacedObject, ICefBase)
    protected
      FData: Pointer;

    public
      constructor CreateData(size: Cardinal; owned: Boolean = False); virtual;
      destructor  Destroy; override;
      function    Wrap: Pointer;
  end;

  TCefBaseRef = class(TInterfacedObject, ICefBase)
    protected
      FData: Pointer;

    public
      constructor Create(data: Pointer); virtual;
      destructor  Destroy; override;
      function    Wrap: Pointer;
      class function UnWrap(data: Pointer): ICefBase;
  end;

implementation

uses
  uCEFTypes, uCEFMiscFunctions;

procedure cef_base_add_ref(self: PCefBase); stdcall;
begin
  TCefBaseOwn(CefGetObject(self))._AddRef;
end;

function cef_base_release(self: PCefBase): Integer; stdcall;
begin
  Result := TCefBaseOwn(CefGetObject(self))._Release;
end;

function cef_base_has_one_ref(self: PCefBase): Integer; stdcall;
begin
  Result := Ord(TCefBaseOwn(CefGetObject(self)).FRefCount = 1);
end;

procedure cef_base_add_ref_owned(self: PCefBase); stdcall;
begin
  //
end;

function cef_base_release_owned(self: PCefBase): Integer; stdcall;
begin
  Result := 1;
end;

function cef_base_has_one_ref_owned(self: PCefBase): Integer; stdcall;
begin
  Result := 1;
end;

constructor TCefBaseOwn.CreateData(size: Cardinal; owned: Boolean);
begin
  GetMem(FData, size + SizeOf(Pointer));
  PPointer(FData)^ := Self;
  Inc(PByte(FData), SizeOf(Pointer));
  FillChar(FData^, size, 0);
  PCefBase(FData)^.size := size;

  if owned then
    begin
      PCefBase(FData)^.add_ref := cef_base_add_ref_owned;
      PCefBase(FData)^.release := cef_base_release_owned;
      PCefBase(FData)^.has_one_ref := cef_base_has_one_ref_owned;
    end
   else
    begin
      PCefBase(FData)^.add_ref := cef_base_add_ref;
      PCefBase(FData)^.release := cef_base_release;
      PCefBase(FData)^.has_one_ref := cef_base_has_one_ref;
    end;
end;

destructor TCefBaseOwn.Destroy;
begin
  Dec(PByte(FData), SizeOf(Pointer));
  FreeMem(FData);
  inherited;
end;

function TCefBaseOwn.Wrap: Pointer;
begin
  Result := FData;
  if Assigned(PCefBase(FData)^.add_ref) then
    PCefBase(FData)^.add_ref(PCefBase(FData));
end;

// TCefBaseRef

constructor TCefBaseRef.Create(data: Pointer);
begin
  Assert(data <> nil);
  FData := data;
end;

destructor TCefBaseRef.Destroy;
begin
  if (FData <> nil) and Assigned(PCefBase(FData)^.release) then PCefBase(FData)^.release(PCefBase(FData));

  inherited Destroy;
end;

class function TCefBaseRef.UnWrap(data: Pointer): ICefBase;
begin
  if data <> nil then
    Result := Create(data) as ICefBase else
    Result := nil;
end;

function TCefBaseRef.Wrap: Pointer;
begin
  Result := FData;
  if Assigned(PCefBase(FData)^.add_ref) then
    PCefBase(FData)^.add_ref(PCefBase(FData));
end;

end.
