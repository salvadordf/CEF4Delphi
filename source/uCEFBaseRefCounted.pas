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

unit uCEFBaseRefCounted;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFInterfaces;

type
  TLoggingInterfacedObject = class(TInterfacedObject)
    protected
      function _AddRef: Integer; reintroduce; stdcall;
      function _Release: Integer; reintroduce; stdcall;
    public
      class function NewInstance: TObject; override;
  end;


  TCefBaseRefCountedOwn = class({$IFDEF INTFLOG}TLoggingInterfacedObject{$ELSE}TInterfacedObject{$ENDIF}, ICefBaseRefCounted)
    protected
      FData: Pointer;

      function HasOneRef : boolean;
      function HasAtLeastOneRef : boolean;

    public
      constructor CreateData(size: Cardinal; owned : boolean = False); virtual;
      destructor  Destroy; override;
      function    SameAs(aData : Pointer) : boolean; overload;
      function    SameAs(const aBaseRefCounted : ICefBaseRefCounted) : boolean; overload;
      procedure   DestroyOtherRefs;
      function    Wrap: Pointer;
  end;

  TCefBaseRefCountedRef = class(TInterfacedObject, ICefBaseRefCounted)
    protected
      FData: Pointer;

      function HasOneRef : boolean;
      function HasAtLeastOneRef : boolean;

    public
      constructor Create(data: Pointer); virtual;
      destructor  Destroy; override;
      function    SameAs(aData : Pointer) : boolean; overload;
      function    SameAs(const aBaseRefCounted : ICefBaseRefCounted) : boolean; overload;
      procedure   DestroyOtherRefs;
      function    Wrap: Pointer;
      class function UnWrap(data: Pointer): ICefBaseRefCounted;
  end;



implementation

uses
  uCEFTypes, uCEFMiscFunctions;


// ***********************************************
// ************ TCefBaseRefCountedOwn ************
// ***********************************************


procedure cef_base_add_ref(self: PCefBaseRefCounted); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBaseRefCountedOwn) then
    TCefBaseRefCountedOwn(TempObject)._AddRef;
end;

function cef_base_release_ref(self: PCefBaseRefCounted): Integer; stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBaseRefCountedOwn) then
    Result := TCefBaseRefCountedOwn(TempObject)._Release
   else
    Result := 0;
end;

function cef_base_has_one_ref(self: PCefBaseRefCounted): Integer; stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBaseRefCountedOwn) then
    Result := Ord(TCefBaseRefCountedOwn(TempObject).FRefCount = 1)
   else
    Result := Ord(False);
end;

function cef_base_has_at_least_one_ref(self: PCefBaseRefCounted): Integer; stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBaseRefCountedOwn) then
    Result := Ord(TCefBaseRefCountedOwn(TempObject).FRefCount >= 1)
   else
    Result := Ord(False);
end;

procedure cef_base_add_ref_owned(self: PCefBaseRefCounted); stdcall;
begin
  //
end;

function cef_base_release_owned(self: PCefBaseRefCounted): Integer; stdcall;
begin
  Result := 1;
end;

function cef_base_has_one_ref_owned(self: PCefBaseRefCounted): Integer; stdcall;
begin
  Result := 1;
end;

function cef_base_has_at_least_one_ref_owned(self: PCefBaseRefCounted): Integer; stdcall;
begin
  Result := 1;
end;

constructor TCefBaseRefCountedOwn.CreateData(size: Cardinal; owned : boolean);
begin
  GetMem(FData, size + SizeOf(Pointer));
  PPointer(FData)^ := Self;
  Inc(PByte(FData), SizeOf(Pointer));
  FillChar(FData^, size, 0);
  PCefBaseRefCounted(FData)^.size := size;

  if owned then
    begin
      PCefBaseRefCounted(FData)^.add_ref              := {$IFDEF FPC}@{$ENDIF}cef_base_add_ref_owned;
      PCefBaseRefCounted(FData)^.release              := {$IFDEF FPC}@{$ENDIF}cef_base_release_owned;
      PCefBaseRefCounted(FData)^.has_one_ref          := {$IFDEF FPC}@{$ENDIF}cef_base_has_one_ref_owned;
      PCefBaseRefCounted(FData)^.has_at_least_one_ref := {$IFDEF FPC}@{$ENDIF}cef_base_has_at_least_one_ref_owned;
    end
   else
    begin
      PCefBaseRefCounted(FData)^.add_ref              := {$IFDEF FPC}@{$ENDIF}cef_base_add_ref;
      PCefBaseRefCounted(FData)^.release              := {$IFDEF FPC}@{$ENDIF}cef_base_release_ref;
      PCefBaseRefCounted(FData)^.has_one_ref          := {$IFDEF FPC}@{$ENDIF}cef_base_has_one_ref;
      PCefBaseRefCounted(FData)^.has_at_least_one_ref := {$IFDEF FPC}@{$ENDIF}cef_base_has_at_least_one_ref;
    end;
end;

destructor TCefBaseRefCountedOwn.Destroy;
var
  TempPointer : pointer;
begin
  try
    if (FData <> nil) then
      begin
        TempPointer := FData;
        Dec(PByte(TempPointer), SizeOf(Pointer));
        FillChar(TempPointer^, SizeOf(Pointer) + SizeOf(TCefBaseRefCounted), 0);
        FreeMem(TempPointer);
      end;
  finally
    FData := nil;
    inherited Destroy;
  end;
end;

function TCefBaseRefCountedOwn.SameAs(aData : Pointer) : boolean;
begin
  Result := (FData = aData);
end;

function TCefBaseRefCountedOwn.SameAs(const aBaseRefCounted : ICefBaseRefCounted) : boolean;
var
  TempData : Pointer;
begin
  Result := False;

  if (aBaseRefCounted <> nil) then
    begin
      TempData := aBaseRefCounted.Wrap;

      if (TempData <> nil) then
        begin
          Result := SameAs(TempData);

          if assigned(PCefBaseRefCounted(TempData)^.release) then
            PCefBaseRefCounted(TempData)^.release(PCefBaseRefCounted(TempData));
        end;
    end;
end;

procedure TCefBaseRefCountedOwn.DestroyOtherRefs;
begin
  while HasAtLeastOneRef and not(HasOneRef) do
    _Release;
end;

function TCefBaseRefCountedOwn.Wrap: Pointer;
begin
  Result := FData;

  if (FData <> nil) and Assigned(PCefBaseRefCounted(FData)^.add_ref) then
    PCefBaseRefCounted(FData)^.add_ref(PCefBaseRefCounted(FData));
end;

function TCefBaseRefCountedOwn.HasOneRef : boolean;
begin
  if (FData <> nil) and Assigned(PCefBaseRefCounted(FData)^.has_one_ref) then
    Result := PCefBaseRefCounted(FData)^.has_one_ref(PCefBaseRefCounted(FData)) <> 0
   else
    Result := False;
end;

function TCefBaseRefCountedOwn.HasAtLeastOneRef : boolean;
begin
  if (FData <> nil) and Assigned(PCefBaseRefCounted(FData)^.has_at_least_one_ref) then
    Result := PCefBaseRefCounted(FData)^.has_at_least_one_ref(PCefBaseRefCounted(FData)) <> 0
   else
    Result := False;
end;


// ***********************************************
// ************ TCefBaseRefCountedRef ************
// ***********************************************


constructor TCefBaseRefCountedRef.Create(data: Pointer);
begin
  Assert(data <> nil);
  FData := data;
end;

destructor TCefBaseRefCountedRef.Destroy;
begin
  try
    if (FData <> nil) and assigned(PCefBaseRefCounted(FData)^.release) then
      begin
        {$IFDEF INTFLOG}
        CefDebugLog(ClassName + '.Destroy -> FRefCount = ' +
                    IntToStr(PCefBaseRefCounted(FData)^.release(PCefBaseRefCounted(FData))));
        {$ELSE}
        PCefBaseRefCounted(FData)^.release(PCefBaseRefCounted(FData));
        {$ENDIF}
      end;
  finally
    FData := nil;
    inherited Destroy;
  end;
end;

function TCefBaseRefCountedRef.SameAs(aData : Pointer) : boolean;
begin
  Result := (FData = aData);
end;

function TCefBaseRefCountedRef.SameAs(const aBaseRefCounted : ICefBaseRefCounted) : boolean;
var
  TempData : Pointer;
begin
  Result := False;

  if (aBaseRefCounted <> nil) then
    begin
      TempData := aBaseRefCounted.Wrap;

      if (TempData <> nil) then
        begin
          Result := SameAs(TempData);

          if assigned(PCefBaseRefCounted(TempData)^.release) then
            PCefBaseRefCounted(TempData)^.release(PCefBaseRefCounted(TempData));
        end;
    end;
end;

class function TCefBaseRefCountedRef.UnWrap(data: Pointer): ICefBaseRefCounted;
begin
  if (data <> nil) then
    Result := Create(data) as ICefBaseRefCounted
   else
    Result := nil;
end;

function TCefBaseRefCountedRef.Wrap: Pointer;
begin
  Result := FData;

  if (FData <> nil) and Assigned(PCefBaseRefCounted(FData)^.add_ref) then
    begin
      PCefBaseRefCounted(FData)^.add_ref(PCefBaseRefCounted(FData));
      {$IFDEF INTFLOG}
      CefDebugLog(ClassName + '.Wrap');
      {$ENDIF}
    end;
end;

function TCefBaseRefCountedRef.HasOneRef : boolean;
begin
  Result := (PCefBaseRefCounted(FData)^.has_one_ref(PCefBaseRefCounted(FData)) <> 0);
end;

function TCefBaseRefCountedRef.HasAtLeastOneRef : boolean;
begin
  Result := (PCefBaseRefCounted(FData)^.has_at_least_one_ref(PCefBaseRefCounted(FData)) <> 0);
end;

procedure TCefBaseRefCountedRef.DestroyOtherRefs;
begin
  //
end;


// ************************************************
// *********** TLoggingInterfacedObject ***********
// ************************************************


function TLoggingInterfacedObject._AddRef: Integer; stdcall;
begin
  Result := inherited _AddRef;
  CefDebugLog(ClassName + '._AddRef -> FRefCount = ' + IntToStr(Result));
end;

function TLoggingInterfacedObject._Release: Integer; stdcall;
begin
  CefDebugLog(ClassName + '._Release -> FRefCount = ' + IntToStr(pred(RefCount)));
  Result := inherited _Release;
end;

class function TLoggingInterfacedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  CefDebugLog(ClassName + '.NewInstance -> FRefCount = ' + IntToStr(TInterfacedObject(Result).RefCount));
end;

end.
