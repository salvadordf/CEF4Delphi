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
//        Copyright © 2022 Salvador Diaz Fau. All rights reserved.
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

unit uCEFMiscFunctions;

{$I cef.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
  {$IFDEF MACOSX}
    {$ModeSwitch objectivec1}
  {$ENDIF}
{$ENDIF}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$IFNDEF FPC}{$IFNDEF DELPHI12_UP}
  // Workaround for "Internal error" in old Delphi versions caused by uint64 handling
  {$R-}
{$ENDIF}{$ENDIF}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}
      WinApi.Windows, WinApi.ActiveX, Winapi.ShellApi,
    {$ELSE}
      {$IFDEF MACOSX}Macapi.Foundation, FMX.Helpers.Mac, Macapi.AppKit,{$ENDIF}
    {$ENDIF}
    {$IFDEF FMX}FMX.Types, FMX.Platform,{$ENDIF} System.Types, System.IOUtils,
    System.Classes, System.SysUtils, System.UITypes, System.Math,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows, ActiveX, ShellApi,{$ENDIF}
    {$IFDEF DELPHI14_UP}Types, IOUtils,{$ENDIF} Classes, SysUtils, Math,
    {$IFDEF FPC}LCLType, LazFileUtils,{$IFNDEF MSWINDOWS}InterfaceBase, Forms,{$ENDIF}{$ENDIF}
    {$IFDEF LINUX}{$IFDEF FPC}
      ctypes, keysym, xf86keysym, x, xlib,
      {$IFDEF LCLGTK2}gtk2, glib2, gdk2, gtk2proc, gtk2int, Gtk2Def, gdk2x, Gtk2Extra,{$ENDIF}
    {$ENDIF}{$ENDIF}
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFLibFunctions, uCEFResourceHandler, uCEFConstants;

const
  Kernel32DLL = 'kernel32.dll';
  SHLWAPIDLL  = 'shlwapi.dll';
  NTDLL       = 'ntdll.dll';
  User32DLL   = 'User32.dll';

function CefColorGetA(color: TCefColor): Byte;
function CefColorGetR(color: TCefColor): byte;
function CefColorGetG(color: TCefColor): Byte;
function CefColorGetB(color: TCefColor): Byte;

function CefColorSetARGB(a, r, g, b: Byte): TCefColor;

function CefInt64Set(int32_low, int32_high: Integer): Int64;

function CefInt64GetLow(const int64_val: Int64): Integer;
function CefInt64GetHigh(const int64_val: Int64): Integer;

function CefGetObject(ptr: Pointer): TObject; {$IFNDEF CEF4DELHI_ALLOC_DEBUG}{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}{$ENDIF}
function CefGetData(const i: ICefBaseRefCounted): Pointer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

function CefStringAlloc(const str: ustring): TCefString;
function CefStringClearAndGet(str: PCefString): ustring;

function  CefString(const str: ustring): TCefString; overload;
function  CefString(const str: PCefString): ustring; overload;
function  CefUserFreeString(const str: ustring): PCefStringUserFree;
procedure CefStringFree(const str: PCefString);
function  CefStringFreeAndGet(const str: PCefStringUserFree): ustring;
procedure CefStringSet(const str: PCefString; const value: ustring);
procedure CefStringInitialize(const aCefString : PCefString); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

function CefRegisterExtension(const name, code: ustring; const Handler: ICefv8Handler): Boolean;

function CefPostTask(aThreadId : TCefThreadId; const aTask: ICefTask) : boolean;
function CefPostDelayedTask(aThreadId : TCefThreadId; const aTask : ICefTask; aDelayMs : Int64) : boolean;
function CefCurrentlyOn(aThreadId : TCefThreadId) : boolean;

{$IFDEF MSWINDOWS}
function CefTimeToSystemTime(const dt: TCefTime): TSystemTime;
function SystemTimeToCefTime(const dt: TSystemTime): TCefTime;
{$ELSE}
  {$IFDEF LINUX}
    {$IFDEF FPC}
    function CefTimeToSystemTime(const dt: TCefTime): TSystemTime;
    function SystemTimeToCefTime(const dt: TSystemTime): TCefTime;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

function FixCefTime(const dt : TCefTime): TCefTime;
function CefTimeToDateTime(const dt: TCefTime): TDateTime;
function DateTimeToCefTime(dt: TDateTime): TCefTime;

function cef_string_wide_copy(const src: PWideChar; src_len: NativeUInt;  output: PCefStringWide): Integer;
function cef_string_utf8_copy(const src: PAnsiChar; src_len: NativeUInt; output: PCefStringUtf8): Integer;
function cef_string_utf16_copy(const src: PChar16; src_len: NativeUInt; output: PCefStringUtf16): Integer;
function cef_string_copy(const src: PCefChar; src_len: NativeUInt; output: PCefString): Integer;

{$IFDEF MSWINDOWS}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aRect : TRect; const aWindowName : ustring = ''; aExStyle : DWORD = 0);
procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring = ''; aExStyle : DWORD = 0);
procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring = ''; aExStyle : DWORD = 0);
{$ENDIF}

{$IFDEF MACOSX}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aRect : TRect; const aWindowName : ustring = ''; aHidden : boolean = False);
procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring = ''; aHidden : boolean = False);
procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring = ''; aHidden : boolean = False);
{$ENDIF}

{$IFDEF LINUX}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aRect : TRect; const aWindowName : ustring = '');
procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring = '');
procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring = '');
{$ENDIF}

{$IFDEF MSWINDOWS}
function ProcessUnderWow64(hProcess: THandle; Wow64Process: PBOOL): BOOL; stdcall; external Kernel32DLL name 'IsWow64Process';
function PathIsRelativeAnsi(pszPath: LPCSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsRelativeA';
function PathIsRelativeUnicode(pszPath: LPCWSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsRelativeW';
function GetGlobalMemoryStatusEx(lpBuffer: LPMEMORYSTATUSEX): BOOL; stdcall; external Kernel32DLL name 'GlobalMemoryStatusEx';
function PathCanonicalizeAnsi(pszBuf: LPSTR; pszPath: LPCSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathCanonicalizeA';
function PathCanonicalizeUnicode(pszBuf: LPWSTR; pszPath: LPCWSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathCanonicalizeW';
function PathIsUNCAnsi(pszPath: LPCSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsUNCA';
function PathIsUNCUnicode(pszPath: LPCWSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsUNCW';
function PathIsURLAnsi(pszPath: LPCSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsURLA';
function PathIsURLUnicode(pszPath: LPCWSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsURLW';
function ShutdownBlockReasonCreate(hWnd: HWND; Reason: LPCWSTR): Bool; stdcall; external User32DLL;
function ShutdownBlockReasonDestroy(hWnd: HWND): Bool; stdcall; external User32DLL;

{$IFNDEF DELPHI12_UP}
const
  GWLP_WNDPROC    = GWL_WNDPROC;
  GWLP_HWNDPARENT = GWL_HWNDPARENT;
  {$IFDEF WIN64}
    function SetWindowLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: int64): int64; stdcall; external user32 name 'SetWindowLongPtrW';
  {$ELSE}
    function SetWindowLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: LongInt): LongInt; stdcall; external user32 name 'SetWindowLongW';
  {$ENDIF}
{$ENDIF}

{$ENDIF}

function CustomPathIsRelative(const aPath : string) : boolean;
function CustomPathCanonicalize(const aOriginalPath : string; var aCanonicalPath : string) : boolean;
function CustomAbsolutePath(const aPath : string; aMustExist : boolean = False) : string;
function CustomPathIsURL(const aPath : string) : boolean;
function CustomPathIsUNC(const aPath : string) : boolean;
function GetModulePath : string;

function CefIsCertStatusError(Status : TCefCertStatus) : boolean;

function  CefCrashReportingEnabled : boolean;
procedure CefSetCrashKeyValue(const aKey, aValue : ustring);

procedure CefLog(const aFile : string; aLine, aSeverity : integer; const aMessage : string);
procedure CefDebugLog(const aMessage : string; aSeverity : integer = CEF_LOG_SEVERITY_ERROR);
procedure CefKeyEventLog(const aEvent : TCefKeyEvent);
procedure CefMouseEventLog(const aEvent : TCefMouseEvent);
procedure OutputDebugMessage(const aMessage : string);
function  CustomExceptionHandler(const aFunctionName : string; const aException : exception) : boolean;

function CefRegisterSchemeHandlerFactory(const SchemeName, DomainName : ustring; const handler: TCefResourceHandlerClass = nil): Boolean;
function CefClearSchemeHandlerFactories : boolean;

function CefAddCrossOriginWhitelistEntry(const SourceOrigin, TargetProtocol, TargetDomain: ustring; AllowTargetSubdomains: Boolean): Boolean;
function CefRemoveCrossOriginWhitelistEntry(const SourceOrigin, TargetProtocol, TargetDomain: ustring; AllowTargetSubdomains: Boolean): Boolean;
function CefClearCrossOriginWhitelist: Boolean;

procedure UInt64ToFileVersionInfo(const aVersion : uint64; var aVersionInfo : TFileVersionInfo);
{$IFDEF MSWINDOWS}
function  GetExtendedFileVersion(const aFileName : ustring) : uint64;
function  GetDLLVersion(const aDLLFile : ustring; var aVersionInfo : TFileVersionInfo) : boolean;
procedure OutputLastErrorMessage;
{$ENDIF}

function SplitLongString(aSrcString : string) : string;
function GetAbsoluteDirPath(const aSrcPath : string; var aRsltPath : string) : boolean;
function CheckSubprocessPath(const aSubprocessPath : string; var aMissingFiles : string) : boolean;
function CheckLocales(const aLocalesDirPath : string; var aMissingFiles : string; const aLocalesRequired : string = '') : boolean;
function CheckResources(const aResourcesDirPath : string; var aMissingFiles : string) : boolean;
function CheckDLLs(const aFrameworkDirPath : string; var aMissingFiles : string) : boolean;
{$IFDEF MSWINDOWS}
function CheckDLLVersion(const aDLLFile : ustring; aMajor, aMinor, aRelease, aBuild : uint16) : boolean;
function GetDLLHeaderMachine(const aDLLFile : ustring; var aMachine : integer) : boolean;
{$ENDIF}
function GetFileTypeDescription(const aExtension : ustring) : ustring;
function FileVersionInfoToString(const aVersionInfo : TFileVersionInfo) : string;
function CheckFilesExist(var aList : TStringList; var aMissingFiles : string) : boolean;
function Is32BitProcess : boolean;

function  CefParseUrl(const url: ustring; var parts: TUrlParts): Boolean;
function  CefCreateUrl(var parts: TUrlParts): ustring;
function  CefFormatUrlForSecurityDisplay(const originUrl: string): string;
function  CefGetMimeType(const extension: ustring): ustring;
procedure CefGetExtensionsForMimeType(const mimeType: ustring; var extensions: TStringList);

function CefBase64Encode(const data: Pointer; dataSize: NativeUInt): ustring;
function CefBase64Decode(const data: ustring): ICefBinaryValue;
function CefUriEncode(const text: ustring; usePlus: Boolean): ustring;
function CefUriDecode(const text: ustring; convertToUtf8: Boolean; unescapeRule: TCefUriUnescapeRule): ustring;

function CefGetPath(const aPathKey : TCefPathKey) : ustring;

function CefIsRTL : boolean;

function CefCreateDirectory(const fullPath: ustring): Boolean;
function CefGetTempDirectory(out tempDir: ustring): Boolean;
function CefCreateNewTempDirectory(const prefix: ustring; out newTempPath: ustring): Boolean;
function CefCreateTempDirectoryInDirectory(const baseDir, prefix: ustring; out newDir: ustring): Boolean;
function CefDirectoryExists(const path: ustring): Boolean;
function CefDeleteFile(const path: ustring; recursive: Boolean): Boolean;
function CefZipDirectory(const srcDir, destFile: ustring; includeHiddenFiles: Boolean): Boolean;
procedure CefLoadCRLSetsFile(const path : ustring);

{$IFDEF MSWINDOWS}
function  CefIsKeyDown(aWparam : WPARAM) : boolean;
function  CefIsKeyToggled(aWparam : WPARAM) : boolean;
function  GetCefMouseModifiers : TCefEventFlags; overload;
function  GetCefMouseModifiers(awparam : WPARAM) : TCefEventFlags; overload;
function  GetCefKeyboardModifiers(aWparam : WPARAM; aLparam : LPARAM) : TCefEventFlags;
procedure CefCheckAltGrPressed(aWparam : WPARAM; var aEvent : TCefKeyEvent);

procedure DropEffectToDragOperation(aEffect : Longint; var aAllowedOps : TCefDragOperations);
procedure DragOperationToDropEffect(const aDragOperations : TCefDragOperations; var aEffect: Longint);

function  GetWindowsMajorMinorVersion(var wMajorVersion, wMinorVersion : DWORD) : boolean;
function  RunningWindows10OrNewer : boolean;
function  GetDPIForHandle(aHandle : HWND; var aDPI : UINT) : boolean;
function  GetDefaultCEFUserAgent : string;
{$IFDEF DELPHI14_UP}
function  TouchPointToPoint(aHandle : HWND; const TouchPoint: TTouchInput): TPoint;
function  GetDigitizerStatus(var aDigitizerStatus : TDigitizerStatus; aDPI : cardinal = 0) : boolean;
function  HasTouchOrPen(aDPI : cardinal = 0) : boolean;
{$ENDIF}
{$ENDIF}

function  DeviceToLogical(aValue : integer; const aDeviceScaleFactor : double) : integer; overload;
function  DeviceToLogical(aValue : single; const aDeviceScaleFactor : double) : single; overload;
procedure DeviceToLogical(var aEvent : TCEFMouseEvent; const aDeviceScaleFactor : double); overload;
procedure DeviceToLogical(var aEvent : TCefTouchEvent; const aDeviceScaleFactor : double); overload;
procedure DeviceToLogical(var aPoint : TPoint; const aDeviceScaleFactor : double); overload;
function  LogicalToDevice(aValue : integer; const aDeviceScaleFactor : double) : integer; overload;
procedure LogicalToDevice(var aRect : TCEFRect; const aDeviceScaleFactor : double); overload;

function GetScreenDPI : integer;
function GetDeviceScaleFactor : single;

function DeleteDirContents(const aDirectory : string; const aExcludeFiles : TStringList = nil) : boolean;
function DeleteFileList(const aFileList : TStringList) : boolean;
function MoveFileList(const aFileList : TStringList; const aSrcDirectory, aDstDirectory : string) : boolean;

function CefGetDataURI(const aString, aMimeType : ustring) : ustring; overload;
function CefGetDataURI(aData : pointer; aSize : integer; const aMimeType : ustring; const aCharset : ustring = '') : ustring; overload;

function ValidCefWindowHandle(aHandle : TCefWindowHandle) : boolean;
procedure InitializeWindowHandle(var aHandle : TCefWindowHandle);

function GetCommandLineSwitchValue(const aKey : string; var aValue : ustring) : boolean;

implementation

uses
  {$IFDEF LINUX}{$IFDEF FMX}uCEFLinuxFunctions, Posix.Unistd, Posix.Stdio,{$ENDIF}{$ENDIF}
  {$IFDEF MACOSX}{$IFDEF FPC}CocoaAll,{$ELSE}Posix.Unistd, Posix.Stdio,{$ENDIF}{$ENDIF}
  uCEFApplicationCore, uCEFSchemeHandlerFactory, uCEFValue,
  uCEFBinaryValue, uCEFStringList;

function CefColorGetA(color: TCefColor): Byte;
begin
  Result := (color shr 24) and $FF;
end;

function CefColorGetR(color: TCefColor): byte;
begin
  Result := (color shr 16) and $FF;
end;

function CefColorGetG(color: TCefColor): Byte;
begin
  Result := (color shr 8) and $FF;
end;

function CefColorGetB(color: TCefColor): Byte;
begin
  Result := color and $FF;
end;

function CefColorSetARGB(a, r, g, b: Byte): TCefColor;
begin
  Result := (a shl 24) or (r shl 16) or (g shl 8) or b;
end;

function CefInt64Set(int32_low, int32_high: Integer): Int64;
begin
  Result := int32_low or (int32_high shl 32);
end;

function CefInt64GetLow(const int64_val: Int64): Integer;
begin
  Result := Integer(int64_val);
end;

function CefInt64GetHigh(const int64_val: Int64): Integer;
begin
  Result := (int64_val shr 32) and $FFFFFFFF;
end;

function CefStringClearAndGet(str: PCefString): ustring;
begin
  if (str <> nil) and (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      Result := CefString(str);
      cef_string_utf16_clear(str);
    end
   else
    Result := '';
end;

{$IFDEF CEF4DELHI_ALLOC_DEBUG}
function CefGetObject(ptr: Pointer): TObject;
var
  TempPointer : pointer;
begin
  Result := nil;

  if (ptr <> nil) then
    begin
      Dec(PByte(ptr), SizeOf(Pointer));
      TempPointer := ptr;

      if (PPointer(ptr)^ <> nil) then
        begin
          Dec(PByte(TempPointer), SizeOf(Pointer) * 2);

          if (PPointer(TempPointer)^ = CEF4DELPHI_ALLOC_PADDING) then
            Result := TObject(PPointer(ptr)^)
           else
            CefDebugLog('Pointer to an unknown memory address!', CEF_LOG_SEVERITY_INFO);
        end
       else
        CefDebugLog('Object pointer is NIL!', CEF_LOG_SEVERITY_INFO);
    end;
end;
{$ELSE}
function CefGetObject(ptr: Pointer): TObject; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  if (ptr <> nil) then
    begin
      Dec(PByte(ptr), SizeOf(Pointer));
      Result := TObject(PPointer(ptr)^);
    end
   else
    Result := nil;
end;
{$ENDIF}

function CefGetData(const i: ICefBaseRefCounted): Pointer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  if (i <> nil) then
    Result := i.Wrap
   else
    Result := nil;
end;

function CefString(const str: PCefString): ustring;
begin
  if (str <> nil) and (str^.str <> nil) and (str^.length > 0) and (str^.length < nativeuint(high(integer))) then
    SetString(Result, str^.str, str^.length)
   else
    Result := '';
end;

function CefString(const str: ustring): TCefString;
begin
  Result.str    := PChar16(PWideChar(str));
  Result.length := Length(str);
  Result.dtor   := nil;
end;

procedure CefStringFree(const str: PCefString);
begin
  if (str <> nil) and (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    cef_string_utf16_clear(str);
end;

procedure CefStringSet(const str: PCefString; const value: ustring);
begin
  if (str <> nil) and (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    cef_string_utf16_set(PWideChar(value), Length(value), str, Ord(True));
end;

procedure CefStringInitialize(const aCefString : PCefString); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  if (aCefString <> nil) then
    begin
      aCefString^.str    := nil;
      aCefString^.length := 0;
      aCefString^.dtor   := nil;
    end;
end;

function CefStringFreeAndGet(const str: PCefStringUserFree): ustring;
begin
  if (str <> nil) and (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      Result := CefString(PCefString(str));
      cef_string_userfree_utf16_free(str);
    end
   else
    Result := '';
end;

function CefStringAlloc(const str: ustring): TCefString;
begin
  CefStringInitialize(@Result);

  if (str <> '') and (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    cef_string_wide_to_utf16(PWideChar(str), Length(str), @Result);
end;


procedure _free_string(str: PChar16); stdcall;
begin
  if (str <> nil) then FreeMem(str);
end;

function CefUserFreeString(const str: ustring): PCefStringUserFree;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      Result         := cef_string_userfree_utf16_alloc();
      Result^.length := Length(str);
      GetMem(Result^.str, Result^.length * SizeOf(TCefChar));
      Move(PCefChar(str)^, Result^.str^, Result^.length * SizeOf(TCefChar));
      Result^.dtor   := @_free_string;
    end
   else
    Result := nil;
end;

function CefRegisterExtension(const name, code: ustring; const Handler: ICefv8Handler): Boolean;
var
  TempName, TempCode : TCefString;
begin
  if (GlobalCEFApp <> nil) and
     GlobalCEFApp.LibLoaded and
     ((GlobalCEFApp.ProcessType = ptRenderer) or GlobalCEFApp.SingleProcess) and
     (length(name) > 0) and
     (length(code) > 0) then
    begin
      TempName := CefString(name);
      TempCode := CefString(code);
      Result   := cef_register_extension(@TempName, @TempCode, CefGetData(handler)) <> 0;
    end
   else
    Result := False;
end;

function CefPostTask(aThreadId : TCefThreadId; const aTask : ICefTask) : boolean;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded and (aTask <> nil) then
    Result := cef_post_task(aThreadId, aTask.Wrap) <> 0
   else
    Result := False;
end;

function CefPostDelayedTask(aThreadId : TCefThreadId; const aTask : ICefTask; aDelayMs : Int64) : boolean;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded and (aTask <> nil) then
    Result := cef_post_delayed_task(aThreadId, aTask.Wrap, aDelayMs) <> 0
   else
    Result := False;
end;

function CefCurrentlyOn(aThreadId : TCefThreadId) : boolean;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    Result := cef_currently_on(aThreadId) <> 0
   else
    Result := False;
end;

{$IFDEF MSWINDOWS}
function CefTimeToSystemTime(const dt: TCefTime): TSystemTime;
begin
  Result.wYear          := dt.year;
  Result.wMonth         := dt.month;
  Result.wDayOfWeek     := dt.day_of_week;
  Result.wDay           := dt.day_of_month;
  Result.wHour          := dt.hour;
  Result.wMinute        := dt.minute;
  Result.wSecond        := dt.second;
  Result.wMilliseconds  := dt.millisecond;
end;

function SystemTimeToCefTime(const dt: TSystemTime): TCefTime;
begin
  Result.year         := dt.wYear;
  Result.month        := dt.wMonth;
  Result.day_of_week  := dt.wDayOfWeek;
  Result.day_of_month := dt.wDay;
  Result.hour         := dt.wHour;
  Result.minute       := dt.wMinute;
  Result.second       := dt.wSecond;
  Result.millisecond  := dt.wMilliseconds;
end;
{$ELSE}
  {$IFDEF LINUX}
    {$IFDEF FPC}
function CefTimeToSystemTime(const dt: TCefTime): TSystemTime;
begin
  Result.Year          := dt.year;
  Result.Month         := dt.month;
  Result.DayOfWeek     := dt.day_of_week;
  Result.Day           := dt.day_of_month;
  Result.Hour          := dt.hour;
  Result.Minute        := dt.minute;
  Result.Second        := dt.second;
  Result.Millisecond   := dt.millisecond;
end;

function SystemTimeToCefTime(const dt: TSystemTime): TCefTime;
begin
  Result.year         := dt.Year;
  Result.month        := dt.Month;
  Result.day_of_week  := dt.DayOfWeek;
  Result.day_of_month := dt.Day;
  Result.hour         := dt.Hour;
  Result.minute       := dt.Minute;
  Result.second       := dt.Second;
  Result.millisecond  := dt.Millisecond;
end;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

function FixCefTime(const dt : TCefTime): TCefTime;
var
  DayTable : PDayTable;
begin
  Result := dt;

  Result.year         := min(9999, max(1, Result.year));
  Result.month        := min(12,   max(1, Result.month));
  Result.hour         := min(23,   max(0, Result.hour));
  Result.minute       := min(59,   max(0, Result.minute));
  Result.second       := min(59,   max(0, Result.second));
  Result.millisecond  := min(999,  max(0, Result.millisecond));

  DayTable            := @MonthDays[IsLeapYear(Result.year)];
  Result.day_of_month := min(DayTable^[Result.month], max(1, Result.day_of_month));
end;

function CefTimeToDateTime(const dt: TCefTime): TDateTime;
var
  TempFixedCefTime : TCefTime;
begin
  TempFixedCefTime := FixCefTime(dt);
  Result := EncodeDate(TempFixedCefTime.year, TempFixedCefTime.month, TempFixedCefTime.day_of_month) +
            EncodeTime(TempFixedCefTime.hour, TempFixedCefTime.minute, TempFixedCefTime.second, TempFixedCefTime.millisecond);
end;

function DateTimeToCefTime(dt: TDateTime): TCefTime;
var
  TempYear, TempMonth, TempDay, TempHour, TempMin, TempSec, TempMSec : Word;
begin
  DecodeDate(dt, TempYear, TempMonth, TempDay);
  DecodeTime(dt, TempHour, TempMin, TempSec, TempMSec);

  Result.year         := TempYear;
  Result.month        := TempMonth;
  Result.day_of_week  := DayOfWeek(dt);
  Result.day_of_month := TempDay;
  Result.hour         := TempHour;
  Result.minute       := TempMin;
  Result.second       := TempSec;
  Result.millisecond  := TempMSec;
end;

function cef_string_wide_copy(const src: PWideChar; src_len: NativeUInt;  output: PCefStringWide): Integer;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    Result := cef_string_wide_set(src, src_len, output, ord(True))
   else
    Result := 0;
end;

function cef_string_utf8_copy(const src: PAnsiChar; src_len: NativeUInt; output: PCefStringUtf8): Integer;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    Result := cef_string_utf8_set(src, src_len, output, ord(True))
   else
    Result := 0;
end;

function cef_string_utf16_copy(const src: PChar16; src_len: NativeUInt; output: PCefStringUtf16): Integer;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    Result := cef_string_utf16_set(src, src_len, output, ord(True))
   else
    Result := 0;
end;

function cef_string_copy(const src: PCefChar; src_len: NativeUInt; output: PCefString): Integer;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    Result := cef_string_utf16_set(src, src_len, output, ord(True))
   else
    Result := 0;
end;

{$IFDEF MSWINDOWS}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aRect : TRect; const aWindowName : ustring; aExStyle : DWORD);
begin
  aWindowInfo.ex_style                     := aExStyle;
  aWindowInfo.window_name                  := CefString(aWindowName);
  aWindowInfo.style                        := WS_CHILD or WS_VISIBLE or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or WS_TABSTOP;
  aWindowInfo.bounds.x                     := aRect.left;
  aWindowInfo.bounds.y                     := aRect.top;
  aWindowInfo.bounds.width                 := aRect.right  - aRect.left;
  aWindowInfo.bounds.height                := aRect.bottom - aRect.top;
  aWindowInfo.parent_window                := aParent;
  aWindowInfo.menu                         := 0;
  aWindowInfo.windowless_rendering_enabled := ord(False);
  aWindowInfo.shared_texture_enabled       := ord(False);
  aWindowInfo.external_begin_frame_enabled := ord(False);
  aWindowInfo.window                       := 0;
end;

procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring; aExStyle : DWORD);
begin
  aWindowInfo.ex_style                     := aExStyle;
  aWindowInfo.window_name                  := CefString(aWindowName);
  aWindowInfo.style                        := WS_OVERLAPPEDWINDOW or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or WS_VISIBLE;
  aWindowInfo.bounds.x                     := integer(CW_USEDEFAULT);
  aWindowInfo.bounds.y                     := integer(CW_USEDEFAULT);
  aWindowInfo.bounds.width                 := integer(CW_USEDEFAULT);
  aWindowInfo.bounds.height                := integer(CW_USEDEFAULT);
  aWindowInfo.parent_window                := aParent;
  aWindowInfo.menu                         := 0;
  aWindowInfo.windowless_rendering_enabled := ord(False);
  aWindowInfo.shared_texture_enabled       := ord(False);
  aWindowInfo.external_begin_frame_enabled := ord(False);
  aWindowInfo.window                       := 0;
end;

procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring; aExStyle : DWORD);
begin
  aWindowInfo.ex_style                     := aExStyle;
  aWindowInfo.window_name                  := CefString(aWindowName);
  aWindowInfo.style                        := 0;
  aWindowInfo.bounds.x                     := 0;
  aWindowInfo.bounds.y                     := 0;
  aWindowInfo.bounds.width                 := 0;
  aWindowInfo.bounds.height                := 0;
  aWindowInfo.parent_window                := aParent;
  aWindowInfo.menu                         := 0;
  aWindowInfo.windowless_rendering_enabled := ord(True);
  aWindowInfo.shared_texture_enabled       := ord(False);
  aWindowInfo.external_begin_frame_enabled := ord(False);
  aWindowInfo.window                       := 0;
end;
{$ENDIF}

{$IFDEF MACOSX}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aRect : TRect; const aWindowName : ustring; aHidden : boolean);
begin
  aWindowInfo.window_name                  := CefString(aWindowName);
  aWindowInfo.bounds.x                     := aRect.left;
  aWindowInfo.bounds.y                     := aRect.top;
  aWindowInfo.bounds.width                 := aRect.right  - aRect.left;
  aWindowInfo.bounds.height                := aRect.bottom - aRect.top;
  aWindowInfo.hidden                       := Ord(aHidden);
  aWindowInfo.parent_view                  := aParent;
  aWindowInfo.windowless_rendering_enabled := ord(False);
  aWindowInfo.shared_texture_enabled       := ord(False);
  aWindowInfo.external_begin_frame_enabled := ord(False);
  {$IFDEF FPC}
  aWindowInfo.view                         := 0;
  {$ELSE}
  aWindowInfo.view                         := nil;
  {$ENDIF}
end;

procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring; aHidden : boolean);
begin
  aWindowInfo.window_name                  := CefString(aWindowName);
  aWindowInfo.bounds.x                     := 0;
  aWindowInfo.bounds.y                     := 0;
  aWindowInfo.bounds.width                 := 0;
  aWindowInfo.bounds.height                := 0;
  aWindowInfo.hidden                       := Ord(aHidden);
  aWindowInfo.parent_view                  := aParent;
  aWindowInfo.windowless_rendering_enabled := ord(False);
  aWindowInfo.shared_texture_enabled       := ord(False);
  aWindowInfo.external_begin_frame_enabled := ord(False);
  {$IFDEF FPC}
  aWindowInfo.view                         := 0;
  {$ELSE}
  aWindowInfo.view                         := nil;
  {$ENDIF}
end;

procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring; aHidden : boolean);
begin
  aWindowInfo.window_name                  := CefString(aWindowName);
  aWindowInfo.bounds.x                     := 0;
  aWindowInfo.bounds.y                     := 0;
  aWindowInfo.bounds.width                 := 0;
  aWindowInfo.bounds.height                := 0;
  aWindowInfo.hidden                       := Ord(aHidden);
  aWindowInfo.parent_view                  := aParent;
  aWindowInfo.windowless_rendering_enabled := ord(True);
  aWindowInfo.shared_texture_enabled       := ord(False);
  aWindowInfo.external_begin_frame_enabled := ord(False);
  {$IFDEF FPC}
  aWindowInfo.view                         := 0;
  {$ELSE}
  aWindowInfo.view                         := nil;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF LINUX}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aRect : TRect; const aWindowName : ustring = '');
var
  TempParent : TCefWindowHandle;
begin
  // TODO: Find a way to get the right "parent_window" in FMX
  TempParent := aParent;
  {$IFDEF FPC}
  if ValidCefWindowHandle(aParent) and (PGtkWidget(aParent)^.window <> nil) then
    TempParent := gdk_window_xwindow(PGtkWidget(aParent)^.window);
  {$ENDIF}

  aWindowInfo.window_name                  := CefString(aWindowName);
  aWindowInfo.bounds.x                     := aRect.left;
  aWindowInfo.bounds.y                     := aRect.top;
  aWindowInfo.bounds.width                 := aRect.right  - aRect.left;
  aWindowInfo.bounds.height                := aRect.bottom - aRect.top;
  aWindowInfo.parent_window                := TempParent;
  aWindowInfo.windowless_rendering_enabled := ord(False);
  aWindowInfo.shared_texture_enabled       := ord(False);
  aWindowInfo.external_begin_frame_enabled := ord(False);
  aWindowInfo.window                       := 0;
end;

// WindowInfoAsPopUp only exists for Windows. The Linux version of cefclient
// calls WindowInfoAsChild with aParent set to NULL to create a popup window.
procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring = '');
begin
  aWindowInfo.window_name                  := CefString(aWindowName);
  aWindowInfo.bounds.x                     := 0;
  aWindowInfo.bounds.y                     := 0;
  aWindowInfo.bounds.width                 := 0;
  aWindowInfo.bounds.height                := 0;
  aWindowInfo.parent_window                := aParent;
  aWindowInfo.windowless_rendering_enabled := ord(False);
  aWindowInfo.shared_texture_enabled       := ord(False);
  aWindowInfo.external_begin_frame_enabled := ord(False);
  aWindowInfo.window                       := 0;
end;

procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring = '');
begin
  aWindowInfo.window_name                  := CefString(aWindowName);
  aWindowInfo.bounds.x                     := 0;
  aWindowInfo.bounds.y                     := 0;
  aWindowInfo.bounds.width                 := 0;
  aWindowInfo.bounds.height                := 0;
  aWindowInfo.parent_window                := aParent;
  aWindowInfo.windowless_rendering_enabled := ord(True);
  aWindowInfo.shared_texture_enabled       := ord(False);
  aWindowInfo.external_begin_frame_enabled := ord(False);
  aWindowInfo.window                       := 0;
end;
{$ENDIF}

function CefIsCertStatusError(Status : TCefCertStatus) : boolean;
begin
  Result := (GlobalCEFApp <> nil)  and
            GlobalCEFApp.LibLoaded and
            (cef_is_cert_status_error(Status) <> 0);
end;

function CefCrashReportingEnabled : boolean;
begin
  Result := (GlobalCEFApp <> nil)  and
            GlobalCEFApp.LibLoaded and
            (cef_crash_reporting_enabled() <> 0);
end;

procedure CefSetCrashKeyValue(const aKey, aValue : ustring);
var
  TempKey, TempValue : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempKey   := CefString(aKey);
      TempValue := CefString(aValue);
      cef_set_crash_key_value(@TempKey, @TempValue);
    end;
end;

procedure CefLog(const aFile : string; aLine, aSeverity : integer; const aMessage : string);
var
  TempFile, TempMessage : AnsiString;
begin
  if (length(aFile) > 0) and (length(aMessage) > 0) then
    begin
      TempFile    := AnsiString(aFile);
      TempMessage := AnsiString(aMessage);

      cef_log(@TempFile[1], aLine, aSeverity, @TempMessage[1]);
    end;
end;

procedure CefDebugLog(const aMessage : string; aSeverity : integer);
const
  DEFAULT_LINE = 1;
var
  TempString : string;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      {$IFDEF MSWINDOWS}
        TempString := 'PID: ' + IntToStr(GetCurrentProcessID) + ', TID: ' + IntToStr(GetCurrentThreadID);
      {$ENDIF}

      {$IFDEF LINUX}
        {$IFDEF FPC}
        TempString := 'PID: ' + IntToStr(GetProcessID()) + ', TID: ' + IntToStr(GetCurrentThreadID());
        {$ELSE}
          // TO-DO: Find the equivalent function to get the process ID in Delphi FMX for Linux
        {$ENDIF}
      {$ENDIF}

      {$IFDEF MACOSX}
        {$IFDEF FPC}
          // TO-DO: Find the equivalent function to get the process ID in Lazarus/FPC for MacOS
        {$ELSE}
          TempString := 'PID: ' + IntToStr(TNSProcessInfo.Wrap(TNSProcessInfo.OCClass.processInfo).processIdentifier) +
                        ', TID: ' + IntToStr(TThread.Current.ThreadID);
        {$ENDIF}
      {$ENDIF}

      case GlobalCEFApp.ProcessType of
        ptBrowser   : TempString := TempString + ', PT: Browser';
        ptRenderer  : TempString := TempString + ', PT: Renderer';
        ptZygote    : TempString := TempString + ', PT: Zygote';
        ptGPU       : TempString := TempString + ', PT: GPU';
        ptUtility   : TempString := TempString + ', PT: Utility';
        ptOther     : TempString := TempString + ', PT: Other';
      end;

      CefLog('CEF4Delphi', DEFAULT_LINE, aSeverity, TempString + ' - ' + aMessage);
    end;
end;

procedure CefKeyEventLog(const aEvent : TCefKeyEvent);
const
  DEFAULT_LINE = 1;
var
  TempString : string;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      case aEvent.kind of
        KEYEVENT_RAWKEYDOWN : TempString := 'kind: KEYEVENT_RAWKEYDOWN';
        KEYEVENT_KEYDOWN    : TempString := 'kind: KEYEVENT_KEYDOWN';
        KEYEVENT_KEYUP      : TempString := 'kind: KEYEVENT_KEYUP';
        KEYEVENT_CHAR       : TempString := 'kind: KEYEVENT_CHAR';
      end;

      TempString := TempString + ', modifiers: $'              + inttohex(aEvent.modifiers, SizeOf(aEvent.modifiers) * 2);
      TempString := TempString + ', windows_key_code: $'       + inttohex(aEvent.windows_key_code, SizeOf(aEvent.windows_key_code) * 2);
      TempString := TempString + ', native_key_code: $'        + inttohex(aEvent.native_key_code, SizeOf(aEvent.native_key_code) * 2);
      TempString := TempString + ', is_system_key: '           + BoolToStr((aEvent.is_system_key <> 0), true);
      TempString := TempString + ', character: $'              + inttohex(ord(aEvent.character), SizeOf(aEvent.character) * 2);
      TempString := TempString + ', unmodified_character: $'   + inttohex(ord(aEvent.unmodified_character), SizeOf(aEvent.unmodified_character) * 2);
      TempString := TempString + ', focus_on_editable_field: ' + BoolToStr((aEvent.focus_on_editable_field <> 0), true);;

      CefLog('CEF4Delphi', DEFAULT_LINE, CEF_LOG_SEVERITY_INFO, TempString);
    end;
end;

procedure CefMouseEventLog(const aEvent : TCefMouseEvent);
const
  DEFAULT_LINE = 1;
var
  TempString : string;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempString := TempString + ', x: $'         + inttohex(aEvent.x, SizeOf(aEvent.x) * 2);
      TempString := TempString + ', y: $'         + inttohex(aEvent.y, SizeOf(aEvent.y) * 2);
      TempString := TempString + ', modifiers: $' + inttohex(aEvent.modifiers, SizeOf(aEvent.modifiers) * 2);

      CefLog('CEF4Delphi', DEFAULT_LINE, CEF_LOG_SEVERITY_INFO, TempString);
    end;
end;

procedure OutputDebugMessage(const aMessage : string);
const
  DEFAULT_LINE = 1;
begin
  {$IFDEF DEBUG}
    {$IFDEF MSWINDOWS}
      {$IFDEF FMX}
        FMX.Types.Log.d(aMessage);
      {$ELSE}
        OutputDebugString({$IFDEF DELPHI12_UP}PWideChar{$ELSE}PAnsiChar{$ENDIF}(aMessage + chr(0)));
      {$ENDIF}
    {$ENDIF}

    {$IFDEF LINUX}
      {$IFDEF FPC}
        // TO-DO: Find a way to write in the error console using Lazarus in Linux
      {$ELSE}
        FMX.Types.Log.d(aMessage);
      {$ENDIF}
    {$ENDIF}
    {$IFDEF MACOSX}
      {$IFDEF FPC}
        // TO-DO: Find a way to write in the error console using Lazarus in MacOS
      {$ELSE}
        FMX.Types.Log.d(aMessage);
      {$ENDIF}
    {$ENDIF}

  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    CefLog('CEF4Delphi', DEFAULT_LINE, CEF_LOG_SEVERITY_ERROR, aMessage);
  {$ENDIF}
end;

function CustomExceptionHandler(const aFunctionName : string; const aException : exception) : boolean;
begin
  OutputDebugMessage(aFunctionName + ' error : ' + aException.message);

  Result := (GlobalCEFApp <> nil) and GlobalCEFApp.ReRaiseExceptions;
end;

function CefRegisterSchemeHandlerFactory(const SchemeName : ustring;
                                         const DomainName : ustring;
                                         const handler    : TCefResourceHandlerClass) : boolean;
var
  TempScheme, TempDomainName : TCefString;
  TempFactory : ICefSchemeHandlerFactory;
  TempDomainNamePtr : PCefString;
begin
  Result := False;

  try
    if (GlobalCEFApp <> nil)    and
       GlobalCEFApp.LibLoaded   and
       (length(SchemeName) > 0) then
      begin
        if (length(DomainName) > 0) then
          begin
            TempDomainName    := CefString(DomainName);
            TempDomainNamePtr := @TempDomainName;
          end
         else
          TempDomainNamePtr := nil;

        TempScheme  := CefString(SchemeName);
        TempFactory := TCefSchemeHandlerFactoryOwn.Create(handler);
        Result      := cef_register_scheme_handler_factory(@TempScheme, TempDomainNamePtr, TempFactory.Wrap) <> 0;
      end;
  finally
    TempFactory := nil;
  end;
end;

function CefClearSchemeHandlerFactories : boolean;
begin
  Result := (GlobalCEFApp <> nil)  and
            GlobalCEFApp.LibLoaded and
            (cef_clear_scheme_handler_factories() <> 0);
end;

function CefAddCrossOriginWhitelistEntry(const SourceOrigin          : ustring;
                                         const TargetProtocol        : ustring;
                                         const TargetDomain          : ustring;
                                               AllowTargetSubdomains : Boolean): Boolean;
var
  TempSourceOrigin, TempTargetProtocol, TempTargetDomain : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempSourceOrigin   := CefString(SourceOrigin);
      TempTargetProtocol := CefString(TargetProtocol);
      TempTargetDomain   := CefString(TargetDomain);
      Result             := cef_add_cross_origin_whitelist_entry(@TempSourceOrigin,
                                                                 @TempTargetProtocol,
                                                                 @TempTargetDomain,
                                                                 Ord(AllowTargetSubdomains)) <> 0;
    end
   else
    Result := False;
end;

function CefRemoveCrossOriginWhitelistEntry(const SourceOrigin          : ustring;
                                            const TargetProtocol        : ustring;
                                            const TargetDomain          : ustring;
                                                  AllowTargetSubdomains : Boolean): Boolean;
var
  TempSourceOrigin, TempTargetProtocol, TempTargetDomain : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempSourceOrigin   := CefString(SourceOrigin);
      TempTargetProtocol := CefString(TargetProtocol);
      TempTargetDomain   := CefString(TargetDomain);
      Result             := cef_remove_cross_origin_whitelist_entry(@TempSourceOrigin,
                                                                    @TempTargetProtocol,
                                                                    @TempTargetDomain,
                                                                    Ord(AllowTargetSubdomains)) <> 0;
    end
   else
    Result := False;
end;

function CefClearCrossOriginWhitelist: Boolean;
begin
  Result := cef_clear_cross_origin_whitelist() <> 0;
end;

function SplitLongString(aSrcString : string) : string;
const
  MAXLINELENGTH = 50;
begin
  Result := '';
  while (length(aSrcString) > 0) do
    begin
      if (Result <> '') then
        Result := Result + CRLF + copy(aSrcString, 1, MAXLINELENGTH)
       else
        Result := Result + copy(aSrcString, 1, MAXLINELENGTH);

      aSrcString := copy(aSrcString, succ(MAXLINELENGTH), length(aSrcString));
    end;
end;

function GetAbsoluteDirPath(const aSrcPath : string; var aRsltPath : string) : boolean;
begin
  Result := True;

  if (length(aSrcPath) > 0) then
    begin
      aRsltPath := IncludeTrailingPathDelimiter(CustomAbsolutePath(aSrcPath));
      Result    := DirectoryExists(aRsltPath);
    end
   else
    aRsltPath := '';
end;

function CheckLocales(const aLocalesDirPath : string; var aMissingFiles : string; const aLocalesRequired : string) : boolean;
const
  LOCALES_REQUIRED_DEFAULT =
    'am,' +
    'ar,' +
    'bg,' +
    'bn,' +
    'ca,' +
    'cs,' +
    'da,' +
    'de,' +
    'el,' +
    'en-GB,' +
    'en-US,' +
    'es,' +
    'es-419,' +
    'et,' +
    'fa,' +
    'fi,' +
    'fil,' +
    'fr,' +
    'gu,' +
    'he,' +
    'hi,' +
    'hr,' +
    'hu,' +
    'id,' +
    'it,' +
    'ja,' +
    'kn,' +
    'ko,' +
    'lt,' +
    'lv,' +
    'ml,' +
    'mr,' +
    'ms,' +
    'nb,' +
    'nl,' +
    'pl,' +
    'pt-BR,' +
    'pt-PT,' +
    'ro,' +
    'ru,' +
    'sk,' +
    'sl,' +
    'sr,' +
    'sv,' +
    'sw,' +
    'ta,' +
    'te,' +
    'th,' +
    'tr,' +
    'uk,' +
    'vi,' +
    'zh-CN,' +
    'zh-TW';
var
  i        : integer;
  TempDir  : string;
  TempList : TStringList;
begin
  Result   := False;
  TempList := nil;

  try
    try
      if (length(aLocalesDirPath) > 0) then
        TempDir := IncludeTrailingPathDelimiter(aLocalesDirPath)
       else
        TempDir := 'locales' + PathDelim;

      TempList := TStringList.Create;

      if (length(aLocalesRequired) > 0) then
        TempList.CommaText := aLocalesRequired
       else
        TempList.CommaText := LOCALES_REQUIRED_DEFAULT;

      i := 0;
      while (i < TempList.Count) do
        begin
          TempList[i] := TempDir + TempList[i] + '.pak';
          inc(i);
        end;

      if DirectoryExists(TempDir) then
        Result := CheckFilesExist(TempList, aMissingFiles)
       else
        aMissingFiles := trim(aMissingFiles) + CRLF + TempList.Text;
    except
      on e : exception do
        if CustomExceptionHandler('CheckLocales', e) then raise;
    end;
  finally
    if (TempList <> nil) then FreeAndNil(TempList);
  end;
end;

function CheckResources(const aResourcesDirPath : string; var aMissingFiles : string) : boolean;
var
  TempDir    : string;
  TempList   : TStringList;
  TempExists : boolean;
begin
  Result := False;

  try
    try
      TempExists := GetAbsoluteDirPath(aResourcesDirPath, TempDir);

      TempList := TStringList.Create;
      TempList.Add(TempDir + 'snapshot_blob.bin');
      TempList.Add(TempDir + 'v8_context_snapshot.bin');
      TempList.Add(TempDir + 'resources.pak');
      TempList.Add(TempDir + 'chrome_100_percent.pak');
      TempList.Add(TempDir + 'chrome_200_percent.pak');

      if TempExists then
        Result := CheckFilesExist(TempList, aMissingFiles)
       else
        aMissingFiles := trim(aMissingFiles) + CRLF + TempList.Text;
    except
      on e : exception do
        if CustomExceptionHandler('CheckResources', e) then raise;
    end;
  finally
    if (TempList <> nil) then FreeAndNil(TempList);
  end;
end;

function CheckSubprocessPath(const aSubprocessPath : string; var aMissingFiles : string) : boolean;
begin
  Result := False;

  try
    if (length(aSubprocessPath) = 0) or FileExists(aSubprocessPath) then
      Result := True
     else
      aMissingFiles := trim(aMissingFiles) + CRLF + ExtractFileName(aSubprocessPath);
  except
    on e : exception do
      if CustomExceptionHandler('CheckSubprocessPath', e) then raise;
  end;
end;

function CheckDLLs(const aFrameworkDirPath : string; var aMissingFiles : string) : boolean;
var
  TempDir    : string;
  TempList   : TStringList;
  TempExists : boolean;
begin
  Result   := False;
  TempList := nil;

  try
    try
      TempExists := GetAbsoluteDirPath(aFrameworkDirPath, TempDir);

      // The icudtl.dat file must be placed next to libcef.dll
      // http://www.magpcss.org/ceforum/viewtopic.php?f=6&t=14503#p32263

      TempList := TStringList.Create;
      TempList.Add(TempDir + LIBCEF_DLL);
      {$IFDEF MSWINDOWS}
      TempList.Add(TempDir + CHROMEELF_DLL);
      TempList.Add(TempDir + 'd3dcompiler_47.dll');
      TempList.Add(TempDir + 'vk_swiftshader.dll');
      TempList.Add(TempDir + 'vk_swiftshader_icd.json');
      TempList.Add(TempDir + 'vulkan-1.dll');
      TempList.Add(TempDir + 'libEGL.dll');
      TempList.Add(TempDir + 'libGLESv2.dll');
      {$ENDIF}
      {$IFDEF LINUX}
      TempList.Add(TempDir + 'libEGL.so');
      TempList.Add(TempDir + 'libGLESv2.so');
      TempList.Add(TempDir + 'libvk_swiftshader.so');
      TempList.Add(TempDir + 'vk_swiftshader_icd.json');
      TempList.Add(TempDir + 'libvulkan.so.1');
      {$ENDIF}
      TempList.Add(TempDir + 'icudtl.dat');

      if TempExists then
        Result := CheckFilesExist(TempList, aMissingFiles)
       else
        aMissingFiles := trim(aMissingFiles) + CRLF + TempList.Text;
    except
      on e : exception do
        if CustomExceptionHandler('CheckDLLs', e) then raise;
    end;
  finally
    if (TempList <> nil) then FreeAndNil(TempList);
  end;
end;

function CheckFilesExist(var aList : TStringList; var aMissingFiles : string) : boolean;
var
  i : integer;
begin
  Result := True;

  try
    if (aList <> nil) then
      begin
        i := 0;

        while (i < aList.Count) do
          begin
            if (length(aList[i]) > 0) and not(FileExists(aList[i])) then
              begin
                Result        := False;
                aMissingFiles := aMissingFiles + aList[i] + CRLF;
              end;

            inc(i);
          end;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('CheckFilesExist', e) then raise;
  end;
end;

procedure UInt64ToFileVersionInfo(const aVersion : uint64; var aVersionInfo : TFileVersionInfo);
begin
  aVersionInfo.MajorVer := uint16(aVersion shr 48);
  aVersionInfo.MinorVer := uint16((aVersion shr 32) and $FFFF);
  aVersionInfo.Release  := uint16((aVersion shr 16) and $FFFF);
  aVersionInfo.Build    := uint16(aVersion and $FFFF);
end;

{$IFDEF MSWINDOWS}
function GetExtendedFileVersion(const aFileName : ustring) : uint64;
var
  TempSize   : DWORD;
  TempBuffer : pointer;
  TempLen    : UINT;
  TempHandle : cardinal;
  TempInfo   : PVSFixedFileInfo;
begin
  Result     := 0;
  TempBuffer := nil;
  TempHandle := 0;
  TempLen    := 0;

  try
    try
      TempSize := GetFileVersionInfoSizeW(PWideChar(aFileName), TempHandle);

      if (TempSize > 0) then
        begin
          GetMem(TempBuffer, TempSize);

          if GetFileVersionInfoW(PWideChar(aFileName), TempHandle, TempSize, TempBuffer) and
             VerQueryValue(TempBuffer, '\', Pointer(TempInfo), TempLen) then
            begin
              Result := TempInfo^.dwFileVersionMS;
              Result := Result shl 32;
              Result := Result or TempInfo^.dwFileVersionLS;
            end;
        end
       else
        OutputLastErrorMessage;
    except
      on e : exception do
        if CustomExceptionHandler('GetExtendedFileVersion', e) then raise;
    end;
  finally
    if (TempBuffer <> nil) then FreeMem(TempBuffer);
  end;
end;

procedure OutputLastErrorMessage;
begin
  {$IFDEF DEBUG}
  OutputDebugString({$IFDEF DELPHI12_UP}PWideChar{$ELSE}PAnsiChar{$ENDIF}(SysErrorMessage(GetLastError()) + chr(0)));
  {$ENDIF}
end;

function GetDLLVersion(const aDLLFile : ustring; var aVersionInfo : TFileVersionInfo) : boolean;
var
  TempVersion : uint64;
begin
  Result := False;

  try
    if FileExists(aDLLFile) then
      begin
        TempVersion := GetExtendedFileVersion(aDLLFile);
        if (TempVersion <> 0) then
          begin
            UInt64ToFileVersionInfo(TempVersion, aVersionInfo);
            Result := True;
          end;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('GetDLLVersion', e) then raise;
  end;
end;      

function CheckDLLVersion(const aDLLFile : ustring; aMajor, aMinor, aRelease, aBuild : uint16) : boolean;
var
  TempVersionInfo : TFileVersionInfo;
begin
  Result := GetDLLVersion(aDLLFile, TempVersionInfo) and
            (TempVersionInfo.MajorVer = aMajor)      and
            (TempVersionInfo.MinorVer = aMinor)      and
            (TempVersionInfo.Release  = aRelease)    and
            (TempVersionInfo.Build    = aBuild);
end;

// This function is based on the answer given by 'Alex' in StackOverflow
// https://stackoverflow.com/questions/2748474/how-to-determine-if-dll-file-was-compiled-as-x64-or-x86-bit-using-either-delphi
function GetDLLHeaderMachine(const aDLLFile : ustring; var aMachine : integer) : boolean;
var
  TempHeader         : TImageDosHeader;
  TempImageNtHeaders : TImageNtHeaders;
  TempStream         : TFileStream;
begin
  Result     := False;
  aMachine   := IMAGE_FILE_MACHINE_UNKNOWN;
  TempStream := nil;

  try
    try
      if FileExists(aDLLFile) then
        begin
          TempStream := TFileStream.Create(aDLLFile, fmOpenRead or fmShareDenyWrite);
          TempStream.seek(0, soFromBeginning);
          TempStream.ReadBuffer(TempHeader, SizeOf(TempHeader));

          if (TempHeader.e_magic = IMAGE_DOS_SIGNATURE) and
             (TempHeader._lfanew <> 0) then
            begin
              TempStream.Position := TempHeader._lfanew;
              TempStream.ReadBuffer(TempImageNtHeaders, SizeOf(TempImageNtHeaders));

              if (TempImageNtHeaders.Signature = IMAGE_NT_SIGNATURE) then
                begin
                  aMachine := TempImageNtHeaders.FileHeader.Machine;
                  Result   := True;
                end;
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('GetDLLHeaderMachine', e) then raise;
    end;
  finally
    if (TempStream <> nil) then FreeAndNil(TempStream);
  end;
end;

function GetFileTypeDescription(const aExtension : ustring) : ustring;
var
  TempInfo : SHFILEINFOW;
  TempExt  : ustring;
begin
  Result := '';

  if (length(aExtension) > 0) then
    begin
      if (aExtension[1] = '.') then
        TempExt := aExtension
       else
        TempExt := '.' + aExtension;

      if (SHGetFileInfoW(@TempExt[1],
                         FILE_ATTRIBUTE_NORMAL,
                         TempInfo,
                         SizeOf(SHFILEINFO),
                         SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES) <> 0) then
        Result := TempInfo.szTypeName;
    end;
end;
{$ELSE}
function GetFileTypeDescription(const aExtension : ustring) : ustring;
begin
  Result := uppercase(aExtension) + ' files';
end;
{$ENDIF}

function FileVersionInfoToString(const aVersionInfo : TFileVersionInfo) : string;
begin
  Result := IntToStr(aVersionInfo.MajorVer) + '.' +
            IntToStr(aVersionInfo.MinorVer) + '.' +
            IntToStr(aVersionInfo.Release)  + '.' +
            IntToStr(aVersionInfo.Build);
end;

{$IFDEF MSWINDOWS}
function Is32BitProcessRunningIn64BitOS : boolean;
var
  TempResult : BOOL;
begin
  Result := ProcessUnderWow64(GetCurrentProcess, @TempResult) and
            TempResult;
end;
{$ENDIF}

function Is32BitProcess : boolean;
begin
  {$IFDEF TARGET_32BITS}
  Result := True;
  {$ELSE}
    {$IFDEF MSWINDOWS}
    Result := Is32BitProcessRunningIn64BitOS;
    {$ELSE}
      {$IFDEF DELPHI17_UP}
        Result := TOSVersion.Architecture in [arIntelX86, arARM32];
      {$ELSE}
        Result := False;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

function CustomPathIsRelative(const aPath : string) : boolean;
begin
  {$IFDEF MSWINDOWS}
    {$IFDEF DELPHI12_UP}
    Result := PathIsRelativeUnicode(PChar(aPath));
    {$ELSE}
    Result := PathIsRelativeAnsi(PChar(aPath));
    {$ENDIF}
  {$ELSE}
  Result := (length(aPath) > 0) and (aPath[1] <> '/');
  {$ENDIF}
end;

function CustomPathIsURL(const aPath : string) : boolean;
begin
  {$IFDEF MSWINDOWS}
    {$IFDEF DELPHI12_UP}
    Result := PathIsURLUnicode(PChar(aPath + #0));
    {$ELSE}
    Result := PathIsURLAnsi(PChar(aPath + #0));
    {$ENDIF}
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function CustomPathIsUNC(const aPath : string) : boolean;
begin
  {$IFDEF MSWINDOWS}
    {$IFDEF DELPHI12_UP}
    Result := PathIsUNCUnicode(PChar(aPath + #0));
    {$ELSE}
    Result := PathIsUNCAnsi(PChar(aPath + #0));
    {$ENDIF}
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function CustomPathCanonicalize(const aOriginalPath : string; var aCanonicalPath : string) : boolean;
var
  TempBuffer: array [0..pred(MAX_PATH)] of Char;
begin
  Result         := False;
  aCanonicalPath := '';

  if (length(aOriginalPath) > MAX_PATH) or
     (Copy(aOriginalPath, 1, 4) = '\\?\') or
     CustomPathIsUNC(aOriginalPath) then
    exit;

  FillChar(TempBuffer, MAX_PATH * SizeOf(Char), 0);

  {$IFDEF MSWINDOWS}
    {$IFDEF DELPHI12_UP}
    if PathCanonicalizeUnicode(@TempBuffer[0], PChar(aOriginalPath + #0)) then
      begin
        aCanonicalPath := TempBuffer;
        Result         := True;
      end;
    {$ELSE}
    if PathCanonicalizeAnsi(@TempBuffer[0], PChar(aOriginalPath + #0)) then
      begin
        aCanonicalPath := TempBuffer;
        Result         := True;
      end;
    {$ENDIF}
  {$ENDIF}
end;

function CustomAbsolutePath(const aPath : string; aMustExist : boolean) : string;
var
  TempNewPath, TempOldPath : string;
begin
  if (length(aPath) > 0) then
    begin
      if CustomPathIsRelative(aPath) then
        TempOldPath := GetModulePath + aPath
       else
        TempOldPath := aPath;

      if not(CustomPathCanonicalize(TempOldPath, TempNewPath)) then
        TempNewPath := TempOldPath;

      if aMustExist and not(DirectoryExists(TempNewPath)) then
        Result := ''
       else
        Result := TempNewPath;
    end
   else
    Result := '';
end;

function GetModulePath : string;
{$IFDEF MACOSX}
const
  MAC_APP_POSTFIX = '.app/';
  MAC_APP_SUBPATH = 'Contents/MacOS/';
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  Result := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HINSTANCE{$IFDEF FPC}(){$ENDIF})));
  {$ENDIF}

  {$IFDEF LINUX}
  Result := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
  {$ENDIF}

  {$IFDEF MACOSX}
  Result := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));

  {$IFDEF FPC}
  if copy(Result, Length(Result) + 1 - Length(MAC_APP_POSTFIX) - Length(MAC_APP_SUBPATH)) = MAC_APP_POSTFIX + MAC_APP_SUBPATH then
    SetLength(Result, Length(Result) - Length(MAC_APP_SUBPATH));

  Result := CreateAbsolutePath(Result, GetCurrentDirUTF8);
  {$ELSE}
  if Result.Contains(MAC_APP_POSTFIX + MAC_APP_SUBPATH) then
    Result := Result.Remove(Result.IndexOf(MAC_APP_SUBPATH));
  {$ENDIF}
  {$ENDIF}
end;

function CefParseUrl(const url: ustring; var parts: TUrlParts): Boolean;
var
  TempURL   : TCefString;
  TempParts : TCefUrlParts;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      FillChar(TempParts, sizeof(TempParts), 0);
      TempURL := CefString(url);
      Result  := cef_parse_url(@TempURL, TempParts) <> 0;

      if Result then
        begin
          parts.spec     := CefString(@TempParts.spec);
          parts.scheme   := CefString(@TempParts.scheme);
          parts.username := CefString(@TempParts.username);
          parts.password := CefString(@TempParts.password);
          parts.host     := CefString(@TempParts.host);
          parts.port     := CefString(@TempParts.port);
          parts.origin   := CefString(@TempParts.origin);
          parts.path     := CefString(@TempParts.path);
          parts.query    := CefString(@TempParts.query);
          parts.fragment := CefString(@TempParts.fragment);
        end;
    end
   else
    Result := False;
end;

function CefCreateUrl(var parts: TUrlParts): ustring;
var
  TempURL   : TCefString;
  TempParts : TCefUrlParts;
begin
  Result := '';

  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempParts.spec     := CefString(parts.spec);
      TempParts.scheme   := CefString(parts.scheme);
      TempParts.username := CefString(parts.username);
      TempParts.password := CefString(parts.password);
      TempParts.host     := CefString(parts.host);
      TempParts.port     := CefString(parts.port);
      TempParts.origin   := CefString(parts.origin);
      TempParts.path     := CefString(parts.path);
      TempParts.query    := CefString(parts.query);
      TempParts.fragment := CefString(parts.fragment);

      CefStringInitialize(@TempURL);

      if (cef_create_url(@TempParts, @TempURL) <> 0) then
        Result := CefStringClearAndGet(@TempURL);
    end;
end;

function CefFormatUrlForSecurityDisplay(const originUrl: string): string;
var
  TempOrigin : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempOrigin := CefString(originUrl);
      Result     := CefStringFreeAndGet(cef_format_url_for_security_display(@TempOrigin));
    end
   else
    Result := '';
end;

function CefGetMimeType(const extension: ustring): ustring;
var
  TempExt : TCefString;
begin
  TempExt := CefString(extension);
  Result  := CefStringFreeAndGet(cef_get_mime_type(@TempExt));
end;

procedure CefGetExtensionsForMimeType(const mimeType: ustring; var extensions: TStringList);
var
  TempSL       : ICefStringList;
  TempMimeType : TCefString;
begin
  if (extensions <> nil) and (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempSL       := TCefStringListOwn.Create;
      TempMimeType := CefString(mimeType);
      cef_get_extensions_for_mime_type(@TempMimeType, TempSL.Handle);
      TempSL.CopyToStrings(extensions);
    end;
end;

function CefBase64Encode(const data: Pointer; dataSize: NativeUInt): ustring;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    Result := CefStringFreeAndGet(cef_base64encode(data, dataSize))
   else
    Result := '';
end;

function CefBase64Decode(const data: ustring): ICefBinaryValue;
var
  TempData : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempData := CefString(data);
      Result   := TCefBinaryValueRef.UnWrap(cef_base64decode(@TempData));
    end
   else
    Result := nil;
end;

function CefUriEncode(const text: ustring; usePlus: Boolean): ustring;
var
  TempText : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempText := CefString(text);
      Result   := CefStringFreeAndGet(cef_uriencode(@TempText, Ord(usePlus)));
    end
   else
    Result := '';
end;

function CefUriDecode(const text: ustring; convertToUtf8: Boolean; unescapeRule: TCefUriUnescapeRule): ustring;
var
  TempText : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempText := CefString(text);
      Result   := CefStringFreeAndGet(cef_uridecode(@TempText, Ord(convertToUtf8), unescapeRule));
    end
   else
    Result := '';
end;

function CefGetPath(const aPathKey : TCefPathKey) : ustring;
var
  TempPath : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      CefStringInitialize(@TempPath);

      if (cef_get_path(aPathKey, @TempPath) <> 0) then
        Result := CefStringClearAndGet(@TempPath);
    end
   else
    Result := '';
end;

function CefIsRTL : boolean;
begin
  Result := (GlobalCEFApp <> nil) and
            GlobalCEFApp.LibLoaded and
            (cef_is_rtl() <> 0);
end;

function CefCreateDirectory(const fullPath: ustring): Boolean;
var
  TempPath : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempPath := CefString(fullPath);
      Result   := cef_create_directory(@TempPath) <> 0;
    end
   else
    Result := False;
end;

function CefGetTempDirectory(out tempDir: ustring): Boolean;
var
  TempPath : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      CefStringInitialize(@TempPath);
      Result  := cef_get_temp_directory(@TempPath) <> 0;
      tempDir := CefStringClearAndGet(@TempPath);
    end
   else
    begin
      Result  := False;
      tempDir := '';
    end;
end;

function CefCreateNewTempDirectory(const prefix: ustring; out newTempPath: ustring): Boolean;
var
  TempPath, TempPref : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      CefStringInitialize(@TempPath);
      TempPref    := CefString(prefix);
      Result      := cef_create_new_temp_directory(@TempPref, @TempPath) <> 0;
      newTempPath := CefStringClearAndGet(@TempPath);
    end
   else
    begin
      Result      := False;
      newTempPath := '';
    end;
end;

function CefCreateTempDirectoryInDirectory(const baseDir, prefix: ustring; out newDir: ustring): Boolean;
var
  TempBase, TempPath, TempPref: TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      CefStringInitialize(@TempPath);
      TempPref := CefString(prefix);
      TempBase := CefString(baseDir);
      Result   := cef_create_temp_directory_in_directory(@TempBase, @TempPref, @TempPath) <> 0;
      newDir   := CefStringClearAndGet(@TempPath);
    end
   else
    begin
      Result   := False;
      newDir   := '';
    end;
end;

function CefDirectoryExists(const path: ustring): Boolean;
var
  TempPath : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempPath := CefString(path);
      Result   := cef_directory_exists(@TempPath) <> 0;
    end
   else
    Result := False;
end;

function CefDeleteFile(const path: ustring; recursive: Boolean): Boolean;
var
  TempPath : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempPath := CefString(path);
      Result   := cef_delete_file(@TempPath, Ord(recursive)) <> 0;
    end
   else
    Result := False;
end;

function CefZipDirectory(const srcDir, destFile: ustring; includeHiddenFiles: Boolean): Boolean;
var
  TempSrc, TempDst : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempSrc := CefString(srcDir);
      TempDst := CefString(destFile);
      Result  := cef_zip_directory(@TempSrc, @TempDst, Ord(includeHiddenFiles)) <> 0;
    end
   else
    Result := False;
end;

procedure CefLoadCRLSetsFile(const path : ustring);
var
  TempPath : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempPath := CefString(path);
      cef_load_crlsets_file(@TempPath);
    end;
end;

{$IFDEF MSWINDOWS}
function CefIsKeyDown(aWparam : WPARAM) : boolean;
begin
  Result := (GetKeyState(aWparam) < 0);
end;

function CefIsKeyToggled(aWparam : WPARAM) : boolean;
begin
  Result := (GetKeyState(aWparam) and $1) <> 0;
end;

function GetCefMouseModifiers(aWparam : WPARAM) : TCefEventFlags;
begin
  Result := EVENTFLAG_NONE;

  if ((aWparam and MK_CONTROL) <> 0) then Result := Result or EVENTFLAG_CONTROL_DOWN;
  if ((aWparam and MK_SHIFT)   <> 0) then Result := Result or EVENTFLAG_SHIFT_DOWN;
  if ((aWparam and MK_LBUTTON) <> 0) then Result := Result or EVENTFLAG_LEFT_MOUSE_BUTTON;
  if ((aWparam and MK_MBUTTON) <> 0) then Result := Result or EVENTFLAG_MIDDLE_MOUSE_BUTTON;
  if ((aWparam and MK_RBUTTON) <> 0) then Result := Result or EVENTFLAG_RIGHT_MOUSE_BUTTON;
  if CefIsKeyDown(VK_MENU)           then Result := Result or EVENTFLAG_ALT_DOWN;
  if CefIsKeyToggled(VK_NUMLOCK)     then Result := Result or EVENTFLAG_NUM_LOCK_ON;
  if CefIsKeyToggled(VK_CAPITAL)     then Result := Result or EVENTFLAG_CAPS_LOCK_ON;
end;

function GetCefMouseModifiers : TCefEventFlags;
begin
  Result := EVENTFLAG_NONE;

  if CefIsKeyDown(MK_CONTROL)    then Result := Result or EVENTFLAG_CONTROL_DOWN;
  if CefIsKeyDown(MK_SHIFT)      then Result := Result or EVENTFLAG_SHIFT_DOWN;
  if CefIsKeyDown(MK_LBUTTON)    then Result := Result or EVENTFLAG_LEFT_MOUSE_BUTTON;
  if CefIsKeyDown(MK_MBUTTON)    then Result := Result or EVENTFLAG_MIDDLE_MOUSE_BUTTON;
  if CefIsKeyDown(MK_RBUTTON)    then Result := Result or EVENTFLAG_RIGHT_MOUSE_BUTTON;
  if CefIsKeyDown(VK_MENU)       then Result := Result or EVENTFLAG_ALT_DOWN;
  if CefIsKeyToggled(VK_NUMLOCK) then Result := Result or EVENTFLAG_NUM_LOCK_ON;
  if CefIsKeyToggled(VK_CAPITAL) then Result := Result or EVENTFLAG_CAPS_LOCK_ON;
end;

function GetCefKeyboardModifiers(aWparam : WPARAM; aLparam : LPARAM) : TCefEventFlags;
begin
  Result := EVENTFLAG_NONE;

  if CefIsKeyDown(VK_SHIFT)      then Result := Result or EVENTFLAG_SHIFT_DOWN;
  if CefIsKeyDown(VK_CONTROL)    then Result := Result or EVENTFLAG_CONTROL_DOWN;
  if CefIsKeyDown(VK_MENU)       then Result := Result or EVENTFLAG_ALT_DOWN;
  if CefIsKeyToggled(VK_NUMLOCK) then Result := Result or EVENTFLAG_NUM_LOCK_ON;
  if CefIsKeyToggled(VK_CAPITAL) then Result := Result or EVENTFLAG_CAPS_LOCK_ON;


  case aWparam of
    VK_RETURN:
      if (((aLparam shr 16) and KF_EXTENDED) <> 0) then
        Result := Result or EVENTFLAG_IS_KEY_PAD;

    VK_INSERT,
    VK_DELETE,
    VK_HOME,
    VK_END,
    VK_PRIOR,
    VK_NEXT,
    VK_UP,
    VK_DOWN,
    VK_LEFT,
    VK_RIGHT :
      if (((aLparam shr 16) and KF_EXTENDED) = 0) then
        Result := Result or EVENTFLAG_IS_KEY_PAD;

    VK_NUMLOCK,
    VK_NUMPAD0,
    VK_NUMPAD1,
    VK_NUMPAD2,
    VK_NUMPAD3,
    VK_NUMPAD4,
    VK_NUMPAD5,
    VK_NUMPAD6,
    VK_NUMPAD7,
    VK_NUMPAD8,
    VK_NUMPAD9,
    VK_DIVIDE,
    VK_MULTIPLY,
    VK_SUBTRACT,
    VK_ADD,
    VK_DECIMAL,
    VK_CLEAR :
      Result := Result or EVENTFLAG_IS_KEY_PAD;

    VK_SHIFT :
      if CefIsKeyDown(VK_LSHIFT) then
        Result := Result or EVENTFLAG_IS_LEFT
       else
        if CefIsKeyDown(VK_RSHIFT) then
          Result := Result or EVENTFLAG_IS_RIGHT;

    VK_CONTROL :
      if CefIsKeyDown(VK_LCONTROL) then
        Result := Result or EVENTFLAG_IS_LEFT
       else
        if CefIsKeyDown(VK_RCONTROL) then
          Result := Result or EVENTFLAG_IS_RIGHT;

    VK_MENU :
      if CefIsKeyDown(VK_LMENU) then
        Result := Result or EVENTFLAG_IS_LEFT
       else
        if CefIsKeyDown(VK_RMENU) then
          Result := Result or EVENTFLAG_IS_RIGHT;

    VK_LWIN :
      Result := Result or EVENTFLAG_IS_LEFT;

    VK_RWIN :
      Result := Result or EVENTFLAG_IS_RIGHT;
  end;
end;

procedure CefCheckAltGrPressed(aWparam : WPARAM; var aEvent : TCefKeyEvent);
const
  EITHER_SHIFT_KEY_PRESSED     = $01;
  EITHER_CONTROL_KEY_PRESSED   = $02;
  EITHER_ALT_KEY_PRESSED       = $04;
  EITHER_HANKAKU_KEY_PRESSED   = $08;
  EITHER_RESERVED1_KEY_PRESSED = $10;
  EITHER_RESERVED2_KEY_PRESSED = $20;
var
  TempKBLayout       : HKL;
  TempTranslatedChar : SHORT;
  TempShiftState     : byte;
begin
  if (aEvent.kind = KEYEVENT_CHAR) and CefIsKeyDown(VK_RMENU) then
    begin
      TempKBLayout       := GetKeyboardLayout(0);
      TempTranslatedChar := VkKeyScanEx(char(aWparam), TempKBLayout);
      TempShiftState     := byte(TempTranslatedChar shr 8);

      if ((TempShiftState and EITHER_CONTROL_KEY_PRESSED) <> 0) and
         ((TempShiftState and EITHER_ALT_KEY_PRESSED)     <> 0) then
        begin
          aEvent.modifiers := aEvent.modifiers and not(EVENTFLAG_CONTROL_DOWN or EVENTFLAG_ALT_DOWN);
          aEvent.modifiers := aEvent.modifiers or EVENTFLAG_ALTGR_DOWN;
        end;
    end;
end;

procedure DropEffectToDragOperation(aEffect: Longint; var aAllowedOps : TCefDragOperations);
begin
  aAllowedOps := DRAG_OPERATION_NONE;

  if ((aEffect and DROPEFFECT_COPY) <> 0) then aAllowedOps := aAllowedOps or DRAG_OPERATION_COPY;
  if ((aEffect and DROPEFFECT_LINK) <> 0) then aAllowedOps := aAllowedOps or DRAG_OPERATION_LINK;
  if ((aEffect and DROPEFFECT_MOVE) <> 0) then aAllowedOps := aAllowedOps or DRAG_OPERATION_MOVE;
end;

procedure DragOperationToDropEffect(const aDragOperations : TCefDragOperations; var aEffect: Longint);
begin
  aEffect := DROPEFFECT_NONE;

  if ((aDragOperations and DRAG_OPERATION_COPY) <> 0) then aEffect := aEffect or DROPEFFECT_COPY;
  if ((aDragOperations and DRAG_OPERATION_LINK) <> 0) then aEffect := aEffect or DROPEFFECT_LINK;
  if ((aDragOperations and DRAG_OPERATION_MOVE) <> 0) then aEffect := aEffect or DROPEFFECT_MOVE;
end;

function GetWindowsMajorMinorVersion(var wMajorVersion, wMinorVersion : DWORD) : boolean;
type
  TRtlGetVersionFunc = function(var lpVersionInformation : TOSVersionInfoEx): LongInt; stdcall;
var
  TempHandle : THandle;
  TempInfo : TOSVersionInfoEx;
  TempRtlGetVersionFunc : TRtlGetVersionFunc;
begin
  Result        := False;
  wMajorVersion := 0;
  wMinorVersion := 0;

  try
    TempHandle := LoadLibrary(NTDLL);

    if (TempHandle <> 0) then
      try
        {$IFDEF FPC}Pointer({$ENDIF}TempRtlGetVersionFunc{$IFDEF FPC}){$ENDIF} := GetProcAddress(TempHandle, 'RtlGetVersion');

        if assigned(TempRtlGetVersionFunc) then
          begin
            ZeroMemory(@TempInfo, SizeOf(TOSVersionInfoEx));

            if (TempRtlGetVersionFunc(TempInfo) = 0) then
              begin
                Result        := True;
                wMajorVersion := TempInfo.dwMajorVersion;
                wMinorVersion := TempInfo.dwMinorVersion;
              end;
          end;
      finally
        FreeLibrary(TempHandle);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('GetWindowsMajorMinorVersion', e) then raise;
  end;
end;

// GetDpiForWindow is only available in Windows 10 (version 1607) or newer
function GetDPIForHandle(aHandle : HWND; var aDPI : UINT) : boolean;
type
  TGetDpiForWindow = function(hwnd: HWND): UINT; stdcall;
var
  TempHandle : THandle;
  TempGetDpiForWindowFunc : TGetDpiForWindow;
begin
  Result := False;
  aDPI   := 0;

  if (aHandle = 0) then exit;

  try
    TempHandle := LoadLibrary(User32DLL);

    if (TempHandle <> 0) then
      try
        {$IFDEF FPC}Pointer({$ENDIF}TempGetDpiForWindowFunc{$IFDEF FPC}){$ENDIF} := GetProcAddress(TempHandle, 'GetDpiForWindow');

        if assigned(TempGetDpiForWindowFunc) then
          begin
            aDPI   := TempGetDpiForWindowFunc(aHandle);
            Result := (aDPI <> 0);
          end;
      finally
        FreeLibrary(TempHandle);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('GetDPIForHandle', e) then raise;
  end;
end;

function RunningWindows10OrNewer : boolean;
var
  TempMajorVer, TempMinorVer : DWORD;
begin
  Result := GetWindowsMajorMinorVersion(TempMajorVer, TempMinorVer) and (TempMajorVer >= 10);
end;

function GetDefaultCEFUserAgent : string;
var
  TempOS, TempChromiumVersion : string;
  TempMajorVer, TempMinorVer : DWORD;
  Temp64bit : BOOL;
begin
  if GetWindowsMajorMinorVersion(TempMajorVer, TempMinorVer) and
     (TempMajorVer >= 4) then
    TempOS := 'Windows NT'
   else
    TempOS := 'Windows';

  TempOS := TempOS + ' ' + inttostr(TempMajorVer) + '.' + inttostr(TempMinorVer);

  if ProcessUnderWow64(GetCurrentProcess(), @Temp64bit) and Temp64bit then
    TempOS := TempOS + '; WOW64';

  if (GlobalCEFApp <> nil) then
    TempChromiumVersion := GlobalCEFApp.ChromeVersion
   else
    TempChromiumVersion := inttostr(CEF_CHROMEELF_VERSION_MAJOR)   + '.' +
                           inttostr(CEF_CHROMEELF_VERSION_MINOR)   + '.' +
                           inttostr(CEF_CHROMEELF_VERSION_RELEASE) + '.' +
                           inttostr(CEF_CHROMEELF_VERSION_BUILD);

  Result  := 'Mozilla/5.0' + ' (' + TempOS + ') ' +
             'AppleWebKit/537.36 (KHTML, like Gecko) ' +
             'Chrome/' + TempChromiumVersion + ' Safari/537.36';
end;

{$IFDEF DELPHI14_UP}
function TouchPointToPoint(aHandle : HWND; const TouchPoint: TTouchInput): TPoint;
begin
  Result := Point(TouchPoint.X div 100, TouchPoint.Y div 100);
  PhysicalToLogicalPoint(aHandle, Result);
end;

function GetDigitizerStatus(var aDigitizerStatus : TDigitizerStatus; aDPI : cardinal) : boolean;
var
  TempStatus : integer;
begin
  {$IFDEF DELPHI26_UP}
  if (aDPI > 0) then
    TempStatus := GetSystemMetricsForDpi(SM_DIGITIZER, aDPI)
   else
  {$ENDIF}
    TempStatus := GetSystemMetrics(SM_DIGITIZER);

  aDigitizerStatus.IntegratedTouch := ((TempStatus and NID_INTEGRATED_TOUCH) <> 0);
  aDigitizerStatus.ExternalTouch   := ((TempStatus and NID_EXTERNAL_TOUCH)   <> 0);
  aDigitizerStatus.IntegratedPen   := ((TempStatus and NID_INTEGRATED_PEN)   <> 0);
  aDigitizerStatus.ExternalPen     := ((TempStatus and NID_EXTERNAL_PEN)     <> 0);
  aDigitizerStatus.MultiInput      := ((TempStatus and NID_MULTI_INPUT)      <> 0);
  aDigitizerStatus.Ready           := ((TempStatus and NID_READY)            <> 0);

  Result := (TempStatus <> 0);
end;

function HasTouchOrPen(aDPI : cardinal) : boolean;
var
  TempStatus : TDigitizerStatus;
begin
  Result := GetDigitizerStatus(TempStatus, aDPI);
end;
{$ENDIF}
{$ENDIF}

function DeviceToLogical(aValue : integer; const aDeviceScaleFactor : double) : integer;
begin
  Result := floor(aValue / aDeviceScaleFactor);
end;

function DeviceToLogical(aValue : single; const aDeviceScaleFactor : double) : single;
begin
  Result := aValue / aDeviceScaleFactor;
end;

procedure DeviceToLogical(var aEvent : TCEFMouseEvent; const aDeviceScaleFactor : double);
begin
  aEvent.x := DeviceToLogical(aEvent.x, aDeviceScaleFactor);
  aEvent.y := DeviceToLogical(aEvent.y, aDeviceScaleFactor);
end;

procedure DeviceToLogical(var aEvent : TCefTouchEvent; const aDeviceScaleFactor : double);
begin
  aEvent.x := DeviceToLogical(aEvent.x, aDeviceScaleFactor);
  aEvent.y := DeviceToLogical(aEvent.y, aDeviceScaleFactor);
end;

procedure DeviceToLogical(var aPoint : TPoint; const aDeviceScaleFactor : double);
begin
  aPoint.x := DeviceToLogical(aPoint.x, aDeviceScaleFactor);
  aPoint.y := DeviceToLogical(aPoint.y, aDeviceScaleFactor);
end;

function LogicalToDevice(aValue : integer; const aDeviceScaleFactor : double) : integer;
begin
  Result := floor(aValue * aDeviceScaleFactor);
end;

procedure LogicalToDevice(var aRect : TCEFRect; const aDeviceScaleFactor : double);
begin
  aRect.x      := LogicalToDevice(aRect.x,      aDeviceScaleFactor);
  aRect.y      := LogicalToDevice(aRect.y,      aDeviceScaleFactor);
  aRect.width  := LogicalToDevice(aRect.width,  aDeviceScaleFactor);
  aRect.height := LogicalToDevice(aRect.height, aDeviceScaleFactor);
end;

function GetScreenDPI : integer;
{$IFDEF MSWINDOWS}
var
  TempDC : HDC;
{$ELSE}
{$IFDEF FMX}
var
  TempService: IFMXScreenService;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  TempDC := GetDC(0);
  Result := GetDeviceCaps(TempDC, LOGPIXELSX);
  ReleaseDC(0, TempDC);
  {$ENDIF}

  {$IFDEF LINUX}
    {$IFDEF FPC}
      if (Application                  <> nil) and
         (Application.MainForm         <> nil) and
         (Application.MainForm.Monitor <> nil) then
        Result := Application.MainForm.Monitor.PixelsPerInch
       else
        if (screen <> nil) then
          begin
            if (WidgetSet <> nil) and (screen.PrimaryMonitor <> nil) then
              Result := screen.PrimaryMonitor.PixelsPerInch
             else
              Result := screen.PixelsPerInch;
          end
         else
          Result := USER_DEFAULT_SCREEN_DPI;
    {$ELSE}
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, TempService) then
      Result := round(TempService.GetScreenScale * USER_DEFAULT_SCREEN_DPI)
     else
      begin
        Result := round(gdk_screen_get_resolution(gdk_screen_get_default));
        if (Result < 0) then
          Result := round(gdk_screen_width / (gdk_screen_width_mm / 25.4));
      end;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF MACOSX}
    {$IFDEF FPC}
    Result := round(NSScreen.mainScreen.backingScaleFactor * USER_DEFAULT_SCREEN_DPI);
    {$ELSE}
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, TempService) then
      Result := round(TempService.GetScreenScale * USER_DEFAULT_SCREEN_DPI)
     else
      Result := round(TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).backingScaleFactor * USER_DEFAULT_SCREEN_DPI);
    {$ENDIF}
  {$ENDIF}
end;

function GetDeviceScaleFactor : single;
{$IFDEF MACOSX}{$IFDEF FMX}
var
  TempService: IFMXScreenService;
{$ENDIF}{$ENDIF}
begin
  {$IFDEF MACOSX}
    {$IFDEF FPC}
    Result := NSScreen.mainScreen.backingScaleFactor;
    {$ELSE}
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, TempService) then
      Result := TempService.GetScreenScale
     else
      Result := TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).backingScaleFactor;
    {$ENDIF}
  {$ELSE}
  Result := GetScreenDPI / USER_DEFAULT_SCREEN_DPI;
  {$ENDIF}
end;

function DeleteDirContents(const aDirectory : string; const aExcludeFiles : TStringList) : boolean;
var
  TempRec  : TSearchRec;
  TempPath : string;
  TempIdx  : integer;
begin
  Result := True;

  try
    if (length(aDirectory) > 0) and
       DirectoryExists(aDirectory) and
       (FindFirst(aDirectory + '\*', faAnyFile, TempRec) = 0) then
      try
        repeat
          TempPath := aDirectory + PathDelim + TempRec.Name;

          if ((TempRec.Attr and faDirectory) <> 0) then
            begin
              if (TempRec.Name <> '.') and (TempRec.Name <> '..') then
                begin
                  if DeleteDirContents(TempPath, aExcludeFiles) then
                    Result := RemoveDir(TempPath) and Result
                   else
                    Result := False;
                end;
            end
           else
            if (aExcludeFiles <> nil) then
              begin
                TempIdx := aExcludeFiles.IndexOf(TempRec.Name);
                Result  := ((TempIdx >= 0) or
                            ((TempIdx < 0) and DeleteFile(TempPath))) and
                           Result;
              end
             else
              Result := DeleteFile(TempPath) and Result;

        until (FindNext(TempRec) <> 0) or not(Result);
      finally
        FindClose(TempRec);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('DeleteDirContents', e) then raise;
  end;
end;

function DeleteFileList(const aFileList : TStringList) : boolean;
var
  i, TempCount : integer;
begin
  Result := False;

  try
    if (aFileList <> nil) then
      begin
        i         := 0;
        TempCount := 0;

        while (i < aFileList.Count) do
          begin
            if FileExists(aFileList[i]) and DeleteFile(aFileList[i]) then inc(TempCount);
            inc(i);
          end;

        Result := (aFileList.Count = TempCount);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('DeleteFileList', e) then raise;
  end;
end;

function MoveFileList(const aFileList : TStringList; const aSrcDirectory, aDstDirectory : string) : boolean;
var
  i, TempCount : integer;
  TempSrcPath, TempDstPath : string;
begin
  Result := False;

  try
    if (aFileList <> nil) and
       (length(aSrcDirectory) > 0) and
       (length(aDstDirectory) > 0) and
       DirectoryExists(aSrcDirectory) and
       (DirectoryExists(aDstDirectory) or CreateDir(aDstDirectory)) then
      begin
        i         := 0;
        TempCount := 0;

        while (i < aFileList.Count) do
          begin
            TempSrcPath := IncludeTrailingPathDelimiter(aSrcDirectory) + aFileList[i];
            TempDstPath := IncludeTrailingPathDelimiter(aDstDirectory) + aFileList[i];

            if FileExists(TempSrcPath) and RenameFile(TempSrcPath, TempDstPath) then inc(TempCount);

            inc(i);
          end;

        Result := (aFileList.Count = TempCount);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('MoveFileList', e) then raise;
  end;
end;

function CefGetDataURI(const aString, aMimeType : ustring) : ustring;
var
  TempUTF : AnsiString;
begin
  TempUTF := UTF8Encode(aString);

  if (length(TempUTF) > 0) then
    Result := CefGetDataURI(@TempUTF[1], length(TempUTF), aMimeType, 'utf-8')
   else
    Result := '';
end;

function CefGetDataURI(aData : pointer; aSize : integer; const aMimeType, aCharset : ustring) : ustring;
begin
  Result := 'data:' + aMimeType;

  if (length(aCharset) > 0) then Result := Result + ';charset=' + aCharset;

  Result := Result + ';base64,' + CefURIEncode(CefBase64Encode(aData, aSize), false);
end;

function ValidCefWindowHandle(aHandle : TCefWindowHandle) : boolean;
begin
  {$IFDEF MACOS}
  Result := (aHandle <> nil);
  {$ELSE}
  Result := (aHandle <> 0);
  {$ENDIF}
end;

procedure InitializeWindowHandle(var aHandle : TCefWindowHandle);
begin
  {$IFDEF MACOS}
  aHandle := nil;
  {$ELSE}
  aHandle := 0;
  {$ENDIF}
end;

function GetCommandLineSwitchValue(const aKey : string; var aValue : ustring) : boolean;
var
  i, TempLen : integer;
  TempKey : string;
begin
  Result  := False;
  TempKey := '--' + aKey + '=';
  TempLen := length(TempKey);
  i       := paramCount;

  while (i >= 1) do
    if (CompareText(copy(paramstr(i), 1, TempLen), TempKey) = 0) then
      begin
        {$IFDEF FPC}
        aValue := UTF8Decode(copy(paramstr(i), succ(TempLen), length(paramstr(i))));
        {$ELSE}
        aValue := copy(paramstr(i), succ(TempLen), length(paramstr(i)));
        {$ENDIF}
        Result := True;
        break;
      end
     else
      dec(i);
end;

end.
