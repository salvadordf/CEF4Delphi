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
      WinApi.Windows, WinApi.ActiveX, Winapi.ShellApi, System.Win.Registry,
    {$ELSE}
      {$IFDEF MACOSX}Macapi.Foundation, FMX.Helpers.Mac, Macapi.AppKit,{$ENDIF}
    {$ENDIF}
    {$IFDEF FMX}FMX.Types, FMX.Platform,{$ENDIF} System.Types, System.IOUtils,
    System.Classes, System.SysUtils, System.UITypes, System.Math,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows, ActiveX, ShellApi, Registry,{$ENDIF}
    {$IFDEF DELPHI14_UP}Types, IOUtils,{$ENDIF} Classes, SysUtils, Math,
    {$IFDEF FPC}LCLType, LazFileUtils,{$IFNDEF MSWINDOWS}InterfaceBase, Forms,{$ENDIF}{$ENDIF}
    {$IFDEF LINUX}{$IFDEF FPC}
      ctypes, keysym, xf86keysym, x, xlib,
      {$IFDEF LCLGTK2}gtk2, glib2, gdk2, gtk2proc, gtk2int, Gtk2Def, gdk2x, Gtk2Extra,{$ENDIF}
      {$IFDEF LCLGTK3}LazGdk3, LazGtk3, LazGLib2, gtk3widgets,{$ENDIF}
    {$ENDIF}{$ENDIF}
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFLibFunctions, uCEFResourceHandler,
  {$IFDEF LINUX}{$IFDEF FPC}uCEFLinuxFunctions,{$ENDIF}{$ENDIF} uCEFConstants;

const
  Kernel32DLL = 'kernel32.dll';
  SHLWAPIDLL  = 'shlwapi.dll';
  NTDLL       = 'ntdll.dll';
  User32DLL   = 'User32.dll';
  Netapi32DLL = 'Netapi32.dll';


/// <summary>Return the alpha byte from a cef_color_t value.</summary>
function CefColorGetA(color: TCefColor): Byte;
/// <summary>Return the red byte from a cef_color_t value.</summary>
function CefColorGetR(color: TCefColor): byte;
/// <summary>Return the green byte from a cef_color_t value.</summary>
function CefColorGetG(color: TCefColor): Byte;
/// <summary>Return the blue byte from a cef_color_t value.</summary>
function CefColorGetB(color: TCefColor): Byte;
/// <summary>Return an cef_color_t value with the specified byte component values.</summary>
function CefColorSetARGB(a, r, g, b: Byte): TCefColor;
/// <summary>Return an int64_t value with the specified low and high int32_t component values.</summary>
function CefInt64Set(int32_low, int32_high: Integer): Int64;
/// <summary>Return the low int32_t value from an int64_t value.</summary>
function CefInt64GetLow(const int64_val: Int64): Integer;
/// <summary>Return the high int32_t value from an int64_t value.</summary>
function CefInt64GetHigh(const int64_val: Int64): Integer;

function CefGetObject(ptr: Pointer): TObject; {$IFNDEF CEF4DELHI_ALLOC_DEBUG}{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}{$ENDIF}
function CefGetData(const i: ICefBaseRefCounted): Pointer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

function CefStringAlloc(const str: ustring): TCefString;
function CefStringClearAndGet(str: PCefString): ustring;

/// <summary>Converts ustring to TCefString.</summary>
function  CefString(const str: ustring): TCefString; overload;
/// <summary>Converts PCefString to ustring.</summary>
function  CefString(const str: PCefString): ustring; overload;
function  CefUserFreeString(const str: ustring): PCefStringUserFree;
procedure CefStringFree(const str: PCefString);
function  CefStringFreeAndGet(const str: PCefStringUserFree): ustring;
procedure CefStringSet(const str: PCefString; const value: ustring); overload;
procedure CefStringSet(const aDstStr, aSrcStr: TCefString); overload;
procedure CefStringInitialize(const aCefString : PCefString); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

/// <summary>
/// Register a new V8 extension with the specified JavaScript extension code and
/// handler. Functions implemented by the handler are prototyped using the
/// keyword 'native'. The calling of a native function is restricted to the
/// scope in which the prototype of the native function is defined. This
/// function may only be called on the render process main thread.
///
/// Example JavaScript extension code: <pre>
///   // create the 'example' global object if it doesn't already exist.
///   if (!example)
///     example = {};
///   // create the 'example.test' global object if it doesn't already exist.
///   if (!example.test)
///     example.test = {};
///   (function() {
///     // Define the function 'example.test.myfunction'.
///     example.test.myfunction = function() {
///       // Call CefV8Handler::Execute() with the function name 'MyFunction'
///       // and no arguments.
///       native function MyFunction();
///       return MyFunction();
///     };
///     // Define the getter function for parameter 'example.test.myparam'.
///     example.test.__defineGetter__('myparam', function() {
///       // Call CefV8Handler::Execute() with the function name 'GetMyParam'
///       // and no arguments.
///       native function GetMyParam();
///       return GetMyParam();
///     });
///     // Define the setter function for parameter 'example.test.myparam'.
///     example.test.__defineSetter__('myparam', function(b) {
///       // Call CefV8Handler::Execute() with the function name 'SetMyParam'
///       // and a single argument.
///       native function SetMyParam();
///       if(b) SetMyParam(b);
///     });
///
///     // Extension definitions can also contain normal JavaScript variables
///     // and functions.
///     var myint = 0;
///     example.test.increment = function() {
///       myint += 1;
///       return myint;
///     };
///   })();
/// </pre>
///
/// Example usage in the page: <pre>
///   // Call the function.
///   example.test.myfunction();
///   // Set the parameter.
///   example.test.myparam = value;
///   // Get the parameter.
///   value = example.test.myparam;
///   // Call another function.
///   example.test.increment();
/// </pre>
/// </summary>
function CefRegisterExtension(const name, code: ustring; const Handler: ICefv8Handler): Boolean;
/// <summary>
/// Post a task for execution on the specified thread. Equivalent to using
/// TCefTaskRunnerRef.GetForThread(threadId).PostTask(task).
/// </summary>
function CefPostTask(aThreadId : TCefThreadId; const aTask: ICefTask) : boolean;
/// <summary>
/// Post a task for delayed execution on the specified thread. Equivalent to
/// using TCefTaskRunnerRef.GetForThread(threadId).PostDelayedTask(task,
/// delay_ms).
/// </summary>
function CefPostDelayedTask(aThreadId : TCefThreadId; const aTask : ICefTask; aDelayMs : Int64) : boolean;
/// <summary>
/// Returns true (1) if called on the specified thread. Equivalent to using
/// TCefTaskRunnerRef.GetForThread(threadId).BelongsToCurrentThread().
/// </summary>
function CefCurrentlyOn(aThreadId : TCefThreadId) : boolean;

{$IFDEF MSWINDOWS}
/// <summary>
/// Converts a TCefTime value to TSystemTime.
/// </summary>
function CefTimeToSystemTime(const dt: TCefTime): TSystemTime;
/// <summary>
/// Converts a TSystemTime value to TCefTime.
/// </summary>
function SystemTimeToCefTime(const dt: TSystemTime): TCefTime;
{$ELSE}
  {$IFDEF LINUX}
    {$IFDEF FPC}
    /// <summary>
    /// Converts a TCefTime value to TSystemTime.
    /// </summary>
    function CefTimeToSystemTime(const dt: TCefTime): TSystemTime;
    /// <summary>
    /// Converts a TSystemTime value to TCefTime.
    /// </summary>
    function SystemTimeToCefTime(const dt: TSystemTime): TCefTime;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
/// <summary>
/// Returns a new TCefTime with a valid time in case the original has errors.
/// </summary>
function FixCefTime(const dt : TCefTime): TCefTime;
/// <summary>
/// Converts a TCefTime value to TDateTime.
/// </summary>
function CefTimeToDateTime(const dt: TCefTime): TDateTime;
/// <summary>
/// Converts a TDateTime value to TCefTime.
/// </summary>
function DateTimeToCefTime(dt: TDateTime): TCefTime;
/// <summary>
/// Converts a TDateTime value to TCefBaseTime.
/// </summary>
function DateTimeToCefBaseTime(dt: TDateTime): TCefBaseTime;
/// <summary>
/// Converts TCefTime to a double which is the number of seconds since
/// epoch (Jan 1, 1970). Webkit uses this format to represent time. A value of 0
/// means "not initialized".
/// </summary>
/// <remarks>
/// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_time.h">CEF source file: /include/internal/cef_time.h (cef_time_to_doublet)</see></para>
/// </remarks>
function CefTimeToDouble(const dt: TCefTime): double;
/// <summary>
/// Converts TCefTime from a double which is the number of seconds since
/// epoch (Jan 1, 1970). Webkit uses this format to represent time. A value of 0
/// means "not initialized".
/// </summary>
/// <remarks>
/// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_time.h">CEF source file: /include/internal/cef_time.h (cef_time_from_doublet)</see></para>
/// </remarks>
function DoubleToCefTime(const dt: double): TCefTime;
/// <summary>
/// Converts cef_time_t to time_t. time_t is almost always an integral value holding the number of seconds (not counting leap seconds) since 00:00, Jan 1 1970 UTC, corresponding to POSIX time.
/// </summary>
/// <remarks>
/// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_time.h">CEF source file: /include/internal/cef_time.h (cef_time_to_timet)</see></para>
/// </remarks>
function CefTimeToUnixTime(const dt: TCefTime): int64;
/// <summary>
/// Converts cef_time_t from time_t. time_t is almost always an integral value holding the number of seconds (not counting leap seconds) since 00:00, Jan 1 1970 UTC, corresponding to POSIX time.
/// </summary>
/// <remarks>
/// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_time.h">CEF source file: /include/internal/cef_time.h (cef_time_from_timet)</see></para>
/// </remarks>
function UnixTimeToCefTime(const dt: int64): TCefTime;
/// <summary>
/// Retrieve the current system time in a TCefTime type.
/// </summary>
/// <remarks>
/// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_time.h">CEF source file: /include/internal/cef_time.h (cef_time_now)</see></para>
/// </remarks>
function CefTimeNow: TCefTime;
/// <summary>
/// Retrieve the current system time in a double type.
/// </summary>
/// <remarks>
/// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_time.h">CEF source file: /include/internal/cef_time.h (cef_time_now)</see></para>
/// </remarks>
function DoubleTimeNow: double;
/// <summary>
/// Retrieve the delta in milliseconds between two time values.
/// </summary>
/// <remarks>
/// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_time.h">CEF source file: /include/internal/cef_time.h (cef_time_delta)</see></para>
/// </remarks>
function CefTimeDelta(const cef_time1, cef_time2: TCefTime): int64;
/// <summary>
/// Retrieve the current system time in a TCefBaseTime type.
/// </summary>
/// <remarks>
/// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_time.h">CEF source file: /include/internal/cef_time.h (cef_basetime_now)</see></para>
/// </remarks>
function CefBaseTimeNow: TCefBaseTime;
/// <summary>
/// Converts TCefTime to TCefBaseTime.
/// </summary>
/// <remarks>
/// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_time.h">CEF source file: /include/internal/cef_time.h (cef_time_to_basetime)</see></para>
/// </remarks>
function CetTimeToCefBaseTime(const ct: TCefTime) : TCefBaseTime;
/// <summary>
/// Converts TCefBaseTime to TCefTime.
/// </summary>
/// <remarks>
/// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_time.h">CEF source file: /include/internal/cef_time.h (cef_time_from_basetime)</see></para>
/// </remarks>
function CetTimeFromCefBaseTime(const cbt: TCefBaseTime) : TCefTime;
/// <summary>
/// Converts TCefBaseTime to TDateTime.
/// </summary>
/// <remarks>
/// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_time.h">CEF source file: /include/internal/cef_time.h (cef_time_from_basetime)</see></para>
/// </remarks>
function CefBaseTimeToDateTime(const cbt: TCefBaseTime) : TDateTime;
/// <summary>
/// Returns the time interval between now and from_ in milliseconds.
/// This funcion should only be used by TCEFTimerWorkScheduler.
/// </summary>
function GetTimeIntervalMilliseconds(const from_: TCefTime): integer;
/// <summary>
/// Initialize a TCefTime variable.
/// </summary>
procedure InitializeCefTime(var aTime : TCefTime);

function cef_string_wide_copy(const src: PWideChar; src_len: NativeUInt;  output: PCefStringWide): Integer;
function cef_string_utf8_copy(const src: PAnsiChar; src_len: NativeUInt; output: PCefStringUtf8): Integer;
function cef_string_utf16_copy(const src: PChar16; src_len: NativeUInt; output: PCefStringUtf16): Integer;
function cef_string_copy(const src: PCefChar; src_len: NativeUInt; output: PCefString): Integer;

{$IFDEF MSWINDOWS}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aRect : TRect; const aWindowName : ustring = ''; aExStyle : DWORD = 0); deprecated;
procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring = ''; aExStyle : DWORD = 0); deprecated;
procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring = ''; aExStyle : DWORD = 0); deprecated;
{$ENDIF}

{$IFDEF MACOSX}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aRect : TRect; const aWindowName : ustring = ''; aHidden : boolean = False); deprecated;
procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring = ''; aHidden : boolean = False); deprecated;
procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring = ''; aHidden : boolean = False); deprecated;
{$ENDIF}

{$IFDEF LINUX}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aRect : TRect; const aWindowName : ustring = ''); deprecated;
procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring = ''); deprecated;
procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring = ''); deprecated;
{$ENDIF}

{$IFDEF ANDROID}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aRect : TRect; const aWindowName : ustring = ''; aExStyle : DWORD = 0); deprecated;
procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring = ''; aExStyle : DWORD = 0); deprecated;
procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring = ''; aExStyle : DWORD = 0); deprecated;
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
function NetServerGetInfo(servername: LPWSTR; level: DWORD; out bufptr: Pointer): DWORD; stdcall; external Netapi32DLL;
function NetApiBufferFree(Buffer: Pointer): DWORD; stdcall; external Netapi32DLL;

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

/// <summary>
/// Returns true if aPath is a relative path.
/// </summary>
/// <remarks>
/// <para><see href="https://learn.microsoft.com/en-us/windows/win32/api/shlwapi/nf-shlwapi-pathisrelativew">See the PathIsRelativeW article.</see></para>
/// </remarks>
function CustomPathIsRelative(const aPath : string) : boolean;
/// <summary>
/// Simplifies a path by removing navigation elements such as "." and ".." to produce a direct, well-formed path.
/// </summary>
/// <remarks>
/// <para><see href="https://learn.microsoft.com/en-us/windows/win32/api/shlwapi/nf-shlwapi-pathcanonicalizew">See the PathCanonicalizeW article.</see></para>
/// </remarks>
function CustomPathCanonicalize(const aOriginalPath : string; var aCanonicalPath : string) : boolean;
/// <summary>
/// Returns the absolute path version of aPath.
/// </summary>
function CustomAbsolutePath(const aPath : string; aMustExist : boolean = False) : string;
/// <summary>
/// Tests aPath to determine if it conforms to a valid URL format.
/// </summary>
/// <remarks>
/// <para><see href="https://learn.microsoft.com/en-us/windows/win32/api/shlwapi/nf-shlwapi-pathisurlw">See the PathIsURLW article.</see></para>
/// </remarks>
function CustomPathIsURL(const aPath : string) : boolean;
/// <summary>
/// Determines if aPath is a valid Universal Naming Convention (UNC) path, as opposed to a path based on a drive letter.
/// </summary>
/// <remarks>
/// <para><see href="https://learn.microsoft.com/en-us/windows/win32/api/shlwapi/nf-shlwapi-pathisuncw">See the PathIsUNCW article.</see></para>
/// </remarks>
function CustomPathIsUNC(const aPath : string) : boolean;
/// <summary>
/// Retrieves the fully qualified path for the current module.
/// </summary>
/// <remarks>
/// <para><see href="https://learn.microsoft.com/en-us/windows/win32/api/libloaderapi/nf-libloaderapi-getmodulefilenamew">See the GetModuleFileNameW article.</see></para>
/// </remarks>
function GetModulePath : string;
/// <summary>
/// Returns true (1) if the certificate status represents an error.
/// </summary>
function CefIsCertStatusError(Status : TCefCertStatus) : boolean;
/// <summary>
/// Crash reporting is configured using an INI-style config file named
/// "crash_reporter.cfg". On Windows and Linux this file must be placed next to
/// the main application executable. On macOS this file must be placed in the
/// top-level app bundle Resources directory (e.g.
/// "<appname>.app/Contents/Resources"). File contents are as follows:
///
/// <pre>
///  # Comments start with a hash character and must be on their own line.
///
///  [Config]
///  ProductName=<Value of the "prod" crash key; defaults to "cef">
///  ProductVersion=<Value of the "ver" crash key; defaults to the CEF version>
///  AppName=<Windows only; App-specific folder name component for storing crash
///           information; default to "CEF">
///  ExternalHandler=<Windows only; Name of the external handler exe to use
///                   instead of re-launching the main exe; default to empty>
///  BrowserCrashForwardingEnabled=<macOS only; True if browser process crashes
///                                 should be forwarded to the system crash
///                                 reporter; default to false>
///  ServerURL=<crash server URL; default to empty>
///  RateLimitEnabled=<True if uploads should be rate limited; default to true>
///  MaxUploadsPerDay=<Max uploads per 24 hours, used if rate limit is enabled;
///                    default to 5>
///  MaxDatabaseSizeInMb=<Total crash report disk usage greater than this value
///                       will cause older reports to be deleted; default to 20>
///  MaxDatabaseAgeInDays=<Crash reports older than this value will be deleted;
///                        default to 5>
///
///  [CrashKeys]
///  my_key1=<small|medium|large>
///  my_key2=<small|medium|large>
/// </pre>
///
/// <b>Config section:</b>
///
/// If "ProductName" and/or "ProductVersion" are set then the specified values
/// will be included in the crash dump metadata. On macOS if these values are
/// set to NULL then they will be retrieved from the Info.plist file using the
/// "CFBundleName" and "CFBundleShortVersionString" keys respectively.
///
/// If "AppName" is set on Windows then crash report information (metrics,
/// database and dumps) will be stored locally on disk under the
/// "C:\Users\[CurrentUser]\AppData\Local\[AppName]\User Data" folder. On other
/// platforms the cef_settings_t.root_cache_path value will be used.
///
/// If "ExternalHandler" is set on Windows then the specified exe will be
/// launched as the crashpad-handler instead of re-launching the main process
/// exe. The value can be an absolute path or a path relative to the main exe
/// directory. On Linux the cef_settings_t.browser_subprocess_path value will be
/// used. On macOS the existing subprocess app bundle will be used.
///
/// If "BrowserCrashForwardingEnabled" is set to true (1) on macOS then browser
/// process crashes will be forwarded to the system crash reporter. This results
/// in the crash UI dialog being displayed to the user and crash reports being
/// logged under "~/Library/Logs/DiagnosticReports". Forwarding of crash reports
/// from non-browser processes and Debug builds is always disabled.
///
/// If "ServerURL" is set then crashes will be uploaded as a multi-part POST
/// request to the specified URL. Otherwise, reports will only be stored locally
/// on disk.
///
/// If "RateLimitEnabled" is set to true (1) then crash report uploads will be
/// rate limited as follows:
///  1. If "MaxUploadsPerDay" is set to a positive value then at most the
///     specified number of crashes will be uploaded in each 24 hour period.
///  2. If crash upload fails due to a network or server error then an
///     incremental backoff delay up to a maximum of 24 hours will be applied
///     for retries.
///  3. If a backoff delay is applied and "MaxUploadsPerDay" is > 1 then the
///     "MaxUploadsPerDay" value will be reduced to 1 until the client is
///     restarted. This helps to avoid an upload flood when the network or
///     server error is resolved.
/// Rate limiting is not supported on Linux.
///
/// If "MaxDatabaseSizeInMb" is set to a positive value then crash report
/// storage on disk will be limited to that size in megabytes. For example, on
/// Windows each dump is about 600KB so a "MaxDatabaseSizeInMb" value of 20
/// equates to about 34 crash reports stored on disk. Not supported on Linux.
///
/// If "MaxDatabaseAgeInDays" is set to a positive value then crash reports
/// older than the specified age in days will be deleted. Not supported on
/// Linux.
///
/// <b>CrashKeys section:</b>
///
/// A maximum of 26 crash keys of each size can be specified for use by the
/// application. Crash key values will be truncated based on the specified size
/// (small = 64 bytes, medium = 256 bytes, large = 1024 bytes). The value of
/// crash keys can be set from any thread or process using the
/// CefSetCrashKeyValue function. These key/value pairs will be sent to the
/// crash server along with the crash dump file.
/// </summary>
function  CefCrashReportingEnabled : boolean;
/// <summary>
/// Sets or clears a specific key-value pair from the crash metadata.
/// </summary>
procedure CefSetCrashKeyValue(const aKey, aValue : ustring);
/// <summary>
/// Add a log message. See the LogSeverity defines for supported |severity|
/// values.
/// </summary>
/// <remarks>
/// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/base/cef_logging.h">CEF source file: /include/base/cef_logging.h (cef_log)</see></para>
/// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/base/cef_logging.h">CEF source file: /include/base/cef_logging.h (LogSeverity)</see></para>
/// </remarks>
procedure CefLog(const aFile : string; aLine, aSeverity : integer; const aMessage : string);
procedure CefDebugLog(const aMessage : string; aSeverity : integer = CEF_LOG_SEVERITY_ERROR);
procedure CefKeyEventLog(const aEvent : TCefKeyEvent);
procedure CefMouseEventLog(const aEvent : TCefMouseEvent);
procedure OutputDebugMessage(const aMessage : string);
function  CustomExceptionHandler(const aFunctionName : string; const aException : exception) : boolean;
/// <summary>
/// Gets the current log verbose level (LogSeverity).
/// </summary>
/// <remarks>
/// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/base/cef_logging.h">CEF source file: /include/base/cef_logging.h (cef_get_min_log_level)</see></para>
/// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/base/cef_logging.h">CEF source file: /include/base/cef_logging.h (LogSeverity)</see></para>
/// </remarks>
function CefGetMinLogLevel: integer;
/// <summary>
/// Gets the current vlog level for the given file.
/// </summary>
/// <remarks>
/// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/base/cef_logging.h">CEF source file: /include/base/cef_logging.h (cef_get_vlog_level)</see></para>
/// </remarks>
function CefGetVLogLevel(const file_start : string): integer;
/// <summary>
/// Gets the log severity name.
/// </summary>
/// <remarks>
/// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/base/cef_logging.h">CEF source file: /include/base/cef_logging.h (LogSeverity)</see></para>
/// </remarks>
function CefGetLogSeverityName(aSeverity: integer): ustring;
/// <summary>
/// Register a scheme handler factory with the global request context. An NULL
/// |DomainName| value for a standard scheme will cause the factory to match
/// all domain names. The |DomainName| value will be ignored for non-standard
/// schemes. If |SchemeName| is a built-in scheme and no handler is returned by
/// |factory| then the built-in scheme handler factory will be called. If
/// |SchemeName| is a custom scheme then you must also implement the
/// ICefApp.OnRegisterCustomSchemes function in all processes. This
/// function may be called multiple times to change or remove the factory that
/// matches the specified |SchemeName| and optional |DomainName|. Returns
/// false (0) if an error occurs. This function may be called on any thread in
/// the browser process. Using this function is equivalent to calling cef_reques
/// t_context_t::cef_request_context_get_global_context()->register_scheme_handl
/// er_factory().
/// </summary>
function CefRegisterSchemeHandlerFactory(const SchemeName, DomainName : ustring; const handler: TCefResourceHandlerClass = nil): Boolean;
/// <summary>
/// Clear all scheme handler factories registered with the global request
/// context. Returns false (0) on error. This function may be called on any
/// thread in the browser process. Using this function is equivalent to calling
/// cef_request_context_t::cef_request_context_get_global_context()->clear_schem
/// e_handler_factories().
/// </summary>
function CefClearSchemeHandlerFactories : boolean;
/// <summary>
/// <para>Add an entry to the cross-origin access whitelist.</para>
/// <para>The same-origin policy restricts how scripts hosted from different origins
/// (scheme + domain + port) can communicate. By default, scripts can only
/// access resources with the same origin. Scripts hosted on the HTTP and HTTPS
/// schemes (but no other schemes) can use the "Access-Control-Allow-Origin"
/// header to allow cross-origin requests. For example,
/// https://source.example.com can make XMLHttpRequest requests on
/// http://target.example.com if the http://target.example.com request returns
/// an "Access-Control-Allow-Origin: https://source.example.com" response
/// header.</para>
/// <para>Scripts in separate frames or iframes and hosted from the same protocol and
/// domain suffix can execute cross-origin JavaScript if both pages set the
/// document.domain value to the same domain suffix. For example,
/// scheme://foo.example.com and scheme://bar.example.com can communicate using
/// JavaScript if both domains set document.domain="example.com".</para>
/// <para>This function is used to allow access to origins that would otherwise
/// violate the same-origin policy. Scripts hosted underneath the fully
/// qualified |source_origin| URL (like http://www.example.com) will be allowed
/// access to all resources hosted on the specified |target_protocol| and
/// |target_domain|. If |target_domain| is non-NULL and
/// |allow_target_subdomains| is false (0) only exact domain matches will be
/// allowed. If |target_domain| contains a top- level domain component (like
/// "example.com") and |allow_target_subdomains| is true (1) sub-domain matches
/// will be allowed. If |target_domain| is NULL and |allow_target_subdomains| if
/// true (1) all domains and IP addresses will be allowed.</para>
/// <para>This function cannot be used to bypass the restrictions on local or display
/// isolated schemes. See the comments on CefRegisterCustomScheme for more
/// information.</para>
/// <para>This function may be called on any thread. Returns false (0) if
/// |source_origin| is invalid or the whitelist cannot be accessed.</para>
/// </summary>
function CefAddCrossOriginWhitelistEntry(const SourceOrigin, TargetProtocol, TargetDomain: ustring; AllowTargetSubdomains: Boolean): Boolean;
/// <summary>
/// Remove an entry from the cross-origin access whitelist. Returns false (0) if
/// |source_origin| is invalid or the whitelist cannot be accessed.
/// </summary>
function CefRemoveCrossOriginWhitelistEntry(const SourceOrigin, TargetProtocol, TargetDomain: ustring; AllowTargetSubdomains: Boolean): Boolean;
/// <summary>
/// Remove all entries from the cross-origin access whitelist. Returns false (0)
/// if the whitelist cannot be accessed.
/// </summary>
function CefClearCrossOriginWhitelist: Boolean;

procedure UInt64ToFileVersionInfo(const aVersion : uint64; var aVersionInfo : TFileVersionInfo);
{$IFDEF MSWINDOWS}
function  GetExtendedFileVersion(const aFileName : ustring) : uint64;
function  GetDLLVersion(const aDLLFile : ustring; var aVersionInfo : TFileVersionInfo) : boolean;
procedure OutputLastErrorMessage;
function  GetRegistryWindowsVersion(var aMajor, aMinor: cardinal) : boolean;
function  GetRealWindowsVersion(var aMajor, aMinor: cardinal) : boolean;
function  CheckRealWindowsVersion(aMajor, aMinor: cardinal) : boolean;
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

/// <summary>
/// Combines specified |base_url| and |relative_url| into a ustring.
/// </summary>
function  CefResolveUrl(const base_url, relative_url: ustring): ustring;
/// <summary>
/// Parse the specified |url| into its component parts. Returns false (0) if the
/// URL is invalid.
/// </summary>
function  CefParseUrl(const url: ustring; var parts: TUrlParts): Boolean;
/// <summary>
/// Creates a URL from the specified |parts|, which must contain a non-NULL spec
/// or a non-NULL host and path (at a minimum), but not both.
/// </summary>
function  CefCreateUrl(var parts: TUrlParts): ustring;
/// <summary>
/// This is a convenience function for formatting a URL in a concise and human-
/// friendly way to help users make security-related decisions (or in other
/// circumstances when people need to distinguish sites, origins, or otherwise-
/// simplified URLs from each other). Internationalized domain names (IDN) may
/// be presented in Unicode if the conversion is considered safe. The returned
/// value will (a) omit the path for standard schemes, excepting file and
/// filesystem, and (b) omit the port if it is the default for the scheme. Do
/// not use this for URLs which will be parsed or sent to other applications.
/// </summary>
function  CefFormatUrlForSecurityDisplay(const originUrl: string): string;
/// <summary>
/// Returns the mime type for the specified file extension or an NULL string if
/// unknown.
/// </summary>
function  CefGetMimeType(const extension: ustring): ustring;
/// <summary>
/// Get the extensions associated with the given mime type. This should be
/// passed in lower case. There could be multiple extensions for a given mime
/// type, like "html,htm" for "text/html", or "txt,text,html,..." for "text/*".
/// Any existing elements in the provided vector will not be erased.
/// </summary>
procedure CefGetExtensionsForMimeType(const mimeType: ustring; var extensions: TStringList);
/// <summary>
/// Encodes |data| as a base64 string.
/// </summary>
function CefBase64Encode(const data: Pointer; dataSize: NativeUInt): ustring;
/// <summary>
/// Decodes the base64 encoded string |data|. The returned value will be NULL if
/// the decoding fails.
/// </summary>
function CefBase64Decode(const data: ustring): ICefBinaryValue;
/// <summary>
/// Escapes characters in |text| which are unsuitable for use as a query
/// parameter value. Everything except alphanumerics and -_.!~*'() will be
/// converted to "%XX". If |use_plus| is true (1) spaces will change to "+". The
/// result is basically the same as encodeURIComponent in Javacript.
/// </summary>
function CefUriEncode(const text: ustring; usePlus: Boolean): ustring;
/// <summary>
/// Unescapes |text| and returns the result. Unescaping consists of looking for
/// the exact pattern "%XX" where each X is a hex digit and converting to the
/// character with the numerical value of those digits (e.g. "i%20=%203%3b"
/// unescapes to "i = 3;"). If |convert_to_utf8| is true (1) this function will
/// attempt to interpret the initial decoded result as UTF-8. If the result is
/// convertable into UTF-8 it will be returned as converted. Otherwise the
/// initial decoded result will be returned.  The |unescape_rule| parameter
/// supports further customization the decoding process.
/// </summary>
function CefUriDecode(const text: ustring; convertToUtf8: Boolean; unescapeRule: TCefUriUnescapeRule): ustring;
/// <summary>
/// Retrieve the path associated with the specified |aPathKey|.
/// Can be called on any thread in the browser process.
/// </summary>
function CefGetPath(const aPathKey : TCefPathKey) : ustring;
/// <summary>
/// Returns true (1) if the application text direction is right-to-left.
/// </summary>
function CefIsRTL : boolean;
/// <summary>
/// Creates a directory and all parent directories if they don't already exist.
/// Returns true (1) on successful creation or if the directory already exists.
/// The directory is only readable by the current user. Calling this function on
/// the browser process UI or IO threads is not allowed.
/// </summary>
function CefCreateDirectory(const fullPath: ustring): Boolean;
/// <summary>
/// Get the temporary directory provided by the system.
/// WARNING: In general, you should use the temp directory variants below
/// instead of this function. Those variants will ensure that the proper
/// permissions are set so that other users on the system can't edit them while
/// they're open (which could lead to security issues).
/// </summary>
function CefGetTempDirectory(out tempDir: ustring): Boolean;
/// <summary>
/// Creates a new directory. On Windows if |prefix| is provided the new
/// directory name is in the format of "prefixyyyy". Returns true (1) on success
/// and sets |newTempPath| to the full path of the directory that was created.
/// The directory is only readable by the current user. Calling this function on
/// the browser process UI or IO threads is not allowed.
/// </summary>
function CefCreateNewTempDirectory(const prefix: ustring; out newTempPath: ustring): Boolean;
/// <summary>
/// Creates a directory within another directory. Extra characters will be
/// appended to |prefix| to ensure that the new directory does not have the same
/// name as an existing directory. Returns true (1) on success and sets
/// |newDir| to the full path of the directory that was created. The directory
/// is only readable by the current user. Calling this function on the browser
/// process UI or IO threads is not allowed.
/// </summary>
function CefCreateTempDirectoryInDirectory(const baseDir, prefix: ustring; out newDir: ustring): Boolean;
/// <summary>
/// Returns true (1) if the given path exists and is a directory. Calling this
/// function on the browser process UI or IO threads is not allowed.
/// </summary>
function CefDirectoryExists(const path: ustring): Boolean;
/// <summary>
/// Deletes the given path whether it's a file or a directory. If |path| is a
/// directory all contents will be deleted.  If |recursive| is true (1) any sub-
/// directories and their contents will also be deleted (equivalent to executing
/// "rm -rf", so use with caution). On POSIX environments if |path| is a
/// symbolic link then only the symlink will be deleted. Returns true (1) on
/// successful deletion or if |path| does not exist. Calling this function on
/// the browser process UI or IO threads is not allowed.
/// </summary>
function CefDeleteFile(const path: ustring; recursive: Boolean): Boolean;
/// <summary>
/// Writes the contents of |srcDir| into a zip archive at |destFile|. If
/// |includeHiddenFiles| is true (1) files starting with "." will be included.
/// Returns true (1) on success.  Calling this function on the browser process
/// UI or IO threads is not allowed.
/// </summary>
function CefZipDirectory(const srcDir, destFile: ustring; includeHiddenFiles: Boolean): Boolean;
/// <summary>
/// Loads the existing "Certificate Revocation Lists" file that is managed by
/// Google Chrome. This file can generally be found in Chrome's User Data
/// directory (e.g. "C:\Users\[User]\AppData\Local\Google\Chrome\User Data\" on
/// Windows) and is updated periodically by Chrome's component updater service.
/// Must be called in the browser process after the context has been
/// initialized. See https://dev.chromium.org/Home/chromium-security/crlsets for
/// background.
/// </summary>
procedure CefLoadCRLSetsFile(const path : ustring);
/// <summary>
/// <para>Return a user-agent string.</para>
/// <para>This function tries to replicate the BuildUserAgentFromOSAndProduct
/// function in Chromium but it's safer to call the 'Browser.getVersion'
/// DevTools method.</para>
/// </summary>
/// <remarks>
/// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:content/common/user_agent.cc">Chromium source file: content/common/user_agent.cc (BuildUserAgentFromOSAndProduct)</see></para>
/// <para><see href="https://chromedevtools.github.io/devtools-protocol/tot/Browser/#method-getVersion">See the Browser.getVersion article.</see></para>
/// </remarks>
function  GetDefaultCEFUserAgent : string;

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
function  GetIsWow64Process2(var aProcessMachine, aNativeMachine : WORD) : boolean;
function  IsWowProcess: boolean;
function  RunningWindows10OrNewer : boolean;
function  GetDPIForHandle(aHandle : HWND; var aDPI : UINT) : boolean;
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
/// <summary>
/// Returns a URI with a DATA scheme using |aString| as the URI's data.
/// </summary>
function CefGetDataURI(const aString, aMimeType : ustring) : ustring; overload;
/// <summary>
/// Returns a URI with a DATA scheme encoding |aData| as a base64 string.
/// </summary>
function CefGetDataURI(aData : pointer; aSize : integer; const aMimeType : ustring; const aCharset : ustring = '') : ustring; overload;

function ValidCefWindowHandle(aHandle : TCefWindowHandle) : boolean;
procedure InitializeWindowHandle(var aHandle : TCefWindowHandle);

/// <summary>
/// Returns a command line switch value if it exists.
/// </summary>
function GetCommandLineSwitchValue(const aKey : string; var aValue : ustring) : boolean;
/// <summary>
/// Returns true if the command line switch has a "type" value.
/// </summary>
function IsCEFSubprocess : boolean;

{$IFNDEF FPC}{$IFNDEF DELPHI7_UP}
function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
{$ENDIF}{$ENDIF}
/// <summary>
/// Convert an editting command to string.
/// </summary>
function EditingCommandToString(aEditingCommand : TCefEditingCommand): ustring;

implementation

uses
  {$IFDEF LINUX}{$IFDEF FMX}uCEFLinuxFunctions, Posix.Unistd, Posix.Stdio,{$ENDIF}{$ENDIF}
  {$IFDEF MACOSX}{$IFDEF FPC}CocoaAll,{$ELSE}Posix.Unistd, Posix.Stdio,{$ENDIF}{$ENDIF}
  uCEFApplicationCore, uCEFSchemeHandlerFactory, uCEFValue,
  uCEFBinaryValue, uCEFStringList, uCEFWindowInfoWrapper;

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

procedure CefStringSet(const aDstStr, aSrcStr: TCefString);
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    cef_string_utf16_set(aSrcStr.str, aSrcStr.length, @aDstStr, Ord(True));
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

function DateTimeToCefBaseTime(dt: TDateTime): TCefBaseTime;
begin
  Result := CetTimeToCefBaseTime(DateTimeToCefTime(dt));
end;

function CefTimeToDouble(const dt: TCefTime): double;
begin
  Result := 0;
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    cef_time_to_doublet(@dt, Result);
end;

function DoubleToCefTime(const dt: double): TCefTime;
begin
  FillChar(Result, SizeOf(TCefTime), #0);
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    cef_time_from_doublet(dt, Result);
end;

function CefTimeToUnixTime(const dt: TCefTime): int64;
begin
  Result := 0;
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    cef_time_to_timet(@dt, Result);
end;

function UnixTimeToCefTime(const dt: int64): TCefTime;
begin                         
  FillChar(Result, SizeOf(TCefTime), #0);
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    cef_time_from_timet(dt, Result);
end;

function CefTimeNow: TCefTime;
begin
  FillChar(Result, SizeOf(TCefTime), #0);
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    cef_time_now(Result);
end;

function DoubleTimeNow: double;
var
  TempTime : TCefTime;
begin
  Result := 0;
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      FillChar(TempTime, SizeOf(TCefTime), #0);
      if (cef_time_now(TempTime) <> 0) then
        cef_time_to_doublet(@TempTime, Result);
    end;
end;

function CefTimeDelta(const cef_time1, cef_time2: TCefTime): int64;
begin
  Result := 0;
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    cef_time_delta(@cef_time1, @cef_time2, Result);
end;

function CefBaseTimeNow: TCefBaseTime;
begin
  Result := 0;
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    Result := cef_basetime_now();
end;

function CetTimeToCefBaseTime(const ct: TCefTime) : TCefBaseTime;
var
  TempResult : TCefBaseTime;
begin
  Result := 0;
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded and (cef_time_to_basetime(@ct, @TempResult) <> 0) then
    Result := TempResult;
end;

function CetTimeFromCefBaseTime(const cbt: TCefBaseTime) : TCefTime;
var
  TempResult : TCefTime;
begin
  FillChar(Result, SizeOf(TCefTime), #0);
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded and (cef_time_from_basetime(cbt, @TempResult) <> 0) then
    Result := TempResult;
end;

function CefBaseTimeToDateTime(const cbt: TCefBaseTime) : TDateTime;
var
  TempResult : TCefTime;
begin
  Result := 0;
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded and (cef_time_from_basetime(cbt, @TempResult) <> 0) then
    Result := CefTimeToDateTime(TempResult);
end;

function GetTimeIntervalMilliseconds(const from_: TCefTime): integer;
var
  TempFrom : double;
  TempDelay : integer;
begin
  Result   := -1;
  TempFrom := CefTimeToDouble(from_);

  if (TempFrom = 0) then exit;

  TempDelay := ceil((TempFrom - DoubleTimeNow) * 1000);
  Result    := max(0, TempDelay);
end;

procedure InitializeCefTime(var aTime : TCefTime);
begin
  aTime.year         := 0;
  aTime.month        := 0;
  aTime.day_of_week  := 0;
  aTime.day_of_month := 0;
  aTime.hour         := 0;
  aTime.minute       := 0;
  aTime.second       := 0;
  aTime.millisecond  := 0;
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
  TCEFWindowInfoWrapper.AsChild(aWindowInfo, aParent, aRect);
  aWindowInfo.ex_style    := aExStyle;
  aWindowInfo.window_name := CefString(aWindowName);
end;

procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring; aExStyle : DWORD);
begin
  TCEFWindowInfoWrapper.AsPopup(aWindowInfo, aParent, aWindowName);
  aWindowInfo.ex_style := aExStyle;
end;

procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring; aExStyle : DWORD);
begin
  TCEFWindowInfoWrapper.AsWindowless(aWindowInfo, aParent);
  aWindowInfo.window_name := CefString(aWindowName);
end;
{$ENDIF}

{$IFDEF MACOSX}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aRect : TRect; const aWindowName : ustring; aHidden : boolean);
begin
  TCEFWindowInfoWrapper.AsChild(aWindowInfo, aParent, aRect);
  aWindowInfo.window_name := CefString(aWindowName);
end;

procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring; aHidden : boolean);
begin
  // WindowInfoAsPopUp only exists for Windows. The macos version of cefclient
  // calls WindowInfoAsChild with aParent set to NULL to create a popup window.
  TCEFWindowInfoWrapper.AsChild(aWindowInfo, aParent, Rect(0, 0, 0, 0));
  aWindowInfo.window_name := CefString(aWindowName);
end;

procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring; aHidden : boolean);
begin
  TCEFWindowInfoWrapper.AsWindowless(aWindowInfo, aParent);
  aWindowInfo.window_name := CefString(aWindowName);
end;
{$ENDIF}

{$IFDEF LINUX}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aRect : TRect; const aWindowName : ustring = '');
begin
  TCEFWindowInfoWrapper.AsChild(aWindowInfo, aParent, aRect);
  aWindowInfo.window_name := CefString(aWindowName);
end;

procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring = '');
begin
  // WindowInfoAsPopUp only exists for Windows. The Linux version of cefclient
  // calls WindowInfoAsChild with aParent set to NULL to create a popup window.
  TCEFWindowInfoWrapper.AsChild(aWindowInfo, aParent, Rect(0, 0, 0, 0));
  aWindowInfo.window_name := CefString(aWindowName);
end;

procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring = '');
begin
  TCEFWindowInfoWrapper.AsWindowless(aWindowInfo, aParent);
  aWindowInfo.window_name := CefString(aWindowName);
end;
{$ENDIF}

{$IFDEF ANDROID}
procedure WindowInfoAsChild(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; aRect : TRect; const aWindowName : ustring; aExStyle : DWORD);
begin
  //
end;

procedure WindowInfoAsPopUp(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring; aExStyle : DWORD);
begin
  //
end;

procedure WindowInfoAsWindowless(var aWindowInfo : TCefWindowInfo; aParent : TCefWindowHandle; const aWindowName : ustring; aExStyle : DWORD);
begin
  //
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

function CefGetMinLogLevel: integer;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    Result := cef_get_min_log_level()
   else
    Result := 0;
end;

function CefGetVLogLevel(const file_start : string): integer;
var
  TempFile : AnsiString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded and (length(file_start) > 0) then
    begin
      TempFile := AnsiString(file_start + #0);
      Result   := cef_get_vlog_level(@TempFile[1], length(file_start) + 1);
    end
   else
    Result := 0;
end;

function CefGetLogSeverityName(aSeverity: integer): ustring;
begin
  case aSeverity of
    CEF_LOG_SEVERITY_VERBOSE : Result := 'VERBOSE';
    CEF_LOG_SEVERITY_INFO    : Result := 'INFO';
    CEF_LOG_SEVERITY_WARNING : Result := 'WARNING';
    CEF_LOG_SEVERITY_ERROR   : Result := 'ERROR';
    CEF_LOG_SEVERITY_FATAL   : Result := 'FATAL';
    else                       Result := 'UNKNOWN';
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
      {$IFDEF WIN64}
      TempList.Add(TempDir + 'dxcompiler.dll');
      TempList.Add(TempDir + 'dxil.dll');
      {$ENDIF}
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

function GetRegistryWindowsVersion(var aMajor, aMinor: cardinal) : boolean;
const
  SUBKEY = '\SOFTWARE\Microsoft\Windows NT\CurrentVersion';
var
  TempRegKey : TRegistry;
  TempBuild  : integer;
begin
  Result     := False;
  aMajor     := 0;
  aMinor     := 0;
  TempRegKey := nil;

  try
    try
      TempRegKey         := TRegistry.Create(KEY_READ);
      TempRegKey.RootKey := HKEY_LOCAL_MACHINE;

      if TempRegKey.KeyExists(SUBKEY) and
         TempRegKey.OpenKeyReadOnly(SUBKEY) then
        try
          if TempRegKey.ValueExists('CurrentMajorVersionNumber') and
             TempRegKey.ValueExists('CurrentMinorVersionNumber') then
            begin
              aMajor := TempRegKey.ReadInteger('CurrentMajorVersionNumber');
              aMinor := TempRegKey.ReadInteger('CurrentMinorVersionNumber');
              Result := True;
            end
           else
            if TempRegKey.ValueExists('CurrentBuildNumber') then
              begin
                TempBuild := StrToIntDef(TempRegKey.ReadString('CurrentBuildNumber'), 0);

                if (TempBuild >= 22000) then // Windows 11
                  begin
                    aMajor := 10;
                    aMinor := 0;
                    Result := True;
                  end
                 else
                  if (TempBuild >= 10240) then // Windows 10
                    begin
                      aMajor := 10;
                      aMinor := 0;
                      Result := True;
                    end
                   else
                    if (TempBuild >= 9600) then // Windows 8.1
                      begin
                        aMajor := 6;
                        aMinor := 3;
                        Result := True;
                      end
                     else
                      if (TempBuild >= 9200) then // Windows 8
                        begin
                          aMajor := 6;
                          aMinor := 2;
                          Result := True;
                        end
                       else
                        if (TempBuild >= 7600) then // Windows 7
                          begin
                            aMajor := 6;
                            aMinor := 1;
                            Result := True;
                          end
                         else
                          if (TempBuild >= 6000) then // Windows Vista
                            begin
                              aMajor := 6;
                              aMinor := 0;
                              Result := True;
                            end
                           else
                            if (TempBuild >= 3790) then // Windows Server 2003
                              begin
                                aMajor := 5;
                                aMinor := 2;
                                Result := True;
                              end
                             else
                              if (TempBuild >= 2600) then // Windows XP
                                begin
                                  aMajor := 5;
                                  aMinor := 1;
                                  Result := True;
                                end
                               else
                                if (TempBuild >= 2195) then // Windows 2000
                                  begin
                                    aMajor := 5;
                                    aMinor := 0;
                                    Result := True;
                                  end;
              end;
        finally
          TempRegKey.CloseKey;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('GetRegistryWindowsVersion', e) then raise;
    end;
  finally
    if assigned(TempRegKey) then
      FreeAndNil(TempRegKey);
  end;
end;

function GetRealWindowsVersion(var aMajor, aMinor: cardinal) : boolean;
type
  SERVER_INFO_101 = record
    sv101_platform_id   : DWORD;
    sv101_name          : LPWSTR;
    sv101_version_major : DWORD;
    sv101_version_minor : DWORD;
    sv101_type          : DWORD;
    sv101_comment       : LPWSTR;
  end;
  PSERVER_INFO_101 = ^SERVER_INFO_101;

const
  MAJOR_VERSION_MASK = $0F;
  NO_ERROR           = 0;

var
  TempBuffer : PSERVER_INFO_101;
begin
  Result     := False;      
  aMajor     := 0;
  aMinor     := 0;
  TempBuffer := nil;

  if (NetServerGetInfo(nil, 101, Pointer(TempBuffer)) = NO_ERROR) then
    try
      aMajor := TempBuffer^.sv101_version_major and MAJOR_VERSION_MASK;
      aMinor := TempBuffer^.sv101_version_minor;
      Result := True;
    finally
      NetApiBufferFree(TempBuffer);
    end;
end;

function CheckRealWindowsVersion(aMajor, aMinor: cardinal) : boolean;
var
  TempMajor, TempMinor : cardinal;
  TempResultAPI, TempResultReg : boolean;
begin
  TempResultAPI := GetRealWindowsVersion(TempMajor, TempMinor) and
                   ((TempMajor > aMajor) or
                    ((TempMajor = aMajor) and (TempMinor >= aMinor)));

  TempResultReg := GetRegistryWindowsVersion(TempMajor, TempMinor) and
                   ((TempMajor > aMajor) or
                    ((TempMajor = aMajor) and (TempMinor >= aMinor)));

  Result := TempResultAPI or TempResultReg;
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

function CefResolveUrl(const base_url, relative_url: ustring): ustring;
var
  TempBaseURL, TempRelativeURL, TempResolvedURL : TCefString;
begin
  Result := '';

  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempBaseURL     := CefString(base_url);
      TempRelativeURL := CefString(relative_url);

      CefStringInitialize(@TempResolvedURL);

      if (cef_resolve_url(@TempBaseURL, @TempRelativeURL, @TempResolvedURL) <> 0) then
        Result := CefStringClearAndGet(@TempResolvedURL);
    end;
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

function GetDefaultCEFUserAgent : string;
var
  TempOS : string;
  {$IFDEF MSWINDOWS}
  TempMajorVer, TempMinorVer : DWORD;
  {$ENDIF}
begin
  // See GetUserAgentPlatform() and BuildOSCpuInfo() in
  // https://source.chromium.org/chromium/chromium/src/+/main:content/common/user_agent.cc
  {$IFDEF MSWINDOWS}
  TempOS := 'Windows NT ';

  if GetWindowsMajorMinorVersion(TempMajorVer, TempMinorVer) then
    TempOS := TempOS + inttostr(TempMajorVer) + '.' + inttostr(TempMinorVer)
   else
    TempOS := TempOS + '10.0'; // oldest Windows version supported by Chromium

  if IsWowProcess then
    TempOS := TempOS + '; WOW64'
   else
    {$IFDEF TARGET_64BITS}
    TempOS := TempOS + '; Win64; x64';
    {$ELSE}
    TempOS := TempOS + '; Win32; x86';
    {$ENDIF};
  {$ENDIF}

  {$IFDEF MACOSX}
  TempOS := 'Macintosh; Intel Mac OS X 10_15_7';
  {$ENDIF}

  {$IFDEF LINUX}
  TempOS := 'X11; Linux ' + {$IFDEF TARGET_64BITS}'x86_64'{$ELSE}'i686'{$ENDIF};
  {$ENDIF}

  Result  := 'Mozilla/5.0' + ' (' + TempOS + ') ' +
             'AppleWebKit/537.36 (KHTML, like Gecko) ' +
             'Chrome/' + inttostr(CEF_CHROMEELF_VERSION_MAJOR) + '.0.0.0 ' +
             'Safari/537.36';
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

function GetIsWow64Process2(var aProcessMachine, aNativeMachine : WORD) : boolean;
type
  TIsWow64Process2Func = function(hProcess: THandle; ProcessMachine, NativeMachine : PWORD): BOOL; stdcall;
var
  TempHandle : THandle;
  TempIsWow64Process2Func : TIsWow64Process2Func;
begin
  Result          := False;
  aProcessMachine := 0;
  aNativeMachine  := 0;

  try
    TempHandle := LoadLibrary(Kernel32DLL);

    if (TempHandle <> 0) then
      try
        {$IFDEF FPC}Pointer({$ENDIF}TempIsWow64Process2Func{$IFDEF FPC}){$ENDIF} := GetProcAddress(TempHandle, 'IsWow64Process2');

        Result := assigned(TempIsWow64Process2Func) and
                  TempIsWow64Process2Func(GetCurrentProcess(), @aProcessMachine, @aNativeMachine);
      finally
        FreeLibrary(TempHandle);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('GetIsWow64Process2', e) then raise;
  end;
end;

function IsWowProcess: boolean;
const
  IMAGE_FILE_MACHINE_I386  = $014C;
  IMAGE_FILE_MACHINE_AMD64 = $8664;
var
  Temp64bit : BOOL;
  TempProcessMachine, TempNativeMachine : WORD;
begin
  if GetIsWow64Process2(TempProcessMachine, TempNativeMachine) then
    Result := (TempProcessMachine = IMAGE_FILE_MACHINE_I386) and
              (TempNativeMachine  = IMAGE_FILE_MACHINE_AMD64)
   else
    Result := ProcessUnderWow64(GetCurrentProcess(), @Temp64bit) and
              Temp64bit;
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
  TempService : IFMXScreenService;
  TempWidth, TempWidthMM : integer;
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
    Result := -1;
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, TempService) then
      Result := round(TempService.GetScreenScale * USER_DEFAULT_SCREEN_DPI);

    if (Result < 0) then
      begin
        Result := round(gdk_screen_get_resolution(gdk_screen_get_default));

        if (Result < 0) then
          begin
            TempWidthMM := gdk_screen_width_mm;
            TempWidth   := gdk_screen_width;

            if (TempWidthMM > 0) and (TempWidth > 0) then
              Result := round(TempWidth / (TempWidthMM / 25.4))
             else
              Result := USER_DEFAULT_SCREEN_DPI;
          end;
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
                    Result := ((TempRec.Name = 'Network') or RemoveDir(TempPath)) and Result
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

function IsCEFSubprocess : boolean;
var
  TempValue : ustring;
begin
  Result := GetCommandLineSwitchValue('type', TempValue) and (length(TempValue) > 0);
end;

{$IFNDEF FPC}{$IFNDEF DELPHI7_UP}
function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
var
  TempString : string;
begin
  if Offset <= 1 then
    Result := Pos(SubStr, S)
   else
    begin
      TempString := copy(S, Offset, length(S));
      Result     := Pos(SubStr, TempString);
      if (Result > 0) then inc(Result, Offset - 1);
    end;
end;
{$ENDIF}{$ENDIF}

function EditingCommandToString(aEditingCommand : TCefEditingCommand): ustring;
begin
  case aEditingCommand of
    ecAlignCenter                                  : Result := 'AlignCenter';
    ecAlignJustified                               : Result := 'AlignJustified';
    ecAlignLeft                                    : Result := 'AlignLeft';
    ecAlignRight                                   : Result := 'AlignRight';
    ecBackColor                                    : Result := 'BackColor';
    ecBackwardDelete                               : Result := 'BackwardDelete';
    ecBold                                         : Result := 'Bold';
    ecCopy                                         : Result := 'Copy';
    ecCreateLink                                   : Result := 'CreateLink';
    ecCut                                          : Result := 'Cut';
    ecDefaultParagraphSeparator                    : Result := 'DefaultParagraphSeparator';
    ecDelete                                       : Result := 'Delete';
    ecDeleteBackward                               : Result := 'DeleteBackward';
    ecDeleteBackwardByDecomposingPreviousCharacter : Result := 'DeleteBackwardByDecomposingPreviousCharacter';
    ecDeleteForward                                : Result := 'DeleteForward';
    ecDeleteToBeginningOfLine                      : Result := 'DeleteToBeginningOfLine';
    ecDeleteToBeginningOfParagraph                 : Result := 'DeleteToBeginningOfParagraph';
    ecDeleteToEndOfLine                            : Result := 'DeleteToEndOfLine';
    ecDeleteToEndOfParagraph                       : Result := 'DeleteToEndOfParagraph';
    ecDeleteToMark                                 : Result := 'DeleteToMark';
    ecDeleteWordBackward                           : Result := 'DeleteWordBackward';
    ecDeleteWordForward                            : Result := 'DeleteWordForward';
    ecFindString                                   : Result := 'FindString';
    ecFontName                                     : Result := 'FontName';
    ecFontSize                                     : Result := 'FontSize';
    ecFontSizeDelta                                : Result := 'FontSizeDelta';
    ecForeColor                                    : Result := 'ForeColor';
    ecFormatBlock                                  : Result := 'FormatBlock';
    ecForwardDelete                                : Result := 'ForwardDelete';
    ecHiliteColor                                  : Result := 'HiliteColor';
    ecIgnoreSpelling                               : Result := 'IgnoreSpelling';
    ecIndent                                       : Result := 'Indent';
    ecInsertBacktab                                : Result := 'InsertBacktab';
    ecInsertHorizontalRule                         : Result := 'InsertHorizontalRule';
    ecInsertHTML                                   : Result := 'InsertHTML';
    ecInsertImage                                  : Result := 'InsertImage';
    ecInsertLineBreak                              : Result := 'InsertLineBreak';
    ecInsertNewline                                : Result := 'InsertNewline';
    ecInsertNewlineInQuotedContent                 : Result := 'InsertNewlineInQuotedContent';
    ecInsertOrderedList                            : Result := 'InsertOrderedList';
    ecInsertParagraph                              : Result := 'InsertParagraph';
    ecInsertTab                                    : Result := 'InsertTab';
    ecInsertText                                   : Result := 'InsertText';
    ecInsertUnorderedList                          : Result := 'InsertUnorderedList';
    ecItalic                                       : Result := 'Italic';
    ecJustifyCenter                                : Result := 'JustifyCenter';
    ecJustifyFull                                  : Result := 'JustifyFull';
    ecJustifyLeft                                  : Result := 'JustifyLeft';
    ecJustifyNone                                  : Result := 'JustifyNone';
    ecJustifyRight                                 : Result := 'JustifyRight';
    ecMakeTextWritingDirectionLeftToRight          : Result := 'MakeTextWritingDirectionLeftToRight';
    ecMakeTextWritingDirectionNatural              : Result := 'MakeTextWritingDirectionNatural';
    ecMakeTextWritingDirectionRightToLeft          : Result := 'MakeTextWritingDirectionRightToLeft';
    ecMoveBackward                                 : Result := 'MoveBackward';
    ecMoveBackwardAndModifySelection               : Result := 'MoveBackwardAndModifySelection';
    ecMoveDown                                     : Result := 'MoveDown';
    ecMoveDownAndModifySelection                   : Result := 'MoveDownAndModifySelection';
    ecMoveForward                                  : Result := 'MoveForward';
    ecMoveForwardAndModifySelection                : Result := 'MoveForwardAndModifySelection';
    ecMoveLeft                                     : Result := 'MoveLeft';
    ecMoveLeftAndModifySelection                   : Result := 'MoveLeftAndModifySelection';
    ecMovePageDown                                 : Result := 'MovePageDown';
    ecMovePageDownAndModifySelection               : Result := 'MovePageDownAndModifySelection';
    ecMovePageUp                                   : Result := 'MovePageUp';
    ecMovePageUpAndModifySelection                 : Result := 'MovePageUpAndModifySelection';
    ecMoveParagraphBackward                        : Result := 'MoveParagraphBackward';
    ecMoveParagraphBackwardAndModifySelection      : Result := 'MoveParagraphBackwardAndModifySelection';
    ecMoveParagraphForward                         : Result := 'MoveParagraphForward';
    ecMoveParagraphForwardAndModifySelection       : Result := 'MoveParagraphForwardAndModifySelection';
    ecMoveRight                                    : Result := 'MoveRight';
    ecMoveRightAndModifySelection                  : Result := 'MoveRightAndModifySelection';
    ecMoveToBeginningOfDocument                    : Result := 'MoveToBeginningOfDocument';
    ecMoveToBeginningOfDocumentAndModifySelection  : Result := 'MoveToBeginningOfDocumentAndModifySelection';
    ecMoveToBeginningOfLine                        : Result := 'MoveToBeginningOfLine';
    ecMoveToBeginningOfLineAndModifySelection      : Result := 'MoveToBeginningOfLineAndModifySelection';
    ecMoveToBeginningOfParagraph                   : Result := 'MoveToBeginningOfParagraph';
    ecMoveToBeginningOfParagraphAndModifySelection : Result := 'MoveToBeginningOfParagraphAndModifySelection';
    ecMoveToBeginningOfSentence                    : Result := 'MoveToBeginningOfSentence';
    ecMoveToBeginningOfSentenceAndModifySelection  : Result := 'MoveToBeginningOfSentenceAndModifySelection';
    ecMoveToEndOfDocument                          : Result := 'MoveToEndOfDocument';
    ecMoveToEndOfDocumentAndModifySelection        : Result := 'MoveToEndOfDocumentAndModifySelection';
    ecMoveToEndOfLine                              : Result := 'MoveToEndOfLine';
    ecMoveToEndOfLineAndModifySelection            : Result := 'MoveToEndOfLineAndModifySelection';
    ecMoveToEndOfParagraph                         : Result := 'MoveToEndOfParagraph';
    ecMoveToEndOfParagraphAndModifySelection       : Result := 'MoveToEndOfParagraphAndModifySelection';
    ecMoveToEndOfSentence                          : Result := 'MoveToEndOfSentence';
    ecMoveToEndOfSentenceAndModifySelection        : Result := 'MoveToEndOfSentenceAndModifySelection';
    ecMoveToLeftEndOfLine                          : Result := 'MoveToLeftEndOfLine';
    ecMoveToLeftEndOfLineAndModifySelection        : Result := 'MoveToLeftEndOfLineAndModifySelection';
    ecMoveToRightEndOfLine                         : Result := 'MoveToRightEndOfLine';
    ecMoveToRightEndOfLineAndModifySelection       : Result := 'MoveToRightEndOfLineAndModifySelection';
    ecMoveUp                                       : Result := 'MoveUp';
    ecMoveUpAndModifySelection                     : Result := 'MoveUpAndModifySelection';
    ecMoveWordBackward                             : Result := 'MoveWordBackward';
    ecMoveWordBackwardAndModifySelection           : Result := 'MoveWordBackwardAndModifySelection';
    ecMoveWordForward                              : Result := 'MoveWordForward';
    ecMoveWordForwardAndModifySelection            : Result := 'MoveWordForwardAndModifySelection';
    ecMoveWordLeft                                 : Result := 'MoveWordLeft';
    ecMoveWordLeftAndModifySelection               : Result := 'MoveWordLeftAndModifySelection';
    ecMoveWordRight                                : Result := 'MoveWordRight';
    ecMoveWordRightAndModifySelection              : Result := 'MoveWordRightAndModifySelection';
    ecOutdent                                      : Result := 'Outdent';
    ecOverWrite                                    : Result := 'OverWrite';
    ecPaste                                        : Result := 'Paste';
    ecPasteAndMatchStyle                           : Result := 'PasteAndMatchStyle';
    ecPasteGlobalSelection                         : Result := 'PasteGlobalSelection';
    ecPrint                                        : Result := 'Print';
    ecRedo                                         : Result := 'Redo';
    ecRemoveFormat                                 : Result := 'RemoveFormat';
    ecScrollLineDown                               : Result := 'ScrollLineDown';
    ecScrollLineUp                                 : Result := 'ScrollLineUp';
    ecScrollPageBackward                           : Result := 'ScrollPageBackward';
    ecScrollPageForward                            : Result := 'ScrollPageForward';
    ecScrollToBeginningOfDocument                  : Result := 'ScrollToBeginningOfDocument';
    ecScrollToEndOfDocument                        : Result := 'ScrollToEndOfDocument';
    ecSelectAll                                    : Result := 'SelectAll';
    ecSelectLine                                   : Result := 'SelectLine';
    ecSelectParagraph                              : Result := 'SelectParagraph';
    ecSelectSentence                               : Result := 'SelectSentence';
    ecSelectToMark                                 : Result := 'SelectToMark';
    ecSelectWord                                   : Result := 'SelectWord';
    ecSetMark                                      : Result := 'SetMark';
    ecStrikethrough                                : Result := 'Strikethrough';
    ecStyleWithCSS                                 : Result := 'StyleWithCSS';
    ecSubscript                                    : Result := 'Subscript';
    ecSuperscript                                  : Result := 'Superscript';
    ecSwapWithMark                                 : Result := 'SwapWithMark';
    ecToggleBold                                   : Result := 'ToggleBold';
    ecToggleItalic                                 : Result := 'ToggleItalic';
    ecToggleUnderline                              : Result := 'ToggleUnderline';
    ecTranspose                                    : Result := 'Transpose';
    ecUnderline                                    : Result := 'Underline';
    ecUndo                                         : Result := 'Undo';
    ecUnlink                                       : Result := 'Unlink';
    ecUnscript                                     : Result := 'Unscript';
    ecUnselect                                     : Result := 'Unselect';
    ecUseCSS                                       : Result := 'UseCSS';
    ecYank                                         : Result := 'Yank';
    ecYankAndSelect                                : Result := 'YankAndSelect';
    else                                             Result := '';
  end;
end;

end.
