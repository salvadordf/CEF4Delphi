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

unit uCEFApplication;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}
      WinApi.Windows, WinApi.ActiveX,
      {$IFDEF FMX}
      FMX.Forms,
      {$ELSE}
      Vcl.Forms,
      {$ENDIF}
    {$ENDIF}
    System.Classes, System.UITypes,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows, Forms, ActiveX,{$ENDIF} Classes, Controls, {$IFDEF FPC}dynlibs,{$ENDIF}
  {$ENDIF}
  uCEFApplicationCore, uCEFTypes;

const
  CEF_SUPPORTED_VERSION_MAJOR   = uCefApplicationCore.CEF_SUPPORTED_VERSION_MAJOR;
  CEF_SUPPORTED_VERSION_MINOR   = uCefApplicationCore.CEF_SUPPORTED_VERSION_MINOR;
  CEF_SUPPORTED_VERSION_RELEASE = uCefApplicationCore.CEF_SUPPORTED_VERSION_RELEASE;
  CEF_SUPPORTED_VERSION_BUILD   = uCefApplicationCore.CEF_SUPPORTED_VERSION_BUILD;

  CEF_CHROMEELF_VERSION_MAJOR   = uCefApplicationCore.CEF_CHROMEELF_VERSION_MAJOR;
  CEF_CHROMEELF_VERSION_MINOR   = uCefApplicationCore.CEF_CHROMEELF_VERSION_MINOR;
  CEF_CHROMEELF_VERSION_RELEASE = uCefApplicationCore.CEF_CHROMEELF_VERSION_RELEASE;
  CEF_CHROMEELF_VERSION_BUILD   = uCefApplicationCore.CEF_CHROMEELF_VERSION_BUILD;

  LIBCEF_DLL                    = uCefApplicationCore.LIBCEF_DLL;
  CHROMEELF_DLL                 = uCefApplicationCore.CHROMEELF_DLL;

type
  TCefApplication = class(TCefApplicationCore)
    protected
      FDestroyApplicationObject      : boolean;
      FDestroyAppWindows             : boolean;

      procedure BeforeInitSubProcess; override;

    public
      constructor Create;
      destructor  Destroy; override;
      procedure   UpdateDeviceScaleFactor; override;

      property DestroyApplicationObject: boolean read FDestroyApplicationObject write FDestroyApplicationObject;
      property DestroyAppWindows       : boolean read FDestroyAppWindows        write FDestroyAppWindows;
  end;

  TCEFDirectoryDeleterThread = uCEFApplicationCore.TCEFDirectoryDeleterThread;

var
  GlobalCEFApp : TCefApplication = nil;

function CefCursorToWindowsCursor(aCefCursor : TCefCursorType) : TCursor;
procedure DestroyGlobalCEFApp;

// *********************************************************
// ********************** ATTENTION ! **********************
// *********************************************************
// **                                                     **
// **  MANY OF THE EVENTS IN CEF4DELPHI COMPONENTS LIKE   **
// **  TCHROMIUM, TFMXCHROMIUM OR TCEFAPPLICATION ARE     **
// **  EXECUTED IN A CEF THREAD BY DEFAULT.               **
// **                                                     **
// **  WINDOWS CONTROLS MUST BE CREATED AND DESTROYED IN  **
// **  THE SAME THREAD TO AVOID ERRORS.                   **
// **  SOME OF THEM RECREATE THE HANDLERS IF THEY ARE     **
// **  MODIFIED AND CAN CAUSE THE SAME ERRORS.            **
// **                                                     **
// **  DON'T CREATE, MODIFY OR DESTROY WINDOWS CONTROLS   **
// **  INSIDE THE CEF4DELPHI EVENTS AND USE               **
// **  SYNCHRONIZATION OBJECTS TO PROTECT VARIABLES AND   **
// **  FIELDS IF THEY ARE ALSO USED IN THE MAIN THREAD.   **
// **                                                     **
// **  READ THIS FOR MORE INFORMATION :                   **
// **  https://www.briskbard.com/index.php?pageid=cef     **
// **                                                     **
// **  USE OUR FORUMS FOR MORE QUESTIONS :                **
// **  https://www.briskbard.com/forum/                   **
// **                                                     **
// *********************************************************
// *********************************************************

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.Math, System.IOUtils, System.SysUtils, {$IFDEF MSWINDOWS}WinApi.TlHelp32, WinApi.PSAPI,{$ENDIF}
  {$ELSE}
    Math, {$IFDEF DELPHI14_UP}IOUtils,{$ENDIF} SysUtils,
    {$IFDEF FPC}
      {$IFDEF MSWINDOWS}jwatlhelp32, jwapsapi,{$ENDIF}
    {$ELSE}
      TlHelp32, {$IFDEF MSWINDOWS}PSAPI,{$ENDIF}
    {$ENDIF}
  {$ENDIF}
  uCEFConstants, uCEFMiscFunctions;

function CefCursorToWindowsCursor(aCefCursor : TCefCursorType) : TCursor;
begin
  case aCefCursor of
    CT_POINTER                  : Result := crArrow;
    CT_CROSS                    : Result := crCross;
    CT_HAND                     : Result := crHandPoint;
    CT_IBEAM                    : Result := crIBeam;
    CT_WAIT                     : Result := crHourGlass;
    CT_HELP                     : Result := crHelp;
    CT_EASTRESIZE               : Result := crSizeWE;
    CT_NORTHRESIZE              : Result := crSizeNS;
    CT_NORTHEASTRESIZE          : Result := crSizeNESW;
    CT_NORTHWESTRESIZE          : Result := crSizeNWSE;
    CT_SOUTHRESIZE              : Result := crSizeNS;
    CT_SOUTHEASTRESIZE          : Result := crSizeNWSE;
    CT_SOUTHWESTRESIZE          : Result := crSizeNESW;
    CT_WESTRESIZE               : Result := crSizeWE;
    CT_NORTHSOUTHRESIZE         : Result := crSizeNS;
    CT_EASTWESTRESIZE           : Result := crSizeWE;
    CT_NORTHEASTSOUTHWESTRESIZE : Result := crSizeNESW;
    CT_NORTHWESTSOUTHEASTRESIZE : Result := crSizeNWSE;
    CT_COLUMNRESIZE             : Result := crHSplit;
    CT_ROWRESIZE                : Result := crVSplit;
    CT_MOVE                     : Result := crSizeAll;
    CT_PROGRESS                 : Result := crAppStart;
    CT_NONE                     : Result := crNone;
    CT_NODROP,
    CT_NOTALLOWED               : Result := crNo;
    CT_GRAB,
    CT_GRABBING                 : Result := crDrag;

    else Result := crDefault;
  end;
end;

procedure DestroyGlobalCEFApp;
begin
  if (GlobalCEFApp <> nil) then FreeAndNil(GlobalCEFApp);
end;

constructor TCefApplication.Create;
begin
  inherited Create;
  if GlobalCEFApp = nil then
    GlobalCEFApp := Self;

  FDestroyApplicationObject      := False;
  FDestroyAppWindows             := True;
end;

destructor TCefApplication.Destroy;
begin
  if GlobalCEFApp = Self then
    GlobalCEFApp := nil;
  inherited Destroy;
end;

procedure TCefApplication.UpdateDeviceScaleFactor;
{$IFDEF MSWINDOWS}
{$IFNDEF FMX}
var
  TempHandle : HWND;
  TempDPI    : UINT;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  {$IFNDEF FMX}
  if RunningWindows10OrNewer then
    begin
      if assigned(screen.ActiveForm) and
         screen.ActiveForm.HandleAllocated then
        TempHandle := screen.ActiveForm.Handle
       else
        if assigned(Application.MainForm) and
           Application.MainForm.HandleAllocated then
          TempHandle := Application.MainForm.Handle
         else
          TempHandle := Application.Handle;

     if GetDPIForHandle(TempHandle, TempDPI) then
       FDeviceScaleFactor := TempDPI / USER_DEFAULT_SCREEN_DPI
      else
       inherited UpdateDeviceScaleFactor;
    end
   else
  {$ENDIF}
    inherited UpdateDeviceScaleFactor;
  {$ELSE}
  inherited UpdateDeviceScaleFactor;
  {$ENDIF}
end;

procedure TCefApplication.BeforeInitSubProcess;
{$IFNDEF FPC}
{$IFNDEF FMX}
var
  AppDestroy: procedure(Obj: TApplication; ReleaseMemoryFlag: Byte);
{$ENDIF}
{$ENDIF}
begin
  {$IFNDEF FPC}
  {$IFNDEF FMX}
  if Application <> nil then
    begin
      if FDestroyApplicationObject then
        begin
          // Call the destructor in "inherited Destroy" mode. This makes it possible to undo
          // all the code that TApplication.Create did without actually releasing the Application
          // object so that TControl.Destroy and DoneApplication dont't crash.
          //
          // Undoing also includes destroying the "AppWindows" and calling OleUninitialize what
          // allows CEF to initialize the COM thread model the way it is required in the
          // sub-processes (debug assertion).
          AppDestroy := @TApplication.Destroy;
          AppDestroy(Application, 0);
          // Set all sub-objects to nil (we destroyed them already). This prevents the second
          // TApplication.Destroy call in DoneApplication from trying to release already released
          // objects.
          TApplication.InitInstance(Application);
        end
       else
        begin
          if FDestroyAppWindows then
            begin
              // This is the fix for the issue #139
              // https://github.com/salvadordf/CEF4Delphi/issues/139
              // Subprocesses will never use these window handles but TApplication creates them
              // before executing the code in the DPR file. Any other application trying to
              // initiate a DDE conversation will use SendMessage or SendMessageTimeout to
              // broadcast the WM_DDE_INITIATE to all top-level windows. The subprocesses never
              // call Application.Run so the SendMessage freezes the other applications.
              if (Application.Handle          <> 0) then DestroyWindow(Application.Handle);
              {$IFDEF DELPHI9_UP}
              if (Application.PopupControlWnd <> 0) then DeallocateHWnd(Application.PopupControlWnd);
              {$ENDIF}
            end;
          if not IsLibrary then
            begin
              // Undo the OleInitialize from TApplication.Create. The sub-processes want a different
              // COM thread model and fail with an assertion if the Debug-DLLs are used.
              OleUninitialize;
            end;
        end;
    end;
  {$ELSE} // FMX
    {$IFDEF MSWINDOWS}
    // Undo the OleInitialize from FMX.Platform.Win::initialization. The sub-processes want a different
    // COM thread model and fail with an assertion if the Debug-DLLs are used.
    OleUninitialize;
    {$ENDIF MSWINDOWS}
  {$ENDIF}
  {$ENDIF}
end;

end.
