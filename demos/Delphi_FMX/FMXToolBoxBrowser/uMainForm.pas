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
//        Copyright � 2019 Salvador Diaz Fau. All rights reserved.
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

unit uMainForm;

{$I cef.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Messages, Winapi.Windows,
  {$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.Controls.Presentation, FMX.StdCtrls;


const
  CEF_CHILDDESTROYED   = WM_APP + $100;
  CEF_INITIALIZED      = WM_APP + $101;

type
  TMainForm = class(TForm)
    ButtonPnl: TPanel;
    AddressEdt: TEdit;
    OpenBtn: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure OpenBtnClick(Sender: TObject);
  private
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True when all the child forms are closed
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    function  GetChildFormCount : integer;
    function  GetChildClosing : boolean;

    procedure CreateToolboxChild(const ChildCaption, URL: string);
    procedure CloseAllChildForms;
    function  PostCustomMessage(aMessage : cardinal; wParam : cardinal = 0; lParam : integer = 0) : boolean;

  protected

  public
    procedure DoCEFInitialized;
    procedure DoChildDestroyed;
    procedure SendChildDestroyedMsg;

    property ChildClosing   : boolean read GetChildClosing;
    property ChildFormCount : integer read GetChildFormCount;
  end;

var
  MainForm: TMainForm;

procedure CreateGlobalCEFApp;

implementation

{$R *.fmx}

uses
  FMX.Platform, FMX.Platform.Win,
  uCEFMiscFunctions, uFMXApplicationService, uChildForm, uCEFApplication;

// This Firemonkey demo shows how to create child windows with browsers using CEF4Delphi.
// It uses a custom IFMXApplicationService to handle Windows messages.

// The application must not try to create browsers before GlobalCEFApp.GlobalContextInitialized
// is TRUE, which is set in a different thread when the GlobalCEFApp.OnContextInitialized
// event is triggered.
// There are several ways to handle the race between the form creation and the
// GlobalCEFApp.OnContextInitialized event but this demo is using a simple check
// in the TForm.OnShow event.

// All FMX applications using CEF4Delphi should add the $(FrameworkType) conditional define
// in the project options to avoid duplicated resources.
// This demo has that define in the menu option :
// Project -> Options -> Building -> Delphi compiler -> Conditional defines (All configurations)

// It's very important to close all the browsers before closing the main form.
// Always follow these destruction steps in your applications to avoid
// crashes when it's closed. Read the code comments in uChildForm.pas

// Main form destruction steps
// ===========================
// 1. Destroy all child forms
// 2. Wait until all the child forms are closed before closing the main form and terminating the application.


procedure GlobalCEFApp_OnContextInitialized;
begin
  if (MainForm <> nil) then MainForm.PostCustomMessage(CEF_INITIALIZED);
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                      := TCefApplication.Create;
  GlobalCEFApp.OnContextInitialized := GlobalCEFApp_OnContextInitialized;
  GlobalCEFApp.DisableFeatures      := 'NetworkService,OutOfBlinkCors';
end;

function TMainForm.PostCustomMessage(aMessage, wParam : cardinal; lParam : integer) : boolean;
{$IFDEF MSWINDOWS}
var
  TempHWND : HWND;
{$ENDIF}
begin
  Result := False;

  {$IFDEF MSWINDOWS}
  if (Handle <> nil) then
    begin
      TempHWND := FmxHandleToHWND(Handle);
      Result   := (TempHWND <> 0) and WinApi.Windows.PostMessage(TempHWND, aMessage, wParam, lParam);
    end;
  {$ENDIF}
end;

procedure TMainForm.CreateToolboxChild(const ChildCaption, URL: string);
var
  TempChild : TChildForm;
begin
  if (length(URL) > 0) then
    begin
      TempChild          := TChildForm.Create(self);
      TempChild.Caption  := ChildCaption;
      TempChild.Homepage := URL;
      TempChild.Show;
    end;
end;

function TMainForm.GetChildFormCount : integer;
var
  i : integer;
  TempComponent : TComponent;
begin
  Result := 0;
  i      := pred(ComponentCount);

  while (i >= 0) do
    begin
      TempComponent := Components[i];

      if (TempComponent <> nil) and
         (TempComponent is TChildForm) then
        inc(Result);

      dec(i);
    end;
end;

function TMainForm.GetChildClosing : boolean;
var
  i : integer;
  TempComponent : TComponent;
begin
  Result := false;
  i      := pred(ComponentCount);

  while (i >= 0) do
    begin
      TempComponent := Components[i];

      if (TempComponent <> nil) and
         (TempComponent is TChildForm) then
        begin
          if TChildForm(Components[i]).Closing then
            begin
              Result := True;
              exit;
            end
           else
            dec(i);
        end
       else
        dec(i);
    end;
end;

procedure TMainForm.OpenBtnClick(Sender: TObject);
begin
  CreateToolboxChild('Browser', AddressEdt.Text);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // The main form closes all child forms first and only after that it sets CanClose to True.
  if FClosing or ChildClosing then
    CanClose := FCanClose
   else
    begin
      FClosing := True;

      if (ChildFormCount = 0) then
        CanClose := True
       else
        begin
          CanClose          := False;
          ButtonPnl.Enabled := False;

          CloseAllChildForms;
        end;
    end;
end;

procedure TMainForm.CloseAllChildForms;
var
  i : integer;
  TempComponent : TComponent;
begin
  i := pred(ComponentCount);

  while (i >= 0) do
    begin
      TempComponent := Components[i];

      if (TempComponent <> nil) and
         (TempComponent is TChildForm) and
         not(TChildForm(Components[i]).Closing) then
        TChildForm(Components[i]).SendCloseMsg;

      dec(i);
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // TFMXApplicationService is used to handle custom Windows messages
  TFMXApplicationService.AddPlatformService;

  FCanClose       := False;
  FClosing        := False;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.GlobalContextInitialized then DoCEFInitialized;
end;

procedure TMainForm.DoCEFInitialized;
begin
  // Enable the GUI when the global context is initialized.
  Caption           := 'FMX ToolBox Browser';
  ButtonPnl.Enabled := True;
  cursor            := crDefault;
end;

procedure TMainForm.DoChildDestroyed;
begin
  // If there are no more child forms we can destroy the main form
  if FClosing and (ChildFormCount = 0) then
    begin
      FCanClose := True;
      PostCustomMessage(WM_CLOSE);
    end;
end;

procedure TMainForm.SendChildDestroyedMsg;
begin
  PostCustomMessage(CEF_CHILDDESTROYED);
end;

end.
