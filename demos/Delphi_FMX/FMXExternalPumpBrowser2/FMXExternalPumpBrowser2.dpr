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
//        Copyright © 2018 Salvador Díaz Fau. All rights reserved.
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

program FMXExternalPumpBrowser2;

uses
  // FMX initializes GTK in the initialization section of some of its units and
  // that means that GTK is already initialized when the code in the DPR is
  // executed.
  // Chromium has to be initialized in a process with only one thread but GTK
  // creates several threads during its initialization. To avoid this problem
  // we have to initialize CEF before GTK.
  // uCEFLoader *MUST* be the first unit in the DPR file to make sure Chromium
  // is initialized before GTK.
  // uCEFLoader *MUST NOT* make any reference to any FMX unit to keep the right
  // initalization order.
  // Read the answer to this question for more more information :
  // https://stackoverflow.com/questions/52103407/changing-the-initialization-order-of-the-unit-in-delphi
  uCEFLoader in 'uCEFLoader.pas',
  System.StartUpCopy,
  FMX.Forms,
  uFMXExternalPumpBrowser2 in 'uFMXExternalPumpBrowser2.pas' {FMXExternalPumpBrowserFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMXExternalPumpBrowserFrm, FMXExternalPumpBrowserFrm);
  Application.Run;

  // The form needs to be destroyed *BEFORE* stopping the work scheduler.
  FMXExternalPumpBrowserFrm.Free;
end.
