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

unit uCEFLinuxTypes;

interface

{$IFDEF LINUX}
type
  PXEvent        = pointer;

  {$IFDEF FMX}
  // https://developer.gnome.org/glib/stable/glib-Basic-Types.htm
  gboolean       = longbool;
  gpointer       = pointer;
  gconstpointer  = pointer;
  gchar          = ansichar;
  guchar         = byte;
  gint           = integer;
  guint          = cardinal;
  gshort         = smallint;
  gushort        = word;
  gulong         = uint64;
  glong          = int64;
  gint8          = shortint;
  guint8         = byte;
  gint16         = int16;
  guint16        = uint16;
  gint32         = int32;
  guint32        = uint32;
  gint64         = int64;
  guint64        = uint64;
  gfloat         = single;
  gdouble        = double;
  gsize          = NativeUInt;
  gssize         = NativeInt;
  goffset        = gint64;
  gintptr        = NativeInt;
  guintptr       = NativeUInt;
  Pgchar         = ^gchar;
  PGData         = pointer;
  PGClosure      = pointer;
  TGConnectFlags = integer;
  GType          = gulong;
  PGtkWidget     = pointer;
  TGdkEventType  = int32;
  PGdkEventKey   = ^TGdkEventKey;
  PXDisplay      = pointer;
  PDisplay       = pointer;

 PXErrorEvent = ^TXErrorEvent;
 TXErrorEvent = record
      _type : longint;
      display : PDisplay;
      resourceid : uint64;
      serial : uint64;
      error_code : uint8;
      request_code : uint8;
      minor_code : uint8;
   end;

  PGTypeClass = ^TGTypeClass;
  TGTypeClass = record
    g_type : GType;
  end;

  TGTypeInstance = record
    g_class : PGTypeClass;
  end;

  TGObject = record
    g_type_instance : TGTypeInstance;
    ref_count       : guint;
    qdata           : PGData;
  end;

  TGdkDrawable = record
    parent_instance : TGObject;
  end;

  PGdkWindow = ^TGdkWindow;
  TGdkWindow = TGdkDrawable;
  TGdkEventKey = record
    _type            : TGdkEventType;
    window           : PGdkWindow;
    send_event       : gint8;
    time             : guint32;
    state            : guint;
    keyval           : guint;
    length           : gint;
    _string          : Pgchar;
    hardware_keycode : guint16;
    group            : guint8;
  end;

  TGCallBackProcedure = procedure;
  TGCallback          = procedure (para1: TGCallBackProcedure); cdecl;
  TGClosureNotify     = procedure(data: gpointer; closure: PGClosure); cdecl;

  TRGBQuad = record
    rgbBlue     : Byte;
    rgbGreen    : Byte;
    rgbRed      : Byte;
    rgbReserved : Byte;
  end;

  PGdkScreen = ^TGdkScreen;
  TGdkScreen = record
    parent_instance : TGObject;
  end;
{$ENDIF}
{$ENDIF}

implementation

end.
