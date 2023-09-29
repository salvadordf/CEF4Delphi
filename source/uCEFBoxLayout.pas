unit uCEFBoxLayout;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFLayout;

type
  /// <summary>
  /// A Layout manager that arranges child views vertically or horizontally in a
  /// side-by-side fashion with spacing around and between the child views. The
  /// child views are always sized according to their preferred size. If the
  /// host's bounds provide insufficient space, child views will be clamped.
  /// Excess space will not be distributed. Methods must be called on the browser
  /// process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefBoxLayout">Implements TCefBoxLayout</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_box_layout_capi.h">CEF source file: /include/capi/views/cef_box_layout_capi.h (cef_box_layout_t)</see></para>
  /// </remarks>
  TCefBoxLayoutRef = class(TCefLayoutRef, ICefBoxLayout)
    protected
      /// <summary>
      /// Set the flex weight for the given |view|. Using the preferred size as the
      /// basis, free space along the main axis is distributed to views in the ratio
      /// of their flex weights. Similarly, if the views will overflow the parent,
      /// space is subtracted in these ratios. A flex of 0 means this view is not
      /// resized. Flex values must not be negative.
      /// </summary>
      procedure SetFlexForView(const view: ICefView; flex: Integer);
      /// <summary>
      /// Clears the flex for the given |view|, causing it to use the default flex
      /// specified via TCefBoxLayoutSettings.default_flex.
      /// </summary>
      procedure ClearFlexForView(const view: ICefView);

    public
      /// <summary>
      /// Returns a ICefBoxLayout instance using a PCefBoxLayout data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefBoxLayout;
  end;

implementation

uses
  uCEFMiscFunctions;

procedure TCefBoxLayoutRef.SetFlexForView(const view: ICefView; flex: Integer);
begin
  PCefBoxLayout(FData)^.set_flex_for_view(PCefBoxLayout(FData),
                                          CefGetData(view),
                                          ord(flex));
end;

procedure TCefBoxLayoutRef.ClearFlexForView(const view: ICefView);
begin
  PCefBoxLayout(FData)^.clear_flex_for_view(PCefBoxLayout(FData),
                                            CefGetData(view));
end;

class function TCefBoxLayoutRef.UnWrap(data: Pointer): ICefBoxLayout;
begin
  if (data <> nil) then
    Result := Create(data) as ICefBoxLayout
   else
    Result := nil;
end;

end.

