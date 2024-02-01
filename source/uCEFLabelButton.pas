unit uCEFLabelButton;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFButton;

type
  /// <summary>
  /// LabelButton is a button with optional text and/or icon. Methods must be
  /// called on the browser process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_label_button_capi.h">CEF source file: /include/capi/views/cef_label_button_capi.h (cef_label_button_t)</see></para>
  /// </remarks>
  TCefLabelButtonRef = class(TCefButtonRef, ICefLabelButton)
    protected
      /// <summary>
      /// Returns this LabelButton as a MenuButton or NULL if this is not a
      /// MenuButton.
      /// </summary>
      function  AsMenuButton : ICefMenuButton;
      /// <summary>
      /// Sets the text shown on the LabelButton. By default |text| will also be
      /// used as the accessible name.
      /// </summary>
      procedure SetText(const text_: ustring);
      /// <summary>
      /// Returns the text shown on the LabelButton.
      /// </summary>
      function  GetText : ustring;
      /// <summary>
      /// Sets the image shown for |button_state|. When this Button is drawn if no
      /// image exists for the current state then the image for
      /// CEF_BUTTON_STATE_NORMAL, if any, will be shown.
      /// </summary>
      procedure SetImage(button_state: TCefButtonState; const image: ICefImage);
      /// <summary>
      /// Returns the image shown for |button_state|. If no image exists for that
      /// state then the image for CEF_BUTTON_STATE_NORMAL will be returned.
      /// </summary>
      function  GetImage(button_state: TCefButtonState): ICefImage;
      /// <summary>
      /// Sets the text color shown for the specified button |for_state| to |color|.
      /// </summary>
      procedure SetTextColor(for_state: TCefButtonState; color: TCefColor);
      /// <summary>
      /// Sets the text colors shown for the non-disabled states to |color|.
      /// </summary>
      procedure SetEnabledTextColors(color: TCefColor);
      /// <summary>
      /// Sets the font list. The format is "<FONT_FAMILY_LIST>,[STYLES] <SIZE>",
      /// where:
      /// - FONT_FAMILY_LIST is a comma-separated list of font family names,
      /// - STYLES is an optional space-separated list of style names (case-sensitive
      ///   "Bold" and "Italic" are supported), and
      /// - SIZE is an integer font size in pixels with the suffix "px".
      ///
      /// Here are examples of valid font description strings:
      /// - "Arial, Helvetica, Bold Italic 14px"
      /// - "Arial, 14px"
      /// </summary>
      procedure SetFontList(const font_list: ustring);
      /// <summary>
      /// Sets the horizontal alignment; reversed in RTL. Default is
      /// CEF_HORIZONTAL_ALIGNMENT_CENTER.
      /// </summary>
      procedure SetHorizontalAlignment(alignment: TCefHorizontalAlignment);
      /// <summary>
      /// Reset the minimum size of this LabelButton to |size|.
      /// </summary>
      procedure SetMinimumSize(const size_: TCefSize);
      /// <summary>
      /// Reset the maximum size of this LabelButton to |size|.
      /// </summary>
      procedure SetMaximumSize(const size_: TCefSize);

    public
      /// <summary>
      /// Returns a ICefLabelButton instance using a PCefLabelButton data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefLabelButton;
      /// <summary>
      /// Create a new LabelButton. A |delegate| must be provided to handle the button
      /// click. |text| will be shown on the LabelButton and used as the default
      /// accessible name.
      /// </summary>
      class function CreateLabelButton(const delegate: ICefButtonDelegate; const text: ustring): ICefLabelButton;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFMenuButton, uCEFImage;

function TCefLabelButtonRef.AsMenuButton : ICefMenuButton;
begin
  Result := TCefMenuButtonRef.UnWrap(PCefLabelButton(FData)^.as_menu_button(PCefLabelButton(FData)));
end;

procedure TCefLabelButtonRef.SetText(const text_: ustring);
var
  TempText : TCefString;
begin
  TempText := CefString(text_);
  PCefLabelButton(FData)^.set_text(PCefLabelButton(FData), @TempText);
end;

function TCefLabelButtonRef.GetText : ustring;
begin
  Result := CefStringFreeAndGet(PCefLabelButton(FData)^.get_text(PCefLabelButton(FData)));
end;

procedure TCefLabelButtonRef.SetImage(button_state: TCefButtonState; const image: ICefImage);
begin
  PCefLabelButton(FData)^.set_image(PCefLabelButton(FData), button_state, CefGetData(image));
end;

function TCefLabelButtonRef.GetImage(button_state: TCefButtonState): ICefImage;
begin
  Result := TCefImageRef.UnWrap(PCefLabelButton(FData)^.get_image(PCefLabelButton(FData), button_state));
end;

procedure TCefLabelButtonRef.SetTextColor(for_state: TCefButtonState; color: TCefColor);
begin
  PCefLabelButton(FData)^.set_text_color(PCefLabelButton(FData), for_state, color);
end;

procedure TCefLabelButtonRef.SetEnabledTextColors(color: TCefColor);
begin
  PCefLabelButton(FData)^.set_enabled_text_colors(PCefLabelButton(FData), color);
end;

procedure TCefLabelButtonRef.SetFontList(const font_list: ustring);
var
  TempFontList : TCefString;
begin
  TempFontList := CefString(font_list);
  PCefLabelButton(FData)^.set_font_list(PCefLabelButton(FData), @TempFontList);
end;

procedure TCefLabelButtonRef.SetHorizontalAlignment(alignment: TCefHorizontalAlignment);
begin
  PCefLabelButton(FData)^.set_horizontal_alignment(PCefLabelButton(FData), alignment);
end;

procedure TCefLabelButtonRef.SetMinimumSize(const size_: TCefSize);
begin
  PCefLabelButton(FData)^.set_minimum_size(PCefLabelButton(FData), @size_);
end;

procedure TCefLabelButtonRef.SetMaximumSize(const size_: TCefSize);
begin
  PCefLabelButton(FData)^.set_maximum_size(PCefLabelButton(FData), @size_);
end;

class function TCefLabelButtonRef.UnWrap(data: Pointer): ICefLabelButton;
begin
  if (data <> nil) then
    Result := Create(data) as ICefLabelButton
   else
    Result := nil;
end;

class function TCefLabelButtonRef.CreateLabelButton(const delegate : ICefButtonDelegate;
                                                    const text     : ustring): ICefLabelButton;
var
  TempText   : TCefString;
  TempButton : PCefLabelButton;
begin
  Result := nil;

  if (delegate <> nil) then
    begin
      TempText   := CefString(text);
      TempButton := cef_label_button_create(CefGetData(delegate), @TempText);

      if (TempButton <> nil) then
        Result := Create(TempButton) as ICefLabelButton;
    end;
end;

end.

