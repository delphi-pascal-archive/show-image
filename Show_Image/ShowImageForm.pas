//  "Show Image"
//
//  Earl F. Glynn, April 1998.  Updated September 1998.

unit ShowImageForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  ExtDlgs,
  ColorLibrary;              // TColorPlane


type
  TFormShow = class(TForm)
    Button1: TButton;
    ImageBig: TImage;
    LabelLocation: TLabel;
    LabelIntensity: TLabel;
    LabelRGB: TLabel;
    LabelHSV: TLabel;
    LabelFilename: TLabel;
    ImageHistogram: TImage;
    RadioGroupColorPlane: TRadioGroup;
    LabelLightness: TLabel;
    RadioGroupColorSpace: TRadioGroup;
    ButtonPrint: TButton;
    LabelAttributes: TLabel;
    LabelColorPlane: TLabel;
    SavePictureDialog: TSavePictureDialog;
    ButtonSave: TButton;
    LabelCMYK: TLabel;
    CheckBoxStretch: TCheckBox;
    LabelHLS: TLabel;
    LabelYIQ: TLabel;
    CheckBoxInvert: TCheckBox;
    CheckBoxMonochrome: TCheckBox;
    LabelUniqueColors: TLabel;
    OpenPictureDialog: TOpenPictureDialog;

    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroupColorPlaneClick(Sender: TObject);
    procedure RadioGroupColorSpaceClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonPrintClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ImageBigMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CheckBoxStretchClick(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CheckBoxInvertClick(Sender: TObject);
  private
    ColorPlane:  TColorPlane;
    PROCEDURE UpdateDisplay (CONST ColorPlane:  TColorPlane);
  public
    BitmapPrint:  TBitmap;
    BitmapBig  :  TBitMap;
  end;

var
  FormShow: TFormShow;

implementation
{$R *.DFM}

uses
{$IFDEF GIF}
  GIFImage,                   // TGIFImage
{$ENDIF}
  RealColorLibrary,           // RGBToHSV
  GraphicsConversionsLibrary, // LoadGraphicFile
  HistogramLibrary,           // THistogram
  ImageProcessingPrimitives,  // GetBitmapDimensionsString
  IniFiles,                   // TIniFile
  ScreenPrint;

  CONST
    KeywordSetup     = 'Setup';
    KeywordDirectory = 'Directory';

procedure TFormShow.Button1Click(Sender: TObject);
  VAR
    Filename:  STRING;
    IniFile :  TIniFile;
    NewPath :  STRING;
begin
  IF   OpenPictureDialog.Execute
  THEN BEGIN
    BitmapBig.Free;
    BitmapBig := TBitmap.Create;
    BitmapBig := LoadGraphicsFile(OpenPictureDialog.Filename);

    // Update INI file for next time
    Filename := ChangeFileExt(ParamStr(0), '.INI');
    NewPath := ExtractFilePath(OpenPictureDialog.Filename);
    OpenPictureDialog.InitialDir := NewPath;
    IniFile := TIniFile.Create(Filename);
    TRY
      Inifile.WriteString(KeywordSetup, KeywordDirectory, NewPath)
    FINALLY
      IniFile.Free
    END;

    // Flush INI cache
    WritePrivateProfileString(NIL, NIL, NIL, pChar(Filename));

    LabelFilename.Caption := OpenPictureDialog.Filename;

    // Update this label before forcing PixelFormat
    LabelAttributes.Caption := GetBitmapDimensionsString(BitmapBig);

    // Force to pf24bit if necessary.  Make other logic much more simple by
    // not having to worry aboug various PixelFormats.
    IF   BitmapBig.PixelFormat <> pf24bit
    THEN BitmapBig.PixelFormat := pf24bit;

    // Clear old histogram (some of the following may be unnecessary now that
    // pf24bit is being forced and it's no longer necessary to hide certain
    // features.
    ImageHistogram.Visible := TRUE;
    ImageHistogram.Picture := NIL;
    RadioGroupColorSpace.ItemIndex := 0;  {Select RGB color space}
    RadioGroupColorSpace.Enabled := TRUE;
    RadioGroupColorPlane.Enabled := TRUE;
    ButtonPrint.Enabled := TRUE;
    FormPrint.ButtonHistograms.Enabled := TRUE;
    FormPrint.ButtonColorSpaceArray.Enabled := TRUE;
    RadioGroupColorSpace.Visible := TRUE;
    RadioGroupColorPlane.Visible := TRUE;
    UpdateDisplay(cpRGB);

    LabelUniqueColors.Caption := 'Unique Colors = ' +
                                 IntToStr( CountColors(BitmapBig) );
  END
end;



procedure TFormShow.ImageBigMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  VAR
    CC,MM,YY,KK:  INTEGER;      // CMYK variables
    Intensity  :  INTEGER;
    Lightness  :  INTEGER;
    RGB        :  TColor;
    RGBTriple  :  TRGBTriple;
    R,G,B      :  BYTE;
    H,S,V, L   :  TReal;
    xActual    :  INTEGER;
    yActual    :  INTEGER;
begin
  IF   CheckBoxStretch.Checked
  THEN BEGIN
    xActual := MulDiv(X, BitmapBig.Width,  ImageBig.Width);
    yActual := MulDiv(Y, BitmapBig.Height, ImageBig.Height)
  END
  ELSE BEGIN
    xActual := X;
    yActual := Y
  END;

  // If bitmap is smaller than image area, only show values if inside bitmap.
  IF   (xActual < ImageBig.Picture.Bitmap.Width) AND
       (yActual < ImageBig.Picture.Bitmap.Height)
  THEN BEGIN

    LabelLocation.Caption :=  '[I, J] = [' + IntToStr(xActual) + ', ' +
                                             IntToStr(yActual) + ']';

    // Get values from original bitmap, which is not necessarily being displayed
    // in RGB format.  Use Pixels so this will work with pf8bit or pf24bit BMPs.
    RGB :=  BitmapBig.Canvas.Pixels[xActual, yActual];
    R := GetRValue(RGB);
    G := GetGValue(RGB);
    B := GetBValue(RGB);
    RGBTriple := ColorToRGBTriple(RGB);

    LabelRGB.Caption := Format('[R, G, B] = [%d, %d, %d] = [%.3f,  %.3f,  %.3f]',
                        [ R, G, B, R/255, G/255, B/255]);

    RGBToHSV(GetRValue(RGB) / 255,
             GetGValue(RGB) / 255,
             GetBValue(RGB) / 255, H, S, V);
    LabelHSV.Caption := Format('[H, S, V] = [%.0f, %.0f, %.0f] = [ %.1f,  %.3f,  %.3f]',
                              [255*H/360, 255*S, 255*V, H, S, V]);

    RGBToHLS(GetRValue(RGB) / 255,
             GetGValue(RGB) / 255,
             GetBValue(RGB) / 255, H, L, S);
    LabelHLS.Caption := Format('[H, L, S] = [%.0f, %.0f, %.0f] = [ %.1f,  %.3f,  %.3f]',
                              [255*H/360, 255*L, 255*S, H, L, S]);

    RGBTripleToCMYK(RGBTriple, CC,MM,YY,KK);
    LabelCMYK.Caption := Format('[C, M, Y, K] = [%d, %d, %d, %d]',
                         [ CC,MM,YY,KK ]);

{// Windows API calls don't seem to work
    LabelCMYK.Caption := Format('[C, M, Y, K] = [%d, %d, %d, %d]',
                        [ GetCValue(ColorToRGB(RGB)),
                          GetMValue(RGB),
                          GetYValue(RGB),
                          GetKValue(RGB) ]);
}

    Intensity  := RGBTripleToIntensity(RGBTriple);
    LabelIntensity.Caption := Format('Intensity = %d = %.3f',
                                     [Intensity, Intensity/255]);

    Lightness := RGBTripleToLightness(RGBTriple);
    LabelLightness.Caption := Format('Lightness = %d = %.3f',
                                     [Lightness, Lightness/255]);

    // See [Foley96, pp. 589]
    LabelYIQ.Caption := Format('[Y, I, Q] = [%.3f,  %.3f,  %.3f]',
                               [(0.299*R + 0.587*G + 0.114*B) / 255,
                                (0.596*R - 0.275*G - 0.321*B) / 255,
                                (0.212*B - 0.523*G + 0.311*B) / 255 ])
  END
  ELSE BEGIN
    LabelLocation.Caption := '';
    LabelRGB.Caption := '';
    LabelHSV.Caption := '';
    LabelHLS.Caption := '';
    LabelCMYK.Caption := '';
    LabelIntensity.Caption := '';
    LabelLightness.Caption := '';
    LabelYIQ.Caption := '';
  END
end;



procedure TFormShow.FormCreate(Sender: TObject);
  VAR
    IniFile :  TIniFile;
{$IFDEF GIF}
    s       :  STRING;
{$ENDIF}
begin
{$IFDEF GIF}
  s := OpenPictureDialog.Filter + '|GIFs (*.gif)|*.gif';
  Insert('*.gif;',s, POS('(',s)+1);  // Put GIF in "All" selection
  Insert('*.gif;',s, POS('|',s)+1);
  OpenPictureDialog.Filter := s;
{$ENDIF}

  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.INI'));
  TRY
    OpenPictureDialog.InitialDir := Inifile.ReadString(KeywordSetup,
                                                       KeywordDirectory,
                             ExtractFilePath(ParamStr(0)))
  FINALLY
    IniFile.Free
  END;

  ImageHistogram.Picture := NIL;

  // Allocate this bitmap so histograms still work without image present
  BitmapBig := TBitmap.Create;
  BitmapBig.Width  := ImageBig.Width;
  BitmapBig.Height := ImageBig.Height;
  BitmapBig.PixelFormat := pf24bit;
  ImageBig.Picture.Graphic := BitmapBig;

  BitmapPrint := TBitmap.Create;
  BitmapPrint.Width  := ImageBig.Width;
  BitmapPrint.Height := ImageBig.Height;
  BitmapPrint.PixelFormat := pf24bit;


end;

procedure TFormShow.RadioGroupColorPlaneClick(Sender: TObject);
begin
  ImageHistogram.Canvas.Brush.Color := clLtGray;
  // Clear old histogram
  ImageHistogram.Canvas.FillRect(ImageHistogram.Canvas.ClipRect);

  ColorPlane := cpIntensity;   // avoid compiler warning
  CASE RadioGroupColorSpace.ItemIndex OF
    0:  // csRGB
        BEGIN
          CASE RadioGroupColorPlane.ItemIndex OF
            0:  ColorPlane := cpRGB;
            1:  ColorPlane := cpRed;
            2:  ColorPlane := cpGreen;
            3:  ColorPlane := cpBlue
          END;
          CheckBoxMonochrome.Visible := TRUE
        END;

    1:  // csHSV
        BEGIN
          CASE RadioGroupColorPlane.ItemIndex OF
            0:  ColorPlane := cpHueHSV;
            1:  ColorPlane := cpSaturationHSV;
            2:  ColorPlane := cpValue
          END;
          CheckBoxMonochrome.Visible := (ColorPlane = cpHueHSV);
        END;

    2:  // csHLS
        BEGIN
          CASE RadioGroupColorPlane.ItemIndex OF
            0:  ColorPlane := cpHueHLS;
            1:  ColorPlane := cpLightness;
            2:  ColorPlane := cpSaturationHLS;
          END;
          CheckBoxMonochrome.Visible := (ColorPlane = cpHueHLS);
        END;

    3:  // csCMYK
        BEGIN
          CASE RadioGroupColorPlane.ItemIndex OF
            0:  ColorPlane := cpCyan;
            1:  ColorPlane := cpMagenta;
            2:  ColorPlane := cpYellow;
            3:  ColorPlane := cpBlack;
          END;
          CheckBoxMonochrome.Visible := FALSE;
        END;
  END;

  UpdateDisplay (ColorPlane);

  ButtonSave.Enabled := TRUE
end;


PROCEDURE TFormShow.UpdateDisplay (CONST ColorPlane:  TColorPlane);
  VAR
    BitmapProcessed:  TBitmap;
    Histogram      :  THistogram;
BEGIN
  Screen.Cursor := crHourGlass;

  TRY
    LabelColorPlane.Caption := GetColorPlaneString(ColorPlane);

    Histogram := THistogram.Create;
    TRY
      GetHistogram(BitmapBig, ColorPlane, Histogram);
      DrawHistogram(ColorPlane, Histogram, ImageHistogram.Canvas)
    FINALLY
      Histogram.Free
    END;

    BitmapProcessed := ExtractImagePlane(ColorPlane,
                                         CheckBoxMonochrome.Visible AND
                                         NOT CheckBoxMonochrome.Checked,
                                         CheckBoxInvert.Checked,
                                         BitmapBig);
    TRY
      BitmapPrint.Assign(BitmapProcessed);
      ImageBig.Picture.Graphic := BitmapProcessed;
    FINALLY
      BitmapProcessed.Free
    END
  FINALLY
    Screen.Cursor := crDefault
  END
END {UpdateDisplay};


procedure TFormShow.RadioGroupColorSpaceClick(Sender: TObject);
begin
  RadioGroupColorPlane.Items.Clear;

  CASE RadioGroupColorSpace.ItemIndex OF
    0:  // RGB
        BEGIN
          WITH RadioGroupColorPlane.Items DO
          BEGIN
            Add ('RGB Composite');
            Add ('Red');
            Add ('Green');
            Add ('Blue')
          END
        END;

    1:  // HSV
        BEGIN
          WITH RadioGroupColorPlane.Items DO
          BEGIN
            Add ('Hue');
            Add ('Saturation');
            Add ('Value')
          END
        END;

    2:  // HLS
        BEGIN
          WITH RadioGroupColorPlane.Items DO
          BEGIN
            Add ('Hue');
            Add ('Lightness');
            Add ('Saturation')
          END
        END;

    3:  // CMYK
        BEGIN
          WITH RadioGroupColorPlane.Items DO
          BEGIN
            Add ('Cyan');
            Add ('Magenta');
            Add ('Yellow');
            Add ('blacK')
          END
        END;
  END;

  RadioGroupColorPlane.ItemIndex := 0
end;


procedure TFormShow.FormDestroy(Sender: TObject);
begin
  BitmapPrint.Free;
  BitmapBig.Free;
end;



procedure TFormShow.ButtonPrintClick(Sender: TObject);
begin
  FormPrint.ShowModal
end;


procedure TFormShow.ButtonSaveClick(Sender: TObject);
begin
  IF   SavePictureDialog.Execute
  THEN BitmapPrint.SaveToFile(SavePictureDialog.Filename)
end;


procedure TFormShow.CheckBoxStretchClick(Sender: TObject);
begin
  ImageBig.Stretch := CheckBoxStretch.Checked
end;


// A better way would be to trap CM_MouseLeave of the ImageBig TImage, but
// this requires a slightly modified version of a TImage.
procedure TFormShow.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  LabelLocation.Caption := '';
  LabelRGB.Caption := '';
  LabelHSV.Caption := '';
  LabelHLS.Caption := '';
  LabelCMYK.Caption := '';
  LabelIntensity.Caption := '';
  LabelLightness.Caption := '';
  LabelYIQ.Caption := ''
end;

procedure TFormShow.CheckBoxInvertClick(Sender: TObject);
begin
  UpdateDisplay (ColorPlane);
end;

end.
