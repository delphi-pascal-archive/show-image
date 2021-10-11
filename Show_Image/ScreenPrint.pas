unit ScreenPrint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormPrint = class(TForm)
    ButtonPrintBigImage: TButton;
    ButtonColorSpaceArray: TButton;
    ButtonHistograms: TButton;
    ButtonCancel: TButton;
    ButtonPrintSmallImage: TButton;
    procedure ButtonPrintBigImageClick(Sender: TObject);
    procedure ButtonHistogramsClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonColorSpaceArrayClick(Sender: TObject);
    procedure ButtonPrintSmallImageClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormPrint: TFormPrint;

implementation
{$R *.DFM}


USES
  ColorLibrary,              // TColorPlane
  HistogramLibrary,          // THistogram
  ImageProcessingPrimitives, // PrintBitmap
  Printers,
  ShowImageForm,
  StatisticsLibrary;

FUNCTION CenterText(s:  STRING):  INTEGER;
BEGIN
  RESULT := (Printer.PageWidth - Printer.Canvas.TextWidth(s)) DIV 2
END {CenterText};


PROCEDURE PrintFooterTimeStamp (CONST LeftMargin:  INTEGER);
  VAR
    s:  STRING;
BEGIN
    {Footer}
      Printer.Canvas.Font.Name := 'Arial';
      Printer.Canvas.Brush.Color := clWhite;
      Printer.Canvas.Font.Size := 8;
      s := FormatDateTime('m/d/yy  h:nn', Now);
      Printer.Canvas.TextOut(LeftMargin,
                             Printer.PageHeight-Printer.Canvas.TextHeight('X'),
                             s);
END {PrinterFooterTimeStamp};


procedure TFormPrint.ButtonPrintBigImageClick(Sender: TObject);
  VAR
    iFromLeftMargin    :  INTEGER;
    iPrintedImageWidth :  INTEGER;
    jDelta             :  INTEGER;
    jFromTopOfPage     :  INTEGER;
    jPrintedImageHeight:  INTEGER;
    s                  :  STRING;

begin
  Screen.Cursor := crHourGlass;
  TRY
    Printer.Orientation := poLandscape;
    Printer.BeginDoc;
      // Header
      Printer.Canvas.Font.Size := 12;
      Printer.Canvas.Font.Name := 'Arial';
      jDelta := Printer.Canvas.TextHeight('X');
      jFromTopOfPage := 3*jDelta;
      s := FormShow.LabelFilename.Caption   + ', ' +
           FormShow.LabelAttributes.Caption;

      IF LENGTH(FormShow.LabelColorPlane.Caption) > 0
      THEN AppendStr(s, ', ' + FormShow.LabelColorPlane.Caption);
      Printer.Canvas.TextOut(CenterText(s), jFromTopOfPage, s);
      jFromTopOfPage := 5*jDelta;

      // Image position and size
      iFromLeftMargin    := MulDiv(Printer.PageWidth, 12, 100);  // 12%
      iPrintedImageWidth := MulDiv(Printer.PageWidth, 80, 100);  // 80%
      // maintain aspect ratio of bitmap
      jPrintedImageHeight := MulDiv(FormShow.BitmapPrint.Height,
                                    iPrintedImageWidth,
                                    FormShow.BitmapPrint.Width);

      // Won't fit without adjusting height
      IF   jPrintedImageHeight + 2*jFromTopOfpage > Printer.PageHeight
      THEN BEGIN
        jPrintedImageHeight := Printer.PageHeight - 2*jFromTopOfPage;
        iPrintedImageWidth  := MulDiv(FormShow.BitmapPrint.Width,
                                      jPrintedImageHeight,
                                      FormShow.BitmapPrint.Height);
        iFromLeftMargin := (Printer.PageWidth - iPrintedImageWidth) DIV 2
      END;

      // Single pixel outline
      Printer.Canvas.Brush.Color := clBlack;
      Printer.Canvas.Rectangle(iFromLeftMargin-1, jFromTopOfPage-1,
                               iFromLeftMargin + iPrintedImageWidth + 2,
                               jFromTopOfPage  + jPrintedImageHeight +2);

      // Print Image
      PrintBitmap (Printer.Canvas,
                  Rect(iFromLeftMargin, jFromTopOfPage,
                       iFromLeftMargin + iPrintedImageWidth,
                       jFromTopOfPage  + jPrintedImageHeight),
                  FormShow.BitmapPrint);

      PrintFooterTimeStamp (iFromLeftMargin);

    Printer.EndDoc;
  FINALLY
    Screen.Cursor := crDefault
  END;

  Close;
end;


procedure TFormPrint.ButtonHistogramsClick(Sender: TObject);
 VAR
    iFromLeftMargin    :  INTEGER;
    iPrintedImageWidth :  INTEGER;
    jDelta             :  INTEGER;
    jFromTopOfPage     :  INTEGER;
    jPrintedImageHeight:  INTEGER;
    s                  :  STRING;
    Statistics         :  TDescriptiveStatistics;

  PROCEDURE PrintHistogram(CONST ColorPlane:  TColorplane);
    VAR
      BitmapHistogram:  TBitmap;
      Histogram      :  THistogram;
      PrintArea      :  TRect;
  BEGIN
    jFromTopOfPage := jFromTopOfPage + jDelta;
    s := GetColorPlaneString(ColorPlane);
    Printer.Canvas.Font.Size := 12;
    Printer.Canvas.Font.Name := 'Arial';
    Printer.Canvas.Brush.Color := clWhite;
    Printer.Canvas.TextOut(iFromLeftMargin, jFromTopOfPage, s);
    jFromTopOfPage := jFromTopOfPage + jDelta;

    PrintArea :=  Rect(iFromLeftMargin, jFromTopOfPage,
                       iFromLeftMargin + iPrintedImageWidth,
                       jFromTopOfPage  + jPrintedImageHeight);

    Statistics := TDescriptiveStatistics.Create;

    BitmapHistogram := TBitmap.Create;
    BitmapHistogram.Width  := 256;
    BitmapHistogram.Height := 3*BitmapHistogram.Width DIV 4;
    TRY
      {Clear histogram}
      BitmapHistogram.Canvas.Brush.Color := RGB(240,240,240);
      BitmapHistogram.Canvas.FillRect(Rect(0, 0,
                                     BitmapHistogram.Width, BitmapHistogram.Height));
      Histogram := THistogram.Create;
      TRY
        GetHistogram(FormShow.BitmapBig, ColorPlane, Histogram);
        DrawHistogram(ColorPlane, Histogram, BitmapHistogram.Canvas);
      FINALLY
        Histogram.Free
      END;

      {Single pixel outline}
      Printer.Canvas.Brush.Color := clBlack;
      Printer.Canvas.Rectangle(PrintArea.Left-1, PrintArea.Top-1,
                               PrintArea.Left + iPrintedImageWidth  + 2,
                               PrintArea.Top  + jPrintedImageHeight + 2);

      {Print Image}
      PrintBitmap (Printer.Canvas, PrintArea,  BitmapHistogram);

      GetBitmapStatistics(FormShow.BitmapBig, ColorPlane, Statistics);
      jFromTopOfPage := jFromTopOfPage + jPrintedImageHeight;

      IF   ColorPlane <> cpHueHSV
      THEN BEGIN
        s := Format('Min=%0.f  Max=%0.f  Mean=%.1f  S.D.=%.1f',
                    [Statistics.MinValue,  Statistics.MaxValue,
                     Statistics.MeanValue, Statistics.StandardDeviation]);
        Printer.Canvas.Brush.Color := clWhite;
        Printer.Canvas.Font.Size := 8;
        Printer.Canvas.Font.Name := 'Arial';
        Printer.Canvas.TextOut(iFromLeftMargin, jFromTopOfPage, s)
      END

    FINALLY
      BitmapHistogram.Free;
      Statistics.Free;
    END

  END {PrintHistogram};

  {Copied only to be quick -- someday get rid of this}
  PROCEDURE PrintImagePlane(CONST ColorPlane:  TColorplane;
                            CONST Invert:  BOOLEAN);
    VAR
      BitmapProcessed:  TBitmap;
      PrintArea      :  TRect;
  BEGIN
    jFromTopOfPage := jFromTopOfPage + jDelta;
    s := GetColorPlaneString(ColorPlane);
    Printer.Canvas.Brush.Color := clWhite;
    Printer.Canvas.TextOut(iFromLeftMargin, jFromTopOfPage, s);
    jFromTopOfPage := jFromTopOfPage + jDelta;

    PrintArea :=  Rect(iFromLeftMargin, jFromTopOfPage,
                       iFromLeftMargin + iPrintedImageWidth,
                       jFromTopOfPage  + jPrintedImageHeight);

    BitmapProcessed := TBitmap.Create;
    TRY
      BitmapProcessed.Height      := FormShow.BitmapBig.Height;
      BitmapProcessed.Width       := FormShow.BitmapBig.Width;
      BitmapProcessed.PixelFormat := FormShow.BitmapBig.PixelFormat;


      BitmapProcessed := ExtractImagePlane (ColorPlane,
                                            (ColorPlane = cpRGB),
                                            Invert,
                                            FormShow.BitmapBig);

      // Single pixel outline
      Printer.Canvas.Brush.Color := clBlack;
      Printer.Canvas.Rectangle(PrintArea.Left-1, PrintArea.Top-1,
                               PrintArea.Left + iPrintedImageWidth  + 2,
                               PrintArea.Top  + jPrintedImageHeight + 2);

      // Print Image
      PrintBitmap (Printer.Canvas, PrintArea,  BitmapProcessed);

      jFromTopOfPage := jFromTopOfPage + jPrintedImageHeight
    FINALLY
      BitmapProcessed.Free
    END;

  END {PrintImagePlane};


begin
  Screen.Cursor := crHourGlass;
  TRY
    Printer.Orientation := poLandscape;
    Printer.BeginDoc;
      {Header}
      Printer.Canvas.Font.Size := 12;
      Printer.Canvas.Font.Name := 'Arial';
      jDelta := Printer.Canvas.TextHeight('X');
      jFromTopOfPage := 2*jDelta;
      s := FormShow.LabelFilename.Caption   + ', ' +
           FormShow.LabelAttributes.Caption;
      Printer.Canvas.TextOut(CenterText(s), jFromTopOfPage, s);

      {Image position and size}

      iPrintedImageWidth := 25{%} * Printer.PageWidth DIV 100;
      jPrintedImageHeight := 3*iPrintedImageWidth DIV 4;  {4:3 aspect ratio}

      iFromLeftMargin    :=  8{%} * Printer.PageWidth DIV 100;
      jFromTopOfPage     := 3*jDelta;
      PrintHistogram(cpRed);
      PrintHistogram(cpGreen);
      PrintHistogram(cpBlue);

      iFromLeftMargin    :=  iFromLeftMargin + 5 * iPrintedImageWidth DIV 4;
      jFromTopOfPage     := 3*jDelta;
      PrintHistogram(cpIntensity);
      PrintHistogram(cpLightness);
      PrintImagePlane(cpRGB, FALSE);

      iFromLeftMargin    :=  iFromLeftMargin + 5 * iPrintedImageWidth DIV 4;
      jFromTopOfPage     := 3*jDelta;
      PrintHistogram(cpHueHSV);
      PrintHistogram(cpSaturationHSV);
      PrintHistogram(cpValue);

      PrintFooterTimeStamp (8{%} * Printer.PageWidth DIV 100);

    Printer.EndDoc;
  FINALLY
    Screen.Cursor := crDefault
  END;

  Close
end;


procedure TFormPrint.ButtonCancelClick(Sender: TObject);
begin
  Close
end;


procedure TFormPrint.ButtonColorSpaceArrayClick(Sender: TObject);
  VAR
    iFromLeftMargin    :  INTEGER;
    iPrintedImageWidth :  INTEGER;
    jDelta             :  INTEGER;
    jFromTopOfPage     :  INTEGER;
    jPrintedImageHeight:  INTEGER;
    s                  :  STRING;


  PROCEDURE PrintImagePlane(CONST ColorPlane:  TColorplane);
    VAR
      BitmapProcessed:  TBitmap;
      PrintArea      :  TRect;
  BEGIN
    jFromTopOfPage := jFromTopOfPage + jDelta;
    s := GetColorPlaneString(ColorPlane);
    Printer.Canvas.Brush.Color := clWhite;
    Printer.Canvas.TextOut(iFromLeftMargin, jFromTopOfPage, s);
    jFromTopOfPage := jFromTopOfPage + jDelta;

    PrintArea :=  Rect(iFromLeftMargin, jFromTopOfPage,
                       iFromLeftMargin + iPrintedImageWidth,
                       jFromTopOfPage  + jPrintedImageHeight);

    BitmapProcessed := TBitmap.Create;
    TRY
      BitmapProcessed.Height      := FormShow.BitmapBig.Height;
      BitmapProcessed.Width       := FormShow.BitmapBig.Width;
      BitmapProcessed.PixelFormat := FormShow.BitmapBig.PixelFormat;

      BitmapProcessed := ExtractImagePlane (ColorPlane,
                                            (ColorPlane = cpRGB)    OR
                                            (ColorPlane = cpHueHSV) OR
                                            (ColorPlane = cpHueHLS),
                                            FALSE,
                                            FormShow.BitmapBig);

      {Single pixel outline}
      Printer.Canvas.Brush.Color := clBlack;
      Printer.Canvas.Rectangle(PrintArea.Left-1, PrintArea.Top-1,
                               PrintArea.Left + iPrintedImageWidth  + 2,
                               PrintArea.Top  + jPrintedImageHeight + 2);

      {Print Image}
      PrintBitmap (Printer.Canvas, PrintArea,  BitmapProcessed);

      jFromTopOfPage := jFromTopOfPage + jPrintedImageHeight
    FINALLY
      BitmapProcessed.Free
    END;

  END {PrintImagePlane};

begin
  Screen.Cursor := crHourGlass;
  TRY
    Printer.Orientation := poLandscape;
    Printer.BeginDoc;
      {Header}
      Printer.Canvas.Font.Size := 12;
      Printer.Canvas.Font.Name := 'Arial';
      jDelta := Printer.Canvas.TextHeight('X');
      jFromTopOfPage := 2*jDelta;
      s := FormShow.LabelFilename.Caption   + ', ' +
           FormShow.LabelAttributes.Caption;
      Printer.Canvas.TextOut(CenterText(s), jFromTopOfPage, s);

      // Image position and size

      iPrintedImageWidth := 25{%} * Printer.PageWidth DIV 100;
      // Make sure image fits, even if distortion is necessary
      // Assumes aspect ratio of 4:3
      jPrintedImageHeight := 3*iPrintedImageWidth DIV 4;

      iFromLeftMargin    :=  8{%} * Printer.PageWidth DIV 100;
      jFromTopOfPage     := 3*jDelta;
      PrintImagePlane(cpRed);
      PrintImagePlane(cpGreen);
      PrintImagePlane(cpBlue);

      iFromLeftMargin    :=  iFromLeftMargin + 5 * iPrintedImageWidth DIV 4;
      jFromTopOfPage     := 3*jDelta;
      PrintImagePlane(cpIntensity);
      PrintImagePlane(cpLightness);
      PrintImagePlane(cpRGB);

      iFromLeftMargin    :=  iFromLeftMargin + 5 * iPrintedImageWidth DIV 4;
      jFromTopOfPage     := 3*jDelta;
      PrintImagePlane(cpHueHSV);
      PrintImagePlane(cpSaturationHSV);
      PrintImagePlane(cpValue);

      PrintFooterTimeStamp (8{%} * Printer.PageWidth DIV 100);

    Printer.EndDoc;
  FINALLY
    Screen.Cursor := crDefault
  END;

  Close
end;

procedure TFormPrint.ButtonPrintSmallImageClick(Sender: TObject);
 VAR
    iFromLeftMargin    :  INTEGER;
    iPrintedImageWidth :  INTEGER;
    jDelta             :  INTEGER;
    jFromTopOfPage     :  INTEGER;
    jPrintedImageHeight:  INTEGER;
    s                  :  STRING;

begin
  Screen.Cursor := crHourGlass;
  TRY
    Printer.Orientation := poPortrait;
    Printer.BeginDoc;
      // Header
      Printer.Canvas.Font.Size := 12;
      Printer.Canvas.Font.Name := 'Arial';
      jDelta := Printer.Canvas.TextHeight('X');
      jFromTopOfPage := 3*jDelta;
      s := FormShow.LabelFilename.Caption   + ', ' +
           FormShow.LabelAttributes.Caption;

      IF LENGTH(FormShow.LabelColorPlane.Caption) > 0
      THEN AppendStr(s, ', ' + FormShow.LabelColorPlane.Caption);
      Printer.Canvas.TextOut(CenterText(s), jFromTopOfPage, s);
      jFromTopOfPage := 5*jDelta;

      // Image position and size
      iFromLeftMargin    := 30{%} * Printer.PageWidth DIV 100;
      iPrintedImageWidth := 40{%} * Printer.PageWidth DIV 100;
      // maintain aspect ratio of bitmap
      jPrintedImageHeight := FormShow.BitmapPrint.Height*iPrintedImageWidth DIV
                             FormShow.BitmapPrint.Width;
      // Single pixel outline
      Printer.Canvas.Brush.Color := clBlack;
      Printer.Canvas.Rectangle(iFromLeftMargin-1, jFromTopOfPage-1,
                               iFromLeftMargin + iPrintedImageWidth + 2,
                               jFromTopOfPage  + jPrintedImageHeight +2);

      // Print Image
      PrintBitmap (Printer.Canvas,
                  Rect(iFromLeftMargin, jFromTopOfPage,
                       iFromLeftMargin + iPrintedImageWidth,
                       jFromTopOfPage  + jPrintedImageHeight),
                  FormShow.BitmapPrint);

      iFromLeftMargin    := 8{%} * Printer.PageWidth DIV 100;
      PrintFooterTimeStamp (iFromLeftMargin);

    Printer.EndDoc;
  FINALLY
    Screen.Cursor := crDefault
  END;

  Close;

end;

end.
