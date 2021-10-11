//  Histogram and Image Statistics Library
//
//  efg, April 1998

UNIT HistogramLibrary;

INTERFACE

  USES
    Windows,              // TRGBTriple
    Graphics,             // TCanvas
    StatisticsLibrary,    // TDescriptiveStatitics
    ColorLibrary;         // TColorPlane

  TYPE
    THistoArray  = ARRAY[BYTE] OF INTEGER;

    TRGBHistoArray =
      RECORD
        Red      :  THistoArray;
        Green    :  THistoArray;
        Blue     :  THistoArray;
        Intensity:  THistoArray;
      END;

    THistogram =
      CLASS(TObject)
        PRIVATE
          FHistogram:  THistoArray;
          FUNCTION GetCount:  INTEGER;

        PUBLIC
          CONSTRUCTOR Create;

          PROCEDURE Clear;
          PROCEDURE Increment(CONST index:  BYTE);
          PROCEDURE GetStatistics(VAR N:  INTEGER;
                                  VAR Minimum,Maximum:  BYTE;
                                  VAR Mode, Median:  BYTE;
                                  VAR Mean, StandardDeviation,
                                      Skewness, Kurtosis:  DOUBLE);
          PROPERTY Frequency:  THistoArray READ FHistogram WRITE FHistogram;
          PROPERTY Count    :  INTEGER     READ GetCount;
      END;

    TRGBHistograms =
      CLASS
        PROTECTED
          FRed  :  THistogram;
          FGreen:  THistogram;
          FBlue :  THistogram;
          FIntensity:  THistogram;

        PUBLIC
          CONSTRUCTOR Create;
          DESTRUCTOR  Destroy;  OVERRIDE;

          PROCEDURE Increment(CONST RGBTriple:  TRGBTriple);

          PROPERTY Red:   THistogram  READ FRed;
          PROPERTY Green: THistogram  READ FGreen;
          PROPERTY Blue:  THistogram  READ FBlue;
      END;

    TRGBStatistics =
    CLASS(TObject)
      PROTECTED
        FRed  :  TDescriptiveStatistics;
        FGreen:  TDescriptiveStatistics;
        FBlue :  TDescriptiveStatistics;

        FUNCTION PixelCount:  INTEGER;

      PUBLIC
        PROPERTY    Count:  INTEGER  READ PixelCount;
        PROPERTY    Red  :  TDescriptiveStatistics READ FRed;
        PROPERTY    Green:  TDescriptiveStatistics READ FGreen;
        PROPERTY    Blue :  TDescriptiveStatistics READ FBlue;

        CONSTRUCTOR Create;
        DESTRUCTOR  Destroy;  OVERRIDE;
        PROCEDURE NextRGBTriple(CONST rgb:  TRGBTriple);
        PROCEDURE ProcessBitmap(CONST Bitmap:  TBitmap);
        PROCEDURE ResetValues;
      END;

  PROCEDURE DrawHistogram(CONST ColorPlane:  TColorPlane;
                          CONST Histogram:  THistogram;
                          CONST Canvas:  TCanvas);
  PROCEDURE GetHistogram(CONST Bitmap:  TBitmap;
                         CONST ColorPlane:  TColorPlane;
                         VAR   Histogram:  THistogram);
  FUNCTION  GetRGBHistograms(CONST Bitmap:  TBitmap;
                             CONST Rect:  TRect):  TRGBHistograms;


  // Get statistics for single image plane.  Use TRGBStatistics.ProcessBitmap
  // to get R, G, and B statistics for any number of 24-bit images.
  PROCEDURE GetBitmapStatistics(CONST Bitmap:  TBitmap;
                                CONST ColorPlane:  TColorPlane;
                                VAR Statistics:  TDescriptiveStatistics);

IMPLEMENTATION

  USES
    Dialogs,                    // ShowMessage
    ImageProcessingPrimitives,  // RGBTriple
    Math,                       // MaxIntValue, IntPwr
    SysUtils;                   // pByteArray, Exception

  TYPE
    EHistogramError  = CLASS(Exception);
    EStatisticsError = CLASS(Exception);


  // == THistogram ======================================================

  // This Histogram class is specialized for image processing applications.
  // The frequency distribution is assumed to be for values ONLY in the
  // range 0..255.


  FUNCTION THistogram.GetCount:  INTEGER;
    VAR
      i:  BYTE;
  BEGIN
    RESULT := 0;
    FOR i := Low(THistoArray) TO High(THistoArray) DO
      INC(RESULT, FHistogram[i])
  END {GetCount};


  CONSTRUCTOR THistogram.Create;
  BEGIN
    Clear
  END {Create};


  PROCEDURE THistogram.Clear;
    VAR
      i:  BYTE;
  BEGIN
    FOR i :=  Low(THistoArray) TO High(THistoArray) DO
      FHistogram[i] := 0
  END {Clear};


  PROCEDURE THistogram.GetStatistics(VAR N:  INTEGER;
                                     VAR Minimum,Maximum:  BYTE;
                                     VAR Mode, Median:  BYTE;
                                     VAR Mean, StandardDeviation,
                                         Skewness, Kurtosis:  DOUBLE);
    VAR
      Cumulative  :  INTEGER;
      i           :  BYTE;
      MaxFrequency:  INTEGER;
      M2          :  EXTENDED;
      M3          :  EXTENDED;
      M3Sum       :  EXTENDED;
      M4          :  EXTENDED;
      M4Sum       :  EXTENDED;
      x           :  EXTENDED;
      xSum        :  EXTENDED;  // Use floats to avoid integer overflow
      xSqrSum     :  EXTENDED;
  BEGIN
    N := 0;

    Minimum := 0;
    WHILE (FHistogram[Minimum] = 0) AND (Minimum < 255)
    DO  INC(Minimum);

    Maximum := 255;
    WHILE (FHistogram[Maximum] = 0) AND (Maximum > 0)
    DO  DEC(Maximum);

    // Mode is value with highest frequency.
    // For now, don't worry about a "tie".
    Mode := Minimum;
    MaxFrequency := FHistogram[Minimum];
    FOR i := Minimum+1 TO Maximum DO
    BEGIN
      IF  FHistogram[i] > MaxFrequency
      THEN BEGIN
        Mode := i;
        MaxFrequency := FHistogram[i]
      END
    END;

    // Calculate Mean and Standard Deviation
    xSum    := 0.0;
    xSqrSum := 0.0;
    FOR i := Minimum TO Maximum DO
    BEGIN
      INC(N, FHistogram[i]);
      x := i;
      xSum    := xSum    + FHistogram[i]*x;
      xSqrSum := xSqrSum + FHistogram[i]*SQR(x)
    END;

    IF  N = 0
    THEN Mean := 0.0
    ELSE Mean := xSum / N;

    IF   N < 2
    THEN BEGIN
      StandardDeviation := 0.0;    // should be a NAN someday
      Skewness := 0.0;
      Kurtosis := 0.0;
    END
    ELSE BEGIN
      StandardDeviation := SQRT( (xSqrSum - N*SQR(Mean)) / (N-1) );

      // Standard Deviation is related to moment M2
      M2 := SQR(StandardDeviation) * (N-1) / N;

      // Calculate third and fourth moments
      M3Sum := 0.0;
      M4Sum := 0.0;
      FOR i := Minimum TO Maximum DO
      BEGIN
        x := i;

        M3Sum := M3Sum + FHistogram[i]*IntPower(x - Mean, 3);
        M4Sum := M4Sum + FHistogram[i]*IntPower(x - Mean, 4);
      END;

      M3 := M3Sum / N;
      M4 := M4Sum / N;

      IF   M2 = 0.0
      THEN BEGIN
        Skewness := 0.0;  // eventually use NAN here
        Kurtosis := 0.0;
      END
      ELSE BEGIN
        Skewness := M3 / Power(M2, 1.5);
        Kurtosis := M4 / SQR(M2)
      END
    END;

    // Median is value with half of values above and below.
    Cumulative := 0;
    i := Minimum;
    WHILE (Cumulative < N DIV 2) AND (i < 255) DO
    BEGIN
      INC(Cumulative, FHistogram[i]);
      INC(i)
    END;
    Median := i;

  END {GetStatistics};


  PROCEDURE THistogram.Increment(CONST index:  BYTE);
  BEGIN
    INC(FHistogram[index])
  END {Increment};

  
  // ==  RGB Histograms =================================================

  CONSTRUCTOR TRGBHistograms.Create;
  BEGIN
    FRed       := THistogram.Create;
    FGreen     := THistogram.Create;
    FBlue      := THistogram.Create;
    FIntensity := THistogram.Create
  END {Create};


  DESTRUCTOR TRGBHistograms.Destroy;
  BEGIN
    FRed.Free;
    FGreen.Free;
    FBlue.Free;
    FIntensity.Free
  END {Destroy};


  PROCEDURE TRGBHistograms.Increment(CONST RGBTriple:  TRGBTriple);
  BEGIN
    WITH RGBTriple DO
    BEGIN
      FRed.Increment(rgbtRed);
      FGreen.Increment(rgbtGreen);
      FBlue.Increment(rgbtBlue)
    END;
    FIntensity.Increment( RGBTripleToIntensity(RGBTriple) );
  END {Increment};




  // ==  RGB Statistics =================================================

  CONSTRUCTOR TRGBStatistics.Create;
  BEGIN
    FRed   := TDescriptiveStatistics.Create;
    FGreen := TDescriptiveStatistics.Create;
    FBlue  := TDescriptiveStatistics.Create;

    FRed.ResetValues;
    FGreen.ResetValues;
    FBlue.ResetValues
  END {Create};


  DESTRUCTOR TRGBStatistics.Destroy;
  BEGIN
    FRed.Free;
    FGreen.Free;
    FBlue.Free
  END {Destroy};


  FUNCTION TRGBStatistics.PixelCount;
  BEGIN
    RESULT := FRed.Count
  END {PixelCount};


  // Use this method to look at given set of pixels, one-by-one
  PROCEDURE TRGBStatistics.NextRGBTriple(CONST rgb:  TRGBTriple);
  BEGIN
    WITH rgb DO
    BEGIN
      FRed.NextValue(rgbtRed);
      FGreen.NextValue(rgbtGreen);
      FBlue.NextValue(rgbtBlue);
    END
  END {NextRGBTriple};


  // Get statistics for complete image
  PROCEDURE TRGBStatistics.ProcessBitmap(CONST Bitmap:  TBitmap);
    VAR
      i  :  INTEGER;
      j  :  INTEGER;
      row:  pRGBTripleArray;
  BEGIN
    IF   Bitmap.PixelFormat <> pf24bit
    THEN RAISE EStatisticsError.Create('Can only process 24-bit image');

    FOR j := 0 TO Bitmap.Height-1 DO
    BEGIN
      row := Bitmap.Scanline[j];
      FOR i := 0 TO Bitmap.Width-1 DO
      BEGIN
        WITH row[i] DO
        BEGIN
          FRed.NextValue(rgbtRed);
          FGreen.NextValue(rgbtGreen);
          FBlue.NextValue(rgbtBlue);
        END
      END
    END
  END {ProcessBitmap};


  PROCEDURE TRGBStatistics.ResetValues;
  BEGIN
    FRed.ResetValues;
    FGreen.ResetValues;
    FBlue.ResetValues
  END {ResetValues};


  // =====================================================================

  PROCEDURE DrawHistogram(CONST ColorPlane:  TColorPlane;
                          CONST Histogram :  THistogram;
                          CONST Canvas    :  TCanvas);
    CONST
      MajorTickSize = 8;

    VAR
      BarLength:  INTEGER;
      Delta    :  INTEGER;
      Color    :  TColor;
      i        :  INTEGER;
      j        :  INTEGER;
      Height   :  INTEGER;
      MaxValue :  INTEGER;
      Width    :  INTEGER;
  BEGIN
    Color := clBlack;    {avoid compiler warning about initialization}

    Height   := Canvas.ClipRect.Bottom;
    Width    := Canvas.ClipRect.Right;

    MaxValue := MaxIntValue(Histogram.Frequency);

    // For now only paint on a canvas exactly 256 pixels wide.  If
    // MaxValue is zero, array was not filled in correctly and is ignored.
    IF   (Width = 256) AND (MaxValue > 0)
    THEN BEGIN
      FOR i := Low(THistoArray) TO High(THistoArray) DO
      BEGIN
        CASE ColorPlane OF
          cpRGB,
          cpY,
          cpIntensity:   Color := RGB(i, i, i);

          cpRed:         Color := RGB(i, 0, 0);
          cpGreen:       Color := RGB(0, i, 0);
          cpBlue:        Color := RGB(0, 0, i);
          cpHueHSV:
              // Rescale Hue from 0..255 to 0..359
              Color := RGBTripleToColor( HSVtoRGBTriple(MulDiv(i, 359, 255), 255, 255) );

          cpSaturationHSV:
                         Color := clBlack;
          cpValue:       Color := RGB(i, i, i);

          cpHueHLS:
              // Rescale Hue from 0..255 to 0..359;  Half Lightness
              Color := RGBTripleToColor( HLStoRGBTriple(MulDiv(i, 359, 255), 128, 255) );

          cpSaturationHLS:
                         Color := clBlack;

          cpLightness:   Color := RGB(i, i, i);

          cpCyan:        Color := RGB(i, i, i);
          cpMagenta:     Color := RGB(i, i, i);
          cpYellow:      Color := RGB(i, i, i);
          cpBlack:       Color := RGB(255-i, 255-i, 255-i)

        END;

        Canvas.Pen.Color := Color;
        BarLength := ROUND(Height*Histogram.Frequency[i] / MaxValue);
        Canvas.MoveTo(i, Height-1);
        Canvas.LineTo(i, Height-1-BarLength)
      END;

      Canvas.Pen.Color := clDkGray;
      // Vertical Lines for visual estimation
      FOR i := 0 TO 25 DO
      BEGIN
        Canvas.MoveTo(10*i, Height-1);
        IF   i MOD 5 = 0
        THEN Delta := MajorTickSize
        ELSE Delta := MajorTickSize DIV 2;
        Canvas.LineTo(10*i, Height-1-Delta);
      END;

      // Horizontal Lines
      FOR j := 0 TO 4 DO
      BEGIN
        Canvas.MoveTo(      0, j*Height DIV 5);
        Canvas.LineTo(Width-1, j*Height DIV 5);
      END;

      Canvas.Brush.Style := bsClear;
      Canvas.TextOut(2,2, 'Max = ' + IntToStr(MaxValue));
      Canvas.TextOut(2, Height-Canvas.TextHeight('X') - MajorTickSize, '0');
      Canvas.TextOut(Width-Canvas.TextWidth('250 '),
                     Height-Canvas.TextHeight('X') - MajorTickSize, '250')
    END
  END {DrawHistogram};


  PROCEDURE GetHistogram(CONST Bitmap    :  TBitmap;
                         CONST ColorPlane:  TColorPlane;
                         VAR Histogram   :  THistogram);
    VAR
      C,M,Y,K:  INTEGER;    //  CMYK color space
      H,S,V  :  INTEGER;    //  HSV color
      i      :  INTEGER;
      index  :  INTEGER;
      j      :  INTEGER;
      L      :  INTEGER;
      Row    :  pRGBTripleArray;
  BEGIN
    IF   (Bitmap.PixelFormat <> pf24bit)
    THEN RAISE EHistogramError.Create('GetHistogram:  ' +
               'Bitmap must be 24-bit.');

    index := 0;   // avoid compiler warning about initialization

    Histogram.Clear;

    // Step through each row of image.
    FOR j := Bitmap.Height-1 DOWNTO 0 DO
    BEGIN
      Row  := Bitmap.Scanline[j];

      FOR i := Bitmap.Width-1 DOWNTO 0 DO
      BEGIN
        CASE ColorPlane OF
          cpY:          index := RGBTripleToY(Row[i]);

          cpRGB,
          cpIntensity:  index := RGBTripleToIntensity(Row[i]);

          cpRed:        index := Row[i].rgbtRed;
          cpGreen:      index := Row[i].rgbtGreen;
          cpBlue:       index := Row[i].rgbtBlue;

          cpHueHSV:
            BEGIN
              RGBTripleToHSV(Row[i], H,S,V);

              // Rescale from 0..359 to 0..255
              index := MulDiv(H, 255, 359)
            END;

          cpSaturationHSV:
            BEGIN
              RGBTripleToHSV(Row[i], H,S,V);
              index := S
            END;

          cpValue:
            BEGIN
              RGBTripleToHSV(Row[i], H,S,V);
              index := V
            END;

         cpHueHLS:
            BEGIN
              RGBTripleToHLS(Row[i], H,L,S);
              index :=  MulDiv(H, 255, 360)
            END;

          cpSaturationHLS:
            BEGIN
              RGBTripleToHLS(Row[i], H,L,S);
              index := S
            END;

          cpCyan:
            BEGIN
              RGBTripleToCMYK(Row[i], C,M,Y,K);
              index := C
            END;

          cpMagenta:
            BEGIN
              RGBTripleToCMYK(Row[i], C,M,Y,K);
              index := M
            END;

          cpYellow:
            BEGIN
              RGBTripleToCMYK(Row[i], C,M,Y,K);
              index := Y
            END;

          cpBlack:
            BEGIN
              RGBTripleToCMYK(Row[i], C,M,Y,K);
              index := K
            END;

          cpLightness:
            BEGIN
              index := RGBTripleToLightness(Row[i])
            END

        END;

        // Debug color conversion problems
        IF   (index < 0) OR (index > 255)
        THEN BEGIN
          ShowMessage ('Histogram index = ' + IntToStr(index) +
                       ' (i,j) = (' + IntToStr(i) + ', ' +
                                      IntToStr(j) + ')');
          index := 0;
        END;

        Histogram.Increment(index)
      END

    END

  END {GetHistogram};


  // Single function to create all three R, G and B Histograms for all or part
  // of a bitmap.  Use Bitmap.Canvas.ClipRect as the second parameters to look
  // at the whole bitmap.
  //
  // The calling routine must free the resulting TRGBHistograms object.
  FUNCTION GetRGBHistograms(CONST Bitmap:  TBitmap;
                            CONST Rect:  TRect):  TRGBHistograms;
    VAR
      i      :  INTEGER;
      j      :  INTEGER;
      Row    :  pRGBTripleArray;
  BEGIN
    IF   Bitmap.PixelFormat <> pf24bit
    THEN RAISE EHistogramError.Create('GetRGBHistograms:  ' +
               'Bitmap must be 24-bit.');

    RESULT := TRGBHistograms.Create;

    // Step through each requested row of image.
    FOR j := Rect.Top TO Rect.Bottom-1 DO
    BEGIN
      Row  := Bitmap.Scanline[j];

      // Look at each requested pixel within a row and increment histogram stats.
      FOR i := Rect.Left TO Rect.Right-1 DO
      BEGIN
        RESULT.Increment(Row[i])
      END

    END

  END {GetRGBHistograms};


  PROCEDURE GetBitmapStatistics(CONST Bitmap:  TBitmap;
                                CONST ColorPlane:  TColorPlane;
                                VAR Statistics:  TDescriptiveStatistics);
    VAR
      C,M,Y,K:  INTEGER;
      H,S,V  :  INTEGER;      // color coordinates
      i      :  INTEGER;
      j      :  INTEGER;
      L      :  INTEGER;
      Row    :  pRGBTripleArray;
      Value  :  INTEGER;
  BEGIN
    IF   Bitmap.PixelFormat <> pf24bit
    THEN RAISE EHistogramError.Create('GetBitmapStatistics:  ' +
               'Bitmap must be 24-bit color.');
 
    // Step through each row of image.
    FOR j := Bitmap.Height-1 DOWNTO 0 DO
    BEGIN
      Row  := Bitmap.Scanline[j];

      FOR i := Bitmap.Width-1 DOWNTO 0 DO
      BEGIN
        CASE ColorPlane OF
          cpRGB,
          cpIntensity:   value := RGBTripleToIntensity(Row[i]);

          cpRed:        value := Row[i].rgbtRed;
          cpGreen:      value := Row[i].rgbtGreen;
          cpBlue:       value := Row[i].rgbtBlue;

          cpHueHSV:
            BEGIN
              RGBTripleToHSV(Row[i], H,S,V);
              value := H
            END;

          cpSaturationHSV:
           BEGIN
              RGBTripleToHSV(Row[i], H,S,V);
              value := S
            END;

          cpValue:
            BEGIN
              RGBTripleToHSV(Row[i], H,S,V);
              value := V
            END;

          cpHueHLS:
            BEGIN
              RGBTripleToHLS(Row[i], H,L,S);
              value := H
            END;

          cpSaturationHLS:
           BEGIN
              RGBTripleToHLS(Row[i], H,L,S);
              value := S
            END;

          cpLightness:
            BEGIN
              RGBTripleToHLS(Row[i], H,L,S);
              value := L
            END;

          cpCyan:
            BEGIN
              RGBTripleToCMYK(Row[i], C,M,Y,K);
              value := C
            END;

          cpMagenta:
            BEGIN
              RGBTripleToCMYK(Row[i], C,M,Y,K);
              value := M
            END;

          cpYellow:
            BEGIN
              RGBTripleToCMYK(Row[i], C,M,Y,K);
              value := Y
            END;

          cpBlack:
            BEGIN
              RGBTripleToCMYK(Row[i], C,M,Y,K);
              value := K
            END;

          ELSE
            value := 0
        END;

        Statistics.NextValue(value)

      END

    END

  END {GetBitmapStatistics};


END.
