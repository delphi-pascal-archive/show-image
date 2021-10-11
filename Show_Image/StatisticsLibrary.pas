//
//  Statistics Library
//  Earl F. Glynn, August 1997
//  Parabolic fit added April 1998
//


UNIT StatisticsLibrary;

INTERFACE

  USES
    Math;  {MaxDouble}

  CONST
    NAN = MaxDouble / 2.0;   {Get "Not a Number" properly defined someday}

  TYPE

   {===================================================================}

   TDescriptiveStatistics =
   CLASS(TObject)
     PROTECTED
       FCount       :  INTEGER;

       FMaxValue    :  DOUBLE;
       FMinValue    :  DOUBLE;

       FSum         :  DOUBLE;
       FSumOfSquares:  DOUBLE;

     PUBLIC

       PROPERTY  Count   :  INTEGER   READ FCount;
       PROPERTY  MaxValue:  DOUBLE    READ FMaxValue;
       PROPERTY  MinValue:  DOUBLE    READ FMinValue;
       PROPERTY  Sum     :  DOUBLE    READ FSum;

       CONSTRUCTOR Create;

       PROCEDURE NextValue(CONST x:  DOUBLE);

       FUNCTION MeanValue        :  DOUBLE;
       FUNCTION StandardDeviation:  DOUBLE;

       PROCEDURE ResetValues;

   END;


  {===================================================================}

   // Formulas from HP-45 Applications Book, pp. 72, 77, 1974
   TLinearRegression =
   CLASS(TObject)
     PROTECTED
       FCount      :  INTEGER;

       FSumX       :  DOUBLE;
       FSumXSquared:  DOUBLE;

       FSumY       :  DOUBLE;
       FSumYSquared:  DOUBLE;

       FSumXY      :  DOUBLE;

     PUBLIC
       CONSTRUCTOR Create;
       FUNCTION  CorrelationCoefficient:  DOUBLE;
       PROCEDURE GetLineCoefficients(VAR A,B:  DOUBLE);
       FUNCTION  RSquared:  DOUBLE;
       PROCEDURE NextValue(CONST x, y:  DOUBLE);
       PROCEDURE ResetValues;

   END;


   {===================================================================}

   TParabolicFit =
   CLASS(TObject)
     PROTECTED
       FCount      :  INTEGER;
       FSumX       :  ARRAY[1..4] OF DOUBLE;
       FSumXY      :  ARRAY[1..3] OF DOUBLE;

     PUBLIC
       CONSTRUCTOR Create;
       PROCEDURE GetCoefficients(VAR A,B,C:  DOUBLE);
       PROCEDURE NextValue(CONST x, y:  DOUBLE);
       PROCEDURE ResetValues;
   END;

   {===================================================================}

   FUNCTION MedianInteger(x:  ARRAY OF INTEGER):  INTEGER;


IMPLEMENTATION

  USES
    SysUtils;   // EInvalidOp


{=====================================================================}


  // 2-by-2 determinant
  FUNCTION Det2(CONST a,b, c,d:  DOUBLE):  DOUBLE;
  BEGIN
    RESULT := a*d - b*c
  END {Det2};


  FUNCTION Det3(CONST a,b,c, d,e,f, g,h,i:  DOUBLE):  DOUBLE;
  BEGIN
    RESULT := a*Det2(e,f, h,i) - b*Det2(d,f, g,i) + c*Det2(d,e, g,h)
  END {Det3};


{==== TDescriptiveStatistics =========================================}

  CONSTRUCTOR TDescriptiveStatistics.Create;
  BEGIN
    ResetValues
  END {Create};


  PROCEDURE TDescriptiveStatistics.NextValue (CONST x:  DOUBLE);
  BEGIN
    INC(FCount);
    FSum := FSum + x;
    FSumOfSquares := FSumOfSquares + x*x;

    IF   x > FMaxValue
    THEN FmaxValue := x;

    IF  x < FMinValue
    THEN FMinValue := x
  END {NextNumber};


  FUNCTION TDescriptiveStatistics.MeanValue:  DOUBLE;
  BEGIN
    IF   FCount = 0
    THEN RESULT := NAN
    ELSE RESULT := FSum / FCount
  END {MeanValue};


  FUNCTION TDescriptiveStatistics.StandardDeviation:  DOUBLE;
  BEGIN
    IF   FCount < 2
    THEN RESULT := NAN
    ELSE RESULT := SQRT( (FSumOfSquares - (FSum*FSum / FCount)) /
                         (FCount - 1) )
  END {StandardDeviation};


  PROCEDURE TDescriptiveStatistics.ResetValues;
  BEGIN
    FCount := 0;

    FMinValue :=  MaxDouble;
    FMaxValue := -MaxDouble;

    FSum          := 0.0;
    FSumOfSquares := 0.0
  END {ResetValues};

{==== TLinearRegression ==============================================}

  CONSTRUCTOR TLinearRegression.Create;
  BEGIN
    ResetValues
  END {Create};


  {Formulas from HP-45 Applications Book, p. 77, 1974}
  PROCEDURE TLinearRegression.GetLineCoefficients(VAR A,B:  DOUBLE);
  BEGIN
    IF   FCount < 1
    THEN BEGIN
      A := NAN;
      B := NAN
    END
    ELSE BEGIN
       A := (FSumXY - FSumX*FSumY/FCount) /
            (FSumXSquared - FSumX*FSumX/FCount);
       B := (FSumY - A*FSumX)/FCount
    END
  END {GetLineCoefficients};


 {Formula from HP-45 Applications Book, p. 72, 1974}
  FUNCTION  TLinearRegression.CorrelationCoefficient:  DOUBLE;
    VAR
      numerator  :  DOUBLE;
      denominator:  DOUBLE;
  BEGIN
    IF   FCount < 2
    THEN RESULT := NAN
    ELSE BEGIN
      numerator   := FSumXY - FSumX*FSumY/FCount;
      denominator := SQRT( (FSumXSquared - FSumX*FSumX/FCount)*
                         (FSumYSquared - FSumY*FSumY/FCount) );
      TRY
        RESULT :=  numerator / denominator
      EXCEPT
        On EInvalidOp
          DO  IF   (numerator = 0.0) AND (denominator = 0.0)
              THEN RESULT := 1.0    // all zeros have "perfect" correlation
              ELSE RESULT := NAN;
      END
    END
  END {CorrelationCoefficient};


  FUNCTION  TLinearRegression.RSquared:  DOUBLE;
  BEGIN
    RESULT := 0.0    {FIX THIS}
  END {RSquared};


  PROCEDURE TLinearRegression.NextValue(CONST x, y:  DOUBLE);
  BEGIN
    INC(FCount);

    FSumX        := FSumX + x;
    FSumXSquared := FSumXSquared + x*x;

    FSumY        := FSumY + y;
    FSumYSquared := FSumYSquared + y*y;

    FSumXY       := FSumXY + x*y
  END {NextValue};


  PROCEDURE TLinearRegression.ResetValues;
  BEGIN
    FCount       := 0;

    FSumX        := 0.0;
    FSumXSquared := 0.0;

    FSumY        := 0.0;
    FSumYSquared := 0.0;

    FSumXY       := 0.0
  END {ResetValues};

{==== TParabolicFit ==================================================}

  // Use "brute force" for now

  CONSTRUCTOR TParabolicFit.Create;
  BEGIN
    ResetValues
  END {Create};


  PROCEDURE TParabolicFit.GetCoefficients(VAR A,B,C:  DOUBLE);
    VAR
      denominator:  DOUBLE;
  BEGIN
    denominator := Det3(FCount,   FSumX[1], FSumX[2],
                        FSumX[1], FSumX[2], FSumX[3],
                        FSumX[2], FSumX[3], FSumX[4]);
    IF   (FCount < 3) OR (ABS(denominator - 1E-5) <= 1E-5)
    THEN BEGIN
      A := NAN;
      B := NAN;
      C := NAN;
    END
    ELSE BEGIN
      A := Det3(FSumXY[1], FSumX[1], FSumX[2],
                FSumXY[2], FSumX[2], FSumX[3],
                FSumXY[3], FSumX[3], FSumX[4])  / denominator;

      B := Det3(FCount,   FSumXY[1], FSumX[2],
                FSumX[1], FSumXY[2], FSumX[3],
                FSumX[2], FSumXY[3], FSumX[4])  / denominator;

      C := Det3(FCount,   FSumX[1], FSumXY[1],
                FSumX[1], FSumX[2], FSumXY[2],
                FSumX[2], FSumX[3], FSumXY[3])  / denominator;
    END
  END {GetCoefficients};


  PROCEDURE TParabolicFit.NextValue(CONST x, y:  DOUBLE);
    VAR
      Term:  DOUBLE;
  BEGIN
    INC(FCount);

    FSumX[1] := FSumX[1] + x;

    Term := x*x;
    FSumX[2] := FSumX[2] + Term;


    Term := Term*x;
    FSumX[3] := FSumX[3] + Term;

    Term := Term*x;
    FSumX[4] := FSumX[4] + Term;

    FSumXY[1] := FSumXY[1] + y;

    Term := x*y;
    FSumXY[2] := FSumXY[2] + Term;

    Term := Term*x;
    FSumXY[3] := FSumXY[3] + Term
  END {NextValue};


  PROCEDURE TParabolicFit.ResetValues;
    VAR
      i:  INTEGER;
  BEGIN
    FCount       := 0;

    FOR i := Low(FSumx) TO High(FSumX) DO
      FSumX[i] := 0.0;

    FOR i := Low(FSumXY) TO High(FSumXY) DO
      FSumXY[i] := 0.0
  END {ResetValues};


{==== Median =========================================================}

  FUNCTION MedianInteger (x:  ARRAY OF INTEGER):  INTEGER;
    VAR
      i        :  INTEGER;
      j        :  INTEGER;
      Middle   :  INTEGER;
      Temporary:  INTEGER;
  BEGIN
    {Use truncated selection sort to find median}

    Middle := (High(x)+1) DIV 2;

    FOR i := 0 TO Middle DO
    BEGIN
      FOR j := 1 TO High(x)-i DO
      BEGIN
        IF   x[j] > x[j-1]
        THEN BEGIN
          Temporary := x[j];
          x[j] := x[j-1];
          x[j-1] := Temporary
        END
      END

    END;

    IF   Odd(High(x))
    THEN BEGIN
      {When High(x) is Odd, there are an even number of elements in array.
       Define median as average of two middle values.}
       RESULT := (x[middle] + x[middle-1]) DIV 2
    END
    ELSE BEGIN
       {When High(x) is Even, there are an odd number of elements in array.
       Median is the middle value.}
       RESULT := x[middle]
    END
  END {MedianInteger};


{=====================================================================}

END.
