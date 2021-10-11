// efg, September 1998

unit GraphicsConversionsLibrary;

interface

  USES
    Graphics;

  FUNCTION LoadGraphicsFile(CONST Filename:  STRING):  TBitmap;

implementation

  USES
{$IFDEF GIF}
    GIFImage,    // TGIFImage
{$ENDIF}
    JPEG,        // TJPEGImage
    SysUtils;    // FileExists


  // Create TBitmap from BMP, JPG, WMF, EMF or GIF disk file.
  // Could be easily extended to other image types.
  FUNCTION LoadGraphicsFile(CONST Filename:  STRING):  TBitmap;
    VAR
      Extension:  STRING;
{$IFDEF GIF}
      GIFImage :  TGIFImage;
{$ENDIF}
      Icon     :  TIcon;
      JPEGImage:  TJPEGImage;
      Metafile :  TMetafile;
  BEGIN
    RESULT := NIL;  // In case anything goes wrong

    IF   FileExists(Filename)
    THEN BEGIN
      Extension := UpperCase( COPY(Filename, LENGTH(Filename)-2, 3) );

      // Quick and dirty check that file type is OK
      ASSERT( (Extension = 'BMP')  OR
              (Extension = 'EMF')  OR
{$IFDEF GIF}
              (Extension = 'GIF')  OR
{$ENDIF}
              (Extension = 'ICO')  OR
              (Extension = 'JPG')  OR
              (Extension = 'WMF') );

      RESULT := TBitmap.Create;

      // BMP File -- no additional work to get TBitmap
      IF   Extension = 'BMP'
      THEN RESULT.LoadFromFile(Filename);

{$IFDEF GIF}
      // GIF File
      IF   Extension = 'GIF'
      THEN BEGIN
        GIFImage := TGIFImage.Create;
        TRY
          GIFImage.LoadFromFile(Filename);
          RESULT.Height      := GIFImage.Height;
          RESULT.Width       := GIFImage.Width;
          RESULT.PixelFormat := pf24bit;
          RESULT.Canvas.Draw(0,0, GIFImage)
        FINALLY
          GIFImage.Free
        END
      END;
{$ENDIF}

      // ICO File
      IF   Extension = 'ICO'
      THEN BEGIN
        Icon := TIcon.Create;
        TRY
          TRY
            Icon.LoadFromFile(Filename);
            RESULT.Height      := Icon.Height;
            RESULT.Width       := Icon.Width;
            RESULT.PixelFormat := pf24bit;    // avoid palette problems
            RESULT.Canvas.Draw(0,0, Icon)
            EXCEPT
            // Ignore problem icons, e.g., Stream read errors
          END;

        FINALLY
          Icon.Free
        END
      END;

      // JPG File
      IF   Extension = 'JPG'
      THEN BEGIN
        JPEGImage := TJPEGImage.Create;
        TRY
          JPEGImage.LoadFromFile(Filename);
          RESULT.Height      := JPEGImage.Height;
          RESULT.Width       := JPEGImage.Width;
          RESULT.PixelFormat := pf24bit;
          RESULT.Canvas.Draw(0,0, JPEGImage)
        FINALLY
          JPEGImage.Free
        END
      END;

      // Windows Metafiles, WMF or EMF
      IF   (Extension = 'WMF') OR
           (Extension = 'EMF')
      THEN BEGIN
        Metafile := TMetafile.Create;
        TRY
          Metafile.LoadFromFile(Filename);
          RESULT.Height      := Metafile.Height;
          RESULT.Width       := Metafile.Width;
          RESULT.PixelFormat := pf24bit;    // avoid palette problems
          RESULT.Canvas.Draw(0,0, Metafile)
        FINALLY
          Metafile.Free
        END
      END;

    END;

    // If Graphic is missing or invalid, create the "Red X"
    IF   RESULT = NIL
    THEN BEGIN
      RESULT.Height      := 32;
      RESULT.Width       := 32;
      RESULT.PixelFormat := pf24bit;
      WITH RESULT.Canvas DO BEGIN
        Pen.Color := clRed;
        Pen.Width := 3;
        MoveTo( 2, 2);
        LineTo(29,29);

        MoveTo( 2,29);
        LineTo(29, 2);
      END
    END

  END {LoadGraphicFile};


end.
