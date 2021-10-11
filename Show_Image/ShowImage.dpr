program ShowImage;

uses
  Forms,
  ShowImageForm in 'ShowImageForm.pas' {FormShow},
  StatisticsLibrary in 'StatisticsLibrary.pas',
  ScreenPrint in 'ScreenPrint.pas' {FormPrint},
  ColorLibrary in 'ColorLibrary.pas',
  HistogramLibrary in 'HistogramLibrary.pas',
  IEEE754 in 'IEEE754.pas',
  RealColorLibrary in 'RealColorLibrary.pas',
  GraphicsConversionsLibrary in 'GraphicsConversionsLibrary.pas',
  ImageProcessingPrimitives in 'ImageProcessingPrimitives.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormShow, FormShow);
  Application.CreateForm(TFormPrint, FormPrint);
  Application.Run;
end.
