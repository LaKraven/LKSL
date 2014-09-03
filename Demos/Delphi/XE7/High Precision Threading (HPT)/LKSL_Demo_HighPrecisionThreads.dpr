program LKSL_Demo_HighPrecisionThreads;

uses
  System.StartUpCopy,
  FMX.Forms,
  LKSL.Demos.HPTMainForm in 'LKSL.Demos.HPTMainForm.pas' {frmMain},
  LKSL.Threads.Base in '..\..\..\..\Source\Delphi\Lib\Threads\LKSL.Threads.Base.pas',
  LKSL.Demos.HPTTestThread in 'LKSL.Demos.HPTTestThread.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}ReportMemoryLeaksOnShutdown := True;{$ENDIF}
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
