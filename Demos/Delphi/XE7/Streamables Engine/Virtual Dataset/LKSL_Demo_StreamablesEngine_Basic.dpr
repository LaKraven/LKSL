program LKSL_Demo_StreamablesEngine_Basic;

uses
  System.StartUpCopy,
  FMX.Forms,
  LKSL.Demos.SEBasicMainForm in 'LKSL.Demos.SEBasicMainForm.pas' {frmMain},
  LKSL.Streamables.Base in '..\..\..\..\..\Source\Delphi\Lib\Streams\LKSL.Streamables.Base.pas',
  LKSL.Streams.System in '..\..\..\..\..\Source\Delphi\Lib\Streams\LKSL.Streams.System.pas',
  LKSL.Streams.Types in '..\..\..\..\..\Source\Delphi\Lib\Streams\LKSL.Streams.Types.pas',
  LKSL.Common.Types in '..\..\..\..\..\Source\Delphi\Lib\Common\LKSL.Common.Types.pas',
  LKSL.Demos.SEBasicStreamables in 'LKSL.Demos.SEBasicStreamables.pas',
  LKSL.Demos.SEBasicViewerForm in 'LKSL.Demos.SEBasicViewerForm.pas' {frmImageViewer},
  LKSL.Demos.SEBasicRecordFrame in 'LKSL.Demos.SEBasicRecordFrame.pas' {frameRecord: TFrame};

{$R *.res}

begin
  {$IFDEF DEBUG}ReportMemoryLeaksOnShutdown := True;{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmImageViewer, frmImageViewer);
  Application.Run;
end.
