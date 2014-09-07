program LKSL_Demo_EventEngine_Basic;

uses
  System.StartUpCopy,
  FMX.Forms,
  LKSL.Demos.EEBasicMainForm in 'LKSL.Demos.EEBasicMainForm.pas' {frmMain},
  LKSL.Demos.EEBasicTestEvents in 'LKSL.Demos.EEBasicTestEvents.pas',
  LKSL.Demos.EEBasicTestThread in 'LKSL.Demos.EEBasicTestThread.pas',
  LKSL.Events.Base in '..\..\..\..\..\Source\Delphi\Lib\Events\LKSL.Events.Base.pas',
  LKSL.Events.Streams in '..\..\..\..\..\Source\Delphi\Lib\Events\LKSL.Events.Streams.pas',
  LKSL.Threads.Base in '..\..\..\..\..\Source\Delphi\Lib\Threads\LKSL.Threads.Base.pas',
  LKSL.Streamables.Base in '..\..\..\..\..\Source\Delphi\Lib\Streams\LKSL.Streamables.Base.pas',
  LKSL.Streams.System in '..\..\..\..\..\Source\Delphi\Lib\Streams\LKSL.Streams.System.pas',
  LKSL.Streams.Types in '..\..\..\..\..\Source\Delphi\Lib\Streams\LKSL.Streams.Types.pas',
  LKSL.Common.Types in '..\..\..\..\..\Source\Delphi\Lib\Common\LKSL.Common.Types.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}ReportMemoryLeaksOnShutdown := True;{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
