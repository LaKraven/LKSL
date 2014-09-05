program LKSL_Demo_EventEngine_Basic;

uses
  System.StartUpCopy,
  FMX.Forms,
  LKSL.Demos.EEBasicMainForm in 'LKSL.Demos.EEBasicMainForm.pas' {frmMain},
  LKSL.Threads.Base in '..\..\..\..\Source\Delphi\Lib\Threads\LKSL.Threads.Base.pas',
  LKSL.Events.Base in '..\..\..\..\Source\Delphi\Lib\Events\LKSL.Events.Base.pas',
  LKSL.Common.Types in '..\..\..\..\Source\Delphi\Lib\Common\LKSL.Common.Types.pas',
  LKSL.Demos.EEBasicTestEvents in 'LKSL.Demos.EEBasicTestEvents.pas',
  LKSL.Demos.EEBasicTestThread in 'LKSL.Demos.EEBasicTestThread.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}ReportMemoryLeaksOnShutdown := True;{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
