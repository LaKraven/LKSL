program LKSL_Demo_EventThreadPooling;

uses
  Vcl.Forms,
  Demo.EventThreadPooling.MainForm in 'Demo.EventThreadPooling.MainForm.pas' {Form1},
  Demo.EventThreadPooling.Events in 'Demo.EventThreadPooling.Events.pas',
  Demo.EventThreadPooling.EventThreads in 'Demo.EventThreadPooling.EventThreads.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}ReportMemoryLeaksOnShutdown := True;{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
