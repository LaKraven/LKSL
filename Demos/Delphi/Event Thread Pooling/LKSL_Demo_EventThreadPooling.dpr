program LKSL_Demo_EventThreadPooling;

uses
  Vcl.Forms,
  Demo.EventThreadPooling.MainForm in 'Demo.EventThreadPooling.MainForm.pas' {Form1},
  Demo.EventThreadPooling.Events in 'Demo.EventThreadPooling.Events.pas',
  Demo.EventThreadPooling.EventThreads in 'Demo.EventThreadPooling.EventThreads.pas',
  LKSL.Common.Streams in '..\..\..\Source\Lib\Pascal\LKSL.Common.Streams.pas',
  LKSL.Common.Types in '..\..\..\Source\Lib\Pascal\LKSL.Common.Types.pas',
  LKSL.Events.Base in '..\..\..\Source\Lib\Pascal\LKSL.Events.Base.pas',
  LKSL.Events.Streams in '..\..\..\Source\Lib\Pascal\LKSL.Events.Streams.pas',
  LKSL.Generics.Collections in '..\..\..\Source\Lib\Pascal\LKSL.Generics.Collections.pas',
  LKSL.Math.SIUnits in '..\..\..\Source\Lib\Pascal\LKSL.Math.SIUnits.pas',
  LKSL.Math.Common in '..\..\..\Source\Lib\Pascal\LKSL.Math.Common.pas',
  LKSL.Math.Length in '..\..\..\Source\Lib\Pascal\LKSL.Math.Length.pas',
  LKSL.Math.Time in '..\..\..\Source\Lib\Pascal\LKSL.Math.Time.pas',
  LKSL.Streamables.Base in '..\..\..\Source\Lib\Pascal\LKSL.Streamables.Base.pas',
  LKSL.Streams.System in '..\..\..\Source\Lib\Pascal\LKSL.Streams.System.pas',
  LKSL.Streams.Types in '..\..\..\Source\Lib\Pascal\LKSL.Streams.Types.pas',
  LKSL.Threads.Base in '..\..\..\Source\Lib\Pascal\LKSL.Threads.Base.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}ReportMemoryLeaksOnShutdown := True;{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
