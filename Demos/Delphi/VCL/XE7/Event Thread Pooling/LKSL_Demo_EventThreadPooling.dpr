program LKSL_Demo_EventThreadPooling;

uses
  Vcl.Forms,
  Demo.EventThreadPooling.MainForm in 'Demo.EventThreadPooling.MainForm.pas' {Form1},
  Demo.EventThreadPooling.Events in 'Demo.EventThreadPooling.Events.pas',
  Demo.EventThreadPooling.EventThreads in 'Demo.EventThreadPooling.EventThreads.pas',
  LKSL.Threads.Main in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Threads.Main.pas',
  LKSL.Common.Streams in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Common.Streams.pas',
  LKSL.Common.Types in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Common.Types.pas',
  LKSL.Events.Main in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Events.Main.pas',
  LKSL.Events.Streams in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Events.Streams.pas',
  LKSL.Generics.Collections in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Generics.Collections.pas',
  LKSL.Streamables.Main in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Streamables.Main.pas',
  LKSL.Streams.System in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Streams.System.pas',
  LKSL.Math.SIUnits in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Math.SIUnits.pas',
  LKSL.Common.Performance in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Common.Performance.pas',
  LKSL.Generics.Defaults in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Generics.Defaults.pas',
  LKSL.Streams.Main in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Streams.Main.pas',
  LKSL.Common.SyncObjs in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Common.SyncObjs.pas',
  LKSL.Generics.CollectionsRedux in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Generics.CollectionsRedux.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}ReportMemoryLeaksOnShutdown := True;{$ENDIF DEBUG}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
