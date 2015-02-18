program LKSL_Demo_EventThreadPooling;

uses
  Vcl.Forms,
  Demo.EventThreadPooling.MainForm in 'Demo.EventThreadPooling.MainForm.pas' {Form1},
  Demo.EventThreadPooling.Events in 'Demo.EventThreadPooling.Events.pas',
  Demo.EventThreadPooling.EventThreads in 'Demo.EventThreadPooling.EventThreads.pas',
  LKSL.Common.Streams in '..\..\..\Source\Lib\Pascal\Common\Common\LKSL.Common.Streams.pas',
  LKSL.Common.Types in '..\..\..\Source\Lib\Pascal\Common\Common\LKSL.Common.Types.pas',
  LKSL.Events.Base in '..\..\..\Source\Lib\Pascal\Common\Events\LKSL.Events.Base.pas',
  LKSL.Events.Streams in '..\..\..\Source\Lib\Pascal\Common\Events\LKSL.Events.Streams.pas',
  LKSL.Generics.Collections in '..\..\..\Source\Lib\Pascal\Common\Generics\LKSL.Generics.Collections.pas',
  LKSL.Math.Base in '..\..\..\Source\Lib\Pascal\Common\Math\LKSL.Math.Base.pas',
  LKSL.Math.Common in '..\..\..\Source\Lib\Pascal\Common\Math\LKSL.Math.Common.pas',
  LKSL.Math.Length in '..\..\..\Source\Lib\Pascal\Common\Math\LKSL.Math.Length.pas',
  LKSL.Math.Time in '..\..\..\Source\Lib\Pascal\Common\Math\LKSL.Math.Time.pas',
  LKSL.Streamables.Base in '..\..\..\Source\Lib\Pascal\Common\Streams\LKSL.Streamables.Base.pas',
  LKSL.Streams.System in '..\..\..\Source\Lib\Pascal\Common\Streams\LKSL.Streams.System.pas',
  LKSL.Streams.Types in '..\..\..\Source\Lib\Pascal\Common\Streams\LKSL.Streams.Types.pas',
  LKSL.Threads.Base in '..\..\..\Source\Lib\Pascal\Common\Threads\LKSL.Threads.Base.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}ReportMemoryLeaksOnShutdown := True;{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
