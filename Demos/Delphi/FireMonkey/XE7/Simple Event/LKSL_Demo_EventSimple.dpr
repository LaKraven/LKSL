program LKSL_Demo_EventSimple;

uses
  System.StartUpCopy,
  FMX.Forms,
  Demo.EventSimple.MainForm in 'Demo.EventSimple.MainForm.pas' {Form1},
  LKSL.Events.Main in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Events.Main.pas',
  LKSL.Common.Types in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Common.Types.pas',
  LKSL.Threads.Main in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Threads.Main.pas',
  LKSL.Streamables.Main in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Streamables.Main.pas',
  LKSL.Generics.Collections in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Generics.Collections.pas',
  LKSL.Streams.System in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Streams.System.pas',
  LKSL.Common.Streams in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Common.Streams.pas',
  LKSL.Events.Streams in '..\..\..\..\..\Source\Lib\Pascal\LKSL.Events.Streams.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
