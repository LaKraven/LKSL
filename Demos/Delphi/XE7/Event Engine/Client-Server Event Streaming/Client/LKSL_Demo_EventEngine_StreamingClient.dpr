program LKSL_Demo_EventEngine_StreamingClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  LKSL.Demos.SEStreamingClientMainForm in 'LKSL.Demos.SEStreamingClientMainForm.pas' {frmMain},
  LKSL.Common.Types in '..\..\..\..\..\..\Source\Delphi\Lib\Common\LKSL.Common.Types.pas',
  LKSL.Events.Base in '..\..\..\..\..\..\Source\Delphi\Lib\Events\LKSL.Events.Base.pas',
  LKSL.Events.Streams in '..\..\..\..\..\..\Source\Delphi\Lib\Events\LKSL.Events.Streams.pas',
  LKSL.Streamables.Base in '..\..\..\..\..\..\Source\Delphi\Lib\Streams\LKSL.Streamables.Base.pas',
  LKSL.Streams.System in '..\..\..\..\..\..\Source\Delphi\Lib\Streams\LKSL.Streams.System.pas',
  LKSL.Streams.Types in '..\..\..\..\..\..\Source\Delphi\Lib\Streams\LKSL.Streams.Types.pas',
  LKSL.Threads.Base in '..\..\..\..\..\..\Source\Delphi\Lib\Threads\LKSL.Threads.Base.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
