program LKSL_Demo_EventConsole;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Demo.EventConsole.Main in 'Demo.EventConsole.Main.pas',
  Demo.EventConsole.Events in 'Demo.EventConsole.Events.pas';

var
  LLine: String;
begin
  {$IFDEF DEBUG}ReportMemoryLeaksOnShutdown := True;{$ENDIF DEBUG}
  try
    repeat
      Readln(LLine);
    until LowerCase(LLine) = 'exit';
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
