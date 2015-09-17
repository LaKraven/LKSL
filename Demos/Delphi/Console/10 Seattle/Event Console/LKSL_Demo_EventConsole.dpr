program LKSL_Demo_EventConsole;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  LKSL.Events.Main,
  Demo.EventConsole.Main in 'Demo.EventConsole.Main.pas',
  Demo.EventConsole.Events in 'Demo.EventConsole.Events.pas';

var
  LLine, LResponse: String;
begin
  {$IFDEF DEBUG}ReportMemoryLeaksOnShutdown := True;{$ENDIF DEBUG}
  try
    repeat
      WriteLn('Type an Event Command to trigger an Event, "help" for help, or "exit" to terminate this demo:');
      Readln(LLine);
      if LowerCase(LLine) <> 'exit' then
        if LowerCase(LLine) = 'help' then
        begin

        end
        else
          case EventConsole.Execute(LLine, LResponse) of
            ecrSuccess: WriteLn('Command Executed Successfully!');
            ecrCommandNotRecognized: begin
                                       WriteLn('Commands should be formatted as follows:');
                                       WriteLn('CommandName(<value1>, <value2>, <value3>)');
                                       WriteLn;
                                       WriteLn('You can request information on a Command thusly:');
                                       WriteLn('CommandName help');
                                     end;
            ecrCommandNotExist: ;
            ecrInvalidValues: ;
            else
              if LResponse = '' then
                WriteLn('Unknown Response with no message')
              else
                WriteLn(Format('Unknown Response with message "%s"', [LResponse]));
          end;
        WriteLn;
    until LowerCase(LLine) = 'exit';
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
