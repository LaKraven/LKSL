program LKSL_DUnitX_Testbed;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}
uses
  SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  LKSL.DUTXGenerics.Collections in 'Generics\LKSL.DUTXGenerics.Collections.pas',
  LKSL.Threads.Main in '..\..\..\Source\Lib\Pascal\LKSL.Threads.Main.pas',
  LKSL.Common.Performance in '..\..\..\Source\Lib\Pascal\LKSL.Common.Performance.pas',
  LKSL.Common.Streams in '..\..\..\Source\Lib\Pascal\LKSL.Common.Streams.pas',
  LKSL.Comparers.System in '..\..\..\Source\Lib\Pascal\LKSL.Comparers.System.pas',
  LKSL.Events.Main in '..\..\..\Source\Lib\Pascal\LKSL.Events.Main.pas',
  LKSL.Events.Streams in '..\..\..\Source\Lib\Pascal\LKSL.Events.Streams.pas',
  LKSL.Generics.Collections in '..\..\..\Source\Lib\Pascal\LKSL.Generics.Collections.pas',
  LKSL.Generics.CollectionsRedux in '..\..\..\Source\Lib\Pascal\LKSL.Generics.CollectionsRedux.pas',
  LKSL.Generics.Defaults in '..\..\..\Source\Lib\Pascal\LKSL.Generics.Defaults.pas',
  LKSL.Math.SIUnits in '..\..\..\Source\Lib\Pascal\LKSL.Math.SIUnits.pas',
  LKSL.Math.Time in '..\..\..\Source\Lib\Pascal\LKSL.Math.Time.pas',
  LKSL.Streamables.Main in '..\..\..\Source\Lib\Pascal\LKSL.Streamables.Main.pas',
  LKSL.Streams.Main in '..\..\..\Source\Lib\Pascal\LKSL.Streams.Main.pas',
  LKSL.Streams.System in '..\..\..\Source\Lib\Pascal\LKSL.Streams.System.pas',
  LKSL.Streams.Types in '..\..\..\Source\Lib\Pascal\LKSL.Streams.Types.pas',
  LKSL.Common.Types in '..\..\..\Source\Lib\Pascal\LKSL.Common.Types.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
{$ENDIF}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    ReportMemoryLeaksOnShutdown := True;
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
