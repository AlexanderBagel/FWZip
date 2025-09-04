program FWZipUnitTest;

{$I ../fwzip.inc}

{$IFDEF FPC}
  {$MODE Delphi}
{$ELSE}
  {$IFDEF CONSOLE_TESTRUNNER}
    {$APPTYPE CONSOLE}
  {$ENDIF}
{$ENDIF}

uses
  {$IF DEFINED(FPC_TESTS)}
    {$IFDEF CONSOLE_TESTRUNNER}
    consoletestrunner,
    {$ELSE}
    Interfaces, Forms, GuiTestRunner,
    {$ENDIF}
  {$ELSEIF DEFINED(DUNITX_TESTS)}
  DUnitX.TestFramework,
  DUnitX.Loggers.Console,
  {$ELSE}
  DUnitTestRunner,
  {$ENDIF}
  FWZipTests in '..\FWZipTests.pas';

{$IF DEFINED(FPC_TESTS)}
  {$IFDEF CONSOLE_TESTRUNNER}
  var Application: TTestRunner;
  {$ENDIF}
{$ELSEIF DEFINED(DUNITX_TESTS)}
  var
    aTestRunner: ITestRunner;
    aRunResults: IRunResults;
  {$R *.res}
{$ELSE}
  {$R *.res}
{$ENDIF}
begin
  {$IF DEFINED(FPC_TESTS)}
    {$IFDEF CONSOLE_TESTRUNNER}
    Application := TTestRunner.Create(nil);
    Application.Initialize;
    Application.Run;
    Application.Free;
    {$ELSE}
    Application.Initialize;
    Application.CreateForm(TGuiTestRunner, TestRunner);
    Application.Run;
    {$ENDIF}
  {$ELSEIF DEFINED(DUNITX_TESTS)}
  aTestRunner := TDUnitX.CreateRunner;

  if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then begin
    aTestRunner.AddLogger(TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet));
  end;

  aRunResults := aTestRunner.Execute;

  if not aRunResults.AllPassed then begin
    System.ExitCode := EXIT_ERRORS;
  end;
  {$ELSE}
  DUnitTestRunner.RunRegisteredTests;
  {$ENDIF}
end.

