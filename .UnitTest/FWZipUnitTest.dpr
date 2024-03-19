program FWZipUnitTest;

{$IFDEF FPC}
  {$MODE Delphi}
{$ELSE}
  {$IFDEF CONSOLE_TESTRUNNER}
    {$APPTYPE CONSOLE}
  {$ENDIF}
{$ENDIF}

uses
  {$IFDEF FPC}
    {$IFDEF CONSOLE_TESTRUNNER}
    consoletestrunner,
    {$ELSE}
    Interfaces, Forms, GuiTestRunner,
    {$ENDIF}
  {$ELSE}
  DUnitTestRunner,
  {$ENDIF}
  FWZipTests in '..\FWZipTests.pas';

{$IFDEF FPC}
  {$IFDEF CONSOLE_TESTRUNNER}
  var Application: TTestRunner;
  {$ENDIF}
{$ELSE}
  {$R *.res}
{$ENDIF}
begin
  {$IFDEF FPC}
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
  {$ELSE}
  DUnitTestRunner.RunRegisteredTests;
  {$ENDIF}
end.

