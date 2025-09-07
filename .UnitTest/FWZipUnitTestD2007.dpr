program FWZipUnitTestD2007;

{$I ../fwzip.inc}

{$IFDEF CONSOLE_TESTRUNNER}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  ZLibEx,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  FWZipTests in '..\FWZipTests.pas';

// Для запуска данных тестов под D2007 необходимо выполнить п.11 файла Readme.txt
// и указать путь до скачанной библиотеки ZLib
// по умолчанию путь установлен в ..\ZLib

{$R *.res}

begin
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.


