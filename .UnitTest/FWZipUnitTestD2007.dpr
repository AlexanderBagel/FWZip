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

// ��� ������� ������ ������ ��� D2007 ���������� ��������� �.11 ����� Readme.txt
// � ������� ���� �� ��������� ���������� ZLib
// �� ��������� ���� ���������� � ..\ZLib

{$R *.res}

begin
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.


