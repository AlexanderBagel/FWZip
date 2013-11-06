////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : ExctractZIPDemo1
//  * Purpose   : ������������ ���������� ������.
//  *           : ������������ ����� ��������� ��������������� CreateZIPDemo1
//  * Author    : ��������� (Rouse_) ������
//  * Copyright : � Fangorn Wizards Lab 1998 - 2013.
//  * Version   : 1.0.10
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Stable Release : http://rouse.drkb.ru/components.php#fwzip
//  * Latest Source  : https://github.com/AlexanderBagel/FWZip
//  ****************************************************************************
//
//  ������������ ���������:
//  ftp://ftp.info-zip.org/pub/infozip/doc/appnote-iz-latest.zip
//  http://zlib.net/zlib-1.2.5.tar.gz
//  http://www.base2ti.com/
//

// ������ ������ ���������� ��� �������� ���������� ���������� �� ������.

program ExctractZIPDemo1;

{$APPTYPE CONSOLE}

uses
  Windows,
  Classes,
  SysUtils,
  TypInfo,
  FWZipReader;

function ExtractResultStr(Value: TExtractResult): string;
begin
  Result := GetEnumName(TypeInfo(TExtractResult), Integer(Value));
end;

var
  Zip: TFWZipReader;
  Index: Integer;
  S: TStringStream;
  OemString: AnsiString;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    Zip := TFWZipReader.Create;
    try
      // ��������� ����� ��������� �����
      Zip.LoadFromFile('..\DemoResults\CreateZIPDemo1.zip');

      // ������ ������� ���������� - ������ ������ � ������� �������� ������

      // � ������� CreateZIPDemo1 �� ������� � ����� ������ ���� Test.txt
      // ��� ���������� �������� ������ ����� �������� � ������
      Index := Zip.GetElementIndex('test.txt');
      if Index >= 0 then
      begin
        // ����������� ����� � ������:
        S := TStringStream.Create('');
        try
          Zip[Index].ExtractToStream(S, '');
          // ���� ��������, ������� ��� ���������� � ���� �������
          {$IFDEF UNICODE}
          Writeln(OemString);
          {$ELSE}
          OemString := AnsiString(S.DataString);
          AnsiToOem(PAnsiChar(OemString), PAnsiChar(OemString));
          Writeln(OemString);
          {$ENDIF}
        finally
          S.Free;
        end;

        // ����������� ���-�� ����� �� ����:
        Write('Extract "', Zip[Index].FileName, '": ');
        Writeln(ExtractResultStr(
          Zip[Index].Extract('..\DemoResults\CreateZIPDemo1\ManualExtract\', '')));
      end;

      // �����-�� ������� ����� �������� ���������� ��������� ������

      // ������ ������� ���������� - �������������� ���������� ������
      // � ��������� ����� �� �����
      Zip.ExtractAll('..\DemoResults\CreateZIPDemo1\');

      // ������ ������� ���������� - �������������� ���������� �� �����
      // (������ ��� ��������� ��� ����� ����������� � ����� AddFolderDemo ������)
      Zip.ExtractAll('AddFolderDemo*', '..\DemoResults\CreateZIPDemo1\ExtractMasked\');
    finally
      Zip.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
