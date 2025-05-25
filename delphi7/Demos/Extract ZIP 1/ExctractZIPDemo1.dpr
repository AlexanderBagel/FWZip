////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : ExctractZIPDemo1
//  * Purpose   : ������������ ���������� ������.
//  *           : ������������ ����� ��������� ��������������� CreateZIPDemo1
//  * Author    : ��������� (Rouse_) ������
//  * Copyright : � Fangorn Wizards Lab 1998 - 2025.
//  * Version   : 2.0.8
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Stable Release : http://rouse.drkb.ru/components.php#fwzip
//  * Latest Source  : https://github.com/AlexanderBagel/FWZip
//  ****************************************************************************
//
//  ������������ ���������:
//  ftp://ftp.info-zip.org/pub/infozip/doc/appnote-iz-latest.zip
//  https://zlib.net/zlib-1.2.13.tar.gz
//  http://www.base2ti.com/
//

// ������ ������ ���������� ��� �������� ���������� ���������� �� ������.

// �����!!!
// �� ������ ������� Delphi ����� ��������� ������ ����������.
// ��� � ��������� ������� Readme ����� 11

program ExctractZIPDemo1;

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  Classes,
  SysUtils,
  TypInfo,
  FWZipReader,
  FWZipUtils;

function ExtractResultStr(Value: TExtractResult): string;
begin
  Result := GetEnumName(TypeInfo(TExtractResult), Integer(Value));
end;

var
  Zip: TFWZipReader;
  Index: Integer;
  M: TStringStream;
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
        M := TStringStream.Create('');
        try
          Zip[Index].ExtractToStream(M, '');
          // ���� ��������, ������� ��� ���������� � ���� �������
          {$IFDEF UNICODE}
          Writeln(M.DataString);
          {$ELSE}
            {$IFDEF FPC}
            Writeln(M.DataString);
            {$ELSE}
            Writeln(ConvertToOemString(AnsiString(M.DataString)));
            {$ENDIF}
          {$ENDIF}
        finally
          M.Free;
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
