////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : ExctractZIPDemo2
//  * Purpose   : ������������ ���������� �������������� ������.
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

// ������ ������ ���������� �������� � ���������� �������������� ������.
// ��� ������������ ������ �� ������� ������� �� �������� �����,
// � ������� ������� �������� �������� ������������ ������ �� ������.
// ��� ��� ��� ��������� ������ �� ����� ����� ������ �����
// ����� �� ������� ������������� � ���� �� ������ ������.

program ExctractZIPDemo2;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  FWZipWriter,
  FWZipReader,
  FWZipConsts;

const
  PasswordList: array [0..3] of string = (
    '', 'password1', 'password2', 'password3');

procedure OnPassword(Self, Sender: TObject; const FileName: string;
  var Password: string; var CancelExtract: Boolean);
begin
  Password := PasswordList[3];
end;

var
  Writer: TFWZipWriter;
  Reader: TFWZipReader;
  Item: TFWZipWriterItem;
  I: Integer;
  ExtractResult: TExtractResult;
  Method: TMethod;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    Writer := TFWZipWriter.Create;
    try

      // ��� ������ ������� � ������ ������ ����� �� �������� ����������
      Writer.AddFolder('..\..\', False);

      // ������ �������� �� ������ ��������� �������
      Randomize;

      // � ������� �������� ������ ������ ����� �������������� (��� ������������)
      Item := Writer[0];
      Item.Password := PasswordList[Random(3) + 1];
      Item.NeedDescriptor := True;

      for I := 1 to Writer.Count - 1 do
      begin
        Item := Writer[I];
        // ���� ������������ ���������� ���������� �������� ���������� �����
        // ��. Readme.txt
        Item.NeedDescriptor := True;
        Item.Password := PasswordList[Random(4)];
      end;

      // ��������� ���������
      ForceDirectories('..\DemoResults\');
      Writer.BuildZip('..\DemoResults\ExctractZIPDemo2.zip');
    finally
      Writer.Free;
    end;

    Reader := TFWZipReader.Create;
    try
      Reader.LoadFromFile('..\DemoResults\ExctractZIPDemo2.zip');

      // ������ ���� ������ ������� ������ �� ������
      // � ������ ������ ���������� �������� ���������� ������ ��������������
      // �������� ��� ���:
      I := 0;
      repeat
        ExtractResult := Reader[0].Extract(
          '..\DemoResults\ExctractZIPDemo2\ManualExtract\', PasswordList[I]);
        Inc(I);
      until ExtractResult <> erNeedPassword;

      // ���� �������������� ������������ ����� �������������� ����������,
      // �� ������� ������ ����� ����� ���������

      // 1. ����� ������ �������
      Reader.PasswordList.Add(PasswordList[1]);
      Reader.PasswordList.Add(PasswordList[2]);

      // 2. ����� ����������
      Method.Code := @OnPassword;
      Method.Data := Reader;
      Reader.OnPassword := TZipNeedPasswordEvent(Method);

      // ��� ������������ � ������ ������� ��������� ������ ��� ������
      // ������ ����� ������� ����� ���������� ������� OnPassword

      Reader.ExtractAll('..\DemoResults\ExctractZIPDemo2\');
    finally
      Reader.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
