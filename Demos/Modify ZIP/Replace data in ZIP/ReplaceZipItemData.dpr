////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : ReplaceZipItemData
//  * Purpose   : ������������ ��������� ������ � ��� ��������� ������
//  * Author    : ��������� (Rouse_) ������
//  * Copyright : � Fangorn Wizards Lab 1998 - 2015.
//  * Version   : 1.0.11
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

// ������ ������ ���������� ��� ����� �������� ������ � ��� ��������� ������
// ��� ������������� ���������� ������������ ��������� � �� ���������� ������.

program ReplaceZipItemData;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Classes,
  FWZipModifier;

var
  Modifier: TFWZipModifier;
  Index: TReaderIndex;
  S: TStringStream;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    // ������� ��������� ������������ �������
    Modifier := TFWZipModifier.Create;
    try
      // ���������� ����� ��������� �����
      Index := Modifier.AddZipFile('..\..\DemoResults\split_main_archive.zip');
      // ��������� �� ���� ������ ��� ��������
      Modifier.AddFromZip(Index, 'test1.txt');
      Modifier.AddFromZip(Index, 'test2.txt');

      // ������ �������� ����� ����� ������
      S := TStringStream.Create('����� ������ ��� �������� �������� ������');
      try
        S.Position := 0;
        Modifier.AddStream('test3.txt', S);
      finally
        S.Free;
      end;

      // �� � ��������� ��������� �������
      Modifier.AddFromZip(Index, 'test4.txt');
      // ������ ������ ����� �����,
      // ��� ���� ������ �� ������� ������� � ���������� ��������
      // ����������� ��� ���� ��� ����������,
      // � ������ �������� �������� ����� �������� ����� ���� ������
      Modifier.BuildZip('..\..\DemoResults\replaced_data_archive1.zip');
    finally
      Modifier.Free;
    end;

    // ���������� ������� ��� � ����������� ������� �������� � ������
    // ���� �� ������� �� �����, �� ����� ������� ��� �����:
    // ������� ��������� ������������ �������
    Modifier := TFWZipModifier.Create;
    try
      // ���������� ����� ��������� �����
      Index := Modifier.AddZipFile('..\..\DemoResults\split_main_archive.zip');
      // ��������� ��� ��������
      Modifier.AddFromZip(Index);
      // ������ ������ ������ � ������� ��������
      Modifier.DeleteItem(2);

      // � ����� ����� ������
      S := TStringStream.Create('����� ������ ��� �������� �������� ������');
      try
        S.Position := 0;
        Modifier.AddStream('test3.txt', S);
      finally
        S.Free;
      end;

      // ������ ������ ����� �����, ������� ��� �� �����
      Modifier.BuildZip('..\..\DemoResults\replaced_data_archive2.zip');
    finally
      Modifier.Free;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
