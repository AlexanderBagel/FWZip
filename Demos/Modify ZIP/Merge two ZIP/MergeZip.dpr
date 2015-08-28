////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : MergeZip
//  * Purpose   : ������������ ����������� ���������� �������
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

// ������ ������ ���������� ��� ����� ���������� ���������
// ����� ��������� ������� � ����,
// ��� ������������� ���������� ������ � ���������� ������.

program MergeZip;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  FWZipModifier;

var
  Modifier: TFWZipModifier;
  Index1, Index2: TReaderIndex;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    // ������� ��������� ������������ �������
    Modifier := TFWZipModifier.Create;
    try
      // ���������� ������ �����, ������� ��� ������ � ������� SplitZip
      Index1 := Modifier.AddZipFile('..\..\DemoResults\splited_archive1.zip');
      // ���������� ������ �����
      Index2 := Modifier.AddZipFile('..\..\DemoResults\splited_archive2.zip');
      // ��������� ��� �������� �� ������� ������
      Modifier.AddFromZip(Index1);
      // � �� �������
      Modifier.AddFromZip(Index2);
      // ������ ������� ����� ����� ������� ����� � ���� ��� �������� ����� �������
      // � ���������� ����� ��������� ������ split_main_archive.zip, �������
      // ��� ������ � ������� SplitZip (��� ������ ����� ��������� ������ �� ����������� �����)
      Modifier.BuildZip('..\..\DemoResults\merged_archive.zip')
    finally
      Modifier.Free;
    end;
    // ������ ��������� ������, �� ������ ��������� �������� ������
    // ����� ������� � ������� ReplaceZipItemData
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
