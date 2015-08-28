////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : SplitZip
//  * Purpose   : ������������ ������ c ��������� ������
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

// ������ ������ ���������� ��� ����� ��������� ����� �� ��������� ������,
// ��� ������������� ���������� ������ � ���������� ������.

program SplitZip;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Classes,
  FWZipWriter,
  FWZipModifier;

procedure AddItem(AWriter: TFWZipWriter; const AName, AData: string);
var
  S: TStringStream;
begin
  S := TStringStream.Create(AData);
  try
    S.Position := 0;
    AWriter.AddStream(AName, S);
  finally
    S.Free;
  end;
end;

var
  Writer: TFWZipWriter;
  Modifier: TFWZipModifier;
  Index: TReaderIndex;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    // ������� ����� ������� ����� ���������
    Writer := TFWZipWriter.Create;
    try
      // ��������� 4 �������� � �����
      AddItem(Writer, 'test1.txt', '������ �������');
      AddItem(Writer, 'test2.txt', '������ �������');
      AddItem(Writer, 'test3.txt', '������ �������');
      AddItem(Writer, 'test4.txt', '��������� �������');
      // ���������
      ForceDirectories('..\..\DemoResults\');
      Writer.BuildZip('..\..\DemoResults\split_main_archive.zip');
    finally
      Writer.Free;
    end;
    // ������� ��������� ������������ �������
    Modifier := TFWZipModifier.Create;
    try
      // ���������� ����� ��������� �����
      Index := Modifier.AddZipFile('..\..\DemoResults\split_main_archive.zip');
      // ��������� �� ���� ������ ��� ��������
      Modifier.AddFromZip(Index, 'test1.txt');
      Modifier.AddFromZip(Index, 'test2.txt');
      // � ��������� � ����� �����
      // ��� ���� �������� ����������� ������ �� ����������,
      // ������ ��������� ��� ���� � ���� ������� ���� ����� � ������ ����
      // �� ������������� ������
      Modifier.BuildZip('..\..\DemoResults\splited_archive1.zip');

      // ������ ������� ����������� �������� � ��������� ������ ���
      Modifier.Clear;
      Modifier.AddFromZip(Index, 'test3.txt');
      Modifier.AddFromZip(Index, 'test4.txt');
      // ��������� �� ����� �����
      Modifier.BuildZip('..\..\DemoResults\splited_archive2.zip');
    finally
      Modifier.Free;
    end;

    // ���� � ���, �� ��������� ����������� ����� �� ��� �����
    // �� ��������� ������������ ������
    // ��� ���������� �������� � ������� MergeZip
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
