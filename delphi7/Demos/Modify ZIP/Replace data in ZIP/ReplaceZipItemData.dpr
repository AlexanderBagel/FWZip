////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : ReplaceZipItemData
//  * Purpose   : ������������ ��������� ������ � ��� ��������� ������
//  * Author    : ��������� (Rouse_) ������
//  * Copyright : � Fangorn Wizards Lab 1998 - 2023.
//  * Version   : 2.0.2
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
// http://www.base2ti.com/
//

// ������ ������ ���������� ��� ����� �������� ������ � ��� ��������� ������
// ��� ������������� ���������� ������������ ��������� � �� ���������� ������.

program ReplaceZipItemData;

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  Classes,
  FWZipReader,
  FWZipModifier;

var
  Modifier: TFWZipModifier;
  Reader: TFWZipReader;
  Index: TReaderIndex;
  S: TStringStream;
  I: Integer;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    // ������� ��������� ������������ �������
    Modifier := TFWZipModifier.Create;
    try
      // ���������� ����� ��������� �����
      Index := Modifier.AddZipFile('..\..\DemoResults\split_main_archive.zip');

      // ��������� �� ���� ������ ������� �������� ��� ���
      Modifier.AddFromZip(Index, 'test1.txt');

      // ��������� ������ �������, ���� ��� �� ����������� ������
      Modifier.AddFromZip(Index, Modifier.Reader[Index].Item[1].FileName);

      // ������ �������� ����� ����� ������
      S := TStringStream.Create('����� ������ ��� �������� �������� ������');
      try
        S.Position := 0;
        Modifier.AddStream('test3.txt', S);
      finally
        S.Free;
      end;

      // �� � ��������� ��������� ������� � ������������� ���������� �����
      Modifier.AddFromZip(Index, 'test4.txt', 'New test4.txt');
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

    // ������ ������� � ��������� ������ �������, ������ ����� ����� ������
    // ����������� �� ����
    // ������ ����� ������� ����� ������������ �� ������ ��� �����������
    // �� � ��� �����-�� ����� �����
    Reader := TFWZipReader.Create;
    try
      // ������ ������ �� ����� ���������� ��������� ������
      Reader.LoadFromFile('..\..\DemoResults\split_main_archive.zip');

      // ������� ��������� ������������ �������
      Modifier := TFWZipModifier.Create;
      try
        // ���������� ����� ����� ��������� ��� �����
        Index := Modifier.AddZipFile(Reader);

        // ��������� �� ���� ��� �������� ����� ����������
        for I := 0 to Modifier.Reader[Index].Count - 2 do
          Modifier.AddFromZip(Index, Modifier.Reader[Index].Item[I].FileName);

        // ������ ���������� ����� ����� ������
        S := TStringStream.Create('����� ������ ��� ���������� �������� ������');
        try
          S.Position := 0;
          Modifier.AddStream('test4.txt', S);
        finally
          S.Free;
        end;

        // � �������� �����
        Modifier.BuildZip('..\..\DemoResults\replaced_data_archive3.zip');
      finally
        Modifier.Free;
      end;

    finally
      Reader.Free;
    end;

    // ��������� �������, ��� ��������� ����������� ������� ��������,
    // ������ ����������� ������ ���������� ���-�� ��� � � ������� ����� �����
    // �� � ���� ��� ����� ����� ������ ����� �������������� �����������

    // ������� ��������� ������������ �������
    Modifier := TFWZipModifier.Create;
    try
      // ������� ��������� ������
      Reader := TFWZipReader.Create;

      // ��������� ����� ��������� �����
      Reader.LoadFromFile('..\..\DemoResults\split_main_archive.zip');

      // � ��������� ��� � ������������ �������� ������ ����������,
      // ��� ��������� ����� ������ �����������
      Index := Modifier.AddZipFile(Reader, roOwned);

      // ��������� �� ���� ������ ������� �������� ��� ���
      Modifier.AddFromZip(Index, 'test1.txt');

      // ��������� ������ �������, ���� ��� �� ����������� ������
      Modifier.AddFromZip(Index, Modifier.Reader[Index].Item[1].FileName);

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
      Modifier.BuildZip('..\..\DemoResults\replaced_data_archive4.zip');
    finally
      Modifier.Free;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
