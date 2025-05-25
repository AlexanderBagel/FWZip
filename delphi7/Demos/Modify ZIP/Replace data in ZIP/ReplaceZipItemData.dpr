////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : ReplaceZipItemData
//  * Purpose   : ������������ ��������� ������ � ��� ��������� ������
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
  TypInfo,
  FWZipReader,
  FWZipWriter,
  FWZipModifier;

procedure Check(Value: TBuildZipResult);
begin
  Writeln(GetEnumName(TypeInfo(TBuildZipResult), Integer(Value)));
end;

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
      Check(Modifier.BuildZip('..\..\DemoResults\replaced_data_archive1.zip'));
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
      Check(Modifier.BuildZip('..\..\DemoResults\replaced_data_archive2.zip'));
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
        Check(Modifier.BuildZip('..\..\DemoResults\replaced_data_archive3.zip'));
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
      Check(Modifier.BuildZip('..\..\DemoResults\replaced_data_archive4.zip'));
    finally
      Modifier.Free;
    end;

    // ������ ������ ��� �������� �� ������� �������������
    // �� ���������� ��� �������������� ����� � ������ �����
    // � ��������� ������������ �������, ������ � ����������� ������� ���������
    // ��� ����� ������������ ����� InsertStream
    Modifier := TFWZipModifier.Create;
    try
      // ���������� ����� ��������� �����
      Index := Modifier.AddZipFile('..\..\DemoResults\split_main_archive.zip');
      // ��������� ��� ��������
      Modifier.AddFromZip(Index);
      // ������ ������ ������ � ��������� ��������
      Modifier.DeleteItem(3);

      // � ����� ����� ������
      S := TStringStream.Create('����� ������ ��� ���������� �������� ������ - ������� ���');
      try
        S.Position := 0;
        Modifier.InsertStream('test4.txt', 3, S);
      finally
        S.Free;
      end;

      // ������ ������ ������ � ����� ������ ��������
      Modifier.DeleteItem(0);
      // � ����� ����� ������
      S := TStringStream.Create('����� ������ ��� ������� �������� ������ - ������� ���');
      try
        S.Position := 0;
        Modifier.InsertStream('test1.txt', 0, S);
      finally
        S.Free;
      end;

      // � ������ ������ ����, ����� ���������� ��� �����, ������� �� �������� ���� �� �����
      Modifier.DeleteItem(1);
      Modifier.InsertFile('..\..\..\FWZipModifier.pas', 1);

      // ������ ������ ����� �����, ������� ��� �� �����
      Check(Modifier.BuildZip('..\..\DemoResults\replaced_data_archive5.zip'));
    finally
      Modifier.Free;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
