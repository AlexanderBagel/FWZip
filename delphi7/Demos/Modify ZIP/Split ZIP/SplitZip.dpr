////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : SplitZip
//  * Purpose   : ������������ ������ c ��������� ������
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

// ������ ������ ���������� ��� ����� ��������� ����� �� ��������� ������,
// ��� ������������� ���������� ������ � ���������� ������.

program SplitZip;

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

procedure Check(Value: TBuildZipResult);
begin
  Writeln(GetEnumName(TypeInfo(TBuildZipResult), Integer(Value)));
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
      Check(Writer.BuildZip('..\..\DemoResults\split_main_archive.zip'));
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
      Check(Modifier.BuildZip('..\..\DemoResults\splited_archive2.zip'));
    finally
      Modifier.Free;
    end;

    // ��� � ���, �� ��������� ����������� ����� �� ��� �����
    // �� ��������� ������������ ������
    // ��� ���������� �������� � ������� MergeZip
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
