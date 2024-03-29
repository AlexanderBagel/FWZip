////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : CreateZIPDemo2
//  * Purpose   : ������������ ��������� ����������� �������
//  * Author    : ��������� (Rouse_) ������
//  * Copyright : � Fangorn Wizards Lab 1998 - 2023.
//  * Version   : 2.0.0
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

// ������ ������ ���������� ��������� �������� ��������� �������
// � ��� �� �������������� ������.

program CreateZIPDemo2;

{$IFDEF FPC}
  {$MODE Delphi}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  TypInfo,
  FWZipZLib,
  FWZipWriter;

var
  Zip: TFWZipWriter;
  Item: TFWZipWriterItem;
  I: Integer;
  BuildZipResult: TBuildZipResult;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    Zip := TFWZipWriter.Create;
    try
      // ��� ������ ������� � ������ ������ ����� �� �������� ����������
      Zip.AddFolder('..\..\', False);

      // ������ ������� �� ��������:
      for I := 0 to Zip.Count - 1 do
      begin
        Item := Zip[I];
        // ������� ����������
        Item.Comment := string('�������� ���������� � ����� ') + Item.FileName;
        // ��������� ������
        Item.Password := 'password';
        // ������� ��� ������
        Item.CompressionLevel := TCompressionLevel(Byte(I mod 3));
      end;

      // ������ ������ ������� ������ ����� ����������, ���������� ������� �
      // ����� ����������� ������� ������ � ����������� �� �����
      // ���������� ������� � ������.
      // �� � ��� ����� ���-�� ����� ����������.
      Zip.Comment := '�������� ���������� �� ����� ������';

      // ������� ����� � ������� ���������
      BuildZipResult := Zip.BuildZip('..\DemoResults\CreateZIPDemo2.zip');
      // ... � ������� ���������
      Writeln(GetEnumName(TypeInfo(TBuildZipResult), Integer(BuildZipResult)));

    finally
      Zip.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
