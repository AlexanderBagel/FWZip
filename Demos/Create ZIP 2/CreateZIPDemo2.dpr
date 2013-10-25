////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : CreateZIPDemo2
//  * Purpose   : ������������ ��������� ����������� �������
//  * Author    : ��������� (Rouse_) ������
//  * Copyright : � Fangorn Wizards Lab 1998 - 2012.
//  * Version   : 1.0.4
//  * Home Page : http://rouse.drkb.ru
//  ****************************************************************************
//
//  ������������ ���������:
//  ftp://ftp.info-zip.org/pub/infozip/doc/appnote-iz-latest.zip
//  http://zlib.net/zlib-1.2.5.tar.gz
//

// ������ ������ ���������� ��������� �������� ��������� �������
// � ��� �� �������������� ������.

program CreateZIPDemo2;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  TypInfo,
  {$IFDEF USE_ZLIB_DLL}
  ZLib_external
  {$ELSE}
  ZLib
  {$ENDIF},
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
        Item.Comment := '�������� ���������� � ����� ' + Item.FileName;
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
      ForceDirectories('..\DemoResults\');
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
end.
