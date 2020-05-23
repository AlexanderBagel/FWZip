////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : CreateMultiPartZip
//  * Purpose   : ������������ �������� ������ � ��������� �� ����� �� 1 ���������
//  * Author    : ��������� (Rouse_) ������
//  * Copyright : � Fangorn Wizards Lab 1998 - 2020.
//  * Version   : 1.1.0
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

// ������ ������ ���������� ��� ����� ��������� ������� ����� �� ��������� ������,
// ��� �� ����������� ������ �� ������� �������� ����������
// ��� �������� ������� �� ���� ��� ������������ ����������, ������������
// �������� ������ �������� �������

program CreateMultiPartZip;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  TypInfo,
  FWZipZLib,
  FWZipWriter,
  FWZipStream;

var
  Zip: TFWZipWriter;
  Item: TFWZipWriterItem;
  I: Integer;
  BuildZipResult: TBuildZipResult;
  MultiStream: TFWFileMultiStream;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    Zip := TFWZipWriter.Create;
    try
      // ��� ������ ������� � ������ ������ ����� �� �������� ����������
      Zip.AddFolder('..\..\..\', False);

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
      Zip.Comment := '�������� ���������� �� ����� ������';

      ForceDirectories('..\..\DemoResults\MultyPartZip\');

      // ��� ������ �������� ����������� ������� ����������� � ������
      // TFWAbstractMultiStream �� �������� ����������� ����������
      // ��������������� ���������� ����������.
      // � ��������� TFWFileMultiStream ��������� �������� � ��������
      // �������������� �� ��������� �����.
      // ���� ����������� ������ � �������� �������������� ��������
      // �� FTP ��� ������, �������� SharePoint �������,
      // ����������� ������������� ����������� ���������.

      // ������ ���������� ���� ��� ������, ������ ������������ ������ ��� ������.
      // ��������� ���� ������������ ������ �� ����������� ����� ���������������
      // ���������� ��������, �.�. �������� ������������, ��������� TCentralDirectoryFileHeader
      // ������ ������� ������������� ������ ����, ������� ������ ���������
      // ����� ������ ����� ������� ����������� � ��������� �� 1 �����
      // �� SizeOf(TCentralDirectoryFileHeader).
      // ���-�� ������ ���������� ���� �� ����� ���� ������ ��� ������ ���������
      // TEndOfCentralDir, � ���� ������������ Zip64, �� � ����� ������� ����������� �������
      // TZip64EOFCentralDirectoryRecord � TZip64EOFCentralDirectoryLocator.
      MultiStream := TFWFileMultiStream.CreateWrite(
        '..\..\DemoResults\MultyPartZip\MultyPartZip.zip', $20000);
      try
        // ��� �������� ������������ ������ ��������� �� ��� �����, � ��� MultiStream
        BuildZipResult := Zip.BuildZip(MultiStream);
      finally
        MultiStream.Free;
      end;

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
