////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : ReadMultiPartZip
//  * Purpose   : ������������ ������ ������������ ������
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

program ReadMultiPartZip;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  TypInfo,
  FWZipStream,
  FWZipReader;

var
  Reader: TFWZipReader;
  MultiStream: TFWFileMultiStream;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try

    // ��� ������ ����������� ������� ���-�� ����������� �����
    // ��������� �� TFWAbstractMultiStream.
    // � ������ ������ ������������ ����������� CreateRead.
    // ������ �������� - ���� � ��������� ������.
    // ������ ��������, ����� ��������.
    // rsmQuick - ������ ��� ���� � ����� �� ����������� �������� �
    // ������ �����, ������ �� ��� ���, ���� �� �������� ������.
    // ��������� ���� ��������� ������� �������.
    // ��� ������� ������ �������� ������������ ������, �� �� ������ ��������,
    // �� ������� ���� ��� � ��� �������, ����� ���� �� �����
    // ���������� ������ TCentralDirectoryFileHeader �������� �� ��������
    // � ������ �����, ��� ���������� ����� ��������� ������� ������
    // ����������� ������, ���� ��� ����� ���� �� ���.
    // ��� ���������������� �������� ����� ������������ ����� rsmFull.
    // � ���� ������ ����� ������� ����������� ������ ������� ���� ������.
    // ��, ��� ����� ��������� �������, �������� � ��� ������, �����
    // �������� ����� ���������� �����.
    // �� ���������� ����� 60 ����� � ����� ����� �������� �����
    // ���������� ��������!
    MultiStream := TFWFileMultiStream.CreateRead(
      '..\..\DemoResults\MultyPartZip\MultyPartZip.zip', rsmQuick);
    try

      Reader := TFWZipReader.Create;
      try

        // ������ ����������� ������� �������������� ������ ����� ����� ������
        // LoadFromStream � ��������� ���������� ���������� ������ TFWAbstractMultiStream
        Reader.LoadFromStream(MultiStream);

        // ��� ��������� ������ � ������� �������� ���-�� ��� � � �������.
        Reader.PasswordList.Add('password');

        // �������� �������� ����������� ������
        Reader.Check;

        // ... ��� ��� ����������
        Reader.ExtractAll('*.pas', '..\..\DemoResults\MultyPartZip\');

        Writeln('done.');

      finally
        Reader.Free;
      end;
    finally
      MultiStream.Free;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
