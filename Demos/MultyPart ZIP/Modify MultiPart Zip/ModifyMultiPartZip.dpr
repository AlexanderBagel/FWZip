////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : ModifyMultiPartZip
//  * Purpose   : ������������ ����������� ������������ ������
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

// ���� ����������� ����������� ������� ���� ���������� �������� ���� �� ����,
// �� ����������� ���������� ����� ��������� ��� �����������.
// ��� ��� ����������� ����� �������� �������� � � �������� �������� � � ������������
// ����������� ��� ���� �������, � ���-�� ����� ����� �������� �����������
// ��� ����� ����������� ������ �� ������������ ������ � ������� � ��������.

program ModifyMultiPartZip;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Classes,
  TypInfo,
  FWZipZLib,
  FWZipWriter,
  FWZipStream,
  FWZipModifier;

var
  Writer: TFWZipWriter;
  S: TStringStream;
  MultiStreamRead, MultiStreamWrite: TFWFileMultiStream;
  Modifier: TFWZipModifier;
  I, Index1, Index2: Integer;
  BuildZipResult: TBuildZipResult;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try

    // ����������� ������������ ������ ����� �� ���������� �� �����������
    // �������� ������, �� ����������� ������������� �����������
    // TFWAbstractMultiStream ��� ������ � ������

    // �������� ������� ����� ��������� � ������� CreateMultyPartZip.dpr
    // � ������ �� ���� ��� *.pas �����.
    // � ���-�� ������� ����� �� �������� �� ������������ ������

    MultiStreamRead := TFWFileMultiStream.CreateRead(
      '..\..\DemoResults\MultyPartZip\MultyPartZip.zip');
    try

      Modifier := TFWZipModifier.Create;
      try
        // ��������� ����������� ����� � �����������
        Index1 := Modifier.AddZipFile(MultiStreamRead);

        // ��������� ��� �������� ������
        Modifier.AddFromZip(Index1);

        // ������� ��� PAS �����
        for I := Modifier.Count - 1 downto 0 do
          if AnsiLowerCase(ExtractFileExt(Modifier[I].FileName)) = '.pas' then
            Modifier.DeleteItem(I);

        // ������ �������� ������� �����
        Writer := TFWZipWriter.Create;
        try
          S := TStringStream.Create('������ �������� ������ ��� ������������');
          Writer.AddStream('test_stream.txt', S, soOwned);
          Writer.BuildZip('..\..\DemoResults\MultyPartZip\stream.zip')
        finally
          Writer.Free;
        end;

        // ��������� ������� ����� � �����������
        Index2 := Modifier.AddZipFile('..\..\DemoResults\MultyPartZip\stream.zip');

        // ��������� ������������ ������� �������� ������
        Modifier.AddFromZip(Index2, 'test_stream.txt');

        // ... � ���������� ��� � ����� �����
        // ������!
        // �.�. �� �� ��������� ������ ��������, ���������� �� ������ ������� ����
        // �� �� ������� �� ��������� � ����� ����� ����� ����������� � ������
        // ������� �������, ��� ��� � ����������� ������.
        // ��� ��������� ���������� �� ����, ��� �� ����� �� ��������������.

        MultiStreamWrite := TFWFileMultiStream.CreateWrite(
          '..\..\DemoResults\MultyPartZip\MultyPartZipWithoutPas.zip');
        try
          BuildZipResult := Modifier.BuildZip(MultiStreamWrite);
        finally
          MultiStreamWrite.Free;
        end;

      finally
        Modifier.Free;
      end;

    finally
      MultiStreamRead.Free;
    end;

    // ... ������� ���������
    Writeln(GetEnumName(TypeInfo(TBuildZipResult), Integer(BuildZipResult)));

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
