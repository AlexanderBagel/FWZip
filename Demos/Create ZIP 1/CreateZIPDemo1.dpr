////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : CreateZIPDemo1
//  * Purpose   : ������������ �������� ������ ��������� ���������
//  *           : �������� ���������� ������
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

// ������ ������ ���������� ��������� �������� ���������� ���������� � �����
// ��� ������� �� �������� ���������� � ������ ����� ������� ��������� �����

program CreateZIPDemo1;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  TypInfo,
  FWZipWriter;

procedure CheckResult(Value: Integer);
  begin
    if Value < 0 then
      raise Exception.Create('������ ���������� ������');
  end;

var
  Zip: TFWZipWriter;
  S: TStringStream;
  PresentFiles: TStringList;
  SR: TSearchRec;
  I, ItemIndex: Integer;
  BuildZipResult: TBuildZipResult;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    Zip := TFWZipWriter.Create;
    try
      // � ����� ������ ������� UTF8 ��������� (���� ����������)
      Zip.UseUTF8String := True;

      // ������� ����������� �� �������������
      Zip.Comment := '����� ����������� � ������';

      // ������� ������� � ����� ����� � �����
      // �� ������������ ��������� �� �����

      // ������� � ��������� ��������� ���� � ������ ������ (AddStream)
      S := TStringStream.Create('�������� ��������� ���� �1');
      try
        S.Position := 0;
        ItemIndex := Zip.AddStream('test.txt', S);
        CheckResult(ItemIndex);
        // ����� �������� ���������� � ������ ��������
        Zip.Item[ItemIndex].Comment := '��� �������� �����������';
      finally
        S.Free;
      end;

      // ������� � ��������� ��������� ���� � ������ ������ (AddStream)
      // ���������� ������ ������ ���� ��� �����
      S := TStringStream.Create('�������� ��������� ���� �2');
      S.Position := 0;
      ItemIndex := Zip.AddStream('test2.txt', S, soOwned);
      CheckResult(ItemIndex);

      // ��� ���������� ����� � ������������ �����
      // ���������� ������� �� ������� � ���� � �����, �������� ��� ���
      S := TStringStream.Create('�������� ��������� ���� �2');
      try
        S.Position := 0;
        CheckResult(Zip.AddStream(
          'AddStreamData\SubFolder1\Subfolder2\Test.txt', S));
      finally
        S.Free;
      end;

      // ������ ����� �������� ���� ��������� ���������� ������
      // ��������� �������������� �� �����

      // ������� ������:
      // ��������� � ����� ���������� ����� "Create ZIP 2" �������
      // �������� ������ AddFolder
      if Zip.AddFolder('..\Create ZIP 2\') = 0 then
        raise Exception.Create('������ ���������� ������');

      // ������� ������:
      // ��������� ���������� ����� �������� ���������� � ����� AddFolderDemo
      // ��� ������ ������ ����������� ������� AddFolder,
      // � ������� ����� ������� ������������ ����� ������ ������ � �������
      // ������������� ���������� �������� (������ ��������)
      if Zip.AddFolder('AddFolderDemo', '..\..\', '*.pas', False) = 0 then
        raise Exception.Create('������ ���������� ������');

      // ������� ������. ���������� ��-�� ����� �� �������� ����������,
      // ������ ��������� ����� ������ ��� ������ ������ AddFile
      PresentFiles := TStringList.Create;
      try
        // ��� ������ �� ��� ������
        if FindFirst('..\..\*.pas', faAnyFile, SR) = 0 then
        try
          repeat
            if (SR.Name = '.') or (SR.Name = '..') then Continue;
            if SR.Attr and faDirectory <> 0 then
              Continue
            else
              PresentFiles.Add(SR.Name);
          until FindNext(SR) <> 0;
        finally
          FindClose(SR);
        end;

        // ������ ������� �� ������,
        // �������� � ����� ����� � ��� ����� ������ �� ���������.
        for I := 0 to PresentFiles.Count - 1 do
          CheckResult(Zip.AddFile('..\..\' + PresentFiles[I],
            'AddFile\' + PresentFiles[I]));

        // ��������� ������� - ���������� ������� ��� ������ ������ AddFiles.
        // ������ ������� ������ ������ ���� ����������� ��������� �������:
        // "������������� ���� � ��� � ������"="���� � �����"
        // �.�. ValueFromIndex ��������� �� ���� � �����,
        // � Names - ������������� ���� � ������
        // ���� �� ������� ������������� ����, �� ����� ������� ������ ��� �����.
        for I := 0 to PresentFiles.Count - 1 do
          PresentFiles[I] :=
            'AddFiles\' + PresentFiles[I] + '=' + '..\..\' + PresentFiles[I];
        if Zip.AddFiles(PresentFiles) <> PresentFiles.Count then
          raise Exception.Create('������ ���������� ������');

      finally
        PresentFiles.Free;
      end;

      // � ��������� �������, ��-�� ���������� �������,
      // ������ � ������ ������ ����� �������� �����. ����� AddFilesAndFolders.
      // ����� ���������� � ������ �� ����-�� �������� ��� � � ������ AddFiles.
      // ������ ��� ����� ����������� �� ��������: "������������� ���� � ������"="���� � �����"
      // �.�. ValueFromIndex ��������� �� ���� � �����,
      // � Names - ������������� ���� � ������ �� �����

      // ����� ������� ��� ����� � ����� �� ����� �������
      PresentFiles := TStringList.Create;
      try
        if FindFirst('..\..\*.*', faAnyFile, SR) = 0 then
        try
          repeat
            if (SR.Name = '.') or (SR.Name = '..') then Continue;
            // ���������� ����� demos, �.�. ��� ����� ���� ���������� ������ �����
            if AnsiLowerCase(SR.Name) = 'demos' then
              Continue;
            PresentFiles.Add('AddFilesAndFolders\' + SR.Name + '=..\..\' + SR.Name);
          until FindNext(SR) <> 0;
        finally
          FindClose(SR);
        end;
        Zip.AddFilesAndFolders(PresentFiles, True);
      finally
        PresentFiles.Free;
      end;

      // ��� ���������� � ��� - �������� ������� ��� �����...
      ForceDirectories('..\DemoResults\');
      BuildZipResult := Zip.BuildZip('..\DemoResults\CreateZIPDemo1.zip');
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
