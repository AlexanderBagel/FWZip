////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : CreateZIPDemo1
//  * Purpose   : ������������ �������� ������ ��������� ���������
//  *           : �������� ���������� ������
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

// ������ ������ ���������� ��������� �������� ���������� ���������� � �����
// ��� ������� �� �������� ���������� � ������ ����� ������� ��������� �����

program CreateZIPDemo1;

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
  FWZipConsts,
  FWZipWriter,
  FWZipUtils;

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
  Attributes: TFileAttributeData;
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
      {$IFDEF FPC}
      S := TStringStream.Create(AnsiString('�������� ��������� ���� �1'));
      {$ELSE}
      S := TStringStream.Create('�������� ��������� ���� �1');
      {$ENDIF}
      try
        S.Position := 0;
        ItemIndex := Zip.AddStream('test.txt', S);
        CheckResult(ItemIndex);
        // ����� �������� ���������� � ������ ��������
        Zip.Item[ItemIndex].Comment := '��� �������� �����������';

        // ��� ������������ ����� ����� ��������, � �������� ����������
        // ������������ ������� �����.
        // ������� ����� ��������� ������������ ���������
        // ��� ������� ������� �x � ������ ����
        if GetFileAttributes(ParamStr(0), Attributes) then
        begin

          // �����! ���� � �������� � ��������� ������������.
          // ������������ ������ ���� �������� �����.

          // ��� ���� ������ ���������� ������������ ��� ��� ����������� ��������
          Zip.Item[ItemIndex].ChangeAttributes(Attributes);

          // � ��� �� ����� ����� ������� ��������� ����� ��� ����������
          CheckResult(Zip.AddStream('file_with_custom_attributes.txt', S));
        end;
      finally
        S.Free;
      end;

      // ������� � ��������� ��������� ���� � ������ ������ (AddStream)
      // ���������� ������ ������ ����� ��� �����
      {$IFDEF FPC}
      S := TStringStream.Create(AnsiString('�������� ��������� ���� �2'));
      {$ELSE}
      S := TStringStream.Create('�������� ��������� ���� �2');
      {$ENDIF}
      S.Position := 0;
      ItemIndex := Zip.AddStream('�������� ���� ����� �2.txt', S, soOwned);
      CheckResult(ItemIndex);

      // ��� ���������� ����� � ������������ �����
      // ���������� ������� �� ������� � ���� � �����, �������� ��� ���
      {$IFDEF FPC}
      S := TStringStream.Create(AnsiString('�������� ��������� ���� �3'));
      {$ELSE}
      S := TStringStream.Create('�������� ��������� ���� �3');
      {$ENDIF}
      try
        S.Position := 0;
        CheckResult(Zip.AddStream(
          'AddStreamData\SubFolder1\Subfolder2\�������� ���� ����� �3.txt', S));
      finally
        S.Free;
      end;

      // �������� ������ ����� � ������
      CheckResult(Zip.AddEmptyFolder('FirstEmptyFolder'));

      // �������� ������ ����� � ������� �������� ����
      CheckResult(Zip.AddEmptyFolder('SecondEmptyFolder\Subfolder1\Subfolder2'));

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
        if FindFirst(PathCanonicalize('..\..\*.pas'), faAnyFile, SR) = 0 then
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
            'AddFiles\' + PresentFiles[I] + '=..\..\' + PresentFiles[I];
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
        if FindFirst(PathCanonicalize('..\..\*'), faAnyFile, SR) = 0 then
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
