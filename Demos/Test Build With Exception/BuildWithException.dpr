////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : BuildWithException
//  * Purpose   : ������������ ������ � ������������
//  *           : ��� �������� � ���������� ������
//  * Author    : ��������� (Rouse_) ������
//  * Copyright : � Fangorn Wizards Lab 1998 - 2013.
//  * Version   : 1.0.10
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

// ������ ������ ���������� ������ � ���������� �������� �������� ����������
// � �������� �������� � ���������� ������, � ���-�� ������� �� ���������.

program BuildWithException;

{$APPTYPE CONSOLE}

uses
  Windows,
  Classes,
  SysUtils,
  TypInfo,
  FWZipConsts,
  FWZipWriter,
  FWZipReader;

var
  Writer: TFWZipWriter;
  Reader: TFWZipReader;
  Method: TMethod;
  hOFStruct: TOFStruct;
  hFile: THandle;

//
// ��������� ������� ��������� ������ ������� BuildZip
// =============================================================================
procedure ShowBuildResult(Value: TBuildZipResult);
begin
  Writeln(GetEnumName(TypeInfo(TBuildZipResult), Integer(Value)));
end;

//
// ��������� ������� ��������� ������ ������� Extract
// =============================================================================
procedure ShowManualExtractResult(const ElementName: string;
  Value: TExtractResult);
begin
  Writeln(Format('%s -> %s', [ElementName,
    GetEnumName(TypeInfo(TExtractResult), Integer(Value))]));
end;

//
// ������ �������� ����������� ����
// =============================================================================
procedure OnException1(Self, Sender: TObject; E: Exception;
  const ItemIndex: Integer; var Action: TExceptionAction;
  var NewFilePath: string; NewFileData: TMemoryStream);
var
  CurrentFilePath: string;
  Src: THandleStream;
  Dst: TFileStream;
  hOFStruct: TOFStruct;
  hFile: THandle;
begin
  CurrentFilePath := string(TFWZipWriter(Sender)[ItemIndex].FilePath);
  NewFilePath := ChangeFileExt(CurrentFilePath, '.tmp');
  hFile := OpenFile(PAnsiChar(AnsiString(CurrentFilePath)),
    hOFStruct, OF_READ);
  try
    Src := THandleStream.Create(hFile);
    try
      Dst := TFileStream.Create(NewFilePath, fmCreate);
      try
        Dst.CopyFrom(Src, 0);
      finally
        Dst.Free;
      end;
    finally
      Src.Free;
    end;
  finally
    CloseHandle(hFile);
  end;
  Action := eaUseNewFilePathAndDel;
end;

//
// ������ �������� ����������� ����
// =============================================================================
procedure OnException2(Self, Sender: TObject; E: Exception;
  const ItemIndex: Integer; var Action: TExceptionAction;
  var NewFilePath: string; NewFileData: TMemoryStream);
begin
  CloseHandle(hFile);
  hFile := INVALID_HANDLE_VALUE;
  Action := eaRetry;
end;

//
// ������ �������� ����������� ����
// =============================================================================
procedure OnException3(Self, Sender: TObject; E: Exception;
  const ItemIndex: Integer; var Action: TExceptionAction;
  var NewFilePath: string; NewFileData: TMemoryStream);
var
  Src: THandleStream;
  hOFStruct: TOFStruct;
  hFile: THandle;
begin
  hFile := OpenFile(
    PAnsiChar(AnsiString(TFWZipWriter(Sender)[ItemIndex].FilePath)),
    hOFStruct, OF_READ);
  try
    Src := THandleStream.Create(hFile);
    try
      NewFileData.CopyFrom(Src, 0);
    finally
      Src.Free;
    end;
  finally
    CloseHandle(hFile);
  end;
  Action := eaUseNewFileData;
end;

//
// ������ �������� ����������� ����
// =============================================================================
procedure OnDuplicate(Self, Sender: TObject;
  var Path: string; var Action: TDuplicateAction);
begin
  Path := MakeUniqueName(Path);
  Action := daUseNewFilePath;
end;

var
  I: Integer;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try

    // ����� ��������� ������ ��� �������� ������ - ��� ���������� �������
    // � ������������ � ����� �����.
    // �������� ��� ����� ��� �������� �������������� ���������� ��������
    // ����� � ������� ������������ ������� ���� �� ���������
    // � ���� ������ ��������� ������ ������� � ���������� �����.
    // ���� �� ��������� ����������� ����������, �� ����� ���� ����� ��������
    // (�������� �� ��������� eaSkip) � ������� BuildZip
    // ������ ��������� ���� ������:
    // brFailed - � ������ ���� � ����� ������ ������ ������
    // ����� ����������� (�.�. � ����� ��������� ������)
    // brPartialBuild - � ������ ���� � ����� ���-�� ���� ��������� �����-���� �����,
    // �� ��������� �� ��� ���� ���������

    Writer := TFWZipWriter.Create;
    try
      Writer.AddFolder('', '..\..\', '*.pas', False);
      ForceDirectories('..\DemoResults\');
      // ����� ���� �� ������ ��� ������������
      hFile := OpenFile(PAnsiChar(AnsiString('..\..\' + Writer[0].FileName)),
        hOFStruct, OF_WRITE);
      try
        Write('BuildWithException1.zip -> ');
        ShowBuildResult(Writer.BuildZip('..\DemoResults\BuildWithException1.zip'));
      finally
        CloseHandle(hFile);
      end;
    finally
      Writer.Free;
    end;

    // ������ ����� ����� ���� ��������� � ���������� ��������� ������ ��������
    // ����� ����������� ������� OnException
    // � ��������� ������� ����� �������� ��� ���-�� ���������� ����� ������
    // � �������� ���������� ���� � �����

    Writer := TFWZipWriter.Create;
    try
      Writer.AddFolder('', '..\..\', '*.pas', False);
      ForceDirectories('..\DemoResults\');

      // ����� ���� �� ������ ��� ������������
      hFile := OpenFile(PAnsiChar(AnsiString('..\..\' + Writer[0].FileName)),
        hOFStruct, OF_WRITE);
      try

        // ��������� ���������� ����� ������� �� ����� ������������ ������
        // � ����������� OnException1 ����� ����������� ����� �����
        // ����� ���� �� ������ � ��������� NewFilePath ����� ���� � �����,
        // � �������� Action �������� eaUseNewFilePathAndDel
        // ����� ������� �� ���������� FWZip ��� ����� ��������� �������
        // ��������� �����, ��� ���� ���������� ������������ ����� ���� � �����,
        // ����� ���� ������ ���� ������� �������.

        Method.Code := @OnException1;
        Method.Data := Writer;
        Writer.OnException := TZipBuildExceptionEvent(Method);

        Write('BuildWithException2.zip -> ');
        ShowBuildResult(Writer.BuildZip('..\DemoResults\BuildWithException2.zip'));

        // ������ �������, ��������� ������� � ����������� OnException2
        // � ��� �� ������� ������������� ���������� ����� � ����������
        // �������� Action � eaRetry.
        // ����� ������� �� ���������� FWZip ��� ����� ��������� �������
        // ��������� �����

        Method.Code := @OnException2;
        Method.Data := Writer;
        Writer.OnException := TZipBuildExceptionEvent(Method);

        Write('BuildWithException3.zip -> ');
        ShowBuildResult(Writer.BuildZip('..\DemoResults\BuildWithException3.zip'));

        // ��� ������������ ���������� ���������� �� �����
        if hFile = INVALID_HANDLE_VALUE then
          hFile := OpenFile(PAnsiChar(AnsiString('..\..\' + Writer[0].FileName)),
            hOFStruct, OF_WRITE);

        // ������ �������, ��������� ������� � ����������� OnException3
        // � ��� �� ��������� ����� �������� ������ ����� � ����� �
        // ���������� �������� Action � eaUseNewFileData
        // ����� ������� ������ ����� ������� ��������������� �� ������
        // NewFileData

        Method.Code := @OnException3;
        Method.Data := Writer;
        Writer.OnException := TZipBuildExceptionEvent(Method);

        Write('BuildWithException4.zip -> ');
        ShowBuildResult(Writer.BuildZip('..\DemoResults\BuildWithException4.zip'));

        // ������������ �� ��� ���������� ����� ������������� ������� ������ �����������.
        // ���� �� �� ������ ��� ���������� �� ��� ���� ����������,
        // ������� ��������� �������� Action � eaSkip (���������� �� ���������)
        // ��� ���� ����� ���������� ���������� ����, ��� eaAbort,
        // ������� ����� ������� �������� ������.

      finally
        if hFile <> INVALID_HANDLE_VALUE then
          CloseHandle(hFile);
      end;
    finally
      Writer.Free;
    end;

    // ��� ���������� ������ ��������� ��������� �������� �������
    // ���������� ��� ������������� �� ����� �����, ���� ������
    // ���������� ������, ���������� ��������� �����������.
    // ��������� ������ ���������� �������� �������������� �����
    // ����� ������ ZLib (��. Readme.txt ����� 9)
    // ������������� ������ �� ������ �������� �������� � �������������
    // ������� OnException. ���� ������ ������� �� ���������,
    // �� � ������ ������ ������ TFWZipReader.ExtractAll
    // ���������� ������ ����� �����������.
    // � ������, ���� ������ ������� ���������, �� �������
    // � ��������� ���������� ������ ��������� �����������,
    // ������������ ����� Handled:
    // (Handled = True, ���������� ����������, ����� ���������� ����������)

    // ��� ������������ ������������ ������ ����������,
    // ��� ����� ����� ������ ����������� ���� � ���-�� �����
    // � ���� � ��-�� �����

    Reader := TFWZipReader.Create;
    try
      Reader.LoadFromFile('..\DemoResults\BuildWithException1.zip');
      ForceDirectories('..\DemoResults\BuildWithExceptionUnpack\');

      // ������������� ������ ���

      Reader.ExtractAll('..\DemoResults\BuildWithExceptionUnpack\');

      // ������ ������� ����������� ��������.
      // ���������� � ������ ������ �� ����������, �� ��� �������� ����� ���������

      Reader.ExtractAll('..\DemoResults\BuildWithExceptionUnpack\');

      // ������� ��������� ����� ������� � ��� ������ ����������.
      // �.�. ����� Reader[I].Extract � ������� �� Reader.ExtractAll
      // ���������� ���������

      Writeln('Manual extract:');
      for I := 0 to Reader.Count - 1 do
        ShowManualExtractResult(
          string(Reader[I].FileName),
          Reader[I].Extract('..\DemoResults\BuildWithExceptionUnpack\', ''));

      // ��� ����� ��������, ��� �������� ������������� ���� ���������
      // (Reader[I].Extract ������ erSkiped ��� ������� ��������)

      // ��� ��������� ������ �������� � ������ �������������� ���������� (ExtractAll)
      // ���������� ��������� ������� OnDuplicate � ������ TFWZipReader.
      // � ������ ������ ����������, ����������� ������� OnDuplicate ���������
      // � ������� �������� (Reader.Items[������ ��������].OnDuplicate)

      Method.Code := @OnDuplicate;
      Method.Data := Reader;
      Reader.OnDuplicate := TZipDuplicateEvent(Method);

      // ������ ��������� �������� �����������.
      // ��� ������������� ������� OnDuplicate � ����������� �������� �����
      // ��������� ����� ��� � �������� Action ����� ��������� � daUseNewFilePath.
      // ����� ������� �� ������ TFWZipReader-� ��� ���������� �����������
      // ���� � ����� ������...
      // (�.�. ������� ������ �������� ������ � ���������� Windows, ��������:
      // New folder -> New folder (2) -> New folder (3) � �.�.)

      Reader.ExtractAll('..\DemoResults\BuildWithExceptionUnpack\');

    finally
      Reader.Free;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
