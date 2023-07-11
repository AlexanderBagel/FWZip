////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : BuildWithException
//  * Purpose   : ������������ ������ � ������������
//  *           : ��� �������� � ���������� ������
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

// ������ ������ ���������� ������ � ���������� �������� �������� ����������
// � �������� �������� � ���������� ������, � ���-�� ������� �� ���������.

program BuildWithException;

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFNDEF FPC}
  Windows,
  {$ENDIF}
  Classes,
  SysUtils,
  TypInfo,
  FWZipConsts,
  FWZipWriter,
  FWZipReader,
  FWZipUtils;

const
  INVALID_HANDLE_VALUE = THandle(-1);
  ReadLock = fmOpenRead or fmShareDenyNone;
  {$IFDEF LINUX}
  WriteLock = fmOpenWrite or fmShareExclusive;
  {$ELSE}
  WriteLock = fmOpenWrite or fmShareDenyNone;
  {$ENDIF}

var
  Writer: TFWZipWriter;
  Reader: TFWZipReader;
  Method: TMethod;
  hLockedFile: THandle;

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

procedure DropLock;
begin
  FileClose(hLockedFile);
  hLockedFile := INVALID_HANDLE_VALUE;
end;

//
// ������ �������� ����������� ����
// =============================================================================
procedure OnException1({%H-}Self, Sender: TObject; {%H-}E: Exception;
  const ItemIndex: Integer; var Action: TExceptionAction;
  var NewFilePath: string; {%H-}NewFileData: TMemoryStream);
var
  CurrentFilePath: string;
  Src: THandleStream;
  Dst: TFileStream;
  hFile: THandle;
begin
  {$IFDEF LINUX}
  // � ����������� ��� ������� ��������, ������� ����� ����� ���
  DropLock;
  {$ENDIF}
  CurrentFilePath := string(TFWZipWriter(Sender)[ItemIndex].FilePath);
  NewFilePath := ChangeFileExt(CurrentFilePath, '.tmp');
  hFile := FileOpen(CurrentFilePath, ReadLock);
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
    FileClose(hFile);
  end;
  Action := eaUseNewFilePathAndDel;
end;

//
// ������ �������� ����������� ����
// =============================================================================
procedure OnException2({%H-}Self, Sender: TObject; {%H-}E: Exception;
  const {%H-}ItemIndex: Integer; var Action: TExceptionAction;
  var {%H-}NewFilePath: string; {%H-}NewFileData: TMemoryStream);
begin
  DropLock;
  Action := eaRetry;
end;

//
// ������ �������� ����������� ����
// =============================================================================
procedure OnException3({%H-}Self, Sender: TObject; {%H-}E: Exception;
  const ItemIndex: Integer; var Action: TExceptionAction;
  var {%H-}NewFilePath: string; NewFileData: TMemoryStream);
var
  Src: THandleStream;
  hFile: THandle;
begin
  {$IFDEF LINUX}
  // � ����������� ��� ������� ��������, ������� ����� ����� ���
  DropLock;
  {$ENDIF}
  hFile := FileOpen(TFWZipWriter(Sender)[ItemIndex].FilePath, ReadLock);
  try
    Src := THandleStream.Create(hFile);
    try
      NewFileData.CopyFrom(Src, 0);
    finally
      Src.Free;
    end;
  finally
    FileClose(hFile);
  end;
  Action := eaUseNewFileData;
end;

//
// ������ �������� ����������� ����
// =============================================================================
procedure OnDuplicate({%H-}Self, Sender: TObject;
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
      // ����� ���� �� ������ ��� ������������
      hLockedFile := FileOpen(PathCanonicalize('..\..\' + Writer[0].FileName), WriteLock);
      try
        Write('BuildWithException1.zip -> ');
        ShowBuildResult(Writer.BuildZip('..\DemoResults\BuildWithException1.zip'));
      finally
        FileClose(hLockedFile);
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

      // ����� ���� �� ������ ��� ������������
      hLockedFile := FileOpen(PathCanonicalize('..\..\' + Writer[0].FileName), WriteLock);
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

        {$IFDEF LINUX}
        if hLockedFile = INVALID_HANDLE_VALUE then
          hLockedFile := FileOpen(PathCanonicalize('..\..\' + Writer[0].FileName), WriteLock);
        {$ENDIF}

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
        if hLockedFile = INVALID_HANDLE_VALUE then
          hLockedFile := FileOpen(PathCanonicalize('..\..\' + Writer[0].FileName), WriteLock);

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
        if hLockedFile <> INVALID_HANDLE_VALUE then
          FileClose(hLockedFile);
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
