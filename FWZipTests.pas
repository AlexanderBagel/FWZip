////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : FWZipTests
//  * Purpose   : Набор классов для юниттестирования FWZip
//  * Author    : Александр (Rouse_) Багель
//  * Copyright : © Fangorn Wizards Lab 1998 - 2020.
//  * Version   : 1.1.0
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Stable Release : http://rouse.drkb.ru/components.php#fwzip
//  * Latest Source  : https://github.com/AlexanderBagel/FWZip
//  ****************************************************************************
//
//  Используемые источники:
//  ftp://ftp.info-zip.org/pub/infozip/doc/appnote-iz-latest.zip
//  http://zlib.net/zlib-1.2.5.tar.gz
//  http://www.base2ti.com/
//

unit FWZipTests;

interface

uses
  TestFramework, Windows, Masks, FWZipStream, Classes, Contnrs, SysUtils,
  FWZipWriter, FWZipConsts, FWZipCrc32, FWZipCrypt, FWZipZLib, FWZipReader,
  FWZipModifier;

type
  EFWZipUnitTestException = class(Exception);
  TFWZipUnitTest = class(TTestCase)
  strict private
    FZipWriter: TFWZipWriter;
    FZipReader: TFWZipReader;
    function Reader: TFWZipReader;
    function Writer: TFWZipWriter;
    procedure CheckResult(Value: Integer);
    procedure CheckCount(ACount, Expected: Integer);
    procedure CheckSize(const FilePath: string; Expected: Int64); overload;
    procedure CheckSize(ASize, Expected: Int64); overload;
    procedure CheckBuildResult(Value: TBuildZipResult; Expected: TBuildZipResult = brDone);
    procedure Clear;
    procedure CheckReaderWidthExcept;
    procedure CheckLoadReaderWidthExcept(const Path: string);
  protected
    procedure TearDown; override;
  published
    // Легкие тесты обычного архива
    procedure TestBuildWithStream;
    procedure TestBuildWithExistingFile;
    procedure TestBuildWithExistingFile2;
    procedure TestBuildWithPassword;
    procedure TestBuildWithPassword1;
    procedure TestMerge2Zip;
    procedure TestChangeZip;
    procedure TestDeleteFromZip;
    procedure TestSplitZip;
    procedure TestBuildWithException;
    {$IFNDEF WINE}
    procedure TestExtractOverride;
    {$ENDIF}
    procedure TestExData;
    // тесты стрима для мультипарт архива
    procedure TestMultiStream1;
    procedure TestMultiStream2;
    procedure TestMultiStream3;
    procedure TestMultiStream4;
    procedure TestMultiStream5;
    procedure TestMultiStream6;
    procedure TestMultiStream7;
    procedure TestMultiStream8;
    procedure TestMultiStream9;
    procedure TestMultiStream10;
    procedure TestMultiStream11;
    // Тесты для MultyPart архива
    procedure TestMultyPartBuildWithStream;
    procedure TestMultyPartBuildWithExistingFile;
    procedure TestMultyPartBuildWithExistingFile2;
    procedure TestMultyPartBuildWithPassword;
    procedure TestMultyPartBuildWithPassword1;
    procedure TestMultyPartMerge2Zip;
    procedure TestMultyPartChangeZip;
    procedure TestMultyPartDeleteFromZip;
    procedure TestMultyPartSplitZip;
    procedure TestMultyPartBuildWithException;
    {$IFNDEF WINE}
    procedure TestMultyPartExtractOverride;
    {$ENDIF}
    procedure TestMultyPartExData;
    procedure TestMultyPartBuildSingleZip;
    // Тяжелые тесты для обычного архива
    procedure TestZip64_BigFiles;
    procedure TestZip64_BigFilesCount;
    // Тяжелые тесты для MultyPart архива
    procedure TestMultyPartZip64_BigFiles;
    procedure TestMultyPartZip64_BigFilesCount;
  end;

implementation

uses
  TypInfo;

type
  TDataCount = record
    Files, Folders: Integer;
  end;

const
  ZipName = 'test.zip';
  ZipName1 = 'test1.zip';
  ZipName2 = 'test2.zip';

  TestStringBlock: array [0..2] of string = (
    'Тестовый текстовый файл №1',
    'Тестовый текстовый файл №2',
    'Тестовый текстовый файл №3'
  );

  TestComment = 'Мой тестовый комментарий';

var
  RootFolder: string
  {$IFDEF CUSTOM_TEMP_FOLDER_PATH}
   = 'E:\FWZip_UnitTest\'
  {$ENDIF};
  _SorceFolder, _DestinationFolder: string;
  TestFolderData: TStringList;

function SourceFolder: string;
begin
  Result := _SorceFolder;
end;

function DestinationFolder: string;
begin
  Result := _DestinationFolder;
end;

procedure InitFolders;
begin
  {$IFNDEF CUSTOM_TEMP_FOLDER_PATH}
  SetLength(RootFolder, MAX_PATH);
  if GetTempPath(MAX_PATH, @RootFolder[1]) = 0 then Exit;
  RootFolder := IncludeTrailingPathDelimiter(PChar(RootFolder));
  {$ENDIF}
  ForceDirectories(RootFolder);
  _SorceFolder := RootFolder + 'fwziptest\src\';
  ForceDirectories(_SorceFolder);
  _DestinationFolder := RootFolder + 'fwziptest\dst\';
  ForceDirectories(_DestinationFolder);

  TestFolderData := TStringList.Create;
  TestFolderData.Add('1.txt');
  TestFolderData.Add('2.txt');
  TestFolderData.Add('3.txt');
  TestFolderData.Add('1\1.txt');
  TestFolderData.Add('1\2.no_txt');
  TestFolderData.Add('1\3.txt');
  TestFolderData.Add('2\1.txt');
  TestFolderData.Add('2\2.txt');
  TestFolderData.Add('2\3.txt');
  TestFolderData.Add('3\');
  TestFolderData.Add('4\5\2.txt');
  TestFolderData.Add('4\5\3.txt');
  TestFolderData.Add('7\8\9\10\1.bin');
  TestFolderData.Add('7\8\9\10\2.bin');
end;

procedure DeleteFolder(const Path: string);
var
  SR: TSearchRec;
begin
  if FindFirst(Path + '*.*', faAnyFile, SR) = 0 then
  try
    repeat
      if SR.Name = '.' then Continue;
      if SR.Name = '..' then Continue;
      if DirectoryExists(Path + SR.Name) then
      begin
        DeleteFolder(Path + SR.Name + '\');
        RemoveDirectory(PChar(Path + SR.Name));
        Continue;
      end;
      DeleteFile(PChar(Path + SR.Name))
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
  RemoveDirectory(PChar(Path));
end;

procedure CreateFile(const Path: string);
var
  I, Index: Integer;
  S: TStringStream;
  F: TFileStream;
begin
  Index := -1;
  if ExtractFileName(Path) = '1.txt' then
    Index := 0;
  if ExtractFileName(Path) = '2.txt' then
    Index := 1;
  if ExtractFileName(Path) = '3.txt' then
    Index := 2;
  if Index >= 0 then
  begin
    S := TStringStream.Create(TestStringBlock[Index]);
    try
      for I := 0 to 255 do
      begin
        S.WriteString(sLineBreak);
        S.WriteString(TestStringBlock[Index]);
      end;
      S.SaveToFile(Path);
    finally
      S.Free;
    end;
    Exit;
  end;
  F := TFileStream.Create(Path, fmCreate);
  try
    for I := 0 to 255 do
      F.Write(I, 1);
  finally
    F.Free;
  end;
end;

function CreateTestDataInSrcFolder(const Root: string): TDataCount;
var
  I: Integer;
  Path: string;
begin
  ZeroMemory(@Result, SizeOf(TDataCount));
  for I := 0 to TestFolderData.Count - 1 do
  begin
    Path := Root + TestFolderData[I];
    ForceDirectories(ExtractFilePath(Path));
    if DirectoryExists(Path) then
    begin
      Inc(Result.Folders);
      Continue;
    end;
    CreateFile(Path);
    Inc(Result.Files);
  end;
end;

procedure CheckFiles(const Src, Dst: string);
begin
  if FileCRC32(Src) <> FileCRC32(Dst) then
    raise Exception.Create('Контрольная сумма распакованного файла не сошлась');
end;

procedure CheckStream(StreamIndex: Integer; const Dst: string);
var
  S: AnsiString;
begin
  S := AnsiString(TestStringBlock[StreamIndex]);
  if CRC32Calc(@S[1], Length(S)) <> FileCRC32(Dst) then
    raise Exception.Create('Контрольная сумма распакованного файла не сошлась');
end;

procedure ClearFolder(const Path: string);
begin
  DeleteFolder(Path);
  ForceDirectories(Path);
end;

function GetTestFolderPath(TestIndex: Integer; SrcFolder: Boolean;
  Clear: Boolean = False): string;
begin
  if SrcFolder then
    Result := SourceFolder
  else
    Result := DestinationFolder;
  Result := Result + 'test_' + IntToStr(TestIndex) + '\';
  if Clear then
    ClearFolder(Result)
  else
    ForceDirectories(Result);
end;

procedure CreateLargeFile(const Path: string);
var
  Buff: array of Byte;
  F: TFileStream;
  I, L: Integer;
  MaxSize: Int64;
begin
  L := MAXWORD;
  SetLength(Buff, $FFFF);
  for I := 0 to L - 1 do
    Buff[I] := Byte(I);

  F := TFileStream.Create(Path, fmCreate);
  try
    MaxSize := $FFFFFFFF;
    MaxSize := MaxSize + 150;
    while F.Size < MaxSize do
      F.WriteBuffer(Buff[0], L);
  finally
    F.Free;
  end;
end;

procedure CreateBigFilesInFolder(const Root: string; One: Boolean = False);
begin
  CreateLargeFile(Root + '1.bin');
  if One then Exit;
  CreateLargeFile(Root + '2.bin');
end;

procedure CreateBinFile(const Path: string);
const
  D: AnsiString = 'TEST';
var
  F: TFileStream;
begin
  F := TFileStream.Create(Path, fmCreate);
  try
    F.WriteBuffer(D[1], 4);
  finally
    F.Free;
  end;
end;

procedure CreateManyFilesInFolder(const Root: string);
var
  I: Integer;
begin
  for I := 0 to MAXWORD + 20 do
    CreateBinFile(Root + IntToStr(I) + '.txt');
end;

procedure DeleteManyFilesInFolder(const Root: string);
var
  I: Integer;
begin
  for I := 0 to MAXWORD + 20 do
    DeleteFile(Root + IntToStr(I) + '.txt');
end;

function GetFileSize(const Path: string): Int64;
var
  F: TFileStream;
begin
  F := TFileStream.Create(Path, fmShareDenyWrite);
  try
    Result := F.Size;
  finally
    F.Free;
  end;
end;

function AddItem(AWriter: TFWZipWriter; const AName, AData: string): Integer;
var
  S: TStringStream;
begin
  S := TStringStream.Create(AData);
  try
    S.Position := 0;
    Result := AWriter.AddStream(AName, S);
  finally
    S.Free;
  end;
end;

{ Test_CreateZip }

procedure TFWZipUnitTest.CheckBuildResult(Value: TBuildZipResult;
  Expected: TBuildZipResult);
begin
  if Expected <> brDone then
  begin
    if Value <> Expected then
      raise EFWZipUnitTestException.Create('Ошибка создания архива. Ожидалось ' +
      GetEnumName(TypeInfo(TBuildZipResult), Integer(Expected)) +
      ', но вернулся результат ' +
      GetEnumName(TypeInfo(TBuildZipResult), Integer(Value)));
  end
  else
    if Value <> Expected then
      raise EFWZipUnitTestException.Create('Ошибка создания архива. Result = ' +
      GetEnumName(TypeInfo(TBuildZipResult), Integer(Value)));
end;

procedure TFWZipUnitTest.CheckCount(ACount, Expected: Integer);
begin
  if ACount <> Expected then
    raise EFWZipUnitTestException.CreateFmt(
      'Неверное количество данных в архиве. Ожидалось %d по факту %d', [Expected, ACount]);
end;

procedure TFWZipUnitTest.CheckLoadReaderWidthExcept(const Path: string);
var
  HasException: Boolean;
begin
  HasException := False;
  try
    Reader.LoadFromFile(Path);
  except
    HasException := True;
  end;
  if not HasException then
    raise EFWZipUnitTestException.Create('Reader.LoadFromFile не поднял ожидаемое исключения');
end;

procedure TFWZipUnitTest.CheckReaderWidthExcept;
var
  HasException: Boolean;
begin
  HasException := False;
  try
    Reader.Check;
  except
    HasException := True;
  end;
  if not HasException then
    raise EFWZipUnitTestException.Create('Reader.Check не поднял ожидаемое исключения');
end;

procedure TFWZipUnitTest.CheckResult(Value: Integer);
begin
  if Value < 0 then
    raise EFWZipUnitTestException.Create('Ошибка добавления данных');
end;

procedure TFWZipUnitTest.CheckSize(ASize, Expected: Int64);
begin
  if ASize <> Expected then
    raise EFWZipUnitTestException.CreateFmt(
      'Неверное количество размер архива. Ожидалось %d по факту %d',
      [Expected, ASize]);
end;

procedure TFWZipUnitTest.CheckSize(const FilePath: string; Expected: Int64);
var
  ASize: Int64;
begin
  ASize := GetFileSize(FilePath);
  if ASize <> Expected then
    raise EFWZipUnitTestException.CreateFmt(
      'Неверное количество размер тома архива "%s". Ожидалось %d по факту %d',
      [FilePath, Expected, ASize]);
end;

procedure TFWZipUnitTest.Clear;
begin
  FZipWriter.Free;
  FZipWriter := TFWZipWriter.Create;
  FZipReader.Free;
  FZipReader := TFWZipReader.Create;
end;

function TFWZipUnitTest.Reader: TFWZipReader;
begin
  Result := FZipReader;
end;

var
  Method: TMethod;
  hOFStruct: TOFStruct;
  hFile: THandle;

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

procedure OnException2(Self, Sender: TObject; E: Exception;
  const ItemIndex: Integer; var Action: TExceptionAction;
  var NewFilePath: string; NewFileData: TMemoryStream);
begin
  CloseHandle(hFile);
  hFile := INVALID_HANDLE_VALUE;
  Action := eaRetry;
end;

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

procedure TFWZipUnitTest.TearDown;
begin
  inherited;
  FreeAndNil(FZipWriter);
  FreeAndNil(FZipReader);
end;

procedure TFWZipUnitTest.TestBuildWithException;
var
  Count: TDataCount;
  SrcFolder, DstFolder, DstFile: string;
begin
  Clear;
  SrcFolder := GetTestFolderPath(9, True, True);
  DstFolder := GetTestFolderPath(9, False, True);
  DstFile := DstFolder + ZipName;
  Count := CreateTestDataInSrcFolder(SrcFolder);

  // лочим один из файлов для демонстрации
  Writer.AddFile(SrcFolder + TestFolderData[0]);
  ZeroMemory(@hOFStruct, SizeOf(TOFStruct));
  hFile := OpenFile(PAnsiChar(AnsiString(SrcFolder + TestFolderData[0])),
    hOFStruct, OF_WRITE);
  try
    CheckBuildResult(Writer.BuildZip(DstFile), brFailed);

    // теперь добавляем еще один не залоченый
    DeleteFile(DstFile);
    Writer.AddFile(SrcFolder + TestFolderData[1]);
    CheckBuildResult(Writer.BuildZip(DstFile), brPartialBuild);
  finally
    CloseHandle(hFile);
  end;

  // а теперь должно быть нормально
  DeleteFile(DstFile);
  CheckCount(Writer.Count, 2);
  CheckBuildResult(Writer.BuildZip(DstFile));

  Reader.LoadFromFile(DstFile);
  Reader.Check;

  Clear;
  Writer.AddFile(SrcFolder + TestFolderData[0]);
  hFile := OpenFile(PAnsiChar(AnsiString(SrcFolder + TestFolderData[0])),
    hOFStruct, OF_WRITE);
  try

    // Назначаем обработчик через который мы будем обрабатывать ошибку
    // В обработчике OnException1 будет создаваться копия файла

    Method.Code := @OnException1;
    Method.Data := Writer;
    Writer.OnException := TZipBuildExceptionEvent(Method);
    CheckBuildResult(Writer.BuildZip(DstFile));
  finally
    CloseHandle(hFile);
  end;

  Clear;
  Writer.AddFile(SrcFolder + TestFolderData[0]);
  hFile := OpenFile(PAnsiChar(AnsiString(SrcFolder + TestFolderData[0])),
    hOFStruct, OF_WRITE);
  try

    // снимаем исскуственную блокировку файла и выставляем
    // свойство Action в eaRetry

    Method.Code := @OnException2;
    Method.Data := Writer;
    Writer.OnException := TZipBuildExceptionEvent(Method);
    CheckBuildResult(Writer.BuildZip(DstFile));
  finally
    CloseHandle(hFile);
  end;

  Clear;
  Writer.AddFile(SrcFolder + TestFolderData[0]);
  hFile := OpenFile(PAnsiChar(AnsiString(SrcFolder + TestFolderData[0])),
    hOFStruct, OF_WRITE);
  try

    // загружаем данные через стрим

    Method.Code := @OnException3;
    Method.Data := Writer;
    Writer.OnException := TZipBuildExceptionEvent(Method);
    CheckBuildResult(Writer.BuildZip(DstFile));
  finally
    CloseHandle(hFile);
  end;

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestBuildWithExistingFile;
var
  Count: TDataCount;
  SrcFolder, DstFolder, SrcFile, DstFile: string;
  I: Integer;
begin
  Clear;
  SrcFolder := GetTestFolderPath(2, True, True);
  Count := CreateTestDataInSrcFolder(SrcFolder);
  if Writer.AddFolder(SrcFolder, True) <> Count.Files then
    raise EFWZipUnitTestException.Create('Ошибка инициализации архива. Неверное количество добавленных файлов');
  CheckBuildResult(Writer.BuildZip(SrcFolder + ZipName));

  DstFolder := GetTestFolderPath(2, False, True);
  Reader.LoadFromFile(SrcFolder + ZipName);
  Reader.Check;

  Reader.ExtractAll(DstFolder);
  for I := 0 to TestFolderData.Count - 1 do
  begin
    SrcFile := SrcFolder + TestFolderData[I];
    DstFile := DstFolder + 'test_2\' + TestFolderData[I];
    if DirectoryExists(SrcFile) then Continue;
    CheckFiles(SrcFile, DstFile);
  end;

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestBuildWithExistingFile2;
var
  Count: TDataCount;
  SrcFolder, DstFolder: string;
  SrcFile, DstFile, Ext: string;
  I: Integer;
begin
  Clear;
  SrcFolder := GetTestFolderPath(3, True, True);
  Count := CreateTestDataInSrcFolder(SrcFolder);
  if Writer.AddFolder('AddFolderDemo', SrcFolder, '*.bin;*.no_txt', True) <> 3 then
    raise EFWZipUnitTestException.Create('Ошибка инициализации архива. Неверное количество добавленных файлов');
  CheckBuildResult(Writer.BuildZip(SrcFolder + ZipName));

  DstFolder := GetTestFolderPath(3, False, True);
  Reader.LoadFromFile(SrcFolder + ZipName);
  Reader.Check;

  Reader.ExtractAll(DstFolder);
  for I := 0 to TestFolderData.Count - 1 do
  begin
    SrcFile := SrcFolder + TestFolderData[I];
    DstFile := DstFolder + 'AddFolderDemo\' + TestFolderData[I];
    Ext := ExtractFileExt(DstFile);
    if (Ext <> '.no_txt') and ((Ext <> '.bin')) then Continue;
    CheckFiles(SrcFile, DstFile);
  end;

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure OnPassword(Self, Sender: TObject; const FileName: string;
  var Password: string; var CancelExtract: Boolean);
begin
  Password := 'password2';
end;

procedure TFWZipUnitTest.TestBuildWithPassword;
var
  Count: TDataCount;
  SrcFolder, DstFolder, SrcFile, DstFile: string;
  I: Integer;
  Item: TFWZipWriterItem;
begin
  Clear;
  SrcFolder := GetTestFolderPath(4, True, True);
  Count := CreateTestDataInSrcFolder(SrcFolder);
  Writer.AlwaysAddEmptyFolder := True;
  if Writer.AddFolder(SrcFolder, True) <> Count.Files then
    raise EFWZipUnitTestException.Create('Ошибка инициализации архива. Неверное количество добавленных файлов/папок');

  // Сначала ставим пароли всем элементам включая папки (что делать нельзя)
  for I := 0 to Writer.Count - 1 do
  begin
    Item := Writer[I];
    // Изменим коментарий
    Item.Comment := 'Тестовый коментарий к файлу ' + Item.FileName;
    // Установим пароль
    Item.Password := 'password' + IntToStr(Byte(I mod 3));
    // Изменим тип сжатия
    Item.CompressionLevel := TCompressionLevel(Byte(I mod 3));
  end;

  CheckBuildResult(Writer.BuildZip(SrcFolder + ZipName));

  // архив должен открыться корретно
  Reader.LoadFromFile(SrcFolder + ZipName);
  CheckCount(Reader.Count, 23);

  Reader.Clear;

  DeleteFile(SrcFolder + ZipName);

  // теперь скинем пароли с папок
  for I := 0 to Writer.Count - 1 do
  begin
    Item := Writer[I];
    if Item.IsFolder then
    begin
      Item.Password := '';
      Item.CompressionLevel := clNone;
    end;
  end;

  // теперь сбилдится должно успешно
  CheckBuildResult(Writer.BuildZip(SrcFolder + ZipName));

  Reader.LoadFromFile(SrcFolder + ZipName);
  CheckCount(Reader.Count, 23);

  CheckReaderWidthExcept; // без пароля обязано зарайзиться
  Reader.PasswordList.Add('password0');
  Reader.PasswordList.Add('password1');
  CheckReaderWidthExcept; // без отстутсвующего пароля обязано зарайзиться

  Method.Code := @OnPassword;
  Method.Data := Reader;
  Reader.OnPassword := TZipNeedPasswordEvent(Method);

  Reader.Check; // повторная проверка, теперь все пароли должны быть на месте

  DstFolder := GetTestFolderPath(4, False, True);
  Reader.ExtractAll(DstFolder);
  for I := 0 to TestFolderData.Count - 1 do
  begin
    SrcFile := SrcFolder + TestFolderData[I];
    DstFile := DstFolder + 'test_4\' + TestFolderData[I];
    if DirectoryExists(SrcFile) then Continue;
    CheckFiles(SrcFile, DstFile);
  end;

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestBuildWithPassword1;
var
  Count: TDataCount;
  SrcFolder, DstFolder, SrcFile, DstFile: string;
  I: Integer;
  Item: TFWZipWriterItem;
begin
  Clear;
  SrcFolder := GetTestFolderPath(41, True, True);
  Count := CreateTestDataInSrcFolder(SrcFolder);
  Writer.AlwaysAddEmptyFolder := True;
  if Writer.AddFolder(SrcFolder, True) <> Count.Files then
    raise EFWZipUnitTestException.Create('Ошибка инициализации архива. Неверное количество добавленных файлов/папок');

  // Сначала ставим пароли всем элементам включая папки
  for I := 0 to Writer.Count - 1 do
  begin
    Item := Writer[I];
    // Изменим коментарий
    Item.Comment := 'Тестовый коментарий к файлу ' + Item.FileName;
    // Установим пароль
    Item.Password := 'password' + IntToStr(Byte(I mod 3));
    // Вместе с дескриптором
    Item.NeedDescriptor := True;
    // Изменим тип сжатия
    Item.CompressionLevel := TCompressionLevel(Byte(I mod 3));
  end;

  CheckBuildResult(Writer.BuildZip(SrcFolder + ZipName));

  // архив должен открыться корретно
  Reader.LoadFromFile(SrcFolder + ZipName);
  CheckCount(Reader.Count, 23);

  Reader.Clear;

  DeleteFile(SrcFolder + ZipName);

  // теперь скинем пароли с папок
  for I := 0 to Writer.Count - 1 do
  begin
    Item := Writer[I];
    if Item.IsFolder then
    begin
      Item.Password := '';
      Item.NeedDescriptor := False;
      Item.CompressionLevel := clNone;
    end;
  end;

  CheckBuildResult(Writer.BuildZip(SrcFolder + ZipName));

  Reader.LoadFromFile(SrcFolder + ZipName);
  CheckCount(Reader.Count, 23);

  CheckReaderWidthExcept; // без пароля обязано зарайзиться
  Reader.PasswordList.Add('password0');
  Reader.PasswordList.Add('password1');
  CheckReaderWidthExcept; // без отстутсвующего пароля обязано зарайзиться

  Method.Code := @OnPassword;
  Method.Data := Reader;
  Reader.OnPassword := TZipNeedPasswordEvent(Method);

  Reader.Check; // повторная проверка, теперь все пароли должны быть на месте

  DstFolder := GetTestFolderPath(41, False, True);
  Reader.ExtractAll(DstFolder);
  for I := 0 to TestFolderData.Count - 1 do
  begin
    SrcFile := SrcFolder + TestFolderData[I];
    DstFile := DstFolder + 'test_41\' + TestFolderData[I];
    if DirectoryExists(SrcFile) then Continue;
    CheckFiles(SrcFile, DstFile);
  end;

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestBuildWithStream;
var
  S: TStringStream;
  ItemIndex: Integer;
  SrcFolder, DstFolder: string;
begin
  Clear;
  ItemIndex := AddItem(Writer, 'test.txt', TestStringBlock[0]);
  CheckResult(ItemIndex);
  Writer.Item[ItemIndex].Comment := TestStringBlock[2];

  S := TStringStream.Create(TestStringBlock[1]);
  S.Position := 0;
  ItemIndex := Writer.AddStream('test2.txt', S, soOwned);
  CheckResult(ItemIndex);

  S := TStringStream.Create(TestStringBlock[0]);
  S.Position := 0;
  ItemIndex := Writer.InsertStream('test0.txt', 0, S, soOwned);
  CheckResult(ItemIndex);

  CheckResult(AddItem(Writer,
    'AddStreamData\SubFolder1\Subfolder2\Test.txt', TestStringBlock[2]));

  SrcFolder := GetTestFolderPath(1, True, True);
  CheckBuildResult(Writer.BuildZip(SrcFolder + ZipName));

  Reader.LoadFromFile(SrcFolder + ZipName);
  Reader.Check;

  DstFolder := GetTestFolderPath(1, False, True);
  Reader.ExtractAll(DstFolder);
  CheckStream(0, DstFolder + 'test.txt');
  CheckStream(0, DstFolder + 'test0.txt');
  CheckStream(1, DstFolder + 'test2.txt');
  CheckStream(2, DstFolder + 'AddStreamData\SubFolder1\Subfolder2\Test.txt');

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestChangeZip;
var
  SrcFolder, DstFolder: string;
  Modifier: TFWZipModifier;
  Index: TReaderIndex;
begin
  Clear;
  SrcFolder := GetTestFolderPath(6, True, True);
  CreateTestDataInSrcFolder(SrcFolder);
  Writer.AddFile(SrcFolder + TestFolderData[0]);
  Writer.AddFile(SrcFolder + TestFolderData[1]);
  Writer.AddFile(SrcFolder + TestFolderData[2]);
  CheckBuildResult(Writer.BuildZip(SrcFolder + ZipName));

  DstFolder := GetTestFolderPath(6, False, True);

  Modifier := TFWZipModifier.Create;
  try
    Index := Modifier.AddZipFile(SrcFolder + ZipName);
    Modifier.AddFromZip(Index, TestFolderData[0]);
    Modifier.AddFile(SrcFolder + TestFolderData[12]);
    Modifier.AddFromZip(Index, TestFolderData[2]);
    CheckCount(Modifier.Count, 3);
    CheckBuildResult(Modifier.BuildZip(DstFolder + ZipName));
  finally
    Modifier.Free;
  end;

  Reader.LoadFromFile(DstFolder + ZipName);
  Reader.Check;
  Reader.ExtractAll(DstFolder);

  CheckFiles(SrcFolder + TestFolderData[1], SrcFolder + TestFolderData[1]);
  CheckFiles(DstFolder + TestFolderData[2], DstFolder + TestFolderData[2]);
  CheckFiles(DstFolder + ExtractFileName(TestFolderData[12]),
    DstFolder + ExtractFileName(TestFolderData[12]));

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestDeleteFromZip;
var
  SrcFolder, DstFolder: string;
  Modifier: TFWZipModifier;
  Index: TReaderIndex;
begin
  Clear;
  SrcFolder := GetTestFolderPath(7, True, True);
  CreateTestDataInSrcFolder(SrcFolder);
  Writer.AddFile(SrcFolder + TestFolderData[0]);
  Writer.AddFile(SrcFolder + TestFolderData[1]);
  Writer.AddFile(SrcFolder + TestFolderData[2]);
  CheckBuildResult(Writer.BuildZip(SrcFolder + ZipName));

  DstFolder := GetTestFolderPath(7, False, True);

  Modifier := TFWZipModifier.Create;
  try
    Index := Modifier.AddZipFile(SrcFolder + ZipName);
    Modifier.AddFromZip(Index);
    Modifier.DeleteItem(0);
    CheckCount(Modifier.Count, 2);
    CheckBuildResult(Modifier.BuildZip(DstFolder + ZipName));
  finally
    Modifier.Free;
  end;

  Reader.LoadFromFile(DstFolder + ZipName);
  Reader.Check;
  Reader.ExtractAll(DstFolder);

  CheckFiles(SrcFolder + TestFolderData[1], SrcFolder + TestFolderData[1]);
  CheckFiles(DstFolder + TestFolderData[2], DstFolder + TestFolderData[2]);

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

var
  NewFilePath: TStringList;

procedure OnDuplicate(Self, Sender: TObject;
  var Path: string; var Action: TDuplicateAction);
var
  OldPath: string;
begin
  OldPath := Path;
  Path := MakeUniqueName(Path);
  NewFilePath.AddPair(OldPath, Path);
  Action := daUseNewFilePath;
end;

const
  TestExDataBlob: Cardinal = $DEADBEEF;

procedure OnSaveExData(Self, Sender: TObject; ItemIndex: Integer;
  UserExDataBlockCount: Integer; var Tag: Word; Data: TStream);
var
  RandomValue: Cardinal;
begin
  case UserExDataBlockCount of
    0:
    begin
      Tag := $FFFA;
      Data.WriteBuffer(TestExDataBlob, 4);
    end;
    1..2:
    begin
      Tag := $FFFB + UserExDataBlockCount;
      Randomize;
      RandomValue := Random(MaxInt);
      Data.WriteBuffer(RandomValue, 4);
    end;
  end;
end;

procedure OnLoadExData(Self, Sender: TObject; ItemIndex: Integer;
  Tag: Word; Data: TStream);
var
  Value: Cardinal;
begin
  if Tag = $FFFA then
  begin
    if Data.Size <> 4 then
      raise Exception.Create('Неверный размер блока ExData');
    Data.ReadBuffer(Value, Data.Size);
    if Value <> TestExDataBlob then
      raise Exception.Create('Неверное значение блока ExData');
    TFWZipReaderItem(Sender).Tag := Integer(Value);
  end;
end;

procedure TFWZipUnitTest.TestExData;
var
  Count: TDataCount;
  SrcFolder, DstFolder, SrcFile, DstFile: string;
  I: Integer;
begin
  Clear;
  SrcFolder := GetTestFolderPath(12, True, True);
  Count := CreateTestDataInSrcFolder(SrcFolder);
  Writer.AddFolder('', SrcFolder, '*.*', True);

  Method.Code := @OnSaveExData;
  Method.Data := Writer;
  Writer.OnSaveExData := TZipSaveExDataEvent(Method);

  CheckBuildResult(Writer.BuildZip(SrcFolder + ZipName));

  DstFolder := GetTestFolderPath(12, False, True);

  Method.Code := @OnLoadExData;
  Method.Data := Reader;
  Reader.OnLoadExData := TZipLoadExDataEvent(Method);

  Reader.LoadFromFile(SrcFolder + ZipName);
  Reader.Check;

  Reader.ExtractAll(DstFolder);
  for I := 0 to TestFolderData.Count - 1 do
  begin
    SrcFile := SrcFolder + TestFolderData[I];
    DstFile := DstFolder + TestFolderData[I];
    if DirectoryExists(SrcFile) then Continue;
    CheckFiles(SrcFile, DstFile);
  end;

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

{$IFNDEF WINE}
procedure TFWZipUnitTest.TestExtractOverride;
var
  Count: TDataCount;
  SrcFolder, DstFolder, SrcFile, DstFile: string;
  I, A: Integer;
begin
  Clear;
  SrcFolder := GetTestFolderPath(10, True, True);
  Count := CreateTestDataInSrcFolder(SrcFolder);
  Writer.AddFolder(SrcFolder);
  CheckBuildResult(Writer.BuildZip(SrcFolder + ZipName));

  DstFolder := GetTestFolderPath(10, False, True);
  Reader.LoadFromFile(SrcFolder + ZipName);
  Reader.Check;

  Reader.ExtractAll(DstFolder);

  Method.Code := @OnDuplicate;
  Method.Data := Reader;
  Reader.OnDuplicate := TZipDuplicateEvent(Method);
  NewFilePath := TStringList.Create;
  try
    Reader.ExtractAll(DstFolder);
    for I := 0 to TestFolderData.Count - 1 do
    begin
      SrcFile := SrcFolder + TestFolderData[I];
      DstFile := DstFolder + 'test_10\' + TestFolderData[I];
      if DirectoryExists(SrcFile) then Continue;
      CheckFiles(SrcFile, DstFile);
      A := NewFilePath.IndexOfName('\\?\' + DstFolder + 'test_10\' + TestFolderData[I]);
      CheckFiles(SrcFile, NewFilePath.ValueFromIndex[A]);
    end;
  finally
    NewFilePath.Free;
  end;

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;
{$ENDIF}

procedure TFWZipUnitTest.TestMerge2Zip;
var
  SrcFolder, DstFolder: string;
  Modifier: TFWZipModifier;
  Index1, Index2: TReaderIndex;
begin
  Clear;
  SrcFolder := GetTestFolderPath(5, True, True);
  CreateTestDataInSrcFolder(SrcFolder);
  Writer.AddFile(SrcFolder + TestFolderData[1]);
  CheckBuildResult(Writer.BuildZip(SrcFolder + ZipName1));

  Clear;
  Writer.AddFile(SrcFolder + TestFolderData[2]);
  CheckBuildResult(Writer.BuildZip(SrcFolder + ZipName2));

  DstFolder := GetTestFolderPath(5, False, True);

  Modifier := TFWZipModifier.Create;
  try
    Index1 := Modifier.AddZipFile(SrcFolder + ZipName1);
    Index2 := Modifier.AddZipFile(SrcFolder + ZipName2);
    Modifier.AddFromZip(Index1);
    Modifier.AddFromZip(Index2);
    CheckCount(Modifier.Count, 2);
    CheckBuildResult(Modifier.BuildZip(DstFolder + ZipName));
  finally
    Modifier.Free;
  end;

  Reader.LoadFromFile(DstFolder + ZipName);
  Reader.Check;
  Reader.ExtractAll(DstFolder);

  CheckFiles(SrcFolder + TestFolderData[1], SrcFolder + TestFolderData[1]);
  CheckFiles(DstFolder + TestFolderData[2], DstFolder + TestFolderData[2]);

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultiStream1;
var
  DstFolder: string;
  F: TFWFileMultiStream;
begin
  DstFolder := GetTestFolderPath(13, False, True);

  F := TFWFileMultiStream.CreateWrite(DstFolder + ZipName, 100);
  try
    F.Size := 250;
  finally
    F.Free;
  end;
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z01'), 100);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z02'), 100);
  CheckSize(DstFolder + ZipName, 50);

  Clear;
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultiStream10;
var
  DstFolder: string;
  F: TFWFileMultiStream;
  I: Integer;
  Buff: array of Byte;
begin
  DstFolder := GetTestFolderPath(22, False, True);

  SetLength(Buff, 400);
  for I := 0 to 399 do
    Buff[I] := Byte(I);

  F := TFWFileMultiStream.CreateWrite(DstFolder + ZipName, 100);
  try
    F.WriteBuffer(Buff[0], 250);
    CheckCount(F.Size, 250);
    F.StartNewVolume;
    F.WriteBuffer(Buff[300], 4);
    F.Position := 240;
    F.WriteBuffer(Buff[240], 95);
    CheckCount(F.Size, 335);
  finally
    F.Free;
  end;

  Clear;
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultiStream11;
var
  DstFolder: string;
  F: TFWFileMultiStream;
  I: Integer;
  Buff, CheckBuff: array of Byte;
begin
  DstFolder := GetTestFolderPath(22, False, True);

  SetLength(Buff, 400);
  for I := 0 to 399 do
    Buff[I] := Byte(I);

  F := TFWFileMultiStream.CreateWrite(DstFolder + ZipName, 100);
  try
    F.WriteBuffer(Buff[0], 250);
    CheckCount(F.Size, 250);
    F.StartNewVolume;
    F.WriteBuffer(Buff[300], 4); // новый том
    F.Position := 240;
    F.WriteBuffer(Buff[240], 95); // запись с накладкой на новый том
    CheckCount(F.Size, 335);
    F.StartNewVolume;
    F.Position := 250;
    F.WriteBuffer(Buff[250], 60);
    CheckCount(F.Size, 335);
    F.Position := 332;
    F.WriteBuffer(Buff[332], 50);
    CheckCount(F.Size, 382);
    F.StartNewVolume;
    F.StartNewVolume;
    F.WriteBuffer(Buff[300], 4);
    F.StartNewVolume;
    F.StartNewVolume;
    F.Position := 300;
    F.WriteBuffer(Buff[300], 100);
    CheckCount(F.Size, 404);
  finally
    F.Free;
  end;

  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z01'), 100);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z02'), 100);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z03'), 100);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z04'), 100);
  CheckSize(DstFolder + ZipName, 4);

  F := TFWFileMultiStream.CreateRead(DstFolder + ZipName, rsmFull);
  try
    SetLength(CheckBuff, F.Size);
    F.Position := 0;
    F.ReadBuffer(CheckBuff[0], F.Size);
  finally
    F.Free;
  end;

  if not CompareMem(@Buff[0], @CheckBuff[0], 400) then
    raise EFWZipUnitTestException.Create('Прочитаны неверные данные');

  Clear;
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultiStream2;
var
  DstFolder: string;
  F: TFWFileMultiStream;
begin
  DstFolder := GetTestFolderPath(14, False, True);
  F := TFWFileMultiStream.CreateWrite(DstFolder + ZipName);
  try
    F.Size := 250;
    F.Size := 10;
    if F.Size <> 10 then
      raise EFWZipUnitTestException.Create('Размер архива должен быть равен 10');
  finally
    F.Free;
  end;

  CheckSize(DstFolder + ZipName, 10);
  if FileExists(DstFolder + ChangeFileExt(ZipName, '.z01')) then
    raise EFWZipUnitTestException.Create('Остался лишний том архива z01');
  if FileExists(DstFolder + ChangeFileExt(ZipName, '.z02')) then
    raise EFWZipUnitTestException.Create('Остался лишний том архива z02');

  Clear;
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultiStream3;
var
  DstFolder: string;
  F: TFWFileMultiStream;
begin
  DstFolder := GetTestFolderPath(15, False, True);
  F := TFWFileMultiStream.CreateWrite(DstFolder + ZipName, 100);
  try
    F.Size := 250;
    F.Position := 250;
    F.StartNewVolume;
    F.Size := 310;
    F.Size := 315;
    F.Position := 315;
    F.StartNewVolume;
    F.Size := 400;
  finally
    F.Free;
  end;
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z01'), 100);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z02'), 100);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z03'), 50);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z04'), 65);
  CheckSize(DstFolder + ZipName, 85);

  Clear;
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultiStream4;
var
  DstFolder: string;
  F: TFWFileMultiStream;
  Buff, CheckBuff: array of Byte;
  I: Integer;
begin
  DstFolder := GetTestFolderPath(16, False, True);

  SetLength(Buff, 400);
  for I := 0 to 399 do
    Buff[I] := Byte(I);

  F := TFWFileMultiStream.CreateWrite(DstFolder + ZipName, 100);
  try
    F.WriteBuffer(Buff[0], 250);
    F.StartNewVolume;
    F.WriteBuffer(Buff[250], 60);
    F.StartNewVolume;
    F.WriteBuffer(Buff[310], 90);
  finally
    F.Free;
  end;
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z01'), 100);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z02'), 100);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z03'), 50);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z04'), 60);
  CheckSize(DstFolder + ZipName, 90);

  F := TFWFileMultiStream.CreateRead(DstFolder + ZipName, rsmFull);
  try
    SetLength(CheckBuff, F.Size);
    F.Position := 0;
    F.ReadBuffer(CheckBuff[0], F.Size);
  finally
    F.Free;
  end;

  if not CompareMem(@Buff[0], @CheckBuff[0], 400) then
    raise EFWZipUnitTestException.Create('Прочитаны неверные данные');

  Clear;
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultiStream5;
var
  DstFolder: string;
  F: TFWFileMultiStream;
  Buff, CheckBuff: array of Byte;
  I: Integer;
begin
  DstFolder := GetTestFolderPath(17, False, True);

  SetLength(Buff, 400);
  for I := 0 to 399 do
    Buff[I] := Byte(I);

  F := TFWFileMultiStream.CreateWrite(DstFolder + ZipName, 100);
  try
    F.WriteBuffer(Buff[0], 250);
    F.WriteBuffer(Buff[250], 60);
    F.WriteBuffer(Buff[310], 90);
  finally
    F.Free;
  end;
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z01'), 100);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z02'), 100);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z03'), 100);
  CheckSize(DstFolder + ZipName, 100);

  F := TFWFileMultiStream.CreateRead(DstFolder + ZipName, rsmFull);
  try
    SetLength(CheckBuff, F.Size);
    F.Position := 0;
    F.ReadBuffer(CheckBuff[0], F.Size);
  finally
    F.Free;
  end;

  if not CompareMem(@Buff[0], @CheckBuff[0], 400) then
    raise EFWZipUnitTestException.Create('Прочитаны неверные данные');

  Clear;
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultiStream6;
var
  DstFolder: string;
  F: TFWFileMultiStream;
  Buff, CheckBuff: array of Byte;
  I: Integer;
begin
  DstFolder := GetTestFolderPath(18, False, True);

  SetLength(Buff, 400);
  for I := 0 to 399 do
    Buff[I] := Byte(I);

  F := TFWFileMultiStream.CreateWrite(DstFolder + ZipName, 100);
  try
    F.WriteBuffer(Buff[0], 250);
    F.Position := 240;
    F.WriteBuffer(Buff[240], 95);
    F.Position := 250;
    F.WriteBuffer(Buff[250], 60);
    F.Position := 300;
    F.WriteBuffer(Buff[300], 100);
  finally
    F.Free;
  end;
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z01'), 100);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z02'), 100);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z03'), 100);
  CheckSize(DstFolder + ZipName, 100);

  F := TFWFileMultiStream.CreateRead(DstFolder + ZipName, rsmFull);
  try
    SetLength(CheckBuff, F.Size);
    F.Position := 0;
    F.ReadBuffer(CheckBuff[0], F.Size);
  finally
    F.Free;
  end;

  if not CompareMem(@Buff[0], @CheckBuff[0], 400) then
    raise EFWZipUnitTestException.Create('Прочитаны неверные данные');

  Clear;
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultiStream7;
var
  DstFolder: string;
  F: TFWFileMultiStream;
  Buff, CheckBuff: array of Byte;
  I: Integer;
begin
  DstFolder := GetTestFolderPath(19, False, True);

  SetLength(Buff, 400);
  for I := 0 to 399 do
    Buff[I] := Byte(I);

  F := TFWFileMultiStream.CreateWrite(DstFolder + ZipName, 100);
  try
    F.WriteBuffer(Buff[0], 250);
    CheckSize(F.Size, 250);
    F.Position := 240;
    F.WriteBuffer(Buff[240], 95);
    CheckSize(F.Size, 335);
    F.Position := 250;
    F.WriteBuffer(Buff[250], 60);
    CheckSize(F.Size, 335);
    F.Position := 332;
    F.WriteBuffer(Buff[332], 60);
    CheckSize(F.Size, 392);
    F.Position := 300;
    F.WriteBuffer(Buff[300], 100);
    CheckSize(F.Size, 400);
  finally
    F.Free;
  end;

  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z01'), 100);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z02'), 100);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z03'), 100);
  CheckSize(DstFolder + ZipName, 100);

  F := TFWFileMultiStream.CreateRead(DstFolder + ZipName, rsmFull);
  try
    SetLength(CheckBuff, F.Size);
    F.Position := 0;
    F.ReadBuffer(CheckBuff[0], F.Size);
  finally
    F.Free;
  end;

  if not CompareMem(@Buff[0], @CheckBuff[0], 400) then
    raise EFWZipUnitTestException.Create('Прочитаны неверные данные');

  Clear;
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultiStream8;
var
  DstFolder: string;
  F: TFWFileMultiStream;
  I: Integer;
  Buff, CheckBuff: array of Byte;
begin
  DstFolder := GetTestFolderPath(20, False, True);

  SetLength(Buff, 400);
  for I := 0 to 399 do
    Buff[I] := Byte(I);

  F := TFWFileMultiStream.CreateWrite(DstFolder + ZipName, 100);
  try
    F.WriteBuffer(Buff[0], 250);
    CheckCount(F.Size, 250);
    F.StartNewVolume;
    F.Position := 240;
    F.WriteBuffer(Buff[240], 95);
    CheckCount(F.Size, 335);
    F.Position := 250;
    F.WriteBuffer(Buff[250], 60);
    CheckCount(F.Size, 335);
    F.Position := 332;
    F.WriteBuffer(Buff[332], 60);
    CheckCount(F.Size, 392);
    F.Position := 300;
    F.WriteBuffer(Buff[300], 100);
    CheckCount(F.Size, 400);
  finally
    F.Free;
  end;

  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z01'), 100);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z02'), 100);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z03'), 100);
  CheckSize(DstFolder + ZipName, 100);

  F := TFWFileMultiStream.CreateRead(DstFolder + ZipName, rsmFull);
  try
    SetLength(CheckBuff, F.Size);
    F.Position := 0;
    F.ReadBuffer(CheckBuff[0], F.Size);
  finally
    F.Free;
  end;

  if not CompareMem(@Buff[0], @CheckBuff[0], 400) then
    raise EFWZipUnitTestException.Create('Прочитаны неверные данные');

  Clear;
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultiStream9;
var
  DstFolder: string;
  F: TFWFileMultiStream;
  I: Integer;
  Buff, CheckBuff: array of Byte;
begin
  DstFolder := GetTestFolderPath(21, False, True);

  SetLength(Buff, 400);
  for I := 0 to 399 do
    Buff[I] := Byte(I);

  F := TFWFileMultiStream.CreateWrite(DstFolder + ZipName, 100);
  try
    F.WriteBuffer(Buff[0], 250);
    CheckCount(F.Size, 250);
    F.StartNewVolume;
    F.Position := 240;
    F.WriteBuffer(Buff[240], 95);
    CheckCount(F.Size, 335);
    F.StartNewVolume;
    F.Position := 250;
    F.WriteBuffer(Buff[250], 60);
    CheckCount(F.Size, 335);
    F.Position := 332;
    F.WriteBuffer(Buff[332], 60);
    CheckCount(F.Size, 392);
    F.StartNewVolume;
    F.Position := 300;
    F.WriteBuffer(Buff[300], 100);
    CheckCount(F.Size, 400);
  finally
    F.Free;
  end;

  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z01'), 100);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z02'), 100);
  CheckSize(DstFolder + ChangeFileExt(ZipName, '.z03'), 100);
  CheckSize(DstFolder + ZipName, 100);

  F := TFWFileMultiStream.CreateRead(DstFolder + ZipName, rsmFull);
  try
    SetLength(CheckBuff, F.Size);
    F.Position := 0;
    F.ReadBuffer(CheckBuff[0], F.Size);
  finally
    F.Free;
  end;

  if not CompareMem(@Buff[0], @CheckBuff[0], 400) then
    raise EFWZipUnitTestException.Create('Прочитаны неверные данные');

  Clear;
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultyPartBuildSingleZip;
var
  SrcFolder, DstFolder: string;
  M: TFWFileMultiStream;
begin
  // проверка, если размер тома указан больше чем финальный размер архива
  // должен создасться обычный немноготомный архив

  Clear;
  SrcFolder := GetTestFolderPath(215, True, True);
  DstFolder := GetTestFolderPath(215, False, True);
  CreateTestDataInSrcFolder(SrcFolder);
  M := TFWFileMultiStream.CreateWrite(DstFolder + ZipName, 1024 * 1024 * 8);
  try
    Writer.AddFolder('', SrcFolder, '');
    Writer.BuildZip(M);
  finally
    M.Free;
  end;

  if FileExists(DstFolder + ChangeFileExt(ZipName, '.z01')) then
    raise EFWZipUnitTestException.Create('Создан многотомный архив!!!');

  Reader.LoadFromFile(DstFolder + ZipName);
  Reader.Check;

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultyPartBuildWithException;
var
  Count: TDataCount;
  SrcFolder, DstFolder, DstFile: string;
  M: TFWFileMultiStream;
begin
  Clear;
  SrcFolder := GetTestFolderPath(209, True, True);
  DstFolder := GetTestFolderPath(209, False, True);
  DstFile := DstFolder + ZipName;
  Count := CreateTestDataInSrcFolder(SrcFolder);

  // лочим один из файлов для демонстрации
  Writer.AddFile(SrcFolder + TestFolderData[0]);
  ZeroMemory(@hOFStruct, SizeOf(TOFStruct));
  hFile := OpenFile(PAnsiChar(AnsiString(SrcFolder + TestFolderData[0])),
    hOFStruct, OF_WRITE);
  try
    M := TFWFileMultiStream.CreateWrite(DstFile);
    try
      CheckBuildResult(Writer.BuildZip(M), brFailed);
    finally
      M.Free;
    end;

    // теперь добавляем еще один не залоченый
    ClearFolder(DstFolder);
    Writer.AddFile(SrcFolder + TestFolderData[1]);

    M := TFWFileMultiStream.CreateWrite(DstFile);
    try
      CheckBuildResult(Writer.BuildZip(M), brPartialBuild);
    finally
      M.Free;
    end;
  finally
    CloseHandle(hFile);
  end;

  // а теперь должно быть нормально

  ClearFolder(DstFolder);
  CheckCount(Writer.Count, 2);

  M := TFWFileMultiStream.CreateWrite(DstFile);
  try
    CheckBuildResult(Writer.BuildZip(M));
  finally
    M.Free;
  end;

  M := TFWFileMultiStream.CreateRead(DstFile);
  try
    Reader.LoadFromStream(M);
    Reader.Check;
  finally
    M.Free;
  end;

  ClearFolder(DstFolder);
  Clear;
  Writer.AddFile(SrcFolder + TestFolderData[0]);
  hFile := OpenFile(PAnsiChar(AnsiString(SrcFolder + TestFolderData[0])),
    hOFStruct, OF_WRITE);
  try

    // Назначаем обработчик через который мы будем обрабатывать ошибку
    // В обработчике OnException1 будет создаваться копия файла

    Method.Code := @OnException1;
    Method.Data := Writer;
    Writer.OnException := TZipBuildExceptionEvent(Method);
    M := TFWFileMultiStream.CreateWrite(DstFile);
    try
      CheckBuildResult(Writer.BuildZip(M));
    finally
      M.Free;
    end;
  finally
    CloseHandle(hFile);
  end;

  ClearFolder(DstFolder);
  Clear;
  Writer.AddFile(SrcFolder + TestFolderData[0]);
  hFile := OpenFile(PAnsiChar(AnsiString(SrcFolder + TestFolderData[0])),
    hOFStruct, OF_WRITE);
  try

    // снимаем исскуственную блокировку файла и выставляем
    // свойство Action в eaRetry

    Method.Code := @OnException2;
    Method.Data := Writer;
    Writer.OnException := TZipBuildExceptionEvent(Method);
    M := TFWFileMultiStream.CreateWrite(DstFile);
    try
      CheckBuildResult(Writer.BuildZip(M));
    finally
      M.Free;
    end;
  finally
    CloseHandle(hFile);
  end;

  ClearFolder(DstFolder);
  Clear;
  Writer.AddFile(SrcFolder + TestFolderData[0]);
  hFile := OpenFile(PAnsiChar(AnsiString(SrcFolder + TestFolderData[0])),
    hOFStruct, OF_WRITE);
  try

    // загружаем данные через стрим
    Method.Code := @OnException3;
    Method.Data := Writer;
    Writer.OnException := TZipBuildExceptionEvent(Method);
    M := TFWFileMultiStream.CreateWrite(DstFile);
    try
      CheckBuildResult(Writer.BuildZip(M));
    finally
      M.Free;
    end;
  finally
    CloseHandle(hFile);
  end;

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultyPartBuildWithExistingFile;
var
  Count: TDataCount;
  SrcFolder, DstFolder, SrcFile, DstFile: string;
  I: Integer;
  M: TFWFileMultiStream;
begin
  Clear;
  SrcFolder := GetTestFolderPath(202, True, True);
  Count := CreateTestDataInSrcFolder(SrcFolder);
  if Writer.AddFolder('', SrcFolder, '*.*', True) <> Count.Files then
    raise EFWZipUnitTestException.Create('Ошибка инициализации архива. Неверное количество добавленных файлов');

  M := TFWFileMultiStream.CreateWrite(SrcFolder + ZipName, 100);
  try
    CheckBuildResult(Writer.BuildZip(M));
  finally
    M.Free;
  end;

  M := TFWFileMultiStream.CreateRead(SrcFolder + ZipName);
  try
    DstFolder := GetTestFolderPath(202, False, True);
    CheckLoadReaderWidthExcept(SrcFolder + ZipName);
    Reader.LoadFromStream(M);
    Reader.Check;
    Reader.ExtractAll(DstFolder);
  finally
    M.Free;
  end;

  for I := 0 to TestFolderData.Count - 1 do
  begin
    SrcFile := SrcFolder + TestFolderData[I];
    DstFile := DstFolder + TestFolderData[I];
    if DirectoryExists(SrcFile) then Continue;
    CheckFiles(SrcFile, DstFile);
  end;

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultyPartBuildWithExistingFile2;
var
  Count: TDataCount;
  SrcFolder, DstFolder: string;
  SrcFile, DstFile, Ext: string;
  I: Integer;
  M: TFWFileMultiStream;
begin
  Clear;
  SrcFolder := GetTestFolderPath(203, True, True);
  Count := CreateTestDataInSrcFolder(SrcFolder);
  if Writer.AddFolder('', SrcFolder, '*.bin;*.no_txt', True) <> 3 then
    raise EFWZipUnitTestException.Create('Ошибка инициализации архива. Неверное количество добавленных файлов');

  M := TFWFileMultiStream.CreateWrite(SrcFolder + ZipName);
  try
    CheckBuildResult(Writer.BuildZip(M));
  finally
    M.Free;
  end;

  DstFolder := GetTestFolderPath(203, False, True);

  M := TFWFileMultiStream.CreateRead(SrcFolder + ZipName);
  try
    Reader.LoadFromStream(M);
    Reader.Check;
    Reader.ExtractAll(DstFolder);
  finally
    M.Free;
  end;

  for I := 0 to TestFolderData.Count - 1 do
  begin
    SrcFile := SrcFolder + TestFolderData[I];
    DstFile := DstFolder + TestFolderData[I];
    Ext := ExtractFileExt(DstFile);
    if (Ext <> '.no_txt') and ((Ext <> '.bin')) then Continue;
    CheckFiles(SrcFile, DstFile);
  end;

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultyPartBuildWithPassword;
var
  Count: TDataCount;
  SrcFolder, DstFolder, SrcFile, DstFile: string;
  I: Integer;
  Item: TFWZipWriterItem;
  M: TFWFileMultiStream;
begin
  Clear;
  SrcFolder := GetTestFolderPath(204, True, True);
  DstFolder := GetTestFolderPath(204, False, True);

  Count := CreateTestDataInSrcFolder(SrcFolder);
  Writer.AlwaysAddEmptyFolder := True;
  if Writer.AddFolder('', SrcFolder, '', True) <> Count.Files then
    raise EFWZipUnitTestException.Create('Ошибка инициализации архива. Неверное количество добавленных файлов/папок');

  // Сначала ставим пароли всем элементам включая папки (что делать нельзя)
  for I := 0 to Writer.Count - 1 do
  begin
    Item := Writer[I];
    // Изменим коментарий
    Item.Comment := 'Тестовый коментарий к файлу ' + Item.FileName;
    // Установим пароль
    Item.Password := 'password' + IntToStr(Byte(I mod 3));
    // Изменим тип сжатия
    Item.CompressionLevel := TCompressionLevel(Byte(I mod 3));
  end;

  M := TFWFileMultiStream.CreateWrite(DstFolder + ZipName);
  try
    CheckBuildResult(Writer.BuildZip(M));
  finally
    M.Free;
  end;

  // архив должен открыться корретно
  M := TFWFileMultiStream.CreateRead(DstFolder + ZipName);
  try
    Reader.LoadFromStream(M);
    CheckCount(Reader.Count, 22);
  finally
    M.Free;
  end;

  Reader.Clear;
  ClearFolder(DstFolder);

  // теперь скинем пароли с папок
  for I := 0 to Writer.Count - 1 do
  begin
    Item := Writer[I];
    if Item.IsFolder then
    begin
      Item.Password := '';
      Item.CompressionLevel := clNone;
    end;
  end;

  // теперь сбилдится должно успешно
  M := TFWFileMultiStream.CreateWrite(DstFolder + ZipName);
  try
    CheckBuildResult(Writer.BuildZip(M));
  finally
    M.Free;
  end;

  M := TFWFileMultiStream.CreateRead(DstFolder + ZipName);
  try
    Reader.LoadFromStream(M);
    CheckCount(Reader.Count, 22);

    CheckReaderWidthExcept; // без пароля обязано зарайзиться
    Reader.PasswordList.Add('password0');
    Reader.PasswordList.Add('password1');
    CheckReaderWidthExcept; // без отстутсвующего пароля обязано зарайзиться

    Method.Code := @OnPassword;
    Method.Data := Reader;
    Reader.OnPassword := TZipNeedPasswordEvent(Method);

    Reader.Check; // повторная проверка, теперь все пароли должны быть на месте


    Reader.ExtractAll(DstFolder);
  finally
    M.Free;
  end;

  for I := 0 to TestFolderData.Count - 1 do
  begin
    SrcFile := SrcFolder + TestFolderData[I];
    DstFile := DstFolder + TestFolderData[I];
    if DirectoryExists(SrcFile) then Continue;
    CheckFiles(SrcFile, DstFile);
  end;

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultyPartBuildWithPassword1;
var
  Count: TDataCount;
  SrcFolder, DstFolder, SrcFile, DstFile: string;
  I: Integer;
  Item: TFWZipWriterItem;
  M: TFWFileMultiStream;
begin
  Clear;
  SrcFolder := GetTestFolderPath(204, True, True);
  DstFolder := GetTestFolderPath(204, False, True);

  Count := CreateTestDataInSrcFolder(SrcFolder);
  Writer.AlwaysAddEmptyFolder := True;
  if Writer.AddFolder('', SrcFolder, '', True) <> Count.Files then
    raise EFWZipUnitTestException.Create('Ошибка инициализации архива. Неверное количество добавленных файлов/папок');

  // Сначала ставим пароли всем элементам включая папки (что делать нельзя)
  for I := 0 to Writer.Count - 1 do
  begin
    Item := Writer[I];
    // Изменим коментарий
    Item.Comment := 'Тестовый коментарий к файлу ' + Item.FileName;
    // Установим пароль
    Item.Password := 'password' + IntToStr(Byte(I mod 3));
    // Вместе с дескриптором
    Item.NeedDescriptor := True;
    // Изменим тип сжатия
    Item.CompressionLevel := TCompressionLevel(Byte(I mod 3));
  end;

  M := TFWFileMultiStream.CreateWrite(DstFolder + ZipName);
  try
    CheckBuildResult(Writer.BuildZip(M));
  finally
    M.Free;
  end;

  // архив должен открыться корретно
  M := TFWFileMultiStream.CreateRead(DstFolder + ZipName);
  try
    Reader.LoadFromStream(M);
    CheckCount(Reader.Count, 22);
  finally
    M.Free;
  end;

  Reader.Clear;
  ClearFolder(DstFolder);

  // теперь скинем пароли с папок
  for I := 0 to Writer.Count - 1 do
  begin
    Item := Writer[I];
    if Item.IsFolder then
    begin
      Item.Password := '';
      Item.NeedDescriptor := False;
      Item.CompressionLevel := clNone;
    end;
  end;

  // теперь сбилдится должно успешно
  M := TFWFileMultiStream.CreateWrite(DstFolder + ZipName);
  try
    CheckBuildResult(Writer.BuildZip(M));
  finally
    M.Free;
  end;

  M := TFWFileMultiStream.CreateRead(DstFolder + ZipName);
  try
    Reader.LoadFromStream(M);
    CheckCount(Reader.Count, 22);

    CheckReaderWidthExcept; // без пароля обязано зарайзиться
    Reader.PasswordList.Add('password0');
    Reader.PasswordList.Add('password1');
    CheckReaderWidthExcept; // без отстутсвующего пароля обязано зарайзиться

    Method.Code := @OnPassword;
    Method.Data := Reader;
    Reader.OnPassword := TZipNeedPasswordEvent(Method);

    Reader.Check; // повторная проверка, теперь все пароли должны быть на месте


    Reader.ExtractAll(DstFolder);
  finally
    M.Free;
  end;

  for I := 0 to TestFolderData.Count - 1 do
  begin
    SrcFile := SrcFolder + TestFolderData[I];
    DstFile := DstFolder + TestFolderData[I];
    if DirectoryExists(SrcFile) then Continue;
    CheckFiles(SrcFile, DstFile);
  end;

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultyPartBuildWithStream;
var
  S: TStringStream;
  ItemIndex: Integer;
  SrcFolder, DstFolder: string;
  M: TFWFileMultiStream;
begin
  Clear;
  ItemIndex := AddItem(Writer, 'test.txt', TestStringBlock[0]);
  CheckResult(ItemIndex);
  Writer.Item[ItemIndex].Comment := TestStringBlock[2];

  S := TStringStream.Create(TestStringBlock[1]);
  S.Position := 0;
  ItemIndex := Writer.AddStream('test2.txt', S, soOwned);
  CheckResult(ItemIndex);

  S := TStringStream.Create(TestStringBlock[0]);
  S.Position := 0;
  ItemIndex := Writer.InsertStream('test0.txt', 0, S, soOwned);
  CheckResult(ItemIndex);

  CheckResult(AddItem(Writer,
    'AddStreamData\SubFolder1\Subfolder2\Test.txt', TestStringBlock[2]));

  SrcFolder := GetTestFolderPath(201, True, True);

  M := TFWFileMultiStream.CreateWrite(SrcFolder + ZipName, 100);
  try
    CheckBuildResult(Writer.BuildZip(M));
  finally
    M.Free;
  end;

  M := TFWFileMultiStream.CreateRead(SrcFolder + ZipName);
  try
    CheckLoadReaderWidthExcept(SrcFolder + ZipName);
    Reader.LoadFromStream(M);
    Reader.Check;

    DstFolder := GetTestFolderPath(201, False, True);
    Reader.ExtractAll(DstFolder);
  finally
    M.Free;
  end;

  CheckStream(0, DstFolder + 'test.txt');
  CheckStream(0, DstFolder + 'test0.txt');
  CheckStream(1, DstFolder + 'test2.txt');
  CheckStream(2, DstFolder + 'AddStreamData\SubFolder1\Subfolder2\Test.txt');

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultyPartChangeZip;
var
  SrcFolder, DstFolder: string;
  Modifier: TFWZipModifier;
  Index: TReaderIndex;
  M, M1: TFWFileMultiStream;
begin
  Clear;
  SrcFolder := GetTestFolderPath(206, True, True);
  CreateTestDataInSrcFolder(SrcFolder);
  Writer.AddFile(SrcFolder + TestFolderData[0]);
  Writer.AddFile(SrcFolder + TestFolderData[1]);
  Writer.AddFile(SrcFolder + TestFolderData[2]);

  M := TFWFileMultiStream.CreateWrite(SrcFolder + ZipName);
  try
    CheckBuildResult(Writer.BuildZip(M));
  finally
    M.Free;
  end;

  DstFolder := GetTestFolderPath(206, False, True);

  Modifier := TFWZipModifier.Create;
  try
    M1 := TFWFileMultiStream.CreateRead(SrcFolder + ZipName);
    try
      Index := Modifier.AddZipFile(M1);
      Modifier.AddFromZip(Index, TestFolderData[0]);
      Modifier.AddFile(SrcFolder + TestFolderData[12]);
      Modifier.AddFromZip(Index, TestFolderData[2]);
      CheckCount(Modifier.Count, 3);
      M := TFWFileMultiStream.CreateWrite(DstFolder + ZipName);
      try
        CheckBuildResult(Modifier.BuildZip(M));
      finally
        M.Free;
      end;
    finally
      M1.Free;
    end;
  finally
    Modifier.Free;
  end;

  M := TFWFileMultiStream.CreateRead(DstFolder + ZipName);
  try
    Reader.LoadFromStream(M);
    Reader.Check;
    Reader.ExtractAll(DstFolder);
  finally
    M.Free;
  end;

  CheckFiles(SrcFolder + TestFolderData[1], SrcFolder + TestFolderData[1]);
  CheckFiles(DstFolder + TestFolderData[2], DstFolder + TestFolderData[2]);
  CheckFiles(DstFolder + ExtractFileName(TestFolderData[12]),
    DstFolder + ExtractFileName(TestFolderData[12]));

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultyPartDeleteFromZip;
var
  SrcFolder, DstFolder: string;
  Modifier: TFWZipModifier;
  Index: TReaderIndex;
  M, M1: TFWFileMultiStream;
begin
  Clear;
  SrcFolder := GetTestFolderPath(207, True, True);
  CreateTestDataInSrcFolder(SrcFolder);
  Writer.AddFile(SrcFolder + TestFolderData[0]);
  Writer.AddFile(SrcFolder + TestFolderData[1]);
  Writer.AddFile(SrcFolder + TestFolderData[2]);

  M := TFWFileMultiStream.CreateWrite(SrcFolder + ZipName);
  try
    CheckBuildResult(Writer.BuildZip(M));
  finally
    M.Free;
  end;

  DstFolder := GetTestFolderPath(207, False, True);

  Modifier := TFWZipModifier.Create;
  try
    M1 := TFWFileMultiStream.CreateRead(SrcFolder + ZipName);
    try
      Index := Modifier.AddZipFile(M1);
      Modifier.AddFromZip(Index);
      Modifier.DeleteItem(0);
      CheckCount(Modifier.Count, 2);

      M := TFWFileMultiStream.CreateWrite(DstFolder + ZipName);
      try
        CheckBuildResult(Modifier.BuildZip(M));
      finally
        M.Free;
      end;
    finally
      M1.Free;
    end;
  finally
    Modifier.Free;
  end;

  M := TFWFileMultiStream.CreateRead(DstFolder + ZipName);
  try
    Reader.LoadFromStream(M);
    Reader.Check;
    Reader.ExtractAll(DstFolder);
  finally
    M.Free;
  end;

  CheckFiles(SrcFolder + TestFolderData[1], SrcFolder + TestFolderData[1]);
  CheckFiles(DstFolder + TestFolderData[2], DstFolder + TestFolderData[2]);

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultyPartExData;
var
  Count: TDataCount;
  SrcFolder, DstFolder, SrcFile, DstFile: string;
  I: Integer;
  M: TFWFileMultiStream;
begin
  Clear;
  SrcFolder := GetTestFolderPath(212, True, True);
  Count := CreateTestDataInSrcFolder(SrcFolder);
  Writer.AddFolder('', SrcFolder, '*.*', True);

  Method.Code := @OnSaveExData;
  Method.Data := Writer;
  Writer.OnSaveExData := TZipSaveExDataEvent(Method);

  M := TFWFileMultiStream.CreateWrite(SrcFolder + ZipName);
  try
    CheckBuildResult(Writer.BuildZip(M));
  finally
    M.Free;
  end;

  DstFolder := GetTestFolderPath(212, False, True);

  Method.Code := @OnLoadExData;
  Method.Data := Reader;
  Reader.OnLoadExData := TZipLoadExDataEvent(Method);

  M := TFWFileMultiStream.CreateRead(SrcFolder + ZipName);
  try
    Reader.LoadFromStream(M);
    Reader.Check;

    Reader.ExtractAll(DstFolder);
    for I := 0 to TestFolderData.Count - 1 do
    begin
      SrcFile := SrcFolder + TestFolderData[I];
      DstFile := DstFolder + TestFolderData[I];
      if DirectoryExists(SrcFile) then Continue;
      CheckFiles(SrcFile, DstFile);
    end;
  finally
    M.Free;
  end;

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

{$IFNDEF WINE}
procedure TFWZipUnitTest.TestMultyPartExtractOverride;
var
  Count: TDataCount;
  SrcFolder, DstFolder, SrcFile, DstFile: string;
  I, A: Integer;
  M: TFWFileMultiStream;
begin
  Clear;
  SrcFolder := GetTestFolderPath(210, True, True);
  Count := CreateTestDataInSrcFolder(SrcFolder);
  Writer.AddFolder(SrcFolder);

  M := TFWFileMultiStream.CreateWrite(SrcFolder + ZipName);
  try
    CheckBuildResult(Writer.BuildZip(M));
  finally
    M.Free;
  end;

  DstFolder := GetTestFolderPath(210, False, True);

  M := TFWFileMultiStream.CreateRead(SrcFolder + ZipName);
  try
    Reader.LoadFromStream(M);
    Reader.Check;
    Reader.ExtractAll(DstFolder);

    Method.Code := @OnDuplicate;
    Method.Data := Reader;
    Reader.OnDuplicate := TZipDuplicateEvent(Method);
    NewFilePath := TStringList.Create;
    try
      Reader.ExtractAll(DstFolder);
      for I := 0 to TestFolderData.Count - 1 do
      begin
        SrcFile := SrcFolder + TestFolderData[I];
        DstFile := DstFolder + 'test_210\' + TestFolderData[I];
        if DirectoryExists(SrcFile) then Continue;
        CheckFiles(SrcFile, DstFile);
        A := NewFilePath.IndexOfName('\\?\' + DstFolder + 'test_210\' + TestFolderData[I]);
        CheckFiles(SrcFile, NewFilePath.ValueFromIndex[A]);
      end;
    finally
      NewFilePath.Free;
    end;

  finally
    M.Free;
  end;

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;
{$ENDIF}

procedure TFWZipUnitTest.TestMultyPartMerge2Zip;
var
  SrcFolder, DstFolder: string;
  Modifier: TFWZipModifier;
  Index1, Index2: TReaderIndex;
  M, M1, M2: TFWFileMultiStream;
begin
  Clear;
  SrcFolder := GetTestFolderPath(205, True, True);
  CreateTestDataInSrcFolder(SrcFolder);
  Writer.AddFile(SrcFolder + TestFolderData[1]);

  M := TFWFileMultiStream.CreateWrite(SrcFolder + ZipName1);
  try
    CheckBuildResult(Writer.BuildZip(M));
  finally
    M.Free;
  end;

  Clear;
  Writer.AddFile(SrcFolder + TestFolderData[2]);

  M := TFWFileMultiStream.CreateWrite(SrcFolder + ZipName2);
  try
    CheckBuildResult(Writer.BuildZip(M));
  finally
    M.Free;
  end;

  DstFolder := GetTestFolderPath(205, False, True);

  Modifier := TFWZipModifier.Create;
  try
    M1 := TFWFileMultiStream.CreateRead(SrcFolder + ZipName1);
    try
      Index1 := Modifier.AddZipFile(M1);
      M2 := TFWFileMultiStream.CreateRead(SrcFolder + ZipName2);
      try
        Index2 := Modifier.AddZipFile(M2);
        Modifier.AddFromZip(Index1);
        Modifier.AddFromZip(Index2);
        CheckCount(Modifier.Count, 2);
        M := TFWFileMultiStream.CreateWrite(DstFolder + ZipName);
        try
          CheckBuildResult(Modifier.BuildZip(M));
        finally
          M.Free;
        end;
      finally
        M2.Free;
      end;
    finally
      M1.Free;
    end;
  finally
    Modifier.Free;
  end;

  M := TFWFileMultiStream.CreateRead(DstFolder + ZipName);
  try
    Reader.LoadFromStream(M);
    Reader.Check;
    Reader.ExtractAll(DstFolder);
  finally
    M.Free;
  end;

  CheckFiles(SrcFolder + TestFolderData[1], SrcFolder + TestFolderData[1]);
  CheckFiles(DstFolder + TestFolderData[2], DstFolder + TestFolderData[2]);

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultyPartSplitZip;
var
  Modifier: TFWZipModifier;
  Index: TReaderIndex;
  SrcFolder, DstFolder: string;
  M, M1: TFWFileMultiStream;
begin
  Clear;
  AddItem(Writer, 'test1.txt', 'первый элемент');
  AddItem(Writer, 'test2.txt', 'второй элемент');
  AddItem(Writer, 'test3.txt', 'третий элемент');
  AddItem(Writer, 'test4.txt', 'четвертый элемент');

  SrcFolder := GetTestFolderPath(208, True, True);
  DstFolder := GetTestFolderPath(208, False, True);

  M := TFWFileMultiStream.CreateWrite(SrcFolder + ZipName);
  try
    CheckBuildResult(Writer.BuildZip(M));
  finally
    M.Free;
  end;

  M1 := TFWFileMultiStream.CreateRead(SrcFolder + ZipName);
  try
    Modifier := TFWZipModifier.Create;
    try
      // подключаем ранее созданный архив
      Index := Modifier.AddZipFile(M);
      // добавляем из него первые два элемента
      Modifier.AddFromZip(Index, 'test1.txt');
      Modifier.AddFromZip(Index, 'test2.txt');
      // и сохраняем в новый архив
      // при этом реальной перепаковки данных не произойдет,
      // данные возьмутся как есть в виде массива байт прямо в сжатом виде
      // из оригинального архива
      CheckCount(Modifier.Count, 2);

      M := TFWFileMultiStream.CreateWrite(DstFolder + ZipName1);
      try
        CheckBuildResult(Modifier.BuildZip(M));
      finally
        M.Free;
      end;

      // теперь удаляем добавленные элементы и добавляем вторые два
      Modifier.Clear;
      Modifier.AddFromZip(Index, 'test3.txt');
      Modifier.AddFromZip(Index, 'test4.txt');
      // сохраняем во торой архив
      CheckCount(Modifier.Count, 2);

      M := TFWFileMultiStream.CreateWrite(DstFolder + ZipName2);
      try
        CheckBuildResult(Modifier.BuildZip(M));
      finally
        M.Free;
      end;
    finally
      Modifier.Free;
    end;
  finally
    M1.Free;
  end;

  M := TFWFileMultiStream.CreateRead(DstFolder + ZipName1);
  try
    Reader.LoadFromStream(M);
    Reader.Check;
    CheckCount(Reader.Count, 2);

    if Reader.Item[0].FileName <> 'test1.txt' then
      raise EFWZipUnitTestException.Create('Имя файла должно быть test1.txt');
    if Reader.Item[1].FileName <> 'test2.txt' then
      raise EFWZipUnitTestException.Create('Имя файла должно быть test2.txt');
  finally
    M.Free;
  end;

  M := TFWFileMultiStream.CreateRead(DstFolder + ZipName2);
  try
    Reader.LoadFromStream(M);
    Reader.Check;
    CheckCount(Reader.Count, 2);
    if Reader.Item[0].FileName <> 'test3.txt' then
      raise EFWZipUnitTestException.Create('Имя файла должно быть test3.txt');
    if Reader.Item[1].FileName <> 'test4.txt' then
      raise EFWZipUnitTestException.Create('Имя файла должно быть test4.txt');
  finally
    M.Free;
  end;

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultyPartZip64_BigFiles;
var
  SrcFolder, DstFolder: string;
  M: TFWFileMultiStream;
begin
  Clear;
  SrcFolder := GetTestFolderPath(210, True, True);
  DstFolder := GetTestFolderPath(210, False, True);
  CreateBigFilesInFolder(SrcFolder, True);

  Writer.AddFolder(SrcFolder, False);
  M := TFWFileMultiStream.CreateWrite(DstFolder + ZipName, 1024 * 1024 * 8);
  try
    CheckBuildResult(Writer.BuildZip(M));
  finally
    M.Free;
  end;

  M := TFWFileMultiStream.CreateRead(DstFolder + ZipName);
  try
    Reader.LoadFromStream(M);
    Reader.Check;
  finally
    M.Free;
  end;

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestMultyPartZip64_BigFilesCount;
var
  SrcFolder, DstFolder: string;
  M: TFWFileMultiStream;
begin
  Clear;
  SrcFolder := GetTestFolderPath(221, True, True);
  DstFolder := GetTestFolderPath(221, False, True);
  CreateManyFilesInFolder(SrcFolder);

  Writer.AddFolder(SrcFolder, False);
  M := TFWFileMultiStream.CreateWrite(DstFolder + ZipName, 1024 * 1024 * 8);
  try
    CheckBuildResult(Writer.BuildZip(M));
  finally
    M.Free;
  end;

  M := TFWFileMultiStream.CreateRead(DstFolder + ZipName);
  try
    Reader.LoadFromStream(M);
    Reader.Check;
  finally
    M.Free;
  end;

  Clear;
  DeleteManyFilesInFolder(SrcFolder);
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestSplitZip;
var
  Modifier: TFWZipModifier;
  S: TMemoryStream;
  Index: TReaderIndex;
  DstFolder: string;
begin
  Clear;
  AddItem(Writer, 'test1.txt', 'первый элемент');
  AddItem(Writer, 'test2.txt', 'второй элемент');
  AddItem(Writer, 'test3.txt', 'третий элемент');
  AddItem(Writer, 'test4.txt', 'четвертый элемент');

  DstFolder := GetTestFolderPath(8, False, True);
  S := TMemoryStream.Create;
  try
    Writer.BuildZip(S);

    Modifier := TFWZipModifier.Create;
    try
      // подключаем ранее созданный архив
      Index := Modifier.AddZipFile(S);
      // добавляем из него первые два элемента
      Modifier.AddFromZip(Index, 'test1.txt');
      Modifier.AddFromZip(Index, 'test2.txt');
      // и сохраняем в новый архив
      // при этом реальной перепаковки данных не произойдет,
      // данные возьмутся как есть в виде массива байт прямо в сжатом виде
      // из оригинального архива
      CheckCount(Modifier.Count, 2);
      CheckBuildResult(Modifier.BuildZip(DstFolder + ZipName1));

      // теперь удаляем добавленные элементы и добавляем вторые два
      Modifier.Clear;
      Modifier.AddFromZip(Index, 'test3.txt');
      Modifier.AddFromZip(Index, 'test4.txt');
      // сохраняем во торой архив
      CheckCount(Modifier.Count, 2);
      CheckBuildResult(Modifier.BuildZip(DstFolder + ZipName2));
    finally
      Modifier.Free;
    end;
  finally
    S.Free;
  end;

  Reader.LoadFromFile(DstFolder + ZipName1);
  Reader.Check;
  CheckCount(Reader.Count, 2);

  if Reader.Item[0].FileName <> 'test1.txt' then
    raise EFWZipUnitTestException.Create('Имя файла должно быть test1.txt');
  if Reader.Item[1].FileName <> 'test2.txt' then
    raise EFWZipUnitTestException.Create('Имя файла должно быть test2.txt');

  Reader.LoadFromFile(DstFolder + ZipName2);
  Reader.Check;
  CheckCount(Reader.Count, 2);
  if Reader.Item[0].FileName <> 'test3.txt' then
    raise EFWZipUnitTestException.Create('Имя файла должно быть test3.txt');
  if Reader.Item[1].FileName <> 'test4.txt' then
    raise EFWZipUnitTestException.Create('Имя файла должно быть test4.txt');

  Clear;
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestZip64_BigFiles;
var
  SrcFolder, DstFolder: string;
begin
  Clear;
  SrcFolder := GetTestFolderPath(100, True, True);
  DstFolder := GetTestFolderPath(100, False, True);
  CreateBigFilesInFolder(SrcFolder);

  Writer.AddFolder(SrcFolder, False);
  CheckBuildResult(Writer.BuildZip(DstFolder + ZipName));

  Reader.LoadFromFile(DstFolder + ZipName);
  Reader.Check;

  Clear;
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

procedure TFWZipUnitTest.TestZip64_BigFilesCount;
var
  SrcFolder, DstFolder: string;
begin
  Clear;
  SrcFolder := GetTestFolderPath(120, True, True);
  DstFolder := GetTestFolderPath(120, False, True);
  CreateManyFilesInFolder(SrcFolder);

  Writer.AddFolder(SrcFolder, False);
  CheckBuildResult(Writer.BuildZip(DstFolder + ZipName));

  Reader.LoadFromFile(DstFolder + ZipName);
  Reader.Check;

  Clear;
  DeleteManyFilesInFolder(SrcFolder);
  DeleteFolder(SrcFolder);
  DeleteFolder(DstFolder);
end;

function TFWZipUnitTest.Writer: TFWZipWriter;
begin
  Result := FZipWriter;
end;

initialization

  InitFolders;
  RegisterTest(TFWZipUnitTest.Suite);

finalization

  TestFolderData.Free;

end.

