////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip - ZipAnalizer2
//  * Unit Name : uZipAnalizer2
//  * Purpose   : Вывод параметров архива при помощи поиска сигнатур
//  * Author    : Александр (Rouse_) Багель
//  * Copyright : © Fangorn Wizards Lab 1998 - 2013.
//  * Version   : 1.0.10
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

unit uZipAnalizer2;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus,
  FWZipConsts;

type
  TdlgZipAnalizer = class(TForm)
    edPath: TLabeledEdit;
    btnBrowse: TButton;
    btnAnalize: TButton;
    GroupBox: TGroupBox;
    edReport: TRichEdit;
    OpenDialog: TOpenDialog;
    PopupMenu: TPopupMenu;
    mnuSave: TMenuItem;
    SaveDialog: TSaveDialog;
    procedure btnBrowseClick(Sender: TObject);
    procedure edPathChange(Sender: TObject);
    procedure btnAnalizeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
  private
    procedure Log(const Value: string);
    procedure Scan(const Value: string);
    function FindSing(Stream: TStream): DWORD;
    procedure ShowLocalFileHeader(Stream: TStream);
    procedure ShowDataDescryptor(Stream: TStream);
    procedure ShowCentralFileHeader(Stream: TStream);
    procedure ShowExtraFields(Stream: TStream; Size: Integer;
      FileHeader: TCentralDirectoryFileHeader);
    procedure ShowZip64(Stream: TStream);
    procedure ShowZip64Locator(Stream: TStream);
    procedure ShowEndOfCentralDir(Stream: TStream);
    procedure LoadStringValue(Stream: TStream; var Value: string;
      nSize: Cardinal; UTF: Boolean);
  end;

var
  dlgZipAnalizer: TdlgZipAnalizer;

implementation

const
  Delim = '===================================================================';

{$R *.dfm}

function GPBFToStr(Value: Word): string;

  procedure AddValue(const S: string);
  begin
    if Result = '' then
      Result := S
    else
      Result := Result + ', ' + S;
  end;

begin
  if Value = 0 then
  begin
    Result := 'EMPTY';
    Exit;
  end;
  if PBF_CRYPTED and Value <> 0 then
    AddValue('PBF_CRYPTED');

  if PBF_DESCRIPTOR and Value <> 0 then
    AddValue('PBF_DESCRIPTOR');

  if PBF_UTF8 and Value <> 0 then
    AddValue('PBF_UTF8');

  if PBF_STRONG_CRYPT and Value <> 0 then
    AddValue('PBF_STRONG_CRYPT');
end;

procedure TdlgZipAnalizer.btnAnalizeClick(Sender: TObject);
begin
  edReport.Lines.BeginUpdate;
  try
    edReport.Clear;
    Log(edPath.Text);
    Log(Delim);
    Scan(edPath.Text);
    Log('DONE');
  finally
    edReport.Lines.EndUpdate;
  end;
end;

procedure TdlgZipAnalizer.btnBrowseClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    edPath.Text := OpenDialog.FileName;
    edReport.Clear;
  end;
end;


procedure TdlgZipAnalizer.edPathChange(Sender: TObject);
begin
  btnAnalize.Enabled := FileExists(edPath.Text);
end;

function TdlgZipAnalizer.FindSing(Stream: TStream): DWORD;
const
  KnownSigns: array [0..5] of DWORD = (
    LOCAL_FILE_HEADER_SIGNATURE,
    DATA_DESCRIPTOR_SIGNATURE,
    CENTRAL_FILE_HEADER_SIGNATURE,
    ZIP64_END_OF_CENTRAL_DIR_SIGNATURE,
    ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIGNATURE,
    END_OF_CENTRAL_DIR_SIGNATURE
  );

  function CalcLen: Integer;
  begin
    Result := Stream.Size - Stream.Position;
    if Result > 1024 then
      Result := 1024;
  end;

var
  pBuff, pCursor: PByte;
  I, A, Len: Integer;
  OldPosition: Int64;
begin
  Result := 0;
  GetMem(pBuff, 1024);
  try
    Len := CalcLen;
    while (Result = 0) and (Len > 4) do
    begin
      OldPosition := Stream.Position;
      Stream.ReadBuffer(pBuff^, Len);
      pCursor := pBuff;
      for I := 0 to Len - 4 do
      begin
        for A := 0 to 5 do
          if PDWORD(pCursor)^ = KnownSigns[A] then
          begin
            Result := KnownSigns[A];
            Break;
          end;
        if Result = 0 then
          Inc(pCursor)
        else
        begin
          Stream.Position := OldPosition + I;
          Break;
        end;
      end;
      if Result = 0 then
      begin
        Len := CalcLen;
        if Len > 0 then
          Stream.Position := Stream.Position - 4;
      end;
    end;
  finally
    FreeMem(pBuff);
  end;
end;

procedure TdlgZipAnalizer.FormCreate(Sender: TObject);
begin
  edReport.PlainText := True;
end;

procedure TdlgZipAnalizer.LoadStringValue(Stream: TStream;
  var Value: string; nSize: Cardinal; UTF: Boolean);
var
  aString: AnsiString;
begin
  if Integer(nSize) > 0 then
  begin
    SetLength(aString, nSize);
    Stream.ReadBuffer(aString[1], nSize);
    if UTF then
    begin
      {$IFDEF UNICODE}
      Value := string(UTF8ToUnicodeString(aString))
      {$ELSE}
      Value := string(UTF8Decode(aString));
      Value := StringReplace(Value, '?', '_', [rfReplaceAll]);
      {$ENDIF}
    end
    else
    begin
      OemToAnsi(@aString[1], @aString[1]);
      Value := string(aString);
    end;
  end;
end;

procedure TdlgZipAnalizer.Log(const Value: string);
begin
  edReport.Lines.Add(Value);
end;

procedure TdlgZipAnalizer.mnuSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    edReport.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TdlgZipAnalizer.PopupMenuPopup(Sender: TObject);
begin
  mnuSave.Enabled := edReport.Lines.Count > 1;
end;

procedure TdlgZipAnalizer.Scan(const Value: string);
var
  F: TFileStream;
  Sign: DWORD;
begin
  F := TFileStream.Create(Value, fmOpenRead or fmShareDenyWrite);
  try
    Sign := FindSing(F);
    while Sign <> 0 do
    begin
      case Sign of
        LOCAL_FILE_HEADER_SIGNATURE: ShowLocalFileHeader(F);
        DATA_DESCRIPTOR_SIGNATURE: ShowDataDescryptor(F);
        CENTRAL_FILE_HEADER_SIGNATURE: ShowCentralFileHeader(F);
        ZIP64_END_OF_CENTRAL_DIR_SIGNATURE: ShowZip64(F);
        ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIGNATURE: ShowZip64Locator(F);
        END_OF_CENTRAL_DIR_SIGNATURE: ShowEndOfCentralDir(F);
      end;
      Sign := FindSing(F);
    end;
  finally
    F.Free;
  end;
end;

procedure TdlgZipAnalizer.ShowCentralFileHeader(Stream: TStream);
var
  Data: TCentralDirectoryFileHeader;
  FileName, Comment: string;
begin
  Log('CENTRAL_FILE_HEADER_SIGNATURE found at offset: ' + IntToStr(Stream.Position));
  Stream.ReadBuffer(Data, SizeOf(TCentralDirectoryFileHeader));
  with Data do
  begin
    if CentralFileHeaderSignature <> CENTRAL_FILE_HEADER_SIGNATURE then
      Log('INVALID SIGNATURE!!!!');
    Log(Format('VersionMadeBy: %d', [VersionMadeBy]));
    Log(Format('VersionNeededToExtract: %d', [VersionNeededToExtract]));
    Log(Format('GeneralPurposeBitFlag: %d (%s)', [GeneralPurposeBitFlag,
      GPBFToStr(GeneralPurposeBitFlag)]));
    Log(Format('CompressionMethod: %d', [CompressionMethod]));
    Log(Format('LastModFileTimeTime: %d', [LastModFileTimeTime]));
    Log(Format('LastModFileTimeDate: %d', [LastModFileTimeDate]));
    Log(Format('Crc32: %d', [Crc32]));
    Log(Format('CompressedSize: %d', [CompressedSize]));
    Log(Format('UncompressedSize: %d', [UncompressedSize]));
    Log(Format('FilenameLength: %d', [FilenameLength]));
    Log(Format('ExtraFieldLength: %d', [ExtraFieldLength]));
    Log(Format('FileCommentLength: %d', [FileCommentLength]));
    Log(Format('DiskNumberStart: %d', [DiskNumberStart]));
    Log(Format('InternalFileAttributes: %d', [InternalFileAttributes]));
    Log(Format('ExternalFileAttributes: %d', [ExternalFileAttributes]));
    Log(Format('RelativeOffsetOfLocalHeader: %d', [RelativeOffsetOfLocalHeader]));
    LoadStringValue(Stream, FileName, FilenameLength,
      GeneralPurposeBitFlag and PBF_UTF8 <> 0);
    Log('>>> FileName: ' + FileName);
    Log(Delim);
    ShowExtraFields(Stream, ExtraFieldLength, Data);
    if FileCommentLength > 0 then
    begin
      LoadStringValue(Stream, Comment, FileCommentLength,
        GeneralPurposeBitFlag and PBF_UTF8 <> 0);
      Log('>>> FileComment: ' + Comment);
      Log(Delim);
    end;
  end;
end;

procedure TdlgZipAnalizer.ShowDataDescryptor(Stream: TStream);
var
  Data: TDataDescriptor;
begin
  Log('DATA_DESCRIPTOR_SIGNATURE found at offset: ' + IntToStr(Stream.Position));
  Stream.ReadBuffer(Data, SizeOf(TDataDescriptor));
  with Data do
  begin
    if DescriptorSignature <> DATA_DESCRIPTOR_SIGNATURE then
      Log('INVALID SIGNATURE!!!!');
    Log(Format('Crc32: %d', [Crc32]));
    Log(Format('CompressedSize: %d', [CompressedSize]));
    Log(Format('UncompressedSize: %d', [UncompressedSize]));
  end;
  Log(Delim);
end;

procedure TdlgZipAnalizer.ShowEndOfCentralDir(Stream: TStream);
var
  Data: TEndOfCentralDir;
  Comment: string;
begin
  Log('END_OF_CENTRAL_DIR_SIGNATURE found at offset: ' + IntToStr(Stream.Position));
  Stream.ReadBuffer(Data, SizeOf(TEndOfCentralDir));
  with Data do
  begin
    if EndOfCentralDirSignature <> END_OF_CENTRAL_DIR_SIGNATURE then
      Log('INVALID SIGNATURE!!!!');
    Log(Format('NumberOfThisDisk: %d', [NumberOfThisDisk]));
    Log(Format('NumberOfTheDiskWithTheStart: %d', [NumberOfTheDiskWithTheStart]));
    Log(Format('TotalNumberOfEntriesOnThisDisk: %d', [TotalNumberOfEntriesOnThisDisk]));
    Log(Format('TotalNumberOfEntries: %d', [TotalNumberOfEntries]));
    Log(Format('SizeOfTheCentralDirectory: %d', [SizeOfTheCentralDirectory]));
    Log(Format('OffsetOfStartOfCentralDirectory: %d', [OffsetOfStartOfCentralDirectory]));
    Log(Format('ZipfileCommentLength: %d', [ZipfileCommentLength]));
    if ZipfileCommentLength > 0 then
    begin
      LoadStringValue(Stream, Comment, ZipfileCommentLength, False);
      Log(Format('>>> Comment: %s', [Comment]));
    end;
  end;
  Log(Delim);
end;

procedure TdlgZipAnalizer.ShowExtraFields(Stream: TStream; Size: Integer;
  FileHeader: TCentralDirectoryFileHeader);
var
  Buff, EOFBuff: Pointer;
  BuffCount: Integer;
  HeaderID, BlockSize: Word;

  function GetOffset(Value: Integer): Pointer;
  begin
    Result := Pointer(Integer(EOFBuff) - Value);
  end;

  function ByteToStr(Bytes: PByte; Size: Integer): string;
  const
    BytesHex: array[0..15] of char =
      ('0', '1', '2', '3', '4', '5', '6', '7',
        '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
  var
    I: integer;
  begin
    SetLength(Result, Size shl 1);
    for I := 0 to Size - 1 do
    begin
      Result[I * 2 + 1] := BytesHex[Bytes^ shr 4];
      Result[I * 2 + 2] := BytesHex[Bytes^ and $0F];
      Inc(Bytes);
    end;
  end;

var
  ExDataStream: TMemoryStream;
  StartPos: Int64;
begin
  if Size = 0 then Exit;
  StartPos := Stream.Position;
  Log('EXDATA found at offset: ' + IntToStr(StartPos));
  Log(Delim);
  GetMem(Buff, Size);
  try
    BuffCount := Size;
    Stream.ReadBuffer(Buff^, BuffCount);
    EOFBuff := Pointer(Integer(Buff) + BuffCount);
    while BuffCount > 0 do
    begin
      HeaderID := PWord(GetOffset(BuffCount))^;
      Dec(BuffCount, 2);
      BlockSize := PWord(GetOffset(BuffCount))^;
      Dec(BuffCount, 2);
      case HeaderID of
        SUPPORTED_EXDATA_ZIP64:
        begin
          Log('SUPPORTED_EXDATA_ZIP64 found at offset: ' +
            IntToStr(StartPos + Size - BuffCount - 4));
          if FileHeader.UncompressedSize = MAXDWORD then
          begin
            if BuffCount < 8 then Break;
            Log('UncompressedSize: ' + IntToStr(PInt64(GetOffset(BuffCount))^));
            Dec(BuffCount, 8);
            Dec(BlockSize, 8);
          end;
          if FileHeader.CompressedSize = MAXDWORD then
          begin
            if BuffCount < 8 then Break;
            Log('CompressedSize: ' + IntToStr(PInt64(GetOffset(BuffCount))^));
            Dec(BuffCount, 8);
            Dec(BlockSize, 8);
          end;
          if FileHeader.RelativeOffsetOfLocalHeader = MAXDWORD then
          begin
            if BuffCount < 8 then Break;
            Log('RelativeOffsetOfLocalHeader: ' + IntToStr(PInt64(GetOffset(BuffCount))^));
            Dec(BuffCount, 8);
            Dec(BlockSize, 8);
          end;
          if FileHeader.DiskNumberStart = MAXWORD then
          begin
            if BuffCount < 4 then Break;
            Log('DiskNumberStart: ' + IntToStr(PInt64(GetOffset(BuffCount))^));
            Dec(BuffCount, 4);
            Dec(BlockSize, 4);
          end;
          Dec(BuffCount, BlockSize);
          Log(Delim);
        end;

        SUPPORTED_EXDATA_NTFSTIME:
        begin

          if BuffCount < 32 then Break;
          if BlockSize <> 32 then
          begin
            Dec(BuffCount, BlockSize);
            Continue;
          end;
          Dec(BuffCount, 4);
          if PWord(GetOffset(BuffCount))^ <> 1 then
          begin
            Dec(BuffCount, BlockSize);
            Continue;
          end;
          Dec(BuffCount, 2);
          if PWord(GetOffset(BuffCount))^ <> SizeOf(TNTFSFileTime) then
          begin
            Dec(BuffCount, BlockSize);
            Continue;
          end;
          Dec(BuffCount, 2);

          Log('SUPPORTED_EXDATA_NTFSTIME found at offset: ' +
            IntToStr(StartPos + Size - BuffCount - 12));
          Log(Delim);

       end;
      else
        Log(Format('UNKNOWN EXDATA TAG %d found at offset: %d',
          [HeaderID, StartPos + Size - BuffCount - 8]));
        ExDataStream := TMemoryStream.Create;
        try
          ExDataStream.WriteBuffer(GetOffset(BuffCount)^, BlockSize);
          ExDataStream.Position := 0;
          Log(ByteToStr(ExDataStream.Memory, ExDataStream.Size));
        finally
          ExDataStream.Free;
        end;
        Log(Delim);
      end;
      Dec(BuffCount, BlockSize);
    end;
  finally
    FreeMem(Buff);
  end;
end;

procedure TdlgZipAnalizer.ShowLocalFileHeader(Stream: TStream);
var
  Data: TLocalFileHeader;
  FileName: string;
begin
  Log('LOCAL_FILE_HEADER_SIGNATURE found at offset: ' + IntToStr(Stream.Position));
  Stream.ReadBuffer(Data, SizeOf(TLocalFileHeader));
  with Data do
  begin
    if LocalFileHeaderSignature <> LOCAL_FILE_HEADER_SIGNATURE then
      Log('INVALID SIGNATURE!!!!');
    Log(Format('VersionNeededToExtract: %d', [VersionNeededToExtract]));
    Log(Format('GeneralPurposeBitFlag: %d (%s)', [GeneralPurposeBitFlag,
      GPBFToStr(GeneralPurposeBitFlag)]));
    Log(Format('CompressionMethod: %d', [CompressionMethod]));
    Log(Format('LastModFileTimeTime: %d', [LastModFileTimeTime]));
    Log(Format('LastModFileTimeDate: %d', [LastModFileTimeDate]));
    Log(Format('Crc32: %d', [Crc32]));
    Log(Format('CompressedSize: %d', [CompressedSize]));
    Log(Format('UncompressedSize: %d', [UncompressedSize]));
    Log(Format('FilenameLength: %d', [FilenameLength]));
    Log(Format('ExtraFieldLength: %d', [ExtraFieldLength]));
    LoadStringValue(Stream, FileName, FilenameLength,
      GeneralPurposeBitFlag and PBF_UTF8 <> 0);
    Log('>>> FileName: ' + FileName);
    //Stream.Position := Stream.Position + CompressedSize;
  end;
  Log(Delim);
end;

procedure TdlgZipAnalizer.ShowZip64(Stream: TStream);
var
  Data: TZip64EOFCentralDirectoryRecord;
begin
  Log('ZIP64_END_OF_CENTRAL_DIR_SIGNATURE found at offset: ' + IntToStr(Stream.Position));
  Stream.ReadBuffer(Data, SizeOf(TZip64EOFCentralDirectoryRecord));
  with Data do
  begin
    if Zip64EndOfCentralDirSignature <> ZIP64_END_OF_CENTRAL_DIR_SIGNATURE then
      Log('INVALID SIGNATURE!!!!');
    Log(Format('SizeOfZip64EOFCentralDirectoryRecord: %d', [SizeOfZip64EOFCentralDirectoryRecord]));
    Log(Format('VersionMadeBy: %d', [VersionMadeBy]));
    Log(Format('VersionNeededToExtract: %d', [VersionNeededToExtract]));
    Log(Format('number of this disk: %d', [Number1]));
    Log(Format('number of the disk with the start of the central directory: %d', [Number2]));
    Log(Format('total number of entries in the central directory on this disk: %d', [TotalNumber1]));
    Log(Format('total number of entries in the central directory: %d', [TotalNumber2]));
    Log(Format('size of the central directory: %d', [Size]));
    Log(Format('offset of start of central directory with respect to the starting disk number: %d', [Offset]));
  end;
  Log(Delim);
end;

procedure TdlgZipAnalizer.ShowZip64Locator(Stream: TStream);
var
  Data: TZip64EOFCentralDirectoryLocator;
begin
  Log('ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIGNATURE found at offset: ' + IntToStr(Stream.Position));
  Stream.ReadBuffer(Data, SizeOf(TZip64EOFCentralDirectoryLocator));
  with Data do
  begin
    if Signature <> ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIGNATURE then
      Log('INVALID SIGNATURE!!!!');
    Log(Format('NumberOfTheDisk: %d', [NumberOfTheDisk]));
    Log(Format('RelativeOffset: %d', [RelativeOffset]));
    Log(Format('TotalNumberOfDisks: %d', [TotalNumberOfDisks]));
  end;
  Log(Delim);
end;

end.
