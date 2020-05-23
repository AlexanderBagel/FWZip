////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip - ZipAnalizer
//  * Unit Name : uZipAnalizer
//  * Purpose   : Вывод параметров архива используя возможности FWZipReader
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

unit uZipAnalizer;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus,
  FWZipReader, FWZipConsts;

type
  TFWZipReaderFriendly = class(TFWZipReader);
  TFWZipReaderItemFriendly = class(TFWZipReaderItem);
  TExDataRecord = record
    Index: Integer;
    Tag: Word;
    Stream: TMemoryStream;
  end;
  TExDataRecords = array of TExDataRecord;

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
    procedure FormDestroy(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
  private
    ExDataRecords: TExDataRecords;
    Zip: TFWZipReaderFriendly;
    procedure OnLoadExData(Sender: TObject; ItemIndex: Integer;
      Tag: Word; Data: TStream);
  private
    procedure ClearExData;
    procedure Log(const Value: string);
    procedure ShowEndOfCentralDir;
    procedure ShowZip64EOFCentralDirectoryLocator;
    procedure ShowZip64EOFCentralDirectoryRecord;
    procedure ShowItemData(Index: Integer);
  end;

var
  dlgZipAnalizer: TdlgZipAnalizer;

implementation

const
  Delim = '===================================================================';

{$R *.dfm}

procedure TdlgZipAnalizer.btnAnalizeClick(Sender: TObject);
var
  I: Integer;
begin
  edReport.Lines.BeginUpdate;
  try
    edReport.Clear;
    Log(edPath.Text);
    Log(Delim);
    ClearExData;
    Zip.Clear;
    Zip.LoadFromFile(edPath.Text);
    ShowEndOfCentralDir;
    ShowZip64EOFCentralDirectoryLocator;
    ShowZip64EOFCentralDirectoryRecord;
    for I := 0 to Zip.Count - 1 do
      ShowItemData(I);
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

procedure TdlgZipAnalizer.ClearExData;
var
  I: Integer;
begin
  for I := 0 to Length(ExDataRecords) - 1 do
    ExDataRecords[I].Stream.Free;
  SetLength(ExDataRecords, 0);;
end;

procedure TdlgZipAnalizer.edPathChange(Sender: TObject);
begin
  btnAnalize.Enabled := FileExists(edPath.Text);
end;

procedure TdlgZipAnalizer.FormCreate(Sender: TObject);
begin
  Zip := TFWZipReaderFriendly.Create;
  Zip.OnLoadExData := OnLoadExData;
  edReport.PlainText := True;
end;

procedure TdlgZipAnalizer.FormDestroy(Sender: TObject);
begin
  ClearExData;
  Zip.Free;
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

procedure TdlgZipAnalizer.OnLoadExData(Sender: TObject; ItemIndex: Integer;
  Tag: Word; Data: TStream);
var
  Count: Integer;
begin
  Count := Length(ExDataRecords);
  SetLength(ExDataRecords, Count + 1);
  ExDataRecords[Count].Index := ItemIndex;
  ExDataRecords[Count].Tag := Tag;
  ExDataRecords[Count].Stream := TMemoryStream.Create;
  ExDataRecords[Count].Stream.CopyFrom(Data, 0);
end;

procedure TdlgZipAnalizer.PopupMenuPopup(Sender: TObject);
begin
  mnuSave.Enabled := edReport.Lines.Count > 1;
end;

procedure TdlgZipAnalizer.ShowEndOfCentralDir;
begin
  Log('END_OF_CENTRAL_DIR_SIGNATURE found');
  with Zip.EndOfCentralDir do
  begin
    Log(Format('NumberOfThisDisk: %d', [NumberOfThisDisk]));
    Log(Format('NumberOfTheDiskWithTheStart: %d', [DiskNumberStart]));
    Log(Format('TotalNumberOfEntriesOnThisDisk: %d', [TotalNumberOfEntriesOnThisDisk]));
    Log(Format('TotalNumberOfEntries: %d', [TotalNumberOfEntries]));
    Log(Format('SizeOfTheCentralDirectory: %d', [SizeOfTheCentralDirectory]));
    Log(Format('OffsetOfStartOfCentralDirectory: %d', [RelativeOffsetOfCentralDirectory]));
    Log(Format('ZipfileCommentLength: %d', [ZipfileCommentLength]));
    if ZipfileCommentLength > 0 then
      Log(Format('Comment: %s', [Zip.Comment]));
  end;
  Log(Delim);
end;

procedure TdlgZipAnalizer.ShowItemData(Index: Integer);

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

var
  I: Integer;
  Item: TFWZipReaderItemFriendly;
begin
  Log('CENTRAL_FILE_HEADER_SIGNATURE found');
  Item := TFWZipReaderItemFriendly(Zip.Item[Index]);
  with Item.CentralDirFileHeader do
  begin
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
    if FilenameLength > 0 then
      Log('>>> FileName: ' + Item.FileName);
    Log(Format('ExtraFieldLength: %d', [ExtraFieldLength]));
    Log(Format('FileCommentLength: %d', [FileCommentLength]));
    if FileCommentLength > 0 then
      Log('>>> FileComment: ' + Item.Comment);
    Log(Format('DiskNumberStart: %d', [DiskNumberStart]));
    Log(Format('InternalFileAttributes: %d', [InternalFileAttributes]));
    Log(Format('ExternalFileAttributes: %d', [ExternalFileAttributes]));
    Log(Format('RelativeOffsetOfLocalHeader: %d', [RelativeOffsetOfLocalHeader]));
  end;

  Log('');

  Item.LoadLocalFileHeader;
  Log('LOCAL_FILE_HEADER_SIGNATURE found');
  with Item.LocalFileHeader do
  begin
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
  end;

  if ssZIP64 in Item.PresentStreams then
  begin
    Log('');
    Log('SUPPORTED_EXDATA_ZIP64 found');
    with Item do
    begin
      Log(Format('UncompressedSize: %d', [UncompressedSize]));
      Log(Format('CompressedSize: %d', [CompressedSize]));
      Log(Format('RelativeOffsetOfLocalHeader: %d', [RelativeOffsetOfLocalHeader]));
      Log(Format('DiskNumberStart: %d', [DiskNumberStart]));
    end;
  end;

  if ssNTFS in Item.PresentStreams then
  begin
    Log('');
    Log('SUPPORTED_EXDATA_NTFSTIME found');
  end;

  for I := 0 to Length(ExDataRecords) - 1 do
    if ExDataRecords[I].Index = Index then
    begin
      Log('');
      Log(Format('UNKNOWN TAG (%d) found', [ExDataRecords[I].Tag]));
      Log(Format('ExData size %d', [ExDataRecords[I].Stream.Size]));
      Log('ExData dump:');
      Log(ByteToStr(ExDataRecords[I].Stream.Memory, ExDataRecords[I].Stream.Size));
    end;

  Log(Delim);
end;

procedure TdlgZipAnalizer.ShowZip64EOFCentralDirectoryLocator;
begin
  with Zip.Zip64EOFCentralDirectoryLocator do
  begin
    if Signature <> ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIGNATURE then Exit;
    Log('ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIGNATURE  found');
    Log(Format('NumberOfTheDisk: %d', [DiskNumberStart]));
    Log(Format('RelativeOffset: %d', [RelativeOffset]));
    Log(Format('TotalNumberOfDisks: %d', [TotalNumberOfDisks]));
  end;
  Log(Delim);
end;

procedure TdlgZipAnalizer.ShowZip64EOFCentralDirectoryRecord;
begin
  with Zip.Zip64EOFCentralDirectoryRecord do
  begin
    if Zip64EndOfCentralDirSignature <> ZIP64_END_OF_CENTRAL_DIR_SIGNATURE then Exit;
    Log('ZIP64_END_OF_CENTRAL_DIR_SIGNATURE  found');
    Log(Format('SizeOfZip64EOFCentralDirectoryRecord: %d', [SizeOfZip64EOFCentralDirectoryRecord]));
    Log(Format('VersionMadeBy: %d', [VersionMadeBy]));
    Log(Format('VersionNeededToExtract: %d', [VersionNeededToExtract]));
    Log(Format('number of this disk: %d', [NumberOfThisDisk]));
    Log(Format('number of the disk with the start of the central directory: %d', [DiskNumberStart]));
    Log(Format('total number of entries in the central directory on this disk: %d', [TotalNumberOfEntriesOnThisDisk]));
    Log(Format('total number of entries in the central directory: %d', [TotalNumberOfEntries]));
    Log(Format('size of the central directory: %d', [SizeOfTheCentralDirectory]));
    Log(Format('offset of start of central directory with respect to the starting disk number: %d', [RelativeOffsetOfCentralDirectory]));
  end;
  Log(Delim);
end;

end.
