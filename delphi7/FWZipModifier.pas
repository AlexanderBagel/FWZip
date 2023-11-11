////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : FWZipModifier
//  * Purpose   : ����� ��� ����������� ���������� ����� ZIP ������
//  * Author    : ��������� (Rouse_) ������
//  * Copyright : � Fangorn Wizards Lab 1998 - 2023.
//  * Version   : 2.0.2
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

unit FWZipModifier;

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  FWZipConsts,
  FWZipReader,
  FWZipWriter,
  FWZipStream,
  FWZipZLib,
  FWZipUtils;

type
  TReaderIndex = Integer;

  TFWZipModifierItem = class(TFWZipWriterItem)
  private
    FReaderIndex: TReaderIndex;     // ������ TFWZipReader � ������� TFWZipModifier.FReaderList
    FOriginalItemIndex: Integer;    // ������������ ������ �������� � ����������� ������
    FOverloadItemPath: string;      // ���������� ����� ���� �������� � ������
  protected
    property ReaderIndex: TReaderIndex read FReaderIndex write FReaderIndex;
    property OriginalItemIndex: Integer read FOriginalItemIndex write FOriginalItemIndex;
    property OverloadItemPath: string read FOverloadItemPath write FOverloadItemPath;
  public
    constructor Create(Owner: TFWZipWriter;
      const InitFilePath: string;
      InitAttributes: TFileAttributeData;
      const InitFileName: string = ''); override;
  end;

  EFWZipModifier = class(Exception);

  // ������ ��������� ������ ��� ����� ExData �� ������������ �������
  TExDataRecord = record
    Index: Integer;
    Tag: Word;
    Stream: TMemoryStream;
  end;
  TExDataRecords = array of TExDataRecord;

  // ��������� ��� �������� ������������� ������ � ��� ������ ExData
  TReaderOwnership = (roReference, roOwned);
  TReaderData = record
    Reader: TFWZipReader;
    ExDataRecords: TExDataRecords;
    Ownership: TReaderOwnership;
  end;

  TReaderList = array of TReaderData;

  TFWZipModifier = class(TFWZipWriter)
  private
    FReaderList: TReaderList;
    function CheckZipFileIndex(Value: TReaderIndex): TReaderIndex;
    function AddItemFromZip(AReader: TFWZipReader;
      ReaderIndex: TReaderIndex; ItemIndex: Integer;
      OverloadItemPath: string): Integer;
    function GetReader(Index: Integer): TFWZipReader;
  protected
    function GetItemClass: TFWZipWriterItemClass; override;
    procedure FillItemCDFHeader(CurrentItem: TFWZipWriterItem;
      var Value: TCentralDirectoryFileHeaderEx); override;
    procedure CompressItem(CurrentItem: TFWZipWriterItem;
      Index: Integer; StreamSizeBeforeCompress: Int64; Stream: TStream); override;
    procedure FillExData(Stream: TStream; Index: Integer); override;
    procedure OnLoadExData(Sender: TObject; ItemIndex: Integer;
      Tag: Word; Data: TStream);
  public
    destructor Destroy; override;
    function AddZipFile(const AReader: TFWZipReader;
      AOwnership: TReaderOwnership = roReference): TReaderIndex; overload;
    function AddZipFile(const FilePath: string; SFXOffset: Integer = -1;
      ZipEndOffset: Integer = -1): TReaderIndex; overload;
    function AddZipFile(FileStream: TStream; SFXOffset: Integer = -1;
      ZipEndOffset: Integer = -1): TReaderIndex; overload;
    function AddFromZip(ReaderIndex: TReaderIndex): Integer; overload;
    function AddFromZip(ReaderIndex: TReaderIndex; const ItemPath: string): Integer; overload;
    function AddFromZip(ReaderIndex: TReaderIndex; const ItemPath, NewItemPath: string): Integer; overload;
    function AddFromZip(ReaderIndex: TReaderIndex; ItemsList: TStringList): Integer; overload;
    function ReadersCount: Integer;
    property Reader[Index: Integer]: TFWZipReader read GetReader;
  end;

implementation

type
  TFWZipReaderFriendly = class(TFWZipReader);
  TFWZipReaderItemFriendly = class(TFWZipReaderItem);

{ TFWZipModifierItem }

//
//  � ������������ ���������� ��������� ������������� �����
//  ���� ���� ReaderIndex � OriginalItemIndex ����� ������������������ ������
//  ��� ���������� �� ����������� ������ TFWZipModifier
// =============================================================================
constructor TFWZipModifierItem.Create(Owner: TFWZipWriter;
  const InitFilePath: string; InitAttributes: TFileAttributeData;
  const InitFileName: string);
begin
  inherited Create(Owner, InitFilePath, InitAttributes, InitFileName);
  FReaderIndex := -1;
  FOriginalItemIndex := -1;
end;

{ TFWZipModifier }

//
//  ������� ��������� ������� � ��������� ����� �� ����� ������������ ������.
//  � �������� ���������� ���������� ������ �������� � ������.
//  ���������:
//  ReaderIndex - ������ ����� ���������� �������� AddZipFile ������
//  ItemPath - ��� ��������, ������� ��������� ��������
// =============================================================================
function TFWZipModifier.AddFromZip(ReaderIndex: TReaderIndex;
  const ItemPath: string): Integer;
begin
  Result := AddFromZip(ReaderIndex, ItemPath, EmptyStr);
end;

//
//  ������� ��������� ��� �������� �� ����� ������������ ������ � ��������� �����.
//  � �������� ���������� ���������� ���������� ������� ����������� ���������.
//  ���������:
//  ReaderIndex - ������ ����� ���������� �������� AddZipFile ������
// =============================================================================
function TFWZipModifier.AddFromZip(ReaderIndex: TReaderIndex): Integer;
var
  I: Integer;
  Reader: TFWZipReader;
begin
  CheckZipFileIndex(ReaderIndex);
  Result := 0;
  Reader := FReaderList[ReaderIndex].Reader;
  for I := 0 to Reader.Count - 1 do
    if AddItemFromZip(Reader, ReaderIndex, I, EmptyStr) >= 0 then
      Inc(Result);
end;

//
//  ������� ��������� ��� �������� �� ����� ������������ ������
//  �� ������ � ��������� �����.
//  � �������� ���������� ���������� ���������� ������� ����������� ���������.
//  ���������:
//  ReaderIndex - ������ ����� ���������� �������� AddZipFile ������
//  ItemsList - ������ ��������� � ����������
// =============================================================================
function TFWZipModifier.AddFromZip(ReaderIndex: TReaderIndex;
  ItemsList: TStringList): Integer;
var
  I: Integer;
  Reader: TFWZipReader;
begin
  CheckZipFileIndex(ReaderIndex);
  Result := 0;
  Reader := FReaderList[ReaderIndex].Reader;
  for I := 0 to ItemsList.Count - 1 do
    if AddItemFromZip(Reader, ReaderIndex,
      Reader.GetElementIndex(ItemsList[I]), EmptyStr) >= 0 then
      Inc(Result);
end;

//
//  ������� ��������� ������� � ��������� ����� �� ����� ������������ ������.
//  � �������� ���������� ���������� ������ �������� � ������.
//  ���������:
//  ReaderIndex - ������ ����� ���������� �������� AddZipFile ������
//  ItemPath - ��� ��������, ������� ��������� ��������
//  NewItemPath - ����� ��� �������� � ������
// =============================================================================
function TFWZipModifier.AddFromZip(ReaderIndex: TReaderIndex; const ItemPath,
  NewItemPath: string): Integer;
var
  Reader: TFWZipReader;
begin
  CheckZipFileIndex(ReaderIndex);
  Reader := FReaderList[ReaderIndex].Reader;
  Result :=
    AddItemFromZip(Reader, ReaderIndex, Reader.GetElementIndex(ItemPath),
      CheckFileNameSlashes( NewItemPath));
end;

//
//  ������� ��������� ������� � ��������� ����� �� ����� ������������ ������.
//  � �������� ���������� ���������� ������ �������� � ������.
// =============================================================================
function TFWZipModifier.AddItemFromZip(AReader: TFWZipReader;
  ReaderIndex: TReaderIndex; ItemIndex: Integer;
  OverloadItemPath: string): Integer;
const
  OldItemType: array [Boolean] of string = ('file', 'folder');
var
  OldItem: TFWZipReaderItemFriendly;
  NewItem: TFWZipModifierItem;
begin
  Result := -1;
  if ItemIndex < 0 then Exit;
  // �������� ��������� �� ������� �� ��� ������������� ������
  OldItem := TFWZipReaderItemFriendly(AReader.Item[ItemIndex]);
  // �������� ������������ ������ ����� ���� ������� ��������
  if OverloadItemPath <> '' then
  begin
    if OverloadItemPath <> OldItem.FileName then
    begin
      if OldItem.IsFolder <>
        (OverloadItemPath[Length(OverloadItemPath)] = ZIP_SLASH) then
        raise EFWZipModifier.CreateFmt(
          '"%s" does not match the %s path.',
          [OverloadItemPath, OldItemType[OldItem.IsFolder]]);
    end
    else
      OverloadItemPath := EmptyStr;
  end;
  // ������� ����� �������, ������� ����� ��������� � ������ ������
  NewItem := TFWZipModifierItem(
    GetItemClass.Create(Self, '', OldItem.Attributes, OldItem.FileName));
  // ����������� ��� � ����� ��������� ������ �������
  NewItem.UseExternalData := True;
  // �������������� ��� �������, ���� ����� ������, ������ ����� � ��� ������
  NewItem.ReaderIndex := ReaderIndex;
  NewItem.OriginalItemIndex := ItemIndex;
  NewItem.OverloadItemPath := OverloadItemPath;
  // �������������� ������� � �������������� ����
  NewItem.Comment := OldItem.Comment;
  NewItem.NeedDescriptor :=
    OldItem.CentralDirFileHeader.GeneralPurposeBitFlag and PBF_DESCRIPTOR <> 0;
  NewItem.UseUTF8String :=
    OldItem.CentralDirFileHeader.GeneralPurposeBitFlag and PBF_UTF8 <> 0;
  // ���������
  Result := AddNewItem(NewItem);
end;

//
//  ������� ��������� ����� ����� �� �������� ����� ����� ������� ������.
//  � �������� ���������� ���������� ������ ������ � ������ �����������.
//  ���������:
//  FileStream - ����� � ������� ������
//  SFXOffset � ZipEndOffset - ��� �������
// =============================================================================
function TFWZipModifier.AddZipFile(FileStream: TStream; SFXOffset,
  ZipEndOffset: Integer): TReaderIndex;
var
  AReader: TFWZipReader;
begin
  AReader := TFWZipReader.Create;
  Result := AddZipFile(AReader, roOwned);
  AReader.OnLoadExData := OnLoadExData;
  AReader.LoadFromStream(FileStream, SFXOffset, ZipEndOffset);
end;

//
//  ������� ��������� ������ ����� � ������ ���������
//  �������� ������ �� ������������, ������� ������ ������� ����������
//  ������ �� �������� �������� � ������ ExData ���� ������� ������������.
// =============================================================================
function TFWZipModifier.AddZipFile(const AReader: TFWZipReader;
  AOwnership: TReaderOwnership): TReaderIndex;
begin
  Result := Length(FReaderList);
  SetLength(FReaderList, Result + 1);
  FReaderList[Result].Reader := AReader;
  FReaderList[Result].Ownership := AOwnership;
end;

//
//  ������� ��������� ����� ����� �� �������� ����� ����� ������� ������.
//  � �������� ���������� ���������� ������ ������ � ������ �����������.
//  ���������:
//  FilePath - ���� � ������������ ������
//  SFXOffset � ZipEndOffset - ��� �������
// =============================================================================
function TFWZipModifier.AddZipFile(const FilePath: string;
  SFXOffset, ZipEndOffset: Integer): TReaderIndex;
var
  AReader: TFWZipReader;
begin
  AReader := TFWZipReader.Create;
  Result := AddZipFile(AReader, roOwned);
  AReader.OnLoadExData := OnLoadExData;
  AReader.LoadFromFile(FilePath, SFXOffset, ZipEndOffset);
end;

//
//  ������� ��������� ������������ ����������� ������� ������ � ������
// =============================================================================
function TFWZipModifier.CheckZipFileIndex(Value: TReaderIndex): TReaderIndex;
begin
  Result := Value;
  if (Value < 0) or (Value >= Length(FReaderList)) then
    raise EFWZipModifier.CreateFmt('Invalid index value (%d).', [Value]);
end;

//
//  ��������� ����������� ������ ������ ���������
//  � ����� ��� ������ �� ����� ��������������� ������.
// =============================================================================
procedure TFWZipModifier.CompressItem(CurrentItem: TFWZipWriterItem;
  Index: Integer; StreamSizeBeforeCompress: Int64; Stream: TStream);
var
  OldItem: TFWZipReaderItemFriendly;
  NewItem: TFWZipModifierItem;
  Reader: TFWZipReaderFriendly;
  Offset: Int64;
begin
  NewItem := TFWZipModifierItem(CurrentItem);
  // ��������, �������� �� �� � ���������, ������ �������� ����������� �������?
  if not NewItem.UseExternalData then
  begin
    inherited;
    Exit;
  end;
  // �������� ��������� �� �����, ������� ������ ����������� ����� �����
  Reader := TFWZipReaderFriendly(FReaderList[NewItem.ReaderIndex].Reader);
  // �������� ��������� �� ������������ ������� ������
  OldItem := TFWZipReaderItemFriendly(Reader.Item[NewItem.OriginalItemIndex]);
  // ������������ ��� ������� � ������
  if IsMultiPartZip(Reader.ZIPStream) then
  begin
    TFWAbstractMultiStream(Reader.ZIPStream).Seek(
      OldItem.CentralDirFileHeader.DiskNumberStart,
      OldItem.CentralDirFileHeader.RelativeOffsetOfLocalHeader);
    Offset := Reader.ZIPStream.Position;
  end
  else
    Offset := OldItem.CentralDirFileHeader.RelativeOffsetOfLocalHeader;
  Inc(Offset, SizeOf(TLocalFileHeader));
  Inc(Offset, OldItem.CentralDirFileHeader.FilenameLength);
  if OldItem.CentralDirFileHeaderEx.UncompressedSize >= MAXDWORD then
    Inc(Offset, SizeOf(TExDataInfo64));
  Reader.ZIPStream.Position := Offset;
  // �������� ������ ��� ����, ��� �����������
  Stream.CopyFrom(Reader.ZIPStream, OldItem.CentralDirFileHeaderEx.CompressedSize);
end;

//
//  Modifier ������ �� ���������� ��������� ������, ������� ���������.
// =============================================================================
destructor TFWZipModifier.Destroy;
var
  I, A: Integer;
begin
  for I := 0 to Length(FReaderList) - 1 do
  begin
    if FReaderList[I].Ownership = roOwned then
      FReaderList[I].Reader.Free;
    for A := 0 to Length(FReaderList[I].ExDataRecords) - 1 do
      FReaderList[I].ExDataRecords[A].Stream.Free;
  end;
  inherited;
end;

//
//  ��������� ����������� ���������� ������ ExData
//  � ����� ��� ������ �� ����� ��������������� ������.
// =============================================================================
procedure TFWZipModifier.FillExData(Stream: TStream; Index: Integer);
var
  NewItem: TFWZipModifierItem;
  ReaderIndex: TReaderIndex;
  I: Integer;
  {%H-}ExDataSize: Word;
  ExDataRecord: TExDataRecord;
begin
  NewItem := TFWZipModifierItem(Item[Index]);
  // ��������, �������� �� �� � ���������, ������ �������� ����������� �������?
  if not NewItem.UseExternalData then
  begin
    inherited;
    Exit;
  end;
  // ��������� �������� � ������, � ��������� �������� �� ����� ��������� ����� ExData
  ReaderIndex := CheckZipFileIndex(NewItem.ReaderIndex);
  for I := 0 to Length(FReaderList[ReaderIndex].ExDataRecords) - 1 do
    if FReaderList[ReaderIndex].ExDataRecords[I].Index = NewItem.OriginalItemIndex then
    begin
      // ������ ����� ���� ���������, ������� ��������� �� ���
      ExDataRecord := FReaderList[ReaderIndex].ExDataRecords[I];
      Stream.WriteBuffer(ExDataRecord.Tag, 2);
      ExDataSize := ExDataRecord.Stream.Size;
      Stream.WriteBuffer(ExDataSize, 2);
      Stream.CopyFrom(ExDataRecord.Stream, 0);
    end;
end;

//
//  ��������� ����������� ���������� ��������� TCentralDirectoryFileHeaderEx
//  � ����� ��� ������ �� ����� ��������������� ������.
// =============================================================================
procedure TFWZipModifier.FillItemCDFHeader(CurrentItem: TFWZipWriterItem;
  var Value: TCentralDirectoryFileHeaderEx);
var
  OldItem: TFWZipReaderItemFriendly;
  NewItem: TFWZipModifierItem;
  Reader: TFWZipReader;
  FileDate: Cardinal;
begin
  NewItem := TFWZipModifierItem(CurrentItem);

  // ��������, �������� �� �� � ���������, ������ �������� ����������� �������?
  if not NewItem.UseExternalData then
  begin
    inherited;
    Exit;
  end;

  Reader := FReaderList[NewItem.ReaderIndex].Reader;
  OldItem := TFWZipReaderItemFriendly(Reader.Item[NewItem.OriginalItemIndex]);

  // ��������� �������������� ��� ������ ���������
  // ����������� �������� ���� RelativeOffsetOfLocalHeader
  // �� ��� ������������������ ����� ������ ������� ������
  Value := OldItem.CentralDirFileHeaderEx;

  // Rouse_ 11.11.2023
  // ���� ��� � ������ ���� �������������,
  // �� ������ ��� � ������� ������� ���������� ���������
  if NewItem.OverloadItemPath <> '' then
  begin
    Value.Filename := NewItem.OverloadItemPath;
    Value.Header.FilenameLength :=
      StringLength(NewItem.OverloadItemPath, CurrentItem.UseUTF8String);

    Value.Attributes.ftLastWriteTime := GetCurrentFileTime;
    FileDate := FileTimeToLocalFileDate(Value.Attributes.ftLastWriteTime);

    Value.Header.LastModFileTimeTime := FileDate and $FFFF;
    Value.Header.LastModFileTimeDate := FileDate shr 16;
  end;
end;

//
//  ��������� ���������
// =============================================================================
function TFWZipModifier.GetItemClass: TFWZipWriterItemClass;
begin
  Result := TFWZipModifierItem;
end;

//
//  ���������� ����� �� ���������� �����
// =============================================================================
function TFWZipModifier.GetReader(Index: Integer): TFWZipReader;
begin
  if (Index < 0) or (Index >= ReadersCount) then
    raise EFWZipModifier.CreateFmt('Invalid reader index value (%d).', [Index]);
  Result := FReaderList[Index].Reader;
end;

//
//  ������ ��������� ������� ��� ExData � ��������� ���������,
//  ����� �� ����� ���� ������������ � ��������� ������ �� ����� �������
// =============================================================================
procedure TFWZipModifier.OnLoadExData(Sender: TObject; ItemIndex: Integer;
  Tag: Word; Data: TStream);
var
  Index, ExDataCount: Integer;
  ExData: TExDataRecord;
begin
  Index := ReadersCount - 1;
  if Index >= 0 then
  begin
    ExData.Index := ItemIndex;
    ExData.Tag := Tag;
    ExData.Stream := TMemoryStream.Create;
    ExData.Stream.CopyFrom(Data, 0);
    ExDataCount := Length(FReaderList[Index].ExDataRecords);
    SetLength(FReaderList[Index].ExDataRecords, ExDataCount + 1);
    FReaderList[Index].ExDataRecords[ExDataCount] := ExData;
  end;
end;

//
//  ���������� ���������� ��������� �������
// =============================================================================
function TFWZipModifier.ReadersCount: Integer;
begin
  Result := Length(FReaderList);
end;

end.
