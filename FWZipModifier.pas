////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : FWZipModifier
//  * Purpose   : Класс для модификации созданного ранее ZIP архива
//  * Author    : Александр (Rouse_) Багель
//  * Copyright : © Fangorn Wizards Lab 1998 - 2015.
//  * Version   : 1.0.11
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

unit FWZipModifier;

interface

uses
  Windows,
  Classes,
  SysUtils,
  FWZipConsts,
  FWZipReader,
  FWZipWriter,
  FWZipZLib;

type
  TReaderIndex = Integer;

  TFWZipModifierItem = class(TFWZipWriterItem)
  private
    FReaderIndex: TReaderIndex;     // индекс TFWZipReader в массиве TFWZipModifier.FReaderList
    FOriginalItemIndex: Integer;    // оригинальный индекс элемента в изначальном архиве
  protected
    property ReaderIndex: TReaderIndex read FReaderIndex write FReaderIndex;
    property OriginalItemIndex: Integer read FOriginalItemIndex write FOriginalItemIndex;
  public
    constructor Create(Owner: TFWZipWriter;
      const InitFilePath: string;
      InitAttributes: TWin32FileAttributeData;
      const InitFileName: string = ''); override;
  end;

  EFWZipModifier = class(Exception);

  TExDataRecord = record
    Index: Integer;
    Tag: Word;
    Stream: TMemoryStream;
  end;
  TExDataRecords = array of TExDataRecord;

  TReaderData = record
    Reader: TFWZipReader;
    ExDataRecords: TExDataRecords;
  end;

  TReaderList = array of TReaderData;

  TFWZipModifier = class(TFWZipWriter)
  private
    FReaderList: TReaderList;
    function CheckZipFileIndex(Value: TReaderIndex): TReaderIndex;
    function AddItemFromZip(AReader: TFWZipReader;
      ReaderIndex: TReaderIndex; ItemIndex: Integer): Integer;
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
    function AddZipFile(const FilePath: string; SFXOffset: Integer = -1;
      ZipEndOffset: Integer = -1): TReaderIndex; overload;
    function AddZipFile(FileStream: TStream; SFXOffset: Integer = -1;
      ZipEndOffset: Integer = -1): TReaderIndex; overload;
    function AddFromZip(ReaderIndex: TReaderIndex): Integer; overload;
    function AddFromZip(ReaderIndex: TReaderIndex; const ItemPath: string): Integer; overload;
    function AddFromZip(ReaderIndex: TReaderIndex; ItemsList: TStringList): Integer; overload;
  end;

implementation

type
  TFWZipReaderFriendly = class(TFWZipReader);
  TFWZipReaderItemFriendly = class(TFWZipReaderItem);

{ TFWZipModifierItem }

constructor TFWZipModifierItem.Create(Owner: TFWZipWriter;
  const InitFilePath: string; InitAttributes: TWin32FileAttributeData;
  const InitFileName: string);
begin
  inherited Create(Owner, InitFilePath, InitAttributes, InitFileName);
  FReaderIndex := -1;
  FOriginalItemIndex := -1;
end;

{ TFWZipModifier }

function TFWZipModifier.AddFromZip(ReaderIndex: TReaderIndex;
  const ItemPath: string): Integer;
var
  Reader: TFWZipReader;
begin
  CheckZipFileIndex(ReaderIndex);
  Reader := FReaderList[ReaderIndex].Reader;
  Result := AddItemFromZip(Reader, ReaderIndex, Reader.GetElementIndex(ItemPath));
end;

function TFWZipModifier.AddFromZip(ReaderIndex: TReaderIndex): Integer;
var
  I: Integer;
  Reader: TFWZipReader;
begin
  CheckZipFileIndex(ReaderIndex);
  Result := 0;
  Reader := FReaderList[ReaderIndex].Reader;
  for I := 0 to Reader.Count - 1 do
    if AddItemFromZip(Reader, ReaderIndex, I) >= 0 then
      Inc(Result);
end;

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
      Reader.GetElementIndex(ItemsList[I])) >= 0 then
      Inc(Result);
end;

function TFWZipModifier.AddItemFromZip(AReader: TFWZipReader;
  ReaderIndex: TReaderIndex; ItemIndex: Integer): Integer;
var
  OldItem: TFWZipReaderItemFriendly;
  NewItem: TFWZipModifierItem;
begin
  Result := -1;
  if ItemIndex < 0 then Exit;
  OldItem := TFWZipReaderItemFriendly(AReader.Item[ItemIndex]);
  NewItem := TFWZipModifierItem(
    GetItemClass.Create(Self, '', OldItem.Attributes, OldItem.FileName));
  NewItem.UseExternalData := True;
  NewItem.ReaderIndex := ReaderIndex;
  NewItem.OriginalItemIndex := ItemIndex;
  NewItem.Comment := OldItem.Comment;
  NewItem.NeedDescriptor :=
    OldItem.CentralDirFileHeader.GeneralPurposeBitFlag and PBF_DESCRIPTOR <> 0;
  NewItem.UseUTF8String :=
    OldItem.CentralDirFileHeader.GeneralPurposeBitFlag and PBF_UTF8 <> 0;
  Result := AddNewItem(NewItem);
end;

function TFWZipModifier.AddZipFile(FileStream: TStream; SFXOffset,
  ZipEndOffset: Integer): TReaderIndex;
var
  AReader: TFWZipReader;
begin
  Result := Length(FReaderList);
  SetLength(FReaderList, Result + 1);
  AReader := TFWZipReader.Create;
  AReader.OnLoadExData := OnLoadExData;
  AReader.LoadFromStream(FileStream, SFXOffset, ZipEndOffset);
  FReaderList[Result].Reader := AReader;
end;

function TFWZipModifier.AddZipFile(const FilePath: string;
  SFXOffset, ZipEndOffset: Integer): TReaderIndex;
var
  AReader: TFWZipReader;
begin
  Result := Length(FReaderList);
  SetLength(FReaderList, Result + 1);
  AReader := TFWZipReader.Create;
  AReader.OnLoadExData := OnLoadExData;
  AReader.LoadFromFile(FilePath, SFXOffset, ZipEndOffset);
  FReaderList[Result].Reader := AReader;
end;

function TFWZipModifier.CheckZipFileIndex(Value: TReaderIndex): TReaderIndex;
begin
  Result := Value;
  if (Value < 0) or (Value >= Length(FReaderList)) then
    raise EFWZipModifier.CreateFmt('Invalid index value (%d).', [Value]);
end;

procedure TFWZipModifier.CompressItem(CurrentItem: TFWZipWriterItem;
  Index: Integer; StreamSizeBeforeCompress: Int64; Stream: TStream);
var
  OldItem: TFWZipReaderItemFriendly;
  NewItem: TFWZipModifierItem;
  Reader: TFWZipReaderFriendly;
  Offset: Int64;
begin
  NewItem := TFWZipModifierItem(CurrentItem);
  if not NewItem.UseExternalData then
  begin
    inherited;
    Exit;
  end;
  Reader := TFWZipReaderFriendly(FReaderList[NewItem.ReaderIndex].Reader);
  OldItem := TFWZipReaderItemFriendly(Reader.Item[NewItem.OriginalItemIndex]);
  Offset := OldItem.CentralDirFileHeader.RelativeOffsetOfLocalHeader;
  Inc(Offset, SizeOf(TLocalFileHeader));
  Inc(Offset, OldItem.CentralDirFileHeader.FilenameLength);
  if OldItem.CentralDirFileHeaderEx.UncompressedSize >= MAXDWORD then
    Inc(Offset, SizeOf(TExDataInfo64));
  Reader.ZIPStream.Position := Offset;
  Stream.CopyFrom(Reader.ZIPStream, OldItem.CentralDirFileHeaderEx.CompressedSize);
end;

destructor TFWZipModifier.Destroy;
var
  I, A: Integer;
begin
  for I := 0 to Length(FReaderList) - 1 do
  begin
    FReaderList[I].Reader.Free;
    for A := 0 to Length(FReaderList[I].ExDataRecords) - 1 do
      FReaderList[I].ExDataRecords[A].Stream.Free;
  end;
  inherited;
end;

procedure TFWZipModifier.FillExData(Stream: TStream; Index: Integer);
var
  NewItem: TFWZipModifierItem;
  ReaderIndex: TReaderIndex;
  I: Integer;
  ExDataSize: Word;
  ExDataRecord: TExDataRecord;
begin
  NewItem := TFWZipModifierItem(Item[Index]);
  if not NewItem.UseExternalData then
  begin
    inherited;
    Exit;
  end;
  ReaderIndex := CheckZipFileIndex(NewItem.ReaderIndex);
  for I := 0 to Length(FReaderList[ReaderIndex].ExDataRecords) - 1 do
    if FReaderList[ReaderIndex].ExDataRecords[I].Index = NewItem.OriginalItemIndex then
    begin
      ExDataRecord := FReaderList[ReaderIndex].ExDataRecords[I];
      Stream.WriteBuffer(ExDataRecord.Tag, 2);
      ExDataSize := ExDataRecord.Stream.Size;
      Stream.WriteBuffer(ExDataSize, 2);
      Stream.CopyFrom(ExDataRecord.Stream, 0);
    end;
end;

procedure TFWZipModifier.FillItemCDFHeader(CurrentItem: TFWZipWriterItem;
  var Value: TCentralDirectoryFileHeaderEx);
var
  OldItem: TFWZipReaderItemFriendly;
  NewItem: TFWZipModifierItem;
  Reader: TFWZipReader;
begin
  NewItem := TFWZipModifierItem(CurrentItem);
  if not NewItem.UseExternalData then
  begin
    inherited;
    Exit;
  end;
  Reader := FReaderList[NewItem.ReaderIndex].Reader;
  OldItem := TFWZipReaderItemFriendly(Reader.Item[NewItem.OriginalItemIndex]);
  Value := OldItem.CentralDirFileHeaderEx;
end;

function TFWZipModifier.GetItemClass: TFWZipWriterItemClass;
begin
  Result := TFWZipModifierItem;
end;

procedure TFWZipModifier.OnLoadExData(Sender: TObject; ItemIndex: Integer;
  Tag: Word; Data: TStream);
var
  ReaderCount, ExDataCount: Integer;
  ExData: TExDataRecord;
begin
  ExData.Index := ItemIndex;
  ExData.Tag := Tag;
  ExData.Stream := TMemoryStream.Create;
  ExData.Stream.CopyFrom(Data, 0);
  ReaderCount := Length(FReaderList);
  ExDataCount := Length(FReaderList[ReaderCount - 1].ExDataRecords);
  SetLength(FReaderList[ReaderCount - 1].ExDataRecords, ExDataCount + 1);
  FReaderList[ReaderCount - 1].ExDataRecords[ExDataCount] := ExData;
end;

end.
