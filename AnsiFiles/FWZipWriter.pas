////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : FWZipWriter
//  * Purpose   : ����� ��� �������� ZIP ������
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

unit FWZipWriter;

interface

{$I fwzip.inc}

uses
  Windows,
  SysUtils,
  Classes,
  Contnrs,
  Masks,
  FWZipConsts,
  FWZipCrc32,
  FWZipCrypt,
  FWZipStream,
  FWZipZLib;

type
  TFWZipWriter = class;

  TFWZipWriterItem = class
  private
    FOwner: TFWZipWriter;
    FComment: string;                // ���������� � ��������
    FFilePath: string;               // ���� � �������� ����� (��� ��. TFWZipWriterItemEx.Data)
    FFileName: string;               // ��� �������� � ������ - �� ����� ���� ������
    FPassword: string;               // ������
    FCmpLevel: TCompressionLevel;    // ������� ������
    FNeedDescriptor: Boolean;        // ���� ���������� ������ �������� ������
                                     // � ��������, (���������� ��� ��������� ���������)
    FSize: Int64;
    FData: TStream;                  // ������ �������� � ������ ���� ����
                                     // ����������� �� �����
    FAttributes:                     // ������� ��������� �����
      TWin32FileAttributeData;
    FTag: Integer;
    FUseUTF8String: Boolean;
    FUseExternalData: Boolean;       // ���� ����������� �� �� ��� ������ ����� ������������ �������
    procedure SetBool(const Value: Boolean);
    procedure SetCmpLevel(const Value: TCompressionLevel);
    procedure SetString(const Index: Integer; const Value: string);
  protected
    property Data: TStream read FData;
    property UseExternalData: Boolean read FUseExternalData write FUseExternalData;
  public
    constructor Create(Owner: TFWZipWriter;
      const InitFilePath: string;
      InitAttributes: TWin32FileAttributeData;
      const InitFileName: string = ''); virtual;
    destructor Destroy; override;
    procedure ChangeDataStream(Value: TStream;
      AOwnerShip: TStreamOwnership = soReference);
    procedure ChangeAttributes(Value: TWin32FileAttributeData);
    function IsFolder: Boolean;
    property Comment: string index 0 read FComment write SetString;
    property FilePath: string index 1 read FFilePath write SetString;
    property FileName: string index 2 read FFileName write SetString;
    property Password: string index 3 read FPassword write SetString;
    property CompressionLevel: TCompressionLevel read FCmpLevel write SetCmpLevel;
    property NeedDescriptor: Boolean read FNeedDescriptor write SetBool;
    property Size: Int64 read FSize;
    property Attributes: TWin32FileAttributeData read FAttributes;
    property Tag: Integer read FTag write FTag;
    property UseUTF8String: Boolean read FUseUTF8String write FUseUTF8String;
  end;

  TFWZipWriterItemClass = class of TFWZipWriterItem;

  // ��������� �������� ������
  TBuildZipResult = 
  (
    brDone,         // ����� ������ �������
    brFailed,       // ������ �������� ������
    brAborted,      // �������� ������ �������� �������������
    brPartialBuild  // ��������� �������� ��������� ��-�� ��������� ������
  );

  TFWZipWriter = class
  private
    FDefaultDescryptorState: Boolean;
    FDefaultCompressionLevel: TCompressionLevel;
    FDefaultPassword: string;
    FItems: TObjectList;
    FCD: array of TCentralDirectoryFileHeaderEx;
    FVersionToExtract: Word;
    FcdfhOffset, FTotalProgress, FTotalSizeCount,
      FTotalProcessedCount, FSizeOfCentralDir: Int64;
    FCompressedStream: TStream;
    FProcessedItemIndex, FcdfhDiskNumber, FcdfhRecordOnDisk: Integer;
    FComment: string;
    FOnProgress: TZipProgressEvent;
    FBuildState: Boolean;
    FSaveExData: TZipSaveExDataEvent;
    FExceptionCount: Integer;
    FBuidException: TZipBuildExceptionEvent;
    FTrimPackedStreamSize: Boolean;
    FAlwaysAddEmptyFolder: Boolean;
    FUseUTF8String: Boolean;
    function GetItem(Index: Integer): TFWZipWriterItem;
  protected
    function GetItemClass: TFWZipWriterItemClass; virtual;
    function AddNewItem(Value: TFWZipWriterItem): Integer;
    function IsMultiPartZip(AStream: TStream): Boolean;
    procedure FillItemCDFHeader(CurrentItem: TFWZipWriterItem;
      var Value: TCentralDirectoryFileHeaderEx); virtual;
    procedure CompressItem(CurrentItem: TFWZipWriterItem;
      Index: Integer; StreamSizeBeforeCompress: Int64; Stream: TStream); virtual;
    procedure FillExData(Stream: TStream; Index: Integer); virtual;
  protected
    procedure DoProgress(ProgressState: TProgressState);
    procedure CompressorOnProcess(Sender: TObject);
  protected
    function CheckFileNameSlashes(const Value: string): string;
    function CreateItemFromStream(const FileName: string;
      Value: TStream; AOwnerShip: TStreamOwnership): TFWZipWriterItem;
    function GetVersionToExtract(Index: Integer): Word;
    function GetCurrentFileTime: TFileTime;
    procedure SaveItemToStream(Stream: TStream; Index: Integer); virtual;
    procedure SaveCentralDirectory(Stream: TStream);
    procedure SaveEndOfCentralDirectory(Stream: TStream);
    procedure SaveString(Stream: TStream; const Value: string;
      UTF8String: Boolean);
    procedure UpdateLocalHeaders(Stream: TStream);
    property BuildState: Boolean read FBuildState;
    function StringLength(const Value: string; UTF8String: Boolean): Integer;
  public
    constructor Create; overload;
    constructor Create(CompressionLevel: TCompressionLevel); overload;
    constructor Create(UseDescryptors: Boolean;
      CompressionLevel: TCompressionLevel;
      const DefaultPassword: string); overload;
    destructor Destroy; override;
    function AddEmptyFolder(const FolderRelativeName: string): Integer; overload;
    function AddEmptyFolder(const FolderRelativeName: string;
      Attributes: TWin32FileAttributeData): Integer; overload;
    function AddFile(const FilePath: string;
      const FileName: string = ''): Integer; overload;
    function AddFile(const FilePath: string;
      Attributes: TWin32FileAttributeData;
      const FileName: string = ''): Integer; overload;
    function AddStream(const FileName: string; Value: TStream;
      AOwnerShip: TStreamOwnership = soReference): Integer;
    function AddFiles(Value: TStringList): Integer;
    function AddFilesAndFolders(Value: TStringList; SubFolders: Boolean = True): Integer;
    function AddFolder(const Path: string;
      SubFolders: Boolean = True): Integer; overload;
    function AddFolder(const RelativePath, Path, Mask: string;
      SubFolders: Boolean = True): Integer; overload;
    function BuildZip(const ZipFilePath: string): TBuildZipResult; overload;
    function BuildZip(Stream: TStream): TBuildZipResult; overload;
    function Count: Integer;
    procedure Clear;
    procedure DeleteItem(Index: Integer);
    function InsertStream(const FileName: string; Index: Integer;
      Value: TStream; AOwnerShip: TStreamOwnership = soReference): Integer;

    // �������� �������� �� ���������� ����� � ���� TFWZipWriterItem
    // ��������������� ����� ����������� ������ �� �������� �����
    property AlwaysAddEmptyFolder: Boolean read FAlwaysAddEmptyFolder
      write FAlwaysAddEmptyFolder;

    property Item[Index: Integer]: TFWZipWriterItem read GetItem; default;
    property Comment: string read FComment write FComment;

    // �������� �������� ������ ��� ���������� ��������� USE_AUTOGENERATED_ZLIB_HEADER
    // � ��������� ������� �� ������ �� ����� � ��������� ��� �������������
    property TrimPackedStreamSize: Boolean read FTrimPackedStreamSize
      write FTrimPackedStreamSize; // deprecated;

    property OnException: TZipBuildExceptionEvent read FBuidException write FBuidException;
    property OnProgress: TZipProgressEvent read FOnProgress write FOnProgress;
    property OnSaveExData: TZipSaveExDataEvent read FSaveExData write FSaveExData;
    property UseUTF8String: Boolean read FUseUTF8String write FUseUTF8String;
  end;

  EZipWriterItem = class(Exception);
  EZipWriter = class(Exception);
  EZipWriterWrite = class(Exception);

implementation

{ TFWZipWriterItem }

//
//  ��������� �������� ��������� �������� ������
// =============================================================================
procedure TFWZipWriterItem.ChangeAttributes(Value: TWin32FileAttributeData);
begin
  if not FOwner.BuildState then
    FAttributes := Value;
end;

//
//  ��������� �������� ���� ������ �� ��������. ������������� �������� ��� � �����.
//  ������ ��� ������ ����� ������� �� ���� FData
// =============================================================================
procedure TFWZipWriterItem.ChangeDataStream(Value: TStream;
  AOwnership: TStreamOwnership);
begin
  if not FOwner.BuildState then
    if Value.Size <> 0 then
    begin
      FreeAndNil(FData);
      if AOwnership = soOwned then
        FData := Value
      else
      begin
        FData := TMemoryStream.Create;
        FData.CopyFrom(Value, 0);
      end;
      FSize := FData.Size;
      FFilePath := '';
    end;
end;

//
//  ����������� ����������� ������
// =============================================================================
constructor TFWZipWriterItem.Create(Owner: TFWZipWriter;
  const InitFilePath: string;
  InitAttributes: TWin32FileAttributeData; const InitFileName: string);
begin
  inherited Create;
  FData := nil;
  FOwner := Owner;
  FFilePath := InitFilePath;
  FAttributes := InitAttributes;
  FSize :=
    FileSizeToInt64(FAttributes.nFileSizeLow, FAttributes.nFileSizeHigh);  
  FFileName := InitFileName;
  UseUTF8String := Owner.UseUTF8String;
end;

//
//  ����������� ���������� ������
// =============================================================================
destructor TFWZipWriterItem.Destroy;
begin
  FData.Free;
  inherited;
end;

//
//  ������� ��������� - �������� �� ������� ������?
// =============================================================================
function TFWZipWriterItem.IsFolder: Boolean;
begin
  Result := Attributes.dwFileAttributes and faDirectory <> 0;
  if not Result then
    Result := FileName[Length(FileName)] = ZIP_SLASH;
end;

//
//  ��������� �������� ���� ����������� ��������
// =============================================================================
procedure TFWZipWriterItem.SetBool(const Value: Boolean);
begin
  if not FOwner.BuildState then
    FNeedDescriptor := Value;
end;

//
//  ��������� �������� ������� ������ ��������
// =============================================================================
procedure TFWZipWriterItem.SetCmpLevel(const Value: TCompressionLevel);
begin
  if not FOwner.BuildState then
    FCmpLevel := Value;
end;

//
//  ��������� �������� ��������� �������� ��������.
//  ��� ��������� ���� � �����, ������������� ��������������
//  ����� ��������� ����� � ��������� ����� FData �� �������������
// =============================================================================
procedure TFWZipWriterItem.SetString(const Index: Integer;
  const Value: string);
var
  Attributes: TWin32FileAttributeData;
begin
  if not FOwner.BuildState then
    case Index of
      0: FComment := Value;
      1:
      begin
        if FileExists(Value) then
        begin
          // �������� ������ � ��� ������ ���� ������ ��������
          // � �� ������ ����� ��� ���������
          if GetFileAttributesEx(PChar(Value),
            GetFileExInfoStandard, @Attributes) then
          begin
            FAttributes := Attributes;
            FSize :=
              FileSizeToInt64(Attributes.nFileSizeLow, Attributes.nFileSizeHigh);
            FFilePath := Value;
            FreeAndNil(FData);
          end;
        end;
      end;
      2:
      // Rouse_ 20.12.2019
      // ���� ����������� ������ � �������� ������
//        if Length(Value) >= MAX_PATH then
//          raise EZipWriterItem.Create('������� ������� ����.')
//        else
          FFileName := Value;
      3: FPassword := Value;
    end;
end;

{ TFWZipWriter }

//
//  ������� ��������� ��������� ���� � ������.
//  � �������� ���������� ���������� ������ �������� � ������.
//  ���������:
//  FilePath - ���� � �����
//  FileName - ������������ ����� � ������
//    (������� ������������� ���� �� ����� ������)
// =============================================================================
function TFWZipWriter.AddFile(const FilePath,
  FileName: string): Integer;
var
  Attributes: TWin32FileAttributeData;
  FullFilePath: string;
begin
  Result := -1;
  FullFilePath := PathCanonicalize(FilePath);
  // ��������� ������ � ��� ������ ���� ������ ��������
  // � �� ������ ����� ��� ���������
  if GetFileAttributesEx(PChar(FullFilePath),
    GetFileExInfoStandard, @Attributes) then
    Result := AddFile(FullFilePath, Attributes, FileName);
end;

//
//  ������� ��������� ������ ����� � ������.
//  � �������� ���������� ���������� ������ �������� � ������.
//  ���������:
//  FolderRelativeName - ������������ ����� � ������
//    (������� ������������� ���� �� ����� ������)
// =============================================================================
function TFWZipWriter.AddEmptyFolder(const FolderRelativeName: string): Integer;
var
  Attributes: TWin32FileAttributeData;
begin
  ZeroMemory(@Attributes, SizeOf(TWin32FileAttributeData));
  Attributes.dwFileAttributes := faDirectory;
  Result := AddEmptyFolder(FolderRelativeName, Attributes);
end;

//
//  ������� ��������� ������ ����� � ������.
//  � �������� ���������� ���������� ������ �������� � ������.
//  ���������:
//  FolderRelativeName - ������������ ����� � ������
//    (������� ������������� ���� �� ����� ������)
//  Attributes - ��������� �����
// =============================================================================
function TFWZipWriter.AddEmptyFolder(const FolderRelativeName: string;
  Attributes: TWin32FileAttributeData): Integer;
var
  FolderPath: string;
  Item: TFWZipWriterItem;
begin
  Result := -1;
  if FolderRelativeName = '' then Exit;
  FolderPath := CheckFileNameSlashes(FolderRelativeName);
  if FolderPath[Length(FolderPath)] <> ZIP_SLASH then
    FolderPath := FolderPath + ZIP_SLASH;
  Item := GetItemClass.Create(Self, FolderPath, Attributes, FolderPath);
  Result := AddNewItem(Item);
end;

//
//  ������� ��������� ��������� ���� � ������.
//  � �������� ���������� ���������� ������ �������� � ������.
//  ���������:
//  FilePath - ���� � �����
//  Attributes - ��������� �����
//  FileName - ������������ ����� � ������
//    (������� ������������� ���� �� ����� ������)
// =============================================================================
function TFWZipWriter.AddFile(const FilePath: string;
  Attributes: TWin32FileAttributeData; const FileName: string): Integer;
var
  Item: TFWZipWriterItem;
  InitFileName, FullFilePath: string;
begin
  // �������� ��� ��� ��������. ����� ��� ����?
  Result := -1;
  FullFilePath := PathCanonicalize(FilePath);
  if not FileExists(FullFilePath) then Exit;
  if FileName = '' then
    InitFileName := ExtractFileName(ExcludeTrailingPathDelimiter(FullFilePath))
  else
    InitFileName := CheckFileNameSlashes(FileName);

  Item := GetItemClass.Create(Self, FullFilePath, Attributes, InitFileName);
  Item.CompressionLevel := FDefaultCompressionLevel;
  Item.Password := FDefaultPassword;

  // � ������ ������� ����������� �� �����
  // ����������� ������ ����������� ����� �� ����, �.�. ��� ����������
  // ������ ���������� ��� �� ��������� � ��������� ��������� �������������
  Item.NeedDescriptor := FDefaultDescryptorState;

  Result := AddNewItem(Item);
end;

//
//  ������� ��������� ����� ������ � ������.
//  � �������� ���������� ���������� ���������� ������� ����������� ���������.
//  ���������:
//  Value - ���� � ������
//  Value.ValueFromIndex[I] �������� ������ ���� � �����
//  Value.Names[I] �������� ��� ����� � ������� �� �����
//  ������� � ����� (�������������� ��������)
// =============================================================================
function TFWZipWriter.AddFiles(Value: TStringList): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Value.Count - 1 do
    if AddFile(Value.ValueFromIndex[I], Value.Names[I]) >= 0 then
      Inc(Result);
end;

//
//  ������� ��������� ����� ������ � ����� � ������.
//  � �������� ���������� ���������� ���������� ������� ����������� ���������.
//  ���������:
//  Value - ���� � ������ � ������ �� ������� ����� ����������� ������
//    Value.ValueFromIndex[I] ��� ����� �������� ������ ���� � �����
//    Value.Names[I] ��� ����� �������� ���� ������������ ����� ������
//      (�������������� ��������)
//    Value.ValueFromIndex[I] ��� ����� �������� ������ ���� � �����
//    Value.Names[I] ��� ����� �������� ��� ����� � ������� �� �����
//      ������� � ����� (�������������� ��������)
//  SubFolders - ��������� ������ �� �������� ��� ���.
// =============================================================================
function TFWZipWriter.AddFilesAndFolders(Value: TStringList;
  SubFolders: Boolean): Integer;
var
  I: Integer;
  Path: string;
begin
  Result := 0;
  for I := 0 to Value.Count - 1 do
  begin
    Path := Value.ValueFromIndex[I];
    if DirectoryExists(Path) then
      Inc(Result, AddFolder(Value.Names[I], Path, '', SubFolders))
    else
      if AddFile(Path, Value.Names[I]) >= 0 then
        Inc(Result);
  end;
end;

//
//  ������� ��������� ����� �� �������� �����
//  � �������� ���������� ���������� ���������� ������� ����������� ���������.
//  ���������:
//  Path - ���� � ����� �� ������� ����� ����������� ������
//  SubFolders - ��������� ������ �� �������� ��� ���.
// =============================================================================
function TFWZipWriter.AddFolder(const Path: string; SubFolders: Boolean): Integer;
begin
  Result := AddFolder(ExtractFileName(
    ExcludeTrailingPathDelimiter(PathCanonicalize(Path))),
    Path, EmptyStr, SubFolders);
end;

//
//  ����������� ������� AddFolder
//  ������� ��������� ����� �� �������� �����
//  � �������� ���������� ���������� ���������� ������� ����������� ���������.
//  ���������:
//  RelativePath - ���� � �������� � ������ ������������ �����.
//  Path - ���� � ����� �� ������� ����� ����������� ������
//  Mask - ����� ������ ������ (����� ����� � �������). �������� "*.txt;*.bin;12???6.dat"
//  SubFolders - ��������� ������ �� �������� ��� ���.
// =============================================================================
function TFWZipWriter.AddFolder(const RelativePath, Path, Mask: string;
  SubFolders: Boolean): Integer;
const
  AllDataMask = '*.*';
var
  SR: TSearchRec;
  TrailingPath, TrailingRelativePath, ResultMask: string;
  Attributes: TWin32FileAttributeData;
  Masks: TStringList;
  I: Integer;
  Matched: Boolean;
begin
  Result := 0;
  // ������� ����������� ������������ �����
  // ������������ ����� - �������� RelativePath
  // � ������� ���������� ���� � ����� ��� ����� ������������ �������� �����
  if RelativePath = '' then
    TrailingRelativePath := ''
  else
    TrailingRelativePath := IncludeTrailingPathDelimiter(RelativePath);
  TrailingPath := IncludeTrailingPathDelimiter(PathCanonicalize(Path));

  Masks := nil;
  if Mask = EmptyStr then
    ResultMask := AllDataMask
  else
    ResultMask := AnsiLowerCase(Mask);

  if ResultMask <> AllDataMask then
  begin
    Masks := TStringList.Create;
    Masks.Delimiter := ';';
    Masks.DelimitedText := ResultMask;
  end;

  try
    if FindFirst(TrailingPath + AllDataMask, faAnyFile, SR) = 0 then
    try
      repeat

        {$WARN SYMBOL_PLATFORM OFF}
        Attributes.dwFileAttributes := SR.FindData.dwFileAttributes;
        Attributes.ftCreationTime := SR.FindData.ftCreationTime;
        Attributes.ftLastAccessTime := SR.FindData.ftLastAccessTime;
        Attributes.ftLastWriteTime := SR.FindData.ftLastWriteTime;
        Attributes.nFileSizeHigh := SR.FindData.nFileSizeHigh;
        Attributes.nFileSizeLow := SR.FindData.nFileSizeLow;
        {$WARN SYMBOL_PLATFORM ON}

        if SR.Name = '.' then
        begin
          // Rouse_ 14.02.2013
          // ���� �������� ���������� ������ �����, �� ��������� ������� ��������� ������
          if AlwaysAddEmptyFolder then
            AddEmptyFolder(TrailingRelativePath, Attributes);
          Continue;
        end;

        if SR.Name = '..' then Continue;

        if SR.Attr and faDirectory <> 0 then
        begin
          if SubFolders then
            Inc(Result, AddFolder(TrailingRelativePath + SR.Name,
              TrailingPath + SR.Name, ResultMask, SubFolders));
        end
        else
        begin
          if Assigned(Masks) then
          begin
            Matched := False;
            for I := 0 to Masks.Count - 1 do
              if MatchesMask(SR.Name, Masks[I]) then
              begin
                Matched := True;
                Break;
              end;
          end
          else
            Matched := True;

          if not Matched then Continue;

          if AddFile(TrailingPath + SR.Name, Attributes,
            TrailingRelativePath + SR.Name) >= 0 then
            Inc(Result);
        end;
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
  finally
    Masks.Free;
  end;
end;

//
//  ������� ��������� � ����� ������ �� ����������� ������.
//  � �������� ���������� ���������� ������ �������� � ������
// =============================================================================
function TFWZipWriter.AddStream(const FileName: string;
  Value: TStream; AOwnerShip: TStreamOwnership): Integer;
var
  Item: TFWZipWriterItem;
begin
  Item := CreateItemFromStream(FileName, Value, AOwnerShip);
  Result := AddNewItem(Item);
end;

//
//  ��������������� ������� ��� ������� � ����� �� �����������
// =============================================================================
function TFWZipWriter.AddNewItem(Value: TFWZipWriterItem): Integer;
begin
  Result := FItems.Add(Value);
end;

//
//  ��������� ��������� ����� � ��������� ��� � ��������� �����.
// =============================================================================
function TFWZipWriter.BuildZip(Stream: TStream): TBuildZipResult;
var
  I, TotalCount: Integer;
  BeforeExceptPosition: Int64;
  ExceptAction: TExceptionAction;
  OldPathName, NewFilePath: string;
  NewFileData: TMemoryStream;
  DeletePackedFile: Boolean;
begin
  FVersionToExtract := 0;
  FTotalProgress := 0;
  FBuildState := True;
  try
    Result := brFailed;
    if Count > 0 then
    begin

      // ��������� ������������ � ����� ������ ��� ������������ ������
      // ������ ����������� SPAN_DESCRIPTOR_SIGNATURE
      // ��. zip_format.txt 8.5.4
      if IsMultiPartZip(Stream) then
      begin
        I := SPAN_DESCRIPTOR_SIGNATURE;
        Stream.WriteBuffer(I, 4);
      end;

      // ���������� ������� CentralDirectory
      SetLength(FCD, Count);

      // ������������ ����� ������ ��������� ��� ����������� ���������
      FTotalSizeCount := 0;
      FTotalProcessedCount := 0;
      for I := 0 to Count - 1 do
      begin
        Inc(FTotalSizeCount, Item[I].Size);
        // Rouse_ 12.05.2020
        // �� �������� ������� ���� ������ � ���������� ������ ������
        FCD[I].ExceptOnWrite := False;
      end;

      // ������� ��� ����� ������ � �������� �� � ��������� �����
      // ��� ���� ������������� ����� ��� LocalHeader � DataDescryptor
      // �.�. ������ ������������� ����� ����� �������� ������ ����� �����
      // ��������� � �� ������� ���� �������� ��������:
      // � LocalHeader, DataDescryptor ��� � ZIP64 ���� ������ � CentralDirectory
      FExceptionCount := 0;
      BeforeExceptPosition := 0;
      TotalCount := 0;
      Result := brDone;
      I := 0;
      DeletePackedFile := False;
      OldPathName := '';
      while I < Count do      
      begin
        try

          BeforeExceptPosition := Stream.Position;
          SaveItemToStream(Stream, I);
          Inc(TotalCount);
          Inc(I);

          // � ������ ���� ��� ��������� ������� ������ � ��� ��������
          // �������� acUseNewFilePathAndDel, �� ���������� ������� ����.
          if DeletePackedFile then
          begin
            DeletePackedFile := False;
            NewFilePath := Item[I - 1].FilePath;
            SetFileAttributes(PChar(NewFilePath), FILE_ATTRIBUTE_NORMAL);
            DeleteFile(NewFilePath);
          end;

          // ���� ���� � ����� ��� �������� ������� ��-�� ���������� ����������
          // acUseNewFilePath ��� acUseNewFilePathAndDel,
          // ���������� ������������ ������ ��������
          if OldPathName <> '' then
          begin
            FBuildState := False;
            try
              Item[I - 1].FilePath := OldPathName;
            finally
              OldPathName := '';
              FBuildState := True;
            end;
          end;

        except

          // ���� ������������ ������� �������� ������, ������� �� �����
          on E: EAbort do
          begin
            Result := brAborted;
            Exit;
          end;

          on E: Exception do
          begin
            // ���������� ������� � ������ �� ������ �����
            Stream.Position := BeforeExceptPosition;

            // ���� ������� ����������� ��������� ������,
            // �� ������� ���� ������� ���������� � ����� ����� ������
            // ��� ������ � ���� ������
            Item[I].UseExternalData := False;

            // ����������� ������������, ��� ������ � �����������?         
            ExceptAction := eaSkip;
            NewFilePath := ''; 
            
            NewFileData := TMemoryStream.Create;
            try
            
              if Assigned(FBuidException) then
                FBuidException(Self, E, I, ExceptAction, 
                  NewFilePath, NewFileData);
                  
              // ������������ ����� �������������
              case ExceptAction of

                // ��������� �������
                eaRetry:
                  Continue;

                // ���������� �������
                eaSkip:
                begin
                  // �� �������� ������� ����������� ����������.
                  // �� �� ����� �������������� ��� �����������.
                  FCD[I].ExceptOnWrite := True;
                  // ����� �������� ����� ����������, ��� ���������� ������
                  // ���������� ��������� ������ � �������� ����������
                  Inc(FExceptionCount); 
                  Inc(I);
                  Result := brPartialBuild;                                
                end;

                // ���������� �������� ������
                eaAbort:
                begin
                  Result := brAborted;
                  Exit;                  
                end;

                // ������������ ������ �� ������� �����
                eaUseNewFilePath, eaUseNewFilePathAndDel:
                begin
                  // ���������� ������� ���� � �����,
                  // ��� ������������ ��������������
                  OldPathName := Item[I].FilePath;
                  FBuildState := False;
                  try
                    Item[I].FilePath := NewFilePath;
                  finally
                    FBuildState := True;
                  end;
                  // ���������� ����, ��� ��� ��������� ������,
                  // ���� ������ �������
                  DeletePackedFile := ExceptAction = eaUseNewFilePathAndDel;
                  Continue;
                end;
                
                // ������������ ������ �� ������
                eaUseNewFileData:
                begin
                  FBuildState := False;
                  try
                    Item[I].ChangeDataStream(NewFileData);                    
                  finally
                    FBuildState := True;
                  end;                
                  Continue;
                end;
              end;

            finally
              NewFileData.Free;
            end;                      
          end;
          
        end;
      end;

      // ���� � ����� �� �������� �� ���� �� ���������,
      // �� ���������� ������ ������ �� ������� �������� � ������� � �������
      if TotalCount = 0 then
      begin
        Stream.Size := Stream.Position;
        Result := brFailed;
        Exit;
      end;

      // ������ ������� ������ ������ ��������,
      // ��������� LocalHeader � DataDescryptor
      UpdateLocalHeaders(Stream);

      // ���������� CentralDirectory
      SaveCentralDirectory(Stream);

      // ����� ��������� EndOfCentralDirectory ��� ���� ��� �������������
      // ����������� � ������� ����� ��������� Zip64EOFCentralDirectoryRecord
      // � Zip64EOFCentralDirectoryLocator
      SaveEndOfCentralDirectory(Stream);
    end;
  finally
    FBuildState := False;
  end;
end;

//
//  ��������� ��������� ����� � ��������� ��� � ��������� ����.
// =============================================================================
function TFWZipWriter.BuildZip(const ZipFilePath: string): TBuildZipResult;
var
  ZIP: TFileStream;
begin
  Result := brFailed;
  if Count = 0 then Exit;
  ZIP := TFileStream.Create(ZipFilepath, fmCreate);
  try
    Result := BuildZip(ZIP);
    FlushFileBuffers(ZIP.Handle);
  finally
    ZIP.Free;
  end;
  if Result in [brFailed, brAborted] then
    DeleteFile(ZipFilePath);
end;

//
//  ������� �������� �������� ������������ ������������ ����� � ������
// =============================================================================
function TFWZipWriter.CheckFileNameSlashes(const Value: string): string;
begin
  {
        The name of the file, with optional relative path.
          The path stored should not contain a drive or
          device letter, or a leading slash.  All slashes
          should be forward slashes '/' as opposed to
          backwards slashes '\' for compatibility with Amiga
          and Unix file systems etc.
  }

  Result := StringReplace(Value, '\', ZIP_SLASH, [rfReplaceAll]);
end;

//
//  ������� ��� ����������� � ����� ��������
// =============================================================================
procedure TFWZipWriter.Clear;
begin
  FItems.Clear;
end;

//
//  ��������� ������ ������
// =============================================================================
procedure TFWZipWriter.CompressItem(CurrentItem: TFWZipWriterItem;
  Index: Integer; StreamSizeBeforeCompress: Int64; Stream: TStream);


  function CopyWithProgress(Src, Dst: TStream;
    Cryptor: TFWZipCryptor): Cardinal;
  var
    Buff: Pointer;
    Size, TotalSize: Integer;
  begin
    Result := $FFFFFFFF;
    GetMem(Buff, MAXWORD);
    try
      Src.Position := 0;
      FCompressedStream := Src;
      DoProgress(psInitialization);
      try
        TotalSize := 0;
        while True do
        begin
          Size := Src.Read(Buff^, MAXWORD);
          Result := CRC32Calc(Result, Buff, Size);
          if Size <> 0 then
          begin
            Inc(TotalSize, Size);
            if Cryptor <> nil then
              Cryptor.EncryptBuffer(Buff, Size);
            Dst.WriteBuffer(Buff^, Size);
            DoProgress(psInProgress);
          end
          else
            Break;
        end;
        if TotalSize <> Src.Size then
          raise EZipWriterWrite.CreateFmt(
            '������ ������ ������ �������� �%d "%s".', [Index, Item[Index].FileName]);
      except
        DoProgress(psException);
        raise;
      end;
      DoProgress(psFinalization);
    finally
      FreeMem(Buff);
    end;
    Result := Result xor $FFFFFFFF;
  end;

var
  F: TFileStream;
  Compressor: TZCompressionStream;
  Cryptor: TFWZipCryptor;
  ZipItemStream: TFWZipItemStream;
  CRC32Stream: TFWZipCRC32Stream;
  EncryptedHeaderStream: TMemoryStream;
begin
  Cryptor := nil;
  try
    EncryptedHeaderStream := TMemoryStream.Create;
    try
      if CurrentItem.Password <> '' then
      begin
        Cryptor := TFWZipCryptor.Create(AnsiString(CurrentItem.Password));
        Cryptor.GenerateEncryptionHeader(EncryptedHeaderStream,
          CurrentItem.NeedDescriptor,
          FCD[Index].Header.Crc32,
          FCD[Index].Header.LastModFileTimeTime +
          FCD[Index].Header.LastModFileTimeDate shl 16);
        // ����������� ����� ��� EncryptedHeaderStream
        Stream.Size := StreamSizeBeforeCompress + EncryptedHeaderSize;
        Stream.Position := Stream.Size;
      end;

      // ����� ��� ������ ����
      case FCD[Index].Header.CompressionMethod of
        Z_NO_COMPRESSION:
        begin
          if CurrentItem.Data <> nil then
            FCD[Index].Header.Crc32 :=
              CopyWithProgress(CurrentItem.Data, Stream, Cryptor)
          else
          begin
            try
              F := TFileStream.Create(CurrentItem.FilePath,
                fmOpenRead or fmShareDenyWrite);
              try
                FCD[Index].Header.Crc32 :=
                  CopyWithProgress(F, Stream, Cryptor)
              finally
                F.Free;
              end;
            except
              on E: Exception do
                raise EZipWriterWrite.CreateFmt(
                  '������ ������� � ������ �������� �%d "%s".' +
                  sLineBreak + E.ClassName + ': ' + E.Message,
                  [Index, CurrentItem.FileName]);
            end;
          end;
          // �������� ������ ������ ������
          // � ������ ���� �������������� ���������� � ������� ����� �����
          // ����� 12-�� ������� ��������� ������������� ����� �����������
          FCD[Index].CompressedSize := Stream.Size - StreamSizeBeforeCompress;
        end;
        Z_DEFLATED:
        begin
          {$IFDEF USE_AUTOGENERATED_ZLIB_HEADER}
          // ������� �������� �� ��� ����� �����,
          // ����� ������� �� ������ �������� ��� ��������� ZLib
          Stream.Position := Stream.Position - 2;
          {$ENDIF}
          if CurrentItem.Data <> nil then
          begin
            // ��������� ������ �� ����� � ������� ��� �������� ���������
            FCompressedStream := CurrentItem.Data;
            ZipItemStream := TFWZipItemStream.Create(Stream, Cryptor, nil,
              0, CurrentItem.Size);
            try
              Compressor := TZCompressionStream.Create(
                ZipItemStream, CurrentItem.CompressionLevel,
                defaultWindowBits, 8, zsDefault);
              try
                Compressor.OnProgress := CompressorOnProcess;
                DoProgress(psInitialization);
                try
                  CRC32Stream := TFWZipCRC32Stream.Create(CurrentItem.Data);
                  try
                    Compressor.CopyFrom(CRC32Stream, 0);
                    FCD[Index].Header.Crc32 := CRC32Stream.CRC32;
                  finally
                    CRC32Stream.Free;
                  end;
                except
                  DoProgress(psException);
                  raise;
                end;
                DoProgress(psFinalization);
              finally
                Compressor.Free;
              end;
            finally
              ZipItemStream.Free;
            end;
          end
          else
          begin
            // TFWZipItemStream ��������� ��� ��������� ����� ��������������
            // ������� � TCompressionStream.
            // ��� ������ ����������� ��� ���������� ����� ����� ������
            ZipItemStream := TFWZipItemStream.Create(Stream, Cryptor, nil,
              0, CurrentItem.Size);
            try
              Compressor := TZCompressionStream.Create(
                ZipItemStream, CurrentItem.CompressionLevel,
                defaultWindowBits, 8, zsDefault);
              try
                try
                  F := TFileStream.Create(CurrentItem.FilePath,
                    fmOpenRead or fmShareDenyWrite);
                  try
                    // ��������� ������ �� ����� � ������� ��� �������� ���������
                    FCompressedStream := F;
                    F.Position := 0;
                    Compressor.OnProgress := CompressorOnProcess;
                    // TFWZipCRC32Stream ��������� ��� ��������� �����
                    // ���������������� ������� � TCompressionStream,
                    // � ������� ���������� ������ ������.
                    // ��� ������ ��������� ��� ���������� ����� ����
                    // ����� ������ � ���������� �� ����������� �����
                    // �� ���� ��� ��� ����� �����
                    DoProgress(psInitialization);
                    try
                      CRC32Stream := TFWZipCRC32Stream.Create(F);
                      try
                        Compressor.CopyFrom(CRC32Stream, 0);
                        FCD[Index].Header.Crc32 := CRC32Stream.CRC32;
                      finally
                        CRC32Stream.Free;
                      end;
                    except
                      DoProgress(psException);
                      raise;
                    end;
                    DoProgress(psFinalization);
                  finally
                    F.Free;
                  end;
                except
                  on E: Exception do
                    raise EZipWriterWrite.CreateFmt(
                      '������ ������� � ������ �������� �%d "%s".' +
                      sLineBreak + E.ClassName + ': ' + E.Message,
                      [Index, CurrentItem.FileName]);
                end;
              finally
                Compressor.Free;
              end;
            finally
              ZipItemStream.Free;
            end;
          end;

          {$IFDEF USE_AUTOGENERATED_ZLIB_HEADER}
          // Rouse_ 14.02.2013
          // �� ���� ������, �� ������� ����� �����������,
          // ��� ������ ����������� ������ ������ ���� ������ �� 4 �����
          // ��� ������������� ����������������� ���������.
          // ���� ������ ����������� � ICSharpCode.SharpZipLibrary
          // ���������� � ����� ������ ���������� ���������
          if TrimPackedStreamSize then
            Stream.Size := Stream.Size - 4;
          {$ENDIF}

          // �������� ������ ������ ������
          // � ������ ���� �������������� ���������� � ������� ����� �����
          // ����� 12-�� ������� ��������� ������������� ����� �����������
          FCD[Index].CompressedSize := Stream.Size - StreamSizeBeforeCompress;

        end;
      end;

      // ���� ���� ����������,
      // ���������� ��������� ������������� ����� �����������
      if EncryptedHeaderStream.Size > 0 then
      begin
        Stream.Position := StreamSizeBeforeCompress;
        Stream.CopyFrom(EncryptedHeaderStream, 0);
      end;
    finally
      EncryptedHeaderStream.Free;
    end;

  finally
    Cryptor.Free;
  end;
end;

//
//  ��������� �������� ������� OnProcess
// =============================================================================
procedure TFWZipWriter.CompressorOnProcess(Sender: TObject);
begin
  DoProgress(psInProgress);
end;

//
//  ������� ���������� ���������� ����������� ��������� ������
// =============================================================================
function TFWZipWriter.Count: Integer;
begin
  Result := FItems.Count;
end;

//
//  ����������� ����������� ������
// =============================================================================
constructor TFWZipWriter.Create;
begin
  Create(False, clDefault, '');
end;

//
//  ����������� ����������� ������,
//  � ������� ����� ������� ������� ������,
//  ������������ ��� ���� ��������� �� ���������.
// =============================================================================
constructor TFWZipWriter.Create(CompressionLevel: TCompressionLevel);
begin
  Create(False, CompressionLevel, '');
end;

//
//  ����������� ����������� ������,
//  � ������� ����� �������� ��������� ��������� ��������� ������ �� ��������.
// =============================================================================
constructor TFWZipWriter.Create(UseDescryptors: Boolean;
  CompressionLevel: TCompressionLevel; const DefaultPassword: string);
begin
  inherited Create;
  FDefaultDescryptorState := UseDescryptors;
  FDefaultCompressionLevel := CompressionLevel;
  FDefaultPassword := DefaultPassword;
  FItems := TObjectList.Create;
  FItems.Capacity := 100000;
  FTrimPackedStreamSize := True;
end;

//
//  ������� ������� �� �� ��������� � ����� ������� ����� �������
// =============================================================================
function TFWZipWriter.CreateItemFromStream(const FileName: string;
  Value: TStream; AOwnerShip: TStreamOwnership): TFWZipWriterItem;
var
  Size: Int64;
  InitFileName: string;
  Attributes: TWin32FileAttributeData;
begin
  // �������� �� �����
  InitFileName := CheckFileNameSlashes(FileName);

  Size := Value.Size;
  ZeroMemory(@Attributes, SizeOf(TWin32FileAttributeData));
  Attributes.ftCreationTime := GetCurrentFileTime;
  Attributes.ftLastAccessTime := Attributes.ftCreationTime;
  Attributes.ftLastWriteTime := Attributes.ftCreationTime;
  Attributes.nFileSizeLow := Size and MAXDWORD;
  Attributes.nFileSizeHigh := Size shr 32;
  Result := GetItemClass.Create(Self, '', Attributes, InitFileName);
  Result.CompressionLevel := FDefaultCompressionLevel;
  Result.Password := FDefaultPassword;

  // � ������ ������� ����������� �� �����
  // ����������� ������ ����������� ����� �� ����, �.�. ��� ����������
  // ������ ���������� ��� �� ��������� � ��������� ��������� �������������
  Result.NeedDescriptor := FDefaultDescryptorState;

  Result.ChangeDataStream(Value, AOwnerShip);
end;

//
//  ��������� ������� �������� ������� ������
// =============================================================================
procedure TFWZipWriter.DeleteItem(Index: Integer);
begin
  FItems.Delete(Index);
end;

//
//  ����������� ���������� ������
// =============================================================================
destructor TFWZipWriter.Destroy;
begin
  FItems.Free;
  inherited;
end;

//
//  ����� �������� ������� � ��������� ������
// =============================================================================
procedure TFWZipWriter.DoProgress(ProgressState: TProgressState);
var
  CurrentProgress: Byte;
  Cancel: Boolean;
begin
  if Assigned(FOnProgress) then
  begin
    case ProgressState of
      psInProgress:
      begin
        if FCompressedStream.Size = 0 then
          CurrentProgress := 100
        else
          CurrentProgress :=
            Round(FCompressedStream.Position / (FCompressedStream.Size / 100));
        if FTotalSizeCount = 0 then
          FTotalProgress := 100
        else
          FTotalProgress :=
            Round((FTotalProcessedCount + FCompressedStream.Position) /
              (FTotalSizeCount / 100));
      end;
      psFinalization, psEnd: CurrentProgress := 100;
    else
      CurrentProgress := 0;
    end;
    Cancel := False;
    FOnProgress(Self, Item[FProcessedItemIndex].FileName,
      CurrentProgress, FTotalProgress, Cancel, ProgressState);
    if Cancel then Abort;
  end;
end;

//
//  ������������ ����� ������ � ������ UTF8
// =============================================================================
function TFWZipWriter.StringLength(const Value: string; UTF8String: Boolean): Integer;
begin
  if UTF8String then
    Result := Length(UTF8Encode(Value))
  else
    Result := Length(Value);
end;

//
//  ������� ���������� ������� ����� � ������� TFileTime
// =============================================================================
function TFWZipWriter.GetCurrentFileTime: TFileTime;
var
  SystemTime: TSystemTime;
begin
  DateTimeToSystemTime(Now, SystemTime);
  SystemTimeToFileTime(SystemTime, Result);
  FileTimeToLocalFileTime(Result, Result);
end;

//
//  ���������� �������� Items
// =============================================================================
function TFWZipWriter.GetItem(Index: Integer): TFWZipWriterItem;
begin
  Result := TFWZipWriterItem(FItems[Index]);
end;

//
//  ����������� ������ � ����������� ������ ������ ��� ������ ������
// =============================================================================
procedure TFWZipWriter.FillExData(Stream: TStream; Index: Integer);
var
  ExDataStream: TMemoryStream;
  EmptyExData: Boolean;
  UserExDataBlockCount, ExDataSize: Integer;
  ExDataHeaderTag: Word;
begin
  if Assigned(FSaveExData) then
  begin
    ExDataStream := TMemoryStream.Create;
    try
      EmptyExData := False;
      UserExDataBlockCount := 0;
      while not EmptyExData do
      begin
        ExDataHeaderTag := 0;
        ExDataStream.Clear;
        FSaveExData(Self, Index, UserExDataBlockCount,
          ExDataHeaderTag, ExDataStream);
        Inc(UserExDataBlockCount);
        EmptyExData := ExDataStream.Size = 0;
        if not EmptyExData then
        begin
          // Rouse_ 18.05.2020
          // ���� ������. ����� ������ ����� EXDATA ��������� MAXWORD
          // � �� ������ ������ � ���!
          if Stream.Size + ExDataStream.Size + 4 > MAXWORD then
            raise EZipWriter.Create(
              '������ ����� ����������� ������' +
              ' �� ����� ��������� 65535 ����.')
          else
            ExDataSize := ExDataStream.Size;
          if ExDataHeaderTag in
            [0, SUPPORTED_EXDATA_ZIP64, SUPPORTED_EXDATA_NTFSTIME] then
            raise EZipWriter.Create(
              '������ ������������ ����������������� ����' +
              ' ����� ����������� ������.');
          Stream.WriteBuffer(ExDataHeaderTag, 2);
          Stream.WriteBuffer(ExDataSize, 2);
          Stream.CopyFrom(ExDataStream, 0);
        end;
      end;
    finally
      ExDataStream.Free;
    end;
  end;
end;

//
//  �������������� CentralDirectoryFileHeader ��������
// =============================================================================
procedure TFWZipWriter.FillItemCDFHeader(CurrentItem: TFWZipWriterItem;
  var Value: TCentralDirectoryFileHeaderEx);
var
  SystemTime: TSystemTime;
  LastWriteTime: TFileTime;
  FileDate: Cardinal;
  Buff: array of Byte;
begin
  Value.Header.CentralFileHeaderSignature := CENTRAL_FILE_HEADER_SIGNATURE;
  Value.Header.VersionMadeBy := CurrentVersionMadeBy;
  Value.Header.VersionNeededToExtract := 0; // �������������� �������

  Value.Header.GeneralPurposeBitFlag := 0;
  if CurrentItem.Password <> '' then
    Value.Header.GeneralPurposeBitFlag :=
      Value.Header.GeneralPurposeBitFlag or PBF_CRYPTED;

  case CurrentItem.CompressionLevel of
    clNone:; // ������ ����� ���������� �� ��������������, ����� ������ �� Store
    clFastest:
      Value.Header.GeneralPurposeBitFlag :=
        Value.Header.GeneralPurposeBitFlag or PBF_COMPRESS_SUPERFAST;
    clDefault:
      Value.Header.GeneralPurposeBitFlag :=
        Value.Header.GeneralPurposeBitFlag or PBF_COMPRESS_NORMAL;
    clMax:
      Value.Header.GeneralPurposeBitFlag :=
        Value.Header.GeneralPurposeBitFlag or PBF_COMPRESS_MAXIMUM;
  end;
  if CurrentItem.NeedDescriptor then
    Value.Header.GeneralPurposeBitFlag :=
      Value.Header.GeneralPurposeBitFlag or PBF_DESCRIPTOR;

  if CurrentItem.CompressionLevel = clNone then
    Value.Header.CompressionMethod := Z_NO_COMPRESSION
  else
    Value.Header.CompressionMethod := Z_DEFLATED;

  if not CurrentItem.IsFolder then
    if not CurrentItem.NeedDescriptor then
      if CurrentItem.Password <> '' then
      begin
        // � ������ ���� ����������� ��������� � �������� ���������� ��������
        // �� ���������� ���������� ��� ����������� ����� �����
        // ���������� ��������� ������������� ����� ����������
        if CurrentItem.Data = nil then
          Value.Header.Crc32 := FileCRC32(CurrentItem.FilePath)
        else
        begin
          CurrentItem.Data.Position := 0;
          SetLength(Buff, CurrentItem.Data.Size);
          CurrentItem.Data.Read(Buff[0], CurrentItem.Data.Size);
          Value.Header.Crc32 :=
            CRC32Calc(@Buff[0], CurrentItem.Data.Size);
        end;
      end;
  Value.UncompressedSize := CurrentItem.Size;

  // Rouse_ 25.10.2013
  // ������ ��������� ������ ��������� ����������� �����������
  //FileTimeToSystemTime(CurrentItem.Attributes.ftLastWriteTime, SystemTyme);
  FileTimeToLocalFileTime(CurrentItem.Attributes.ftLastWriteTime, LastWriteTime);
  FileTimeToSystemTime(LastWriteTime, SystemTime);
  FileDate := DateTimeToFileDate(SystemTimeToDateTime(SystemTime));

  Value.Header.LastModFileTimeTime := FileDate and $FFFF;
  Value.Header.LastModFileTimeDate := FileDate shr 16;

  Value.Filename := CurrentItem.FileName;
  Value.Header.FilenameLength :=
    StringLength(CurrentItem.Filename, CurrentItem.UseUTF8String);
  Value.Header.ExtraFieldLength := 0;
  Value.FileComment := CurrentItem.Comment;
  Value.Header.FileCommentLength :=
    StringLength(CurrentItem.Comment, CurrentItem.UseUTF8String);

  Value.Header.InternalFileAttributes := 0;
  Value.Header.ExternalFileAttributes :=
    CurrentItem.Attributes.dwFileAttributes;
  Value.Attributes := CurrentItem.Attributes;

  if CurrentItem.UseUTF8String then
    Value.Header.GeneralPurposeBitFlag :=
      Value.Header.GeneralPurposeBitFlag or PBF_UTF8;
end;

//
//  ��������� ����������� ��������� ����������� �� �������� ������
// =============================================================================
function TFWZipWriter.GetItemClass: TFWZipWriterItemClass;
begin
  Result := TFWZipWriterItem;
end;

//
//  ������� ������������ ����������� ������ ��� ����������
//  ���������� �������� ������
// =============================================================================
function TFWZipWriter.GetVersionToExtract(Index: Integer): Word;
begin
{
          Current minimum feature versions are as defined below:

          1.0 - Default value
          1.1 - File is a volume label
          2.0 - File is a folder (directory)
          2.0 - File is compressed using Deflate compression
          2.0 - File is encrypted using traditional PKWARE encryption
          2.1 - File is compressed using Deflate64(tm)
          2.5 - File is compressed using PKWARE DCL Implode
          2.7 - File is a patch data set
          4.5 - File uses ZIP64 format extensions
          4.6 - File is compressed using BZIP2 compression*
          5.0 - File is encrypted using DES
          5.0 - File is encrypted using 3DES
          5.0 - File is encrypted using original RC2 encryption
          5.0 - File is encrypted using RC4 encryption
          5.1 - File is encrypted using AES encryption
          5.1 - File is encrypted using corrected RC2 encryption**
          5.2 - File is encrypted using corrected RC2-64 encryption**
          6.1 - File is encrypted using non-OAEP key wrapping***
          6.2 - Central directory encryption
}

  // TFWZIPWriter ������������ ��������� ���������� ���������:
  // 1. ������������� ���������� (������ ��� ���������� - 2.0)
  // 2. ������������� ZIP64 ���������� (������ ��� ���������� - 4.5)

  // ��� �����������, ����� �� ��� ������������ ZIP64 ���������� ���������
  // ��������� ���������:
  // ������ ������� �������� ������ ������� � �� �������,
  // ������ �� ������ ����� ������ ��� ������� ��������
  // ���� ����� �� ���� �������� ������� �� �������� MAXDWORD,
  // ��� ���������� ��������� ������ ������� �� �������� MAXWORD,
  // ��� ���������� ��������� ZIP64

  Result := 20;
  if (FCD[Index].UncompressedSize >= MAXDWORD) or
    (FCD[Index].CompressedSize >= MAXDWORD) or
    (FCD[Index].RelativeOffsetOfLocalHeader >= MAXDWORD) or
    (FCD[Index].DiskNumberStart >= MAXWORD) then
    Result := 45;
end;

//
//  ������� ��������� � ����� ������ �� ����������� ������.
//  � �������� ���������� ���������� ������ �������� � ������
// =============================================================================
function TFWZipWriter.InsertStream(const FileName: string; Index: Integer;
  Value: TStream; AOwnerShip: TStreamOwnership): Integer;
var
  Item: TFWZipWriterItem;
begin
  Item := CreateItemFromStream(FileName, Value, AOwnerShip);
  FItems.Insert(Index, Item);
  Result := Index;
end;

//
//  ������� ��� ������������ ������ ������ � ������������ ��������
// =============================================================================
function TFWZipWriter.IsMultiPartZip(AStream: TStream): Boolean;
begin
  Result := AStream is TFWAbstractMultiStream;
end;

//
//  ��������� �������� ���������� ������ CentralDirectory
// =============================================================================
procedure TFWZipWriter.SaveCentralDirectory(Stream: TStream);
var
  I, DiskNumber: Integer;
  RealtiveOffset, VolumeSize: Int64;
  ExDataHeader: TExDataHeaderAndSize;
  ExDataNTFS: TExDataNTFS;
  ZIP64Data: TMemoryStream;
  TotalExDataStream: TMemoryStream;
  FirstDisk: Boolean;
begin
  FirstDisk := True;
  FcdfhRecordOnDisk := 0;
  FSizeOfCentralDir := FcdfhOffset;
  if IsMultiPartZip(Stream) then
    TFWAbstractMultiStream(Stream).GetRelativeInfo(FcdfhDiskNumber, FcdfhOffset)
  else
    FcdfhDiskNumber := 0;

  ZeroMemory(@ExDataNTFS, SizeOf(TExDataNTFS));
  for I := 0 to Count - 1 do
  begin

    // ���������� �������� ��� ������ ������� ��������� ����������
    if FCD[I].ExceptOnWrite then Continue;

    // ����� ������� ������� �������� CentralDirectory
    // ���������� ����������� ������� � ������������ �������
    // � ������� �� ������
    ZIP64Data := TMemoryStream.Create;
    try
      // ������������� ������ � ZIP64 �������

      {
          The order of the fields in the ZIP64 extended
          information record is fixed, but the fields will
          only appear if the corresponding Local or Central
          directory record field is set to 0xFFFF or 0xFFFFFFFF.
      }

      if FCD[I].UncompressedSize >= MAXDWORD then
        ZIP64Data.WriteBuffer(FCD[I].UncompressedSize, 8);
      if FCD[I].CompressedSize >= MAXDWORD then
        ZIP64Data.WriteBuffer(FCD[I].CompressedSize, 8);
      if FCD[I].RelativeOffsetOfLocalHeader >= MAXDWORD then
        ZIP64Data.WriteBuffer(FCD[I].RelativeOffsetOfLocalHeader, 8);
      if FCD[I].DiskNumberStart >= MAXWORD then
        ZIP64Data.WriteBuffer(FCD[I].DiskNumberStart, 4);

      ZeroMemory(@ExDataNTFS, SizeOf(TExDataHeaderAndSize));
      if IsAttributesPresent(FCD[I].Attributes) then
      begin
        // �������������� ������ � NTFS ��������
        FCD[I].Header.ExtraFieldLength := SizeOf(TExDataNTFS);

        //   (NTFS)  0x000a        Short       Tag for this "extra" block type
        ExDataNTFS.HS.Header := SUPPORTED_EXDATA_NTFSTIME;
        {
          In the current implementations, this field has
          a fixed total data size of 32 bytes and is only stored as local
          extra field
        }
        ExDataNTFS.HS.Size := 32;
        // Reserved      Long        for future use
        ExDataNTFS.Reserved := 0;
        // Tag1          Short       NTFS attribute tag value #1
        ExDataNTFS.Tag := 1;
        //Size1      2 bytes    Size of attribute #1, in bytes (24)
        ExDataNTFS.RecordSize := 24;
        ExDataNTFS.Data.Mtime := FCD[I].Attributes.ftLastWriteTime;
        ExDataNTFS.Data.Atime := FCD[I].Attributes.ftLastAccessTime;
        ExDataNTFS.Data.Ctime := FCD[I].Attributes.ftCreationTime;
      end;
      if ZIP64Data.Size > 0 then
        Inc(FCD[I].Header.ExtraFieldLength,
          ZIP64Data.Size + SizeOf(TExDataHeaderAndSize));

      TotalExDataStream := TMemoryStream.Create;
      try
        // ����������� ����� ExData �� ������������
        FillExData(TotalExDataStream, I);

        // ������ ����� ������ ����������� ������
        Inc(FCD[I].Header.ExtraFieldLength, TotalExDataStream.Size);

        // ��� MultiPart ������ ���� ������� ����� CentralDirectoryFileHeader
        // ������� ������� � ��� � ������ �������� ����� ������ ���� �����
        if IsMultiPartZip(Stream) then
        begin
          VolumeSize := TFWAbstractMultiStream(Stream).GetWriteVolumeSize;
          TFWAbstractMultiStream(Stream).GetRelativeInfo(
            DiskNumber, RealtiveOffset);
          if VolumeSize < RealtiveOffset + SizeOf(TCentralDirectoryFileHeader) +
            FCD[I].Header.FilenameLength + FCD[I].Header.FileCommentLength +
            FCD[I].Header.ExtraFieldLength then
          begin
            TFWAbstractMultiStream(Stream).StartNewVolume;
            if FcdfhRecordOnDisk = 0 then
            begin
              Inc(FcdfhDiskNumber);
              FcdfhOffset := 0;
            end
            else
              FirstDisk := False;
          end;
        end;

        // ����� ��������� TCentralDirectoryFileHeader, ����������� �������
        Stream.WriteBuffer(FCD[I].Header, SizeOf(TCentralDirectoryFileHeader));

        // ����� ������������ ��������
        SaveString(Stream, FCD[I].FileName,
          FCD[I].Header.GeneralPurposeBitFlag and PBF_UTF8 = PBF_UTF8);

        // ���� ����� -  ��� ���������� � ������� ZIP64
        if ZIP64Data.Size > 0 then
        begin
          ExDataHeader.Header := SUPPORTED_EXDATA_ZIP64;
          ExDataHeader.Size := ZIP64Data.Size;
          Stream.WriteBuffer(ExDataHeader, SizeOf(TExDataHeaderAndSize));
          Stream.CopyFrom(ZIP64Data, 0);
        end;

        // ����� ���������� � NTFSTime
        if ExDataNTFS.HS.Header = SUPPORTED_EXDATA_NTFSTIME then
          Stream.WriteBuffer(ExDataNTFS, SizeOf(TExDataNTFS));

        // � ����������� ���������� ���������� �� ������������
        if TotalExDataStream.Size > 0 then
          Stream.CopyFrom(TotalExDataStream, 0);

      finally
         TotalExDataStream.Free;
      end;

      // � ����������, ����� ���������� � ��������
      SaveString(Stream, FCD[I].FileComment,
        FCD[I].Header.GeneralPurposeBitFlag and PBF_UTF8 = PBF_UTF8);

      if FirstDisk then
        Inc(FcdfhRecordOnDisk);

    finally
      ZIP64Data.Free;
    end;
  end;
  FSizeOfCentralDir := Stream.Position - FSizeOfCentralDir;
end;

//
//  ��������� �������� ���������� ������ EndOfCentralDirectory
// =============================================================================
procedure TFWZipWriter.SaveEndOfCentralDirectory(Stream: TStream);
var
  eo64cd: TZip64EOFCentralDirectoryRecord;
  locator: TZip64EOFCentralDirectoryLocator;
  eocd: TEndOfCentralDir;
  eo64cdOffset: Int64;
  DiskNumber: Integer;
  VolumeSize: Int64;
begin
  eocd.ZipfileCommentLength := StringLength(FComment, False);
  if IsMultiPartZip(Stream) then
  begin
    VolumeSize := TFWAbstractMultiStream(Stream).GetWriteVolumeSize;
    TFWAbstractMultiStream(Stream).GetRelativeInfo(DiskNumber, eo64cdOffset);
    if VolumeSize < eo64cdOffset + SizeOf(TZip64EOFCentralDirectoryRecord) +
      SizeOf(TZip64EOFCentralDirectoryLocator) + SizeOf(TEndOfCentralDir) +
      eocd.ZipfileCommentLength then
    begin
      TFWAbstractMultiStream(Stream).StartNewVolume;
      Inc(DiskNumber);
      eo64cdOffset := 0;
    end;
    locator.TotalNumberOfDisks := TFWAbstractMultiStream(Stream).GetDiskCount;
  end
  else
  begin
    DiskNumber := 0;
    eo64cdOffset := Stream.Position;
    locator.TotalNumberOfDisks := 1;
  end;

  // ���������� ������� � ���������� ����
  eo64cd.TotalNumberOfEntriesOnThisDisk := FcdfhRecordOnDisk;
  // ��������� �� ������ ���������� ��������� ������ ���������� ����������
  eo64cd.TotalNumberOfEntries := Count - FExceptionCount;

  // ������ ZIP64 ������������ � ������ ���� ���������� ���������
  // ������ ��������� MAXWORD, ��� �������� �� ������ ����������� ����������
  // ��������� MAXDWORD ��� �� ������ ��������� MAXDWORD
  if (FcdfhOffset > MAXDWORD) or (FSizeOfCentralDir > MAXDWORD) or
    (eo64cd.TotalNumberOfEntriesOnThisDisk > MAXWORD) or
    (eo64cd.TotalNumberOfEntries > MAXWORD) or (DiskNumber > MAXWORD) or
    (FcdfhDiskNumber > MAXWORD) then
  begin
    // � ������ ������������� ������� ZIP64
    // ���������� �������� �������������� ���������

    // TZip64EOFCentralDirectoryRecord
    eo64cd.Zip64EndOfCentralDirSignature := ZIP64_END_OF_CENTRAL_DIR_SIGNATURE;


    // Rouse_ 20.07.2013
    // � ������������ 6.3.0 �� September 29, 2006 ���������
    {
        The value stored into the "size of zip64 end of central
        directory record" should be the size of the remaining
        record and should not include the leading 12 bytes.

        Size = SizeOfFixedFields + SizeOfVariableData - 12.
    }
    // FWZip �������������� �� ������ ����� ������ ������������
    // (���������� �� 6.0 ����� �� 6.2) � �� �������� ���� ������

    // ������� ������
    // oe64cd.SizeOfZip64EOFCentralDirectoryRecord :=
    //   SizeOf(TZip64EOFCentralDirectoryRecord);
    // �����:

    eo64cd.SizeOfZip64EOFCentralDirectoryRecord :=
      SizeOf(TZip64EOFCentralDirectoryRecord) - 12;


    eo64cd.VersionMadeBy := CurrentVersionMadeBy;
    eo64cd.VersionNeededToExtract := FVersionToExtract;
    eo64cd.NumberOfThisDisk := DiskNumber;
    eo64cd.DiskNumberStart := FcdfhDiskNumber;
    eo64cd.SizeOfTheCentralDirectory := FSizeOfCentralDir;
    eo64cd.RelativeOffsetOfCentralDirectory := FcdfhOffset;
    Stream.WriteBuffer(eo64cd, SizeOf(TZip64EOFCentralDirectoryRecord));

    // TZip64EOFCentralDirectoryLocator
    locator.Signature := ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIGNATURE;
    locator.DiskNumberStart := DiskNumber;
    locator.RelativeOffset := eo64cdOffset;
    Stream.WriteBuffer(locator, SizeOf(TZip64EOFCentralDirectoryLocator));

  end;
  eocd.EndOfCentralDirSignature := END_OF_CENTRAL_DIR_SIGNATURE;
  if DiskNumber > MAXWORD then
    eocd.NumberOfThisDisk := MAXWORD
  else
    eocd.NumberOfThisDisk := DiskNumber;
  if FcdfhDiskNumber > MAXWORD then
    eocd.DiskNumberStart := MAXWORD
  else
    eocd.DiskNumberStart := FcdfhDiskNumber;
  if eo64cd.TotalNumberOfEntriesOnThisDisk > MAXWORD then
    eocd.TotalNumberOfEntriesOnThisDisk := MAXWORD
  else
    eocd.TotalNumberOfEntriesOnThisDisk := eo64cd.TotalNumberOfEntriesOnThisDisk;
  if eo64cd.TotalNumberOfEntries > MAXWORD then
    eocd.TotalNumberOfEntries := MAXWORD
  else
    eocd.TotalNumberOfEntries := eo64cd.TotalNumberOfEntries;
  if FSizeOfCentralDir > MAXDWORD then
    eocd.SizeOfTheCentralDirectory := MAXDWORD
  else
    eocd.SizeOfTheCentralDirectory := FSizeOfCentralDir;
  if FcdfhOffset > MAXDWORD then
    eocd.RelativeOffsetOfCentralDirectory := MAXDWORD
  else
    eocd.RelativeOffsetOfCentralDirectory := FcdfhOffset;
  Stream.WriteBuffer(eocd, SizeOf(TEndOfCentralDir));
  if eocd.ZipfileCommentLength > 0 then
    SaveString(Stream, FComment, False);
end;

//
//  ��������� �������� ��� ��������� ����������, ������ � ���������� ���������� �������� ������
// =============================================================================
procedure TFWZipWriter.SaveItemToStream(Stream: TStream; Index: Integer);
var
  CurrentItem: TFWZipWriterItem;
  FileNameOffset, StreamSizeBeforeCompress, RealtiveOffset: Int64;
  DiskNumber: Integer;
begin
  CurrentItem := Item[Index];

  // �������� �� ������������������
  if not CurrentItem.UseExternalData then
    if (CurrentItem.FilePath = '') and (CurrentItem.Data = nil) then
      raise EZipWriter.CreateFmt('������ �������� �%d "%s" �����������',
        [Index, CurrentItem.FileName]);

  FProcessedItemIndex := Index;

  // Rouse_ 25.10.2013
  // ���������� ������� ������ ���������� ����� ��� ��� �������������� ���� ����� �������
  DoProgress(psStart);
  try

    // ��������� ���������� � CentralDirectory
    // ===========================================================================
    FillItemCDFHeader(CurrentItem, FCD[Index]);

    if IsMultiPartZip(Stream) then
    begin
      TFWAbstractMultiStream(Stream).GetRelativeInfo(DiskNumber, RealtiveOffset);
      FCD[Index].DiskNumberStart := DiskNumber;
      FCD[Index].RelativeOffsetOfLocalHeader := RealtiveOffset;
    end
    else
    begin
      FCD[Index].DiskNumberStart := 0;
      FCD[Index].RelativeOffsetOfLocalHeader := Stream.Position;
    end;

    // �������� ������ � �������������� ����
    // ===========================================================================

    // ���������� ������ �� �������� ���������� ����� ������ ��� �����
    FileNameOffset := Stream.Position + SizeOf(TLocalFileHeader);

    // ������������ ������ �������������� ����� ���
    // LocalFileHeader � ��� �����
    StreamSizeBeforeCompress := FileNameOffset +
      FCD[Index].Header.FilenameLength;

    // ����������� ����� ��� ZIP64
    if FCD[Index].UncompressedSize >= MAXDWORD then
      Inc(StreamSizeBeforeCompress, SizeOf(TExDataInfo64));

    // �������� ���� ������ ��� LocalFileHeader � ��� �����
    Stream.Size := StreamSizeBeforeCompress;
    Stream.Position := Stream.Size;

    // ������� ������
    if not CurrentItem.IsFolder then
      CompressItem(CurrentItem, Index, StreamSizeBeforeCompress, Stream);

    Inc(FTotalProcessedCount, CurrentItem.Size);

    // ����� ��� �����
    Stream.Position := FileNameOffset;
    SaveString(Stream, FCD[Index].Filename, CurrentItem.UseUTF8String);

    // ����������� ����� ��� ����������
    if CurrentItem.NeedDescriptor then
      Stream.Size := Stream.Size + SizeOf(TDataDescriptor);

    Stream.Position := Stream.Size;

  finally
    // Rouse_ 25.10.2013
    // �������������� ���� ����������, ���������� �������
    DoProgress(psEnd);
  end;
end;

//
//  ��������� �������� �������������� ���������� ������ � OEM � �� ����������
// =============================================================================
procedure TFWZipWriter.SaveString(Stream: TStream; const Value: string;
  UTF8String: Boolean);
var
  OemString: AnsiString;
begin
  if Value <> '' then
  begin
    OemString := AnsiString(Value);
    UniqueString(OemString);
    if UTF8String then
      OemString := UTF8Encode(Value)
    else
      AnsiToOem(PAnsiChar(OemString), PAnsiChar(OemString));
    Stream.WriteBuffer(OemString[1], Length(OemString));
  end;
end;

//
//  ��������� ��������� ������ LocalFileHeader
// =============================================================================
procedure TFWZipWriter.UpdateLocalHeaders(Stream: TStream);
var
  I: Integer;
  lfh: TLocalFileHeader;
  dd: TDataDescriptor;
  UseDescriptor: Boolean;
  Info64: TExDataInfo64;
begin
  FcdfhOffset := Stream.Position;
  for I := 0 to Count - 1 do
  begin

    // ���������� �������� ��� ������ ������� ��������� ����������
    if FCD[I].ExceptOnWrite then Continue;

    // ���� �� ����� ��� ������ �������������� ��� LocalFileHeader
    // � DataDescriptor (���� ���������)
    lfh.LocalFileHeaderSignature := LOCAL_FILE_HEADER_SIGNATURE;

    // ������������ ������ ����������� ��� ���������� �������� ������
    lfh.VersionNeededToExtract := GetVersionToExtract(I);
    lfh.GeneralPurposeBitFlag := FCD[I].Header.GeneralPurposeBitFlag;
    UseDescriptor := lfh.GeneralPurposeBitFlag and PBF_DESCRIPTOR <> 0;
    lfh.CompressionMethod := FCD[I].Header.CompressionMethod;
    lfh.LastModFileTimeTime := FCD[I].Header.LastModFileTimeTime;
    lfh.LastModFileTimeDate := FCD[I].Header.LastModFileTimeDate;
    if UseDescriptor then
    begin
      dd.DescriptorSignature := DATA_DESCRIPTOR_SIGNATURE;
      // ���� � ��������� ������� ��� ��� ������������� ������������ 
      // ���� Crc32, CompressedSize � UncompressedSize ������ ���� �����������
      // � ����, �� ����������� ����������� ����� �� ������,
      // ������� ���������� �� :)
      lfh.Crc32 := FCD[I].Header.Crc32;
      dd.Crc32 := lfh.Crc32;
      if FCD[I].CompressedSize > MAXDWORD then
        dd.CompressedSize := MAXDWORD
      else
        dd.CompressedSize := FCD[I].CompressedSize;
      lfh.CompressedSize := dd.CompressedSize;
      if FCD[I].UncompressedSize > MAXDWORD then
        dd.UncompressedSize := MAXDWORD
      else
        dd.UncompressedSize := FCD[I].UncompressedSize;
      lfh.UncompressedSize := dd.UncompressedSize;
    end
    else
    begin
      lfh.Crc32 := FCD[I].Header.Crc32;
      if FCD[I].CompressedSize > MAXDWORD then
        lfh.CompressedSize := MAXDWORD
      else
        lfh.CompressedSize := FCD[I].CompressedSize;
      if FCD[I].UncompressedSize > MAXDWORD then
        lfh.UncompressedSize := MAXDWORD
      else
        lfh.UncompressedSize := FCD[I].UncompressedSize;
    end;
    lfh.FilenameLength := FCD[I].Header.FilenameLength;

    if (FCD[I].UncompressedSize >= MAXDWORD) or
      (FCD[I].CompressedSize >= MAXDWORD) then
      lfh.ExtraFieldLength := SizeOf(TExDataInfo64)
    else
      lfh.ExtraFieldLength := 0;

    if IsMultiPartZip(Stream) then
      TFWAbstractMultiStream(Stream).Seek(
        FCD[I].DiskNumberStart, FCD[I].RelativeOffsetOfLocalHeader)
    else
      Stream.Position := FCD[I].RelativeOffsetOfLocalHeader;

    Stream.WriteBuffer(lfh, SizeOf(TLocalFileHeader));

    // Rouse_ 20.03.2015
    // ����� ������ ��� ��������� ZIP64
    // � �� WinRar, WinZip � 7Zip �� ����� ������������� ����� �����
    // (�� ������� ������, ������ ��� �� ������ ��� ���������� �� CentralDirectory)
    if lfh.ExtraFieldLength > 0 then
    begin
      Stream.Position := Stream.Position + lfh.FilenameLength;
      Info64.HS.Header := SUPPORTED_EXDATA_ZIP64;
      Info64.HS.Size := SizeOf(TExDataInfo64) - SizeOf(TExDataHeaderAndSize);
      Info64.UncompressedSize := FCD[I].UncompressedSize;
      // ���� CompressedSize ������ MAXDWORD ��� ������ �� ���� �� ���������,
      // �� ����� ��� ���� ��� ����������������, ������� �������� ������ ��� � ����� ������
      // ��� ������ ��� ����������� �� ���������� ������� �������� - � ���� �� ����
      Info64.CompressedSize := FCD[I].CompressedSize;
      Stream.WriteBuffer(Info64, SizeOf(TExDataInfo64));
    end;

    if UseDescriptor then
    begin
      // ���������� ������� ����� ������� ����� ������
      if IsMultiPartZip(Stream) then
      begin
        TFWAbstractMultiStream(Stream).Seek(
          FCD[I].DiskNumberStart, FCD[I].RelativeOffsetOfLocalHeader);
        Stream.Seek(SizeOf(TLocalFileHeader) + lfh.FilenameLength +
          lfh.ExtraFieldLength + FCD[I].CompressedSize, soCurrent);
      end
      else
        Stream.Position := FCD[I].RelativeOffsetOfLocalHeader +
          SizeOf(TLocalFileHeader) + lfh.FilenameLength +
          lfh.ExtraFieldLength + FCD[I].CompressedSize;
      Stream.WriteBuffer(dd, SizeOf(TDataDescriptor));
    end;

    // ��������� ���������� � ������� CentralDirectoryFileHeader

    // Rouse_ 14.02.2013
    // ������� ������������ ������ ��� ���������� � CentralDirectoryFileHeader
    //FCD[I].Header.VersionNeededToExtract := FVersionToExtract;
    FCD[I].Header.VersionNeededToExtract := lfh.VersionNeededToExtract;

    if UseDescriptor then
      FCD[I].Header.CompressedSize := dd.CompressedSize
    else
      FCD[I].Header.CompressedSize := lfh.CompressedSize;
    if UseDescriptor then
      FCD[I].Header.UncompressedSize := dd.UncompressedSize
    else
      FCD[I].Header.UncompressedSize := lfh.UncompressedSize;
    if FCD[I].RelativeOffsetOfLocalHeader > MAXDWORD then
      FCD[I].Header.RelativeOffsetOfLocalHeader := MAXDWORD
    else
      FCD[I].Header.RelativeOffsetOfLocalHeader :=
        FCD[I].RelativeOffsetOfLocalHeader;
    if FCD[I].DiskNumberStart > MAXWORD then
      FCD[I].Header.DiskNumberStart := MAXWORD
    else
      FCD[I].Header.DiskNumberStart := FCD[I].DiskNumberStart;

    // Rouse_ 14.02.2013
    // ��������� ���������� ����� ������
    if FVersionToExtract < lfh.VersionNeededToExtract then
      FVersionToExtract := lfh.VersionNeededToExtract;
  end;
  Stream.Position := FcdfhOffset;
end;

end.
