////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : FWZipConsts
//  * Purpose   : Типы и константы используемые для работы с ZIP архивами
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

unit FWZipConsts;

interface

uses
  Windows,
  SysUtils,
  Classes;

type

{
  IV. General Format of a .ZIP file
  ---------------------------------

  Files stored in arbitrary order.  Large .ZIP files can span multiple
  diskette media or be split into user-defined segment sizes.  [The
  minimum user-defined segment size for a split .ZIP file is 64K.
  (removed by PKWare 2003-06-01)]

  Overall .ZIP file format:

    [local file header 1]
    [file data 1]
    [data descriptor 1]
    .
    .
    .
    [local file header n]
    [file data n]
    [data descriptor n]
    [archive decryption header] (EFS)
    [archive extra data record] (EFS)
    [central directory]
    [zip64 end of central directory record]
    [zip64 end of central directory locator]
    [end of central directory record]
  }

  PLocalFileHeader = ^TLocalFileHeader;
  TLocalFileHeader = packed record
    LocalFileHeaderSignature: Cardinal; // (0x04034b50)
    VersionNeededToExtract,
    GeneralPurposeBitFlag,
    CompressionMethod,
    LastModFileTimeTime,
    LastModFileTimeDate: Word;
    Crc32,
    CompressedSize,
    UncompressedSize: Cardinal;
    FilenameLength,
    ExtraFieldLength: Word;
    // file name (variable size)
    // extra field (variable size)
  end;

  {
    If bit 3 of the general purpose bit flag
    is set, these fields are set to zero in the local header
    and the correct values are put in the data descriptor and
    in the central directory.
  }

  TDataDescriptor = packed record
    DescriptorSignature,        // (0x08074b50)
    Crc32,
    CompressedSize,
    UncompressedSize: Cardinal;
    {For Zip64 format archives, the compressed
    and uncompressed sizes are 8 bytes each. ??!!}
  end;

  TEFS = packed record
    ArchiveExtraDataSignature,   // (0x08064b50)
    ExtraFieldLength: Cardinal;
    // extra field data                (variable size)
  end;

  {
  F.  Central directory structure:

      [file header 1]
      .
      .
      .
      [file header n]
      [digital signature]
  }

  TCentralDirectoryFileHeader = packed record
    CentralFileHeaderSignature: Cardinal;   // (0x02014b50)
    VersionMadeBy,
    VersionNeededToExtract,
    GeneralPurposeBitFlag,
    CompressionMethod,
    LastModFileTimeTime,
    LastModFileTimeDate: Word;
    Crc32,
    CompressedSize,
    UncompressedSize: Cardinal;
    FilenameLength,
    ExtraFieldLength,
    FileCommentLength,
    DiskNumberStart,
    InternalFileAttributes: Word;
    ExternalFileAttributes,
    RelativeOffsetOfLocalHeader: Cardinal;
    // file name (variable size)
    // extra field (variable size)
    // file comment (variable size)
  end;

  TCentralDirectoryFileHeaderEx = packed record
    Header: TCentralDirectoryFileHeader;
    UncompressedSize,
    CompressedSize,
    RelativeOffsetOfLocalHeader,
    DataOffset: Int64;
    DiskNumberStart: Integer;
    FileName,
    FileComment: string;
    Attributes: TWin32FileAttributeData;
    ExceptOnWrite: Boolean;
  end;

  TNTFSFileTime = packed record
    Mtime: TFileTime;
    Atime: TFileTime;
    Ctime: TFileTime;
  end;

  TExDataHeaderAndSize = packed record
    Header: Word;
    Size: Word;
  end;

  TExDataNTFS = packed record
    HS: TExDataHeaderAndSize;
    Reserved: Cardinal;
    Tag: Word;
    RecordSize: Word;
    Data: TNTFSFileTime;
  end;

  TCentralDirectoryDigitalSignature = packed record
    HeaderSignature: Cardinal; // (0x05054b50)
    SizeOfData: Word;
    // signature data (variable size)
  end;

  TZip64EOFCentralDirectoryRecord = packed record
    Zip64EndOfCentralDirSignature: Cardinal; // (0x06064b50)
    SizeOfZip64EOFCentralDirectoryRecord: int64;
    VersionMadeBy,
    VersionNeededToExtract: Word;
    Number1,           // number of this disk
    Number2: Cardinal; // number of the disk with the start of the central directory
    TotalNumber1,      // total number of entries in the central directory on this disk
    TotalNumber2,      // total number of entries in the central directory
    Size,              // size of the central directory
    Offset: Int64;     // offset of start of central directory with respect to the starting disk number
    // zip64 extensible data sector    (variable size)
  end;

  TZip64EOFCentralDirectoryLocator = packed record
    Signature, // zip64 end of central dir locator signature  (0x07064b50)
    NumberOfTheDisk: Cardinal; //  number of the disk with the start of the zip64 end of central directory
    RelativeOffset: Int64; // relative offset of the zip64 end of central directory record
    TotalNumberOfDisks: Cardinal;
  end;

  TEndOfCentralDir = packed record
    EndOfCentralDirSignature: Cardinal; // (0x06054b50)
    NumberOfThisDisk,
    NumberOfTheDiskWithTheStart,
    TotalNumberOfEntriesOnThisDisk,
    TotalNumberOfEntries: Word;
    SizeOfTheCentralDirectory,
    OffsetOfStartOfCentralDirectory: Cardinal;
    ZipfileCommentLength: Word;
    // .ZIP file comment       (variable size)
  end;

const
  LOCAL_FILE_HEADER_SIGNATURE = $04034B50;
  DATA_DESCRIPTOR_SIGNATURE = $08074B50;
  EXTRA_DATA_SIGNATURE = $08064B50;
  CENTRAL_FILE_HEADER_SIGNATURE = $02014B50;
  CENTRAL_DIRECTORY_DIGITAL_SIGNATURE = $05054B50;
  ZIP64_END_OF_CENTRAL_DIR_SIGNATURE = $06064B50;
  ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIGNATURE = $07064B50;
  END_OF_CENTRAL_DIR_SIGNATURE = $06054B50;

  ZIP_SLASH = '/';

  // Флаги для GeneralPurposeBitFlag
  PBF_CRYPTED = 1;

  // (For Methods 8 and 9 - Deflating)
  PBF_COMPRESS_NORMAL = 0;
  PBF_COMPRESS_MAXIMUM = 2;
  PBF_COMPRESS_FAST = 4;
  PBF_COMPRESS_SUPERFAST = 6;

  PBF_DESCRIPTOR = 8;
  PBF_STRONG_CRYPT = 64;

  PBF_UTF8 = $800;

  // константы поддерживаемых полей ExData
  SUPPORTED_EXDATA_ZIP64 = 1;
  SUPPORTED_EXDATA_NTFSTIME = 10;

  defaultWindowBits = -15;

type
  TProgressState = (
    psStart,            // начало распаковки элемента, результирующий файл еще не создан
    psInitialization,   // результирующий файл создан и залочен, производится подготовка к распаковке
    psInProgress,       // идет распаковка
    psFinalization,     // распаковка завершена, сейчас будут разрушены все служебные объекты, результирующий файл все еще залочен
    psEnd,              // операция распаковки полностью завершена, результирующий файл доступен на чтение/запись
    psException         // ошибка
    );

  TZipProgressEvent = procedure(Sender: TObject; const FileName: string;
    Percent, TotalPercent: Byte; var Cancel: Boolean; ProgressState: TProgressState) of object;
  TZipExtractItemEvent = procedure(Sender: TObject; const FileName: string;
    Extracted, TotalSize: Int64; ProgressState: TProgressState) of object;
  TZipNeedPasswordEvent = procedure(Sender: TObject; const FileName: string;
    var Password: string; var CancelExtract: Boolean) of object;
  TZipSaveExDataEvent = procedure(Sender: TObject; ItemIndex: Integer;
    UserExDataBlockCount: Integer; var Tag: Word; Data: TStream) of object;
  TZipLoadExDataEvent = procedure(Sender: TObject; ItemIndex: Integer;
    Tag: Word; Data: TStream) of object;

  // Типы поведения TFWZipWriter при ошибке в процессе создания архива
  TExceptionAction =
  (
    eaRetry,                // повторить попытку
    eaSkip,                 // пропустить текущий элемент
    eaAbort,                // остановить создание архива
    eaUseNewFilePath,       // использовать новый путь к файлу (пар. NewFilePath)
    eaUseNewFilePathAndDel, // то-же что и acUseNewFilePath, только файл удаляется после использования
    eaUseNewFileData        // использовать содержимое файла из стрима (пар. NewFileData)
  );

  TZipBuildExceptionEvent = procedure(Sender: TObject;
    E: Exception; const ItemIndex: Integer;
    var Action: TExceptionAction;
    var NewFilePath: string; NewFileData: TMemoryStream) of object;

  TZipExtractExceptionEvent = procedure(Sender: TObject;
    E: Exception; const ItemIndex: Integer;
    var Handled: Boolean) of object;

  // Типы поведения TFWZipReader при конфликте имен файлов
  TDuplicateAction =
  (
    daSkip,                // пропустить файл
    daOverwrite,           // перезаписать
    daUseNewFilePath,      // сохранить с новым именем
    daAbort                // отменить распаковку
  );

  TZipDuplicateEvent = procedure(Sender: TObject;
    var Path: string; var Action: TDuplicateAction) of object;

const
  CRC32Table: array[Byte] of Cardinal =
    (
      $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F,
      $E963A535, $9E6495A3, $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
      $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91, $1DB71064, $6AB020F2,
      $F3B97148, $84BE41DE, $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
      $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9,
      $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
      $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B, $35B5A8FA, $42B2986C,
      $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
      $26D930AC, $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423,
      $CFBA9599, $B8BDA50F, $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
      $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D, $76DC4190, $01DB7106,
      $98D220BC, $EFD5102A, $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
      $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $086D3D2D,
      $91646C97, $E6635C01, $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
      $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457, $65B0D9C6, $12B7E950,
      $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
      $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7,
      $A4D1C46D, $D3D6F4FB, $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
      $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9, $5005713C, $270241AA,
      $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F,
      $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81,
      $B7BD5C3B, $C0BA6CAD, $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
      $EAD54739, $9DD277AF, $04DB2615, $73DC1683, $E3630B12, $94643B84,
      $0D6D6A3E, $7A6A5AA8, $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
      $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB,
      $196C3671, $6E6B06E7, $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
      $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5, $D6D6A3E8, $A1D1937E,
      $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
      $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55,
      $316E8EEF, $4669BE79, $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
      $CC0C7795, $BB0B4703, $220216B9, $5505262F, $C5BA3BBE, $B2BD0B28,
      $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
      $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9, $EB0E363F,
      $72076785, $05005713, $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
      $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21, $86D3D2D4, $F1D4E242,
      $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
      $88085AE6, $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69,
      $616BFFD3, $166CCF45, $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
      $A7672661, $D06016F7, $4969474D, $3E6E77DB, $AED16A4A, $D9D65ADC,
      $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
      $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693,
      $54DE5729, $23D967BF,$B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
      $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D
    );

  CurrentVersionMadeBy = 63;

  function IsAttributesPresent(Value: TWin32FileAttributeData): Boolean;
  function FileSizeToInt64(FileSizeLo, FileSizeHi: DWORD): Int64;
  function PathCanonicalize(Value: string): string;
  function MakeUniqueName(const Value: string): string;
  function FileSizeToStr(Value: Int64): string;

implementation

  function PathCanonicalizeA(lpszDes, lpszSrc: PAnsiChar): BOOL; stdcall; external 'shlwapi.dll';
  function PathCanonicalizeW(lpszDes, lpszSrc: PWideChar): BOOL; stdcall; external 'shlwapi.dll';
  function PathMakeUniqueName(pszUniqueName: PWideChar; cchMax: UINT;
    pszTemplate, pszLongPlate, pszDir: PWideChar): BOOL; stdcall; external 'shell32.dll';

function IsAttributesPresent(Value: TWin32FileAttributeData): Boolean;
begin
  Result := (Value.ftCreationTime.dwLowDateTime <> 0) and
    (Value.ftCreationTime.dwHighDateTime <> 0);
end;

function FileSizeToInt64(FileSizeLo, FileSizeHi: DWORD): Int64;
begin
  Result := FileSizeHi;
  Result := Result shl 32;
  Inc(Result, FileSizeLo);
end;

function PathCanonicalize(Value: string): string;
begin
  if Value = '' then
  begin
    Result := '';
    Exit;
  end;
  if Value[1] = '.' then
    Value := IncludeTrailingPathDelimiter(GetCurrentDir) + Value;
  SetLength(Result, MAX_PATH);
  {$IFDEF UNICODE}
  PathCanonicalizeW(PWideChar(Result), PWideChar(Value));
  {$ELSE}
  PathCanonicalizeA(PAnsiChar(Result), PAnsiChar(Value));
  {$ENDIF}
  Result := PChar(Result);
end;

function MakeUniqueName(const Value: string): string;
{$IFDEF UNICODE}
var
  FilePath, FileName: string;
begin
  Result := Value;
  FilePath := ExtractFilePath(Value);
  FileName := ExtractFileName(Value);
  SetLength(Result, MAX_PATH);
  if PathMakeUniqueName(PWideChar(Result), MAX_PATH,
    nil, PWideChar(FileName), PWideChar(FilePath)) then
    Result := PWideChar(Result);
{$ELSE}
var
  UnicodeResult, FilePath, FileName: WideString;
begin
  Result := Value;
  FilePath := WideString(ExtractFilePath(Value));
  FileName := WideString(ExtractFileName(Value));
  SetLength(UnicodeResult, MAX_PATH);
  if PathMakeUniqueName(PWideChar(UnicodeResult), MAX_PATH,
    nil, PWideChar(FileName), PWideChar(FilePath)) then
    Result := AnsiString(PWideChar(UnicodeResult));
{$ENDIF}
end;

function FileSizeToStr(Value: Int64): string;
begin
  if Value < 1024 then
  begin
    Result := Format('%d байт', [Value]);
    Exit;
  end;
  Value := Value div 1024;
  if Value < 1024 then
  begin
    Result := Format('%d килобайт', [Value]);
    Exit;
  end;
  Value := Value div 1024;
  if Value < 1024 then
  begin
    Result := Format('%d мегабайт', [Value]);
    Exit;
  end;
  Value := Value div 1024;
  if Value < 1024 then
  begin
    Result := Format('%d гигабайт', [Value]);
    Exit;
  end;
  // ну а чем бог не шутит? :)
  Value := Value div 1024;
  Result := Format('%d терабайт', [Value]);
end;

end.
