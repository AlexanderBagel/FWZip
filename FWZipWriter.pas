////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : FWZipWriter
//  * Purpose   : Класс для создания ZIP архива
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

unit FWZipWriter;

interface

{$I fwzip.inc}

uses
  Windows,
  SysUtils,
  Classes,
  Contnrs,
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
    FComment: string;                // коментарий к элементу
    FFilePath: string;               // путь к внешнему файлу (или см. TFWZipWriterItemEx.Data)
    FFileName: string;               // имя элемента в архиве - не может быть пустым
    FPassword: string;               // пароль
    FCmpLevel: TCompressionLevel;    // уровень сжатия
    FNeedDescriptor: Boolean;        // флаг определяет способ хранения данных
                                     // о элементе, (дескриптор или локальный заголовок)
    FSize: Int64;
    FData: TMemoryStream;            // Данные элемента в случае если файл
                                     // отсутствует на диске
    FAttributes:                     // Внешние аттрибуты файла
      TWin32FileAttributeData;
    FTag: Integer;
    FUseUTF8String: Boolean;
    FUseExternalData: Boolean;       // Флаг указывающий на то что данные будут передаваться снаружи
    procedure SetBool(const Value: Boolean);
    procedure SetCmpLevel(const Value: TCompressionLevel);
    procedure SetString(const Index: Integer; const Value: string);
  protected
    property Data: TMemoryStream read FData;
    property UseExternalData: Boolean read FUseExternalData write FUseExternalData;
  public
    constructor Create(Owner: TFWZipWriter;
      const InitFilePath: string;
      InitAttributes: TWin32FileAttributeData;
      const InitFileName: string = ''); virtual;
    destructor Destroy; override;
    procedure ChangeDataStream(Value: TStream);
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

  // Результат создания архива
  TBuildZipResult = 
  (
    brDone,         // архив создан успешно
    brFailed,       // ошибка создания архива
    brAborted,      // создание архива отменено пользователем
    brPartialBuild  // некоторые элементы пропущены из-за возникших ошибок
  );

  TFWZipWriter = class
  private
    FDefaultDescryptorState: Boolean;
    FDefaultCompressionLevel: TCompressionLevel;
    FDefaultPassword: string;
    FItems: TObjectList;
    FCD: array of TCentralDirectoryFileHeaderEx;
    FVersionToExtract: Word;
    FcdfhOffset, FTotalProgress, FTotalSizeCount, FTotalProcessedCount: Int64;
    FCompressedStream: TStream;
    FProcessedItemIndex: Integer;
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
    function AddStream(const FileName: string; Value: TStream): Integer;
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

    // Свойство отвечает за добавление папки в виде TFWZipWriterItem
    // непосредственно перед добавлением данных из указаной папки
    property AlwaysAddEmptyFolder: Boolean read FAlwaysAddEmptyFolder
      write FAlwaysAddEmptyFolder;

    property Item[Index: Integer]: TFWZipWriterItem read GetItem; default;
    property Comment: string read FComment write FComment;

    // Свойство работает только при включенной директиве USE_AUTOGENERATED_ZLIB_HEADER
    // В остальных случаях не влияет на архив и оставлено для совместимости
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
//  Процедура изменяет аттрибуты элемента архива
// =============================================================================
procedure TFWZipWriterItem.ChangeAttributes(Value: TWin32FileAttributeData);
begin
  if not FOwner.BuildState then
    FAttributes := Value;
end;

//
//  Процедура изменяет блок данных об элементе. Автоматически чистится имя к файлу.
//  Данные при сжатии будут браться из поля FData
// =============================================================================
procedure TFWZipWriterItem.ChangeDataStream(Value: TStream);
begin
  if not FOwner.BuildState then
    if Value.Size <> 0 then
    begin
      if FData = nil then
        FData := TMemoryStream.Create;
      FData.Clear;
      FData.CopyFrom(Value, 0);
      FSize := FData.Size;
      FFilePath := '';
    end;
end;

//
//  Стандартный конструктор класса
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
//  Стандартный деструктор класса
// =============================================================================
destructor TFWZipWriterItem.Destroy;
begin
  FData.Free;
  inherited;
end;

//
//  Функция проверяет - является ли элемент папкой?
// =============================================================================
function TFWZipWriterItem.IsFolder: Boolean;
begin
  Result := Attributes.dwFileAttributes and faDirectory <> 0;
  if not Result then
    Result := FileName[Length(FileName)] = ZIP_SLASH;
end;

//
//  Процедура изменяет флаг дескриптора элемента
// =============================================================================
procedure TFWZipWriterItem.SetBool(const Value: Boolean);
begin
  if not FOwner.BuildState then
    FNeedDescriptor := Value;
end;

//
//  Процедура изменяет степень сжатия элемента
// =============================================================================
procedure TFWZipWriterItem.SetCmpLevel(const Value: TCompressionLevel);
begin
  if not FOwner.BuildState then
    FCmpLevel := Value;
end;

//
//  Процедура изменяет строковые свойства элемента.
//  При изменении пути к файлу, автоматически рассчитываются
//  новые аттрибуты файла и очишается стрим FData за ненадобностью
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
          // Изменяем только в том случае если объект доступен
          // и мы смогли снять его аттрибуты
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
        if Length(Value) >= MAX_PATH then
          raise EZipWriterItem.Create('Слишком длинный путь.')
        else
          FFileName := Value;
      3: FPassword := Value;
    end;
end;

{ TFWZipWriter }

//
//  Функция добавляет очередной файл в список.
//  В качестве результата возвращает индекс элемента в списке.
//  Параметры:
//  FilePath - путь к файлу
//  FileName - наименование файла в архиве
//    (включая относительный путь от корня архива)
// =============================================================================
function TFWZipWriter.AddFile(const FilePath,
  FileName: string): Integer;
var
  Attributes: TWin32FileAttributeData;
  FullFilePath: string;
begin
  Result := -1;
  FullFilePath := PathCanonicalize(FilePath);
  // Добавляем только в том случае если объект доступен
  // и мы смогли снять его аттрибуты
  if GetFileAttributesEx(PChar(FullFilePath),
    GetFileExInfoStandard, @Attributes) then
    Result := AddFile(FullFilePath, Attributes, FileName);
end;

//
//  Функция добавляет пустую папку в список.
//  В качестве результата возвращает индекс элемента в списке.
//  Параметры:
//  FolderRelativeName - наименование папки в архиве
//    (включая относительный путь от корня архива)
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
//  Функция добавляет пустую папку в список.
//  В качестве результата возвращает индекс элемента в списке.
//  Параметры:
//  FolderRelativeName - наименование папки в архиве
//    (включая относительный путь от корня архива)
//  Attributes - аттрибуты папки
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
//  Функция добавляет очередной файл в список.
//  В качестве результата возвращает индекс элемента в списке.
//  Параметры:
//  FilePath - путь к файлу
//  Attributes - аттрибуты файла
//  FileName - наименование файла в архиве
//    (включая относительный путь от корня архива)
// =============================================================================
function TFWZipWriter.AddFile(const FilePath: string;
  Attributes: TWin32FileAttributeData; const FileName: string): Integer;
var
  Item: TFWZipWriterItem;
  InitFileName, FullFilePath: string;
begin
  // Проверка что нам передали. папку или файл?
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

  // в случае наличия дескриптора мы можем
  // производить расчет контрольной суммы на лету, т.к. при включенном
  // режиме шифрования она не участвует в генерации заголовка инициализации
  Item.NeedDescriptor := FDefaultDescryptorState;

  Result := AddNewItem(Item);
end;

//
//  Функция добавляет набор файлов в список.
//  В качестве результата возвращает количество успешно добавленных элементов.
//  Параметры:
//  Value - пути к файлам
//  Value.ValueFromIndex[I] содержит полный путь к файлу
//  Value.Names[I] содержит имя файла с которым он будет
//  помещен в архив (необязательный параметр)
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
//  Функция добавляет набор файлов и папок в список.
//  В качестве результата возвращает количество успешно добавленных элементов.
//  Параметры:
//  Value - пути к файлам и папкам из которых будут добавляться данные
//    Value.ValueFromIndex[I] для папки содержит полный путь к папке
//    Value.Names[I] для папки содержит путь относительно корня архива
//      (необязательный параметр)
//    Value.ValueFromIndex[I] для файла содержит полный путь к файлу
//    Value.Names[I] для файла содержит имя файла с которым он будет
//      помещен в архив (необязательный параметр)
//  SubFolders - добавлять данные из подпапок или нет.
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
//  Функция добавляет файлы из указаной папки
//  В качестве результата возвращает количество успешно добавленных элементов.
//  Параметры:
//  Path - путь к папке из которой будут добавляться данные
//  SubFolders - добавлять данные из подпапок или нет.
// =============================================================================
function TFWZipWriter.AddFolder(const Path: string; SubFolders: Boolean): Integer;
begin
  Result := AddFolder(ExtractFileName(
    ExcludeTrailingPathDelimiter(PathCanonicalize(Path))),
    Path, '*.*', SubFolders);
end;

//
//  Расширенный вариант AddFolder
//  Функция добавляет файлы из указаной папки
//  В качестве результата возвращает количество успешно добавленных элементов.
//  Параметры:
//  RelativePath - путь к элементу в архиве относительно корня.
//  Path - путь к папке из которой будут добавляться данные
//  Mask - маска отбора файлов
//  SubFolders - добавлять данные из подпапок или нет.
// =============================================================================
function TFWZipWriter.AddFolder(const RelativePath, Path, Mask: string;
  SubFolders: Boolean): Integer;
var
  SR: TSearchRec;
  TrailingPath, TrailingRelativePath, ResultMask: string;
  Attributes: TWin32FileAttributeData;
begin
  Result := 0;
  // обычное рекурсивное сканирование папки
  // единственный нюанс - параметр RelativePath
  // в котором передается путь к файлу или папке относительно корневой папки
  if RelativePath = '' then
    TrailingRelativePath := ''
  else
    TrailingRelativePath := IncludeTrailingPathDelimiter(RelativePath);
  TrailingPath := IncludeTrailingPathDelimiter(PathCanonicalize(Path));

  if Mask = '' then
    ResultMask := '*.*'
  else
    ResultMask := Mask;
  if FindFirst(TrailingPath + ResultMask, faAnyFile, SR) = 0 then
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
        // Если включено добавление пустых папок, то добавляем сначала требуемую запись
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
        if AddFile(TrailingPath + SR.Name, Attributes,
          TrailingRelativePath + SR.Name) >= 0 then
          Inc(Result);
      end;
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

//
//  Функция добавляет в архив данные из переданного стрима.
//  В качестве результата возвращает индекс элемента в списке
// =============================================================================
function TFWZipWriter.AddStream(const FileName: string;
  Value: TStream): Integer;
var
  Size: Int64;
  InitFileName: string;
  Item: TFWZipWriterItem;
  Attributes: TWin32FileAttributeData;
begin
  // проверка на дубли
  InitFileName := CheckFileNameSlashes(FileName);

  Size := Value.Size;
  ZeroMemory(@Attributes, SizeOf(TWin32FileAttributeData));
  Attributes.ftCreationTime := GetCurrentFileTime;
  Attributes.ftLastAccessTime := Attributes.ftCreationTime;
  Attributes.ftLastWriteTime := Attributes.ftCreationTime;
  Attributes.nFileSizeLow := Size and MAXDWORD;
  Attributes.nFileSizeHigh := Size shr 32;
  Item := GetItemClass.Create(Self, '', Attributes, InitFileName);
  Item.CompressionLevel := FDefaultCompressionLevel;
  Item.Password := FDefaultPassword;

  // в случае наличия дескриптора мы можем
  // производить расчет контрольной суммы на лету, т.к. при включенном
  // режиме шифрования она не участвует в генерации заголовка инициализации
  Item.NeedDescriptor := FDefaultDescryptorState;

  Item.ChangeDataStream(Value);
  Result := AddNewItem(Item);
end;

//
//  Вспомогательная функция для доступа к листу из наследников
// =============================================================================
function TFWZipWriter.AddNewItem(Value: TFWZipWriterItem): Integer;
begin
  Result := FItems.Add(Value);
end;

//
//  Процедура формирует архив и сохраняет его в указанный стрим.
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
      // Выставляем размеры CentralDirectory
      SetLength(FCD, Count);

      // Рассчитываем общий размер элементов для отображения прогресса
      FTotalSizeCount := 0;
      FTotalProcessedCount := 0;
      for I := 0 to Count - 1 do
        Inc(FTotalSizeCount, Item[I].Size);

      // Сжимаем все файлы архива и помещаем их в финальный стрим
      // при этом резервируется место под LocalHeader и DataDescryptor
      // т.к. размер запакованного файла будет известен только после этапа
      // архивации и не понятно куда помещать значение:
      // в LocalHeader, DataDescryptor или в ZIP64 блок данных в CentralDirectory
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

          // в случае если это повторная попытка сжатия и был применен
          // аттрибут acUseNewFilePathAndDel, то необходимо удалить файл.
          if DeletePackedFile then
          begin
            DeletePackedFile := False;
            NewFilePath := Item[I - 1].FilePath;
            SetFileAttributes(PChar(NewFilePath), FILE_ATTRIBUTE_NORMAL);
            DeleteFile(NewFilePath);
          end;

          // если путь к файлу был временно изменен из-за применения аттрибутов
          // acUseNewFilePath или acUseNewFilePathAndDel,
          // необходимо восстановить старое значение
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

          // Если пользователь отменил создание архива, выходим из цикла
          on E: EAbort do
          begin
            Result := brAborted;
            Exit;
          end;

          on E: Exception do
          begin
            // возвращаем позицию в стриме на старое место
            Stream.Position := BeforeExceptPosition;

            // если элемент использовал кастомные данные,
            // то снимаем этот вариант заполнения и пусть извне решают
            // что делать в этом случае
            Item[I].UseExternalData := False;

            // запрашиваем пользователя, что делать с исключением?         
            ExceptAction := eaSkip;
            NewFilePath := ''; 
            
            NewFileData := TMemoryStream.Create;
            try
            
              if Assigned(FBuidException) then
                FBuidException(Self, E, I, ExceptAction, 
                  NewFilePath, NewFileData);
                  
              // обрабатываем выбор польтзователя
              case ExceptAction of

                // повторить попытку
                eaRetry:
                  Continue;

                // пропустить элемент
                eaSkip:
                begin
                  // то помечаем элемент центральной директории.
                  // Он не будет обрабатываться при сохранениии.
                  FCD[I].ExceptOnWrite := True;
                  // Также увеличим число исключений, для правильной записи
                  // количества элементов архива в корневой директории
                  Inc(FExceptionCount); 
                  Inc(I);
                  Result := brPartialBuild;                                
                end;

                // остановить создание архива
                eaAbort:
                begin
                  Result := brAborted;
                  Exit;                  
                end;

                // использовать данные из другого файла
                eaUseNewFilePath, eaUseNewFilePathAndDel:
                begin
                  // запоминаем текущий путь к файлу,
                  // для последующего восстановления
                  OldPathName := Item[I].FilePath;
                  FBuildState := False;
                  try
                    Item[I].FilePath := NewFilePath;
                  finally
                    FBuildState := True;
                  end;
                  // выставляем флаг, что при завершени сжатия,
                  // файл слудет удалить
                  DeletePackedFile := ExceptAction = eaUseNewFilePathAndDel;
                  Continue;
                end;
                
                // использовать данные из стрима
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

      // Если в архив не добавлен ни один из элементов,
      // то возвращаем размер стрима на прежнее значение и выходим с ошибкой
      if TotalCount = 0 then
      begin
        Stream.Size := Stream.Position;
        Result := brFailed;
        Exit;
      end;

      // Теперь размеры сжатых файлов известны,
      // обновляем LocalHeader и DataDescryptor
      UpdateLocalHeaders(Stream);

      // Записываем CentralDirectory
      SaveCentralDirectory(Stream);

      // Пишем структуру EndOfCentralDirectory при этом при необходимости
      // формируются и пишутся также структуры Zip64EOFCentralDirectoryRecord
      // и Zip64EOFCentralDirectoryLocator
      SaveEndOfCentralDirectory(Stream);
    end;
  finally
    FBuildState := False;
  end;
end;

//
//  Процедура формирует архив и сохраняет его в указанный файл.
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
//  Функция проводит проверку правильности наименования файла в архиве
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
//  Очищаем все добавленные в архив элементы
// =============================================================================
procedure TFWZipWriter.Clear;
begin
  FItems.Clear;
end;

//
//  Процедура сжимет данные
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
            'Ошибка записи данных элемента №%d "%s".', [Index, Item[Index].FileName]);
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
        // резервируем место под EncryptedHeaderStream
        Stream.Size := StreamSizeBeforeCompress + EncryptedHeaderSize;
        Stream.Position := Stream.Size;
      end;

      // пишем сам сжатый файл
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
                  'Ошибка доступа к данным элемента №%d "%s".' +
                  sLineBreak + E.ClassName + ': ' + E.Message,
                  [Index, CurrentItem.FileName]);
            end;
          end;
          // Получаем размер сжатых данных
          // В случае если использовалось шифрование в размере срузу будет
          // учтен 12-ти байтный заголовок инициализации ключа расшифровки
          FCD[Index].CompressedSize := Stream.Size - StreamSizeBeforeCompress;
        end;
        Z_DEFLATED:
        begin
          {$IFDEF USE_AUTOGENERATED_ZLIB_HEADER}
          // позицию сдвигаем на два байта влево,
          // таким образом мы затрем ненужный нам заголовок ZLib
          Stream.Position := Stream.Position - 2;
          {$ENDIF}
          if CurrentItem.Data <> nil then
          begin
            // сохраняем ссылку на стрим с данными для рассчета прогресса
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
            // TFWZipItemStream выступает как посредник между результирующим
            // стримом и TCompressionStream.
            // Его задача зашифровать все проходящие через нешго данные
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
                    // сохраняем ссылку на стрим с данными для рассчета прогресса
                    FCompressedStream := F;
                    F.Position := 0;
                    Compressor.OnProgress := CompressorOnProcess;
                    // TFWZipCRC32Stream выступает как посредник между
                    // нераспакованными данными и TCompressionStream,
                    // в котором происходит сжатие данных.
                    // Его задача отследить все переданные через него
                    // блоки данных и рассчитать их контрольную сумму
                    // до того как они будут сжаты
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
                      'Ошибка доступа к данным элемента №%d "%s".' +
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
          // Не знаю почему, но опытным путем установлено,
          // что размер запакованых данных должен быть меньше на 4 байта
          // при использовании автогенерируемого заголовка.
          // Этот момент учитывается в ICSharpCode.SharpZipLibrary
          // Распаковка в любом случае происходит нормально
          if TrimPackedStreamSize then
            Stream.Size := Stream.Size - 4;
          {$ENDIF}

          // Получаем размер сжатых данных
          // В случае если использовалось шифрование в размере срузу будет
          // учтен 12-ти байтный заголовок инициализации ключа расшифровки
          FCD[Index].CompressedSize := Stream.Size - StreamSizeBeforeCompress;

        end;
      end;

      // если файл зашифрован,
      // записываем заголовок инициализации ключа расшифровки
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
//  Процедура вызывает событие OnProcess
// =============================================================================
procedure TFWZipWriter.CompressorOnProcess(Sender: TObject);
begin
  DoProgress(psInProgress);
end;

//
//  Функция возвращает количество добавленных элементов архива
// =============================================================================
function TFWZipWriter.Count: Integer;
begin
  Result := FItems.Count;
end;

//
//  Стандартный конструктор класса
// =============================================================================
constructor TFWZipWriter.Create;
begin
  Create(False, clDefault, '');
end;

//
//  Расширенный конструктор класса,
//  в котором можно указать степень сжатия,
//  используемую для всех элементов по умолчанию.
// =============================================================================
constructor TFWZipWriter.Create(CompressionLevel: TCompressionLevel);
begin
  Create(False, CompressionLevel, '');
end;

//
//  Расширенный конструктор класса,
//  в котором можно изменить настройки элементов будующего архива по умолчнию.
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
//  Процедура удаляет ненужный элемент архива
// =============================================================================
procedure TFWZipWriter.DeleteItem(Index: Integer);
begin
  FItems.Delete(Index);
end;

//
//  Стандартный деструктор класса
// =============================================================================
destructor TFWZipWriter.Destroy;
begin
  FItems.Free;
  inherited;
end;

//
//  Вызов внешнего события о прогрессе сжатия
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
//  Рассчитываем длину строки с учетом UTF8
// =============================================================================
function TFWZipWriter.StringLength(const Value: string; UTF8String: Boolean): Integer;
begin
  if UTF8String then
    Result := Length(UTF8Encode(Value))
  else
    Result := Length(Value);
end;

//
//  Функция возвращает текущее время в формате TFileTime
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
//  Обработчик свойства Items
// =============================================================================
function TFWZipWriter.GetItem(Index: Integer): TFWZipWriterItem;
begin
  Result := TFWZipWriterItem(FItems[Index]);
end;

//
//  Запрашиваем данные о расширенных блоках данных для каждой записи
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
          if ExDataStream.Size > MAXWORD then
            raise EZipWriter.Create(
              'Размер каждого блока расширенных данных' +
              ' не может превышать 65535 байт.')
          else
            ExDataSize := ExDataStream.Size;
          if ExDataHeaderTag in
            [0, SUPPORTED_EXDATA_ZIP64, SUPPORTED_EXDATA_NTFSTIME] then
            raise EZipWriter.Create(
              'Нельзя использовать зарезервированные тэги' +
              ' блока расширенных данных.');
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
//  Инициализируем CentralDirectoryFileHeader элемента
// =============================================================================
procedure TFWZipWriter.FillItemCDFHeader(CurrentItem: TFWZipWriterItem;
  var Value: TCentralDirectoryFileHeaderEx);
var
  SystemTime: TSystemTime;
  LastWriteTime: TFileTime;
  FileDate: Cardinal;
begin
  Value.Header.CentralFileHeaderSignature := CENTRAL_FILE_HEADER_SIGNATURE;
  Value.Header.VersionMadeBy := CurrentVersionMadeBy;
  Value.Header.VersionNeededToExtract := 0; // Рассчитывается позднее

  Value.Header.GeneralPurposeBitFlag := 0;
  if CurrentItem.Password <> '' then
    Value.Header.GeneralPurposeBitFlag :=
      Value.Header.GeneralPurposeBitFlag or PBF_CRYPTED;

  case CurrentItem.CompressionLevel of
    clNone:; // данный режим компрессии не поддерживается, сразу меняем на Store
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

  if not CurrentItem.NeedDescriptor then
    if CurrentItem.Password <> '' then
    begin
      // в случае если дескрипторы отключены и включено шифрование элемента
      // то необходимо рассчитать его контрольную сумму перед
      // генерацией заголовка инициализации ключа шифрования
      if CurrentItem.Data = nil then
        Value.Header.Crc32 := FileCRC32(CurrentItem.FilePath)
      else
        Value.Header.Crc32 :=
          CRC32Calc(CurrentItem.Data.Memory, CurrentItem.Data.Size);
    end;
  Value.UncompressedSize := CurrentItem.Size;

  // Rouse_ 25.10.2013
  // Правка небольшой ошибки замеченой Владиславом Нечепоренко
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
  Value.Header.DiskNumberStart := 0;
  Value.Header.InternalFileAttributes := 0;
  Value.Header.ExternalFileAttributes :=
    CurrentItem.Attributes.dwFileAttributes;
  Value.Attributes := CurrentItem.Attributes;

  if CurrentItem.UseUTF8String then
    Value.Header.GeneralPurposeBitFlag :=
      Value.Header.GeneralPurposeBitFlag or PBF_UTF8;
end;

//
//  Добавляем возможность создавать наследников от базового класса
// =============================================================================
function TFWZipWriter.GetItemClass: TFWZipWriterItemClass;
begin
  Result := TFWZipWriterItem;
end;

//
//  Функция рассчитывает минимальную версию для извлечения
//  указанного элемента архива
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

  // TGSZIPWriter поддерживает следующие расширения стандарта:
  // 1. использование директорий (версия для извлечения - 2.0)
  // 2. использование ZIP64 расширения (версия для извлечения - 4.5)

  // Для определения, нужно ли нам использовать ZIP64 необходимо проверить
  // следующие параметры:
  // размер каждого элемента архива сжатого и не сжатого,
  // оффсет на начало блока данных для каждого элемента
  // если любое из этих значений выходит за значение MAXDWORD,
  // или количество элементов архива выходит за значение MAXWORD,
  // нам необходимо применять ZIP64

  Result := 20;
  if (FCD[Index].UncompressedSize >= MAXDWORD) or
    (FCD[Index].CompressedSize >= MAXDWORD) or
    (FCD[Index].RelativeOffsetOfLocalHeader >= MAXDWORD) or
    (FCD[Index].DiskNumberStart >= MAXWORD) then
    Result := 45;
end;

//
//  Процедура проводит сохранение секции CentralDirectory
// =============================================================================
procedure TFWZipWriter.SaveCentralDirectory(Stream: TStream);
var
  I: Integer;
  ExDataHeader: TExDataHeaderAndSize;
  ExDataNTFS: TExDataNTFS;
  ZIP64Data: TMemoryStream;
  TotalExDataStream: TMemoryStream;
begin
  ZeroMemory(@ExDataNTFS, SizeOf(TExDataNTFS));
  for I := 0 to Count - 1 do
  begin

    // пропускаем элементы при записи которых произошло исключение
    if FCD[I].ExceptOnWrite then Continue;

    // перед записью каждого элемента CentralDirectory
    // необходимо подготовить буфферы с расширенными данными
    // и указать их расмер
    ZIP64Data := TMemoryStream.Create;
    try
      // подготавиваем буффер с ZIP64 данными

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
        // подготавливаем буффер с NTFS временем
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
        // Запрашиваем блоки ExData от пользователя
        FillExData(TotalExDataStream, I);

        // правим общий размер расширенных блоков
        Inc(FCD[I].Header.ExtraFieldLength, TotalExDataStream.Size);

        // Пишем структуру TCentralDirectoryFileHeader, описывающую элемент
        Stream.WriteBuffer(FCD[I].Header, SizeOf(TCentralDirectoryFileHeader));

        // Пишем наименование элемента
        SaveString(Stream, FCD[I].FileName,
          FCD[I].Header.GeneralPurposeBitFlag and PBF_UTF8 = PBF_UTF8);

        // если нужно -  доп информацию в формате ZIP64
        if ZIP64Data.Size > 0 then
        begin
          ExDataHeader.Header := SUPPORTED_EXDATA_ZIP64;
          ExDataHeader.Size := ZIP64Data.Size;
          Stream.WriteBuffer(ExDataHeader, SizeOf(TExDataHeaderAndSize));
          Stream.CopyFrom(ZIP64Data, 0);
        end;

        // потом информацию о NTFSTime
        if ExDataNTFS.HS.Header = SUPPORTED_EXDATA_NTFSTIME then
          Stream.WriteBuffer(ExDataNTFS, SizeOf(TExDataNTFS));

        // и расширенную информацию полученную от пользователя
        if TotalExDataStream.Size > 0 then
          Stream.CopyFrom(TotalExDataStream, 0);

      finally
         TotalExDataStream.Free;
      end;

      // в завершение, пишем коментарий к элементу
      SaveString(Stream, FCD[I].FileComment,
        FCD[I].Header.GeneralPurposeBitFlag and PBF_UTF8 = PBF_UTF8);

    finally
      ZIP64Data.Free;
    end;
  end;
end;

//
//  Процедура проводит сохранение секции EndOfCentralDirectory
// =============================================================================
procedure TFWZipWriter.SaveEndOfCentralDirectory(Stream: TStream);
var
  oe64cd: TZip64EOFCentralDirectoryRecord;
  locator: TZip64EOFCentralDirectoryLocator;
  eocd: TEndOfCentralDir;
  oe64cdOffset, SizeOfCentralDir: Int64;
begin
  oe64cdOffset := Stream.Position;
  SizeOfCentralDir := oe64cdOffset - FcdfhOffset;
  // Исключаем из общего количества элементов архива количество исключений
  oe64cd.TotalNumber1 := Count - FExceptionCount;
  // формат ZIP64 используется в случае если количество элементов  
  // архива превышает MAXWORD, или смещение на начало центральной директории
  // превышает MAXDWORD или ее размер превышает MAXDWORD
  if (FcdfhOffset > MAXDWORD) or (SizeOfCentralDir > MAXDWORD) or
    (oe64cd.TotalNumber1 > MAXWORD) then
  begin
    // В случае использования формата ZIP64
    // необходимо записать дополнительные структуры

    // TZip64EOFCentralDirectoryRecord
    oe64cd.Zip64EndOfCentralDirSignature := ZIP64_END_OF_CENTRAL_DIR_SIGNATURE;


    // Rouse_ 20.07.2013
    // в спецификации 6.3.0 от September 29, 2006 оговорено
    {
        The value stored into the "size of zip64 end of central
        directory record" should be the size of the remaining
        record and should not include the leading 12 bytes.

        Size = SizeOfFixedFields + SizeOfVariableData - 12.
    }
    // FWZip разрабатывался на основе более ранних спецификаций
    // (изначально на 6.0 потом на 6.2) и не учитывал этот момент

    // Поэтому вместо
    // oe64cd.SizeOfZip64EOFCentralDirectoryRecord :=
    //   SizeOf(TZip64EOFCentralDirectoryRecord);
    // пишем:

    oe64cd.SizeOfZip64EOFCentralDirectoryRecord :=
      SizeOf(TZip64EOFCentralDirectoryRecord) - 12;


    oe64cd.VersionMadeBy := CurrentVersionMadeBy;
    oe64cd.VersionNeededToExtract := FVersionToExtract;
    oe64cd.Number1 := 0;
    oe64cd.Number2 := 0;
    oe64cd.TotalNumber2 := oe64cd.TotalNumber1;
    oe64cd.Size := SizeOfCentralDir;
    oe64cd.Offset := FcdfhOffset;
    Stream.WriteBuffer(oe64cd, SizeOf(TZip64EOFCentralDirectoryRecord));

    // TZip64EOFCentralDirectoryLocator
    locator.Signature := ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIGNATURE;
    locator.NumberOfTheDisk := 0;
    locator.RelativeOffset := oe64cdOffset;
    locator.TotalNumberOfDisks := 1;
    Stream.WriteBuffer(locator, SizeOf(TZip64EOFCentralDirectoryLocator));

  end;
  eocd.EndOfCentralDirSignature := END_OF_CENTRAL_DIR_SIGNATURE;
  eocd.NumberOfThisDisk := 0;
  eocd.NumberOfTheDiskWithTheStart := 0;
  if oe64cd.TotalNumber1 > MAXWORD then
    eocd.TotalNumberOfEntriesOnThisDisk := MAXWORD
  else
    eocd.TotalNumberOfEntriesOnThisDisk := oe64cd.TotalNumber1;
  eocd.TotalNumberOfEntries := eocd.TotalNumberOfEntriesOnThisDisk;
  if SizeOfCentralDir > MAXDWORD then
    eocd.SizeOfTheCentralDirectory := MAXDWORD
  else
    eocd.SizeOfTheCentralDirectory := SizeOfCentralDir;
  if FcdfhOffset > MAXDWORD then
    eocd.OffsetOfStartOfCentralDirectory := MAXDWORD
  else
    eocd.OffsetOfStartOfCentralDirectory := FcdfhOffset;
  eocd.ZipfileCommentLength := StringLength(FComment, False);
  Stream.WriteBuffer(eocd, SizeOf(TEndOfCentralDir));
  if eocd.ZipfileCommentLength > 0 then
    SaveString(Stream, FComment, False);
end;

//
//  Процедура проводит все процедуры подготовки, сжатия и сохранения указанного элемента архива
// =============================================================================
procedure TFWZipWriter.SaveItemToStream(Stream: TStream; Index: Integer);
var
  CurrentItem: TFWZipWriterItem;
  FileNameOffset, StreamSizeBeforeCompress: Int64;
begin
  CurrentItem := Item[Index];

  // проверка на дуракоустойчивость
  if not CurrentItem.UseExternalData then
    if (CurrentItem.FilePath = '') and (CurrentItem.Data = nil) then
      raise EZipWriter.CreateFmt('Данные элемента №%d "%s" отсутствуют',
        [Index, CurrentItem.FileName]);

  FProcessedItemIndex := Index;

  // Rouse_ 25.10.2013
  // Генерируем событие начала распаковки перед тем как результирующий файл будет залочен
  DoProgress(psStart);
  try

    // Заполняем информацию в CentralDirectory
    // ===========================================================================
    FillItemCDFHeader(CurrentItem, FCD[Index]);
    FCD[Index].RelativeOffsetOfLocalHeader := Stream.Position;

    // Помещаем данные в результирующий файл
    // ===========================================================================

    // Запоминаем оффсет по которому необходимо будет писать имя файла
    FileNameOffset := Stream.Position + SizeOf(TLocalFileHeader);

    // рассчитываем размер резервируемого места под
    // LocalFileHeader и имя файла
    StreamSizeBeforeCompress := FileNameOffset +
      FCD[Index].Header.FilenameLength;

    // резервируем место под ZIP64
    if FCD[Index].UncompressedSize >= MAXDWORD then
      Inc(StreamSizeBeforeCompress, SizeOf(TExDataInfo64));

    // выделяем блок данных под LocalFileHeader и имя файла
    Stream.Size := StreamSizeBeforeCompress;
    Stream.Position := Stream.Size;

    // сжимаем данные
    if not CurrentItem.IsFolder then
      CompressItem(CurrentItem, Index, StreamSizeBeforeCompress, Stream);

    Inc(FTotalProcessedCount, CurrentItem.Size);

    // пишем имя файла
    Stream.Position := FileNameOffset;
    SaveString(Stream, FCD[Index].Filename, CurrentItem.UseUTF8String);

    // резервируем место под дескриптор
    if CurrentItem.NeedDescriptor then
      Stream.Size := Stream.Size + SizeOf(TDataDescriptor);

    Stream.Position := Stream.Size;

  finally
    // Rouse_ 25.10.2013
    // Результирующий файл освобожден, генерируем событие
    DoProgress(psEnd);
  end;
end;

//
//  Процедура проводит преобразование переданной строки в OEM и ее сохранение
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
//  Процедура обновляет секции LocalFileHeader
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

    // пропускаем элементы при записи которых произошло исключение
    if FCD[I].ExceptOnWrite then Continue;

    // Имея на руках все данные перезаписываем все LocalFileHeader
    // и DataDescriptor (если требуется)
    lfh.LocalFileHeaderSignature := LOCAL_FILE_HEADER_SIGNATURE;

    // рассчитываем версию необходимую для распаковки элемента архива
    lfh.VersionNeededToExtract := GetVersionToExtract(I);
    lfh.GeneralPurposeBitFlag := FCD[I].Header.GeneralPurposeBitFlag;
    UseDescriptor := lfh.GeneralPurposeBitFlag and PBF_DESCRIPTOR <> 0;
    lfh.CompressionMethod := FCD[I].Header.CompressionMethod;
    lfh.LastModFileTimeTime := FCD[I].Header.LastModFileTimeTime;
    lfh.LastModFileTimeDate := FCD[I].Header.LastModFileTimeDate;
    if UseDescriptor then
    begin
      dd.DescriptorSignature := DATA_DESCRIPTOR_SIGNATURE;
      // хоть в стандарте сказано что при использовании дескрипторов 
      // поля Crc32, CompressedSize и UncompressedSize должны быль установлены
      // в ноль, но большинство архиваторов этого не делают,
      // поэтому уподобимся им :)
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

    Stream.Position := FCD[I].RelativeOffsetOfLocalHeader;

    Stream.WriteBuffer(lfh, SizeOf(TLocalFileHeader));

    // Rouse_ 20.03.2015
    // Пишем данные для поддержки ZIP64
    // а то WinRar, WinZip и 7Zip не будут распаковывать такой архив
    // (не понятно правда, почему они не читают эту информацию из CentralDirectory)
    if lfh.ExtraFieldLength > 0 then
    begin
      Stream.Position := Stream.Position + lfh.FilenameLength;
      Info64.HS.Header := SUPPORTED_EXDATA_ZIP64;
      Info64.HS.Size := SizeOf(TExDataInfo64) - SizeOf(TExDataHeaderAndSize);
      Info64.UncompressedSize := FCD[I].UncompressedSize;
      // если CompressedSize меньше MAXDWORD его писать не надо по стандарту,
      // но место под него уже зарезервированно, поэтому придется писать его в любом случае
      // как обойти это расхождение со стандартом хорошим способом - я пока не знаю
      Info64.CompressedSize := FCD[I].CompressedSize;
      Stream.WriteBuffer(Info64, SizeOf(TExDataInfo64));
    end;

    if UseDescriptor then
    begin
      // дескриптор пишется после сжатого блока данных
      Stream.Position := FCD[I].RelativeOffsetOfLocalHeader +
        SizeOf(TLocalFileHeader) + lfh.FilenameLength +
        lfh.ExtraFieldLength + FCD[I].CompressedSize;
      Stream.WriteBuffer(dd, SizeOf(TDataDescriptor));
    end;

    // обновляем информацию в массиве CentralDirectoryFileHeader

    // Rouse_ 14.02.2013
    // Неверно выставлялась версия для распаковки в CentralDirectoryFileHeader
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

    // Rouse_ 14.02.2013
    // обновляем глобальную метку версии
    if FVersionToExtract < lfh.VersionNeededToExtract then
      FVersionToExtract := lfh.VersionNeededToExtract;
  end;
  Stream.Position := FcdfhOffset;
end;

end.
