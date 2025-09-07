////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : FWZipUtils
//  * Purpose   : Набор платформозависимых методов
//  * Author    : Александр (Rouse_) Багель
//  * Copyright : © Fangorn Wizards Lab 1998 - 2024.
//  * Version   : 2.0.4
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Stable Release : http://rouse.drkb.ru/components.php#fwzip
//  * Latest Source  : https://github.com/AlexanderBagel/FWZip
//  ****************************************************************************
//
//  Используемые источники:
//  ftp://ftp.info-zip.org/pub/infozip/doc/appnote-iz-latest.zip
//  https://zlib.net/zlib-1.3.1.tar.gz
//  http://www.base2ti.com/
//

unit FWZipUtils;

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+}
  // у FPC криво реализованы инлайны, в частности FileSizeToInt64 теряет
  // значение FileSizeHi при компиляции в 32 бита, поэтому отключаем
  {$IFDEF CPU32}
    {$UNDEF USE_INLINE}
  {$ELSE}
    {$DEFINE USE_INLINE}
  {$ENDIF}
{$ELSE}
  {$IF COMPILERVERSION > 15.0 }
    {$DEFINE USE_INLINE}
  {$IFEND}
{$ENDIF}

interface

{$I fwzip.inc}

uses
  {$IFDEF FPC}LConvEncoding,{$ENDIF}

  {$IFDEF LINUX_DELPHI}Posix.SysStat,{$ENDIF}

  {$IFDEF LINUX}
  {$IFDEF FPC}Unix, Baseunix,{$ENDIF}
  DateUtils, Types,
  {$ELSE}
  Windows,
  {$ENDIF}
  SysUtils,
  {$IFDEF LINUX_DELPHI}FWZipLinuxDelphiCompability,{$ENDIF}
  FWZipConsts;

  // утилитарные преобразования для отключения ворнингов под FPC
  // ===========================================================================
  function HiByte(W: Word): Byte; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function HiWord(L: DWORD): Word; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function PtrToUInt(Value: Pointer): NativeUInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function UIntToPtr(Value: NativeUInt): Pointer; {$IFDEF USE_INLINE}inline;{$ENDIF}

  function FileSizeToInt64(FileSizeLo, FileSizeHi: DWORD): Int64; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function FileSizeToStr(Value: Int64): string;

  // это позволит избежать сообщения об неинициализированных переменных под FPC
  procedure ZeroMemory(Destination: Pointer; Length: NativeUInt); {$IFDEF USE_INLINE}inline;{$ENDIF}

  // конвертация строк
  // ===========================================================================
  function ConvertToOemString(const Value: AnsiString): AnsiString;
  function ConvertFromOemString(const Value: AnsiString): AnsiString;
  function ExceptionMessage(const E: Exception): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function LongPrefixPresent(const {%H-}APath: string): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function IncludeLongNamePrefix(const Value: string): string;

  // работа с аттрибутами файла
  // ===========================================================================
  function GetDiskFreeAvailable(const AFilePath: string): Int64;
  function GetFileAttributes(const AFilePath: string;
  	out AAttr: TFileAttributeData): Boolean;
  function IsAttributesPresent(Value: TFileAttributeData): Boolean;
  procedure SetNormalFileAttributes(const AFilePath: string);
  procedure SetFileAttributes(const AFilePath: string; AAttr: TFileAttributeData);

  // работа с путями
  // ===========================================================================
  function PathCanonicalize(const AFilePath: string): string;
  function MakeUniqueName(const AFilePath: string): string;
  function ForceDirectoriesEx(Dir: string): Boolean;

  // преобразование времени
  // ===========================================================================
  function FileTimeToLocalFileDate(AFileTime: TFileTime): Cardinal;
  function FileTimeToLocalDateTime(AFileTime: TFileTime): TDateTime;
  function DateTimeToFileTime(ADateTime: TDateTime): TFileTime;

  // не реализованные под Linux аналоги Windows функций
  // ===========================================================================
  procedure FinallyFileBuffers(AHandle: THandle);

implementation

{$IFDEF MSWINDOWS}
  function PathCanonicalizeApi(lpszDes, lpszSrc: PChar): BOOL; stdcall; external 'shlwapi.dll'
    name {$IFDEF UNICODE}'PathCanonicalizeW'{$ELSE}'PathCanonicalizeA'{$ENDIF};
  function PathMakeUniqueName(pszUniqueName: PWideChar; cchMax: Cardinal;
    pszTemplate, pszLongPlate, pszDir: PWideChar): Boolean; stdcall; external 'shell32.dll';
{$ENDIF}

function FileSizeToInt64(FileSizeLo, FileSizeHi: DWORD): Int64;
begin
  Result := FileSizeHi;
  Result := Result shl 32;
  Inc(Result, FileSizeLo);
end;

function HiByte(W: Word): Byte;
begin
  Result := W shr 8;
end;

function HiWord(L: DWORD): Word;
begin
  Result := L shr 16;
end;

procedure ZeroMemory(Destination: Pointer; Length: NativeUInt);
begin
  FillChar(Destination^, Length, 0);
end;

function PtrToUInt(Value: Pointer): NativeUInt;
begin
  Result := {%H-}NativeUInt(Value);
end;

function UIntToPtr(Value: NativeUInt): Pointer;
begin
  Result := {%H-}Pointer(Value);
end;

{$IFDEF LINUX}

// эмуляция апи для работы с NTFS атрибутами, в частности с временем

const
  NSECPERSEC = 10000000;
  NSECPERMSEC = 10000;
  MSECPERSEC = 1000;
  SECSPERMIN  = 60;
  MINSPERHOUR = 60;
  HOURSPERDAY = 24;
  EPOCHWEEKDAY = 1;  // 1 января 1601 был понедельником
  DAYSPERWEEK = 7;
  DAYSPERQUADRICENTENNIUM = 365 * 400 + 97;
  DAYSPERNORMALQUADRENNIUM = 365 * 4 + 1;
  MonthLength: array [Boolean] of array [0..11] of Integer =
	  ((31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
	   (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));

type
  InternalStat = {$IFDEF FPC}Stat{$ELSE}_stat{$ENDIF};

function SystemTimeToFileTime(
  const ATime: TSystemTime; out AFileTime: TFileTime): Boolean;

  function IsLeapYear: Boolean;
  begin
    Result := (ATime.Year mod 400 = 0) and
      (ATime.Year mod 100 <> 0) or (ATime.Year mod 4 = 0);
  end;

  function IsDayTooBig: Boolean;
  begin
    Result := ATime.Day >
      MonthLength[(ATime.Month = 2) and IsLeapYear][ATime.Month - 1];
  end;

var
  CalcYear, CalcMonth, CalcLeapCount, CalcDay: UInt64;
begin
  Result := False;

  // проверка входных данных
  if ATime.Millisecond > 999 then Exit;
  if ATime.Second > 59 then Exit;
  if ATime.Minute > 59 then Exit;
  if ATime.Hour > 23 then Exit;
  if (ATime.Month < 1) or (ATime.Month > 12) then Exit;
  if (ATime.Day < 1) or IsDayTooBig then Exit;
  if (ATime.Year < 1601) or (ATime.Year > 30827) then Exit;

  // преобразование с учетом високосных годов
  // если текущий месяц меньше марта, то просто отнимаем год и добавляем 12 месяцев
  // это учтется в дальнейшем
  CalcYear := ATime.Year;
  CalcMonth := ATime.Month;
  if ATime.Month < 3 then
  begin
    Dec(CalcYear);
    Inc(CalcMonth, 12);
  end;

  // количество високосных годов в пределах столетия
  CalcLeapCount := (3 * (CalcYear div 100) + 3) shr 2;

  // количество дней с 1601 года
  CalcDay :=
    (36525 * CalcYear) div 100 - CalcLeapCount + // год * количество дней в столетии с коррекцией по високосным
    (1959 * (CalcMonth + 1)) shr 6 +             // месяц * среднее кол-во дней
    ATime.Day -                                  // день
    584817;                                      // за вычетом количества дней до 1601 года

  // считаем результат
  PUint64(@AFileTime)^ := ((((
    CalcDay * HOURSPERDAY +
    ATime.Hour) * MINSPERHOUR +
    ATime.Minute) * SECSPERMIN +
    ATime.Second) * MSECPERSEC +
    ATime.Millisecond) * NSECPERMSEC;

  Result := True;
end;

function FileTimeToSystemTime(const AFileTime: TFileTime;
  out ASystemTime: TSystemTime): Boolean;
var
  FullTime, CalcYear, CalcMonth, CalcLeapCount,
  CalcYearDay, CalcDay, CalcSecond: Int64;
begin
  Result := False;
  FullTime := PInt64(@AFileTime)^;
  if FullTime < 0 then Exit;

  // вытаскиваем количество миллисекунд и преобразуем время в секунды
  ASystemTime.Millisecond := (FullTime mod NSECPERSEC) div NSECPERMSEC;
  FullTime := FullTime div NSECPERSEC;

  // получаем количество секунд
  CalcSecond := FullTime mod SECSPERDAY;

  // считаем время дня
  ASystemTime.Hour := CalcSecond div SECSPERHOUR;
  CalcSecond := CalcSecond mod SECSPERHOUR;
  ASystemTime.Minute := CalcSecond div SECSPERMIN;
  ASystemTime.Second := CalcSecond mod SECSPERMIN;

  // получаем количество дней
  CalcDay := FullTime div SECSPERDAY;

  // считаем день недели
  ASystemTime.DayOfWeek := (EPOCHWEEKDAY + CalcDay) mod DAYSPERWEEK;

  // считаем год, месяц и день месяца
  CalcLeapCount :=
    (3 * ((CalcDay shl 2 + 1227) div DAYSPERQUADRICENTENNIUM) + 3) shr 2;
  Inc(CalcDay, 28188 + CalcLeapCount);
  CalcYear := (20 * CalcDay - 2442) div (5 * DAYSPERNORMALQUADRENNIUM);
  CalcYearDay := CalcDay - (CalcYear * DAYSPERNORMALQUADRENNIUM) shr 2;
  CalcMonth := (CalcYearDay shl 6) div 1959;

  // результат для года который начинается с марта, если залазит на следущий
  // то для преобразования отнимаем 12 месяцев и увеличиваем год
  ASystemTime.Month := CalcMonth - 1;
  ASystemTime.Year := CalcYear + 1524;
  if ASystemTime.Month > 12 then
  begin
    Dec(ASystemTime.Month, 12);
    Inc(ASystemTime.Year);
  end;

  ASystemTime.Day := CalcYearDay - (1959 * CalcMonth) shr 6;
end;

// преобразование из stat.st_atime (qword) в TFileTime
function UnixDateToFileTime(Value: Int64): TFileTime;
var
  SystemTime: TSystemTime;
begin
  DateTimeToSystemTime(UnixToDateTime(Value), SystemTime);
  SystemTimeToFileTime(SystemTime, Result);
end;

function FileTimeToUnixDate(Value: TFileTime): Int64;
var
  SystemTime: TSystemTime;
begin
  FileTimeToSystemTime(Value, SystemTime);
  Result := DateTimeToUnix(SystemTimeToDateTime(SystemTime));
end;


function Internal_fpstat(aSystemFileName: RawByteString; var aBuf: InternalStat): Integer;
begin
  {$IFDEF FPC}
  Result := fpstat(PChar(aSystemFileName), aBuf);
  {$ELSE}
  Result := stat(PAnsiChar(AnsiString(aSystemFileName)), aBuf);
  {$ENDIF}
end;


function Internal_fpS_ISDIR(aMode: Cardinal): Boolean;
begin
  {$IFDEF FPC}
  Result := fpS_ISDIR(aMode);
  {$ELSE}
  Result := S_ISDIR(aMode);
  {$ENDIF}
end;


function Internal_fputime(aSystemFileName: RawByteString; var aTimBuf: TUTimBuf): Integer;
begin
  {$IFDEF FPC}
  Result := fputime(PChar(aSystemFileName), @aTimBuf);
  {$ELSE}
  Result := utime(PAnsiChar(AnsiString(aSystemFileName)), @aTimBuf);
  {$ENDIF}
end;

{$ENDIF LINUX}

function ConvertToOemString(const Value: AnsiString): AnsiString;
begin
  Result := Value;
  if Result = '' then Exit;
  UniqueString(Result);
  {$IFDEF FPC}
  Result := UTF8ToCP866(Value);
  {$ELSE}
    {$IFDEF LINUX}
    Result := Value;
    {$ELSE}
    AnsiToOem(PAnsiChar(Value), PAnsiChar(Result));
    {$ENDIF}
  {$ENDIF}
end;

function ConvertFromOemString(const Value: AnsiString): AnsiString;
begin
  Result := Value;
  if Result = '' then Exit;
  UniqueString(Result);
  {$IFDEF FPC}
  Result := CP866ToUTF8(Value);
  {$ELSE}
    {$IFDEF LINUX}
    Result := Value;
    {$ELSE}
    OemToAnsi(PAnsiChar(Result), PAnsiChar(Result));
    {$ENDIF}
  {$ENDIF}
end;

function ExceptionMessage(const E: Exception): string;
begin
  Result := E.ClassName + ': ' +  E.Message;
end;

function LongPrefixPresent(const APath: string): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := (Length(APath) >= 4) and
    ( (APath[1] = LongNamePrefix[1]) and
      (APath[2] = LongNamePrefix[2]) and
      (APath[3] = LongNamePrefix[3]) and
      (APath[4] = LongNamePrefix[4]));
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function IsLocalDrive(const Value: string): Boolean;
begin
  Result := (Length(Value) > 2) and (Value[2] = ':');
end;

function IsNetworkSlashPresent(const Value: string): Boolean;
begin
  Result := (Length(Value) > 2) and (Value[1] = '\') and (Value[2] = '\');
end;

function FixupFilePrefix(const Value: string): string;
begin
  Result := StringReplace(Value, '/', '\', [rfReplaceAll]);
  if AnsiSameText(Copy(Result, 1, 7), 'file:\\') then
  begin
    Delete(Result, 1, 7);
    if not IsLocalDrive(Result) then
      Result := '\\' + Result;
  end;
end;

function IncludeLongNamePrefix(const Value: string): string;
begin
  {$IFDEF MSWINDOWS}
  if UseLongNamePrefix and not LongPrefixPresent(Value) and (Length(Value) > MAX_PATH) then
  begin
    Result := FixupFilePrefix(Value);
    if IsLocalDrive(Result) then
      Result := LongNamePrefix + Result
    else
    begin
      if IsNetworkSlashPresent(Result) then
        Delete(Result, 1, 2);
      Result := UNCLongNamePrefix + Result;
    end;
  end
  else
  {$ENDIF}
    Result := Value;
end;

function GetPresentFolder(const AFilePath: string): string;
begin
  Result := AFilePath;
  {$IFDEF LINUX}
  while (Result <> '') and not DirectoryExists(Result) do
    Result := ExtractFilePath(ExcludeTrailingPathDelimiter(Result));
  if Result = '' then
    Result := ExpandFileName('~');
  {$ELSE}
  if LongPrefixPresent(Result) then
    Delete(Result, 1, 4);
  Result := ExtractFileDrive(Result);
  {$ENDIF}
end;

function GetDiskFreeAvailable(const AFilePath: string): Int64;
{$IFDEF MSWINDOWS}
var
  FreeAvailable, TotalSpace: Int64;
{$ENDIF}
begin
  {$IFDEF LINUX}
  Result := DiskFree(AddDisk(GetPresentFolder(AFilePath)));
  {$ELSE}
  {$IFDEF FPC}
  FreeAvailable := -1;
  TotalSpace := -1;
  {$ENDIF}
  if GetDiskFreeSpaceEx(PChar(GetPresentFolder(AFilePath)), FreeAvailable, TotalSpace, nil) then
    Result := FreeAvailable
  else
    Result := -1;
  {$ENDIF}
end;

function GetFileAttributes(const AFilePath: string;
	out AAttr: TFileAttributeData): Boolean;
{$IFDEF LINUX}
var
  Info: InternalStat;
  SystemFileName: RawByteString;
begin
  AAttr := Default(TFileAttributeData);
  SystemFileName := {$IFDEF FPC}ToSingleByteFileSystemEncodedFileName(AFilePath){$ELSE}AFilePath{$ENDIF};
  Info := Default(InternalStat);
  if (Internal_fpstat(SystemFileName, Info) < 0) or Internal_fpS_ISDIR(info.st_mode) then
    Result := False
  else
  begin
    AAttr.dwFileAttributes := FileGetAttr(SystemFileName);
    Result := AAttr.dwFileAttributes <> DWORD(-1);
    if Result then
    begin
      AAttr.ftCreationTime := UnixDateToFileTime(Info.st_ctime);
      AAttr.ftLastAccessTime := UnixDateToFileTime(Info.st_atime);
      AAttr.ftLastWriteTime := UnixDateToFileTime(Info.st_mtime);
      AAttr.nFileSizeHigh := Info.st_size shr 32;
      AAttr.nFileSizeLow := DWORD(Info.st_size);
    end;
  end;
{$ELSE}
begin
  Result := GetFileAttributesEx(PChar(AFilePath),
    GetFileExInfoStandard, @AAttr);
{$ENDIF}
end;

function IsAttributesPresent(Value: TFileAttributeData): Boolean;
begin
  Result := (Value.ftCreationTime.dwLowDateTime <> 0) and
    (Value.ftCreationTime.dwHighDateTime <> 0);
end;

procedure SetNormalFileAttributes(const AFilePath: string);
var
  AAttr: TFileAttributeData;
begin
  {$IFDEF FPC}
  AAttr := Default(TFileAttributeData);
  {$ELSE}
  {$IFDEF LINUX}
  AAttr := Default(TFileAttributeData);
  {$ENDIF}
  {$ENDIF}
  AAttr.dwFileAttributes := $80; // FILE_ATTRIBUTE_NORMAL
  SetFileAttributes(AFilePath, AAttr);
end;

procedure SetFileAttributes(const AFilePath: string; AAttr: TFileAttributeData);
{$IFDEF MSWINDOWS}
var
  hFile: THandle;
{$ELSE}
var
  SystemFileName: RawByteString;
  t: TUTimBuf;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  Windows.SetFileAttributes(PChar(AFilePath), AAttr.dwFileAttributes);
  {$ENDIF}

  // проверка, есть ли аттрибуты времени?
  if not IsAttributesPresent(AAttr) then Exit;

  {$IFDEF LINUX}
  t.actime := FileTimeToUnixDate(AAttr.ftLastAccessTime);
  t.modtime:= FileTimeToUnixDate(AAttr.ftLastWriteTime);
  SystemFileName := {$IFDEF FPC}ToSingleByteFileSystemEncodedFileName(AFilePath){$ELSE}AFilePath{$ENDIF};
  Internal_fputime(SystemFileName, t);
  {$ELSE}
  hFile := FileOpen(AFilePath, fmOpenWrite);
  try
    SetFileTime(hFile,
      @AAttr.ftCreationTime,
      @AAttr.ftLastAccessTime,
      @AAttr.ftLastWriteTime);
  finally
    FileClose(hFile);
  end;
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
function CallPathCanonicalize(const AFilePath: string): string;
begin
  Result := StringOfChar(#0, MAX_PATH);
  if PathCanonicalizeApi(PChar(Result), PChar(AFilePath)) then
    Result := PChar(Result)
  else
    Result := AFilePath;
end;
{$ENDIF}

function PathCanonicalize(const AFilePath: string): string;
begin
  Result := AFilePath;
  if Result = '' then Exit;
  if Result[1] = '.' then
    Result := IncludeTrailingPathDelimiter(GetCurrentDir) + Result;
  {$IFDEF MSWINDOWS}
  Result := IncludeLongNamePrefix(CallPathCanonicalize(Result));
  {$ELSE}
  Result := ExpandFileName(Result);
  {$ENDIF}
end;

function MakeUniqueName(const AFilePath: string): string;
{$IFDEF LINUX}
var
  FilePath, FileName, FileExt, NewFileName: string;
  I: Integer;
begin
  Result := AFilePath;
  if not FileExists(Result) then Exit;
  FilePath := ExtractFilePath(AFilePath);
  FileName := ExtractFileName(AFilePath);
  FileExt := ExtractFileExt(FileName);
  FileName := ChangeFileExt(FileName, '');
  I := 1;
  repeat
    Inc(I);
    NewFileName := FileName + ' (' + IntToStr(I) + ')' + FileExt;
  until not FileExists(FilePath + NewFileName);
  Result := FilePath + NewFileName;
{$ELSE}
  {$IFDEF UNICODE}
  var
    FilePath, FileName: string;
  begin
    Result := AFilePath;
    if not FileExists(Result) then Exit;
    FilePath := ExtractFilePath(AFilePath);
    FileName := ExtractFileName(AFilePath);
    SetLength(Result, MAX_PATH);
    if PathMakeUniqueName(PWideChar(Result), MAX_PATH,
      nil, PWideChar(FileName), PWideChar(FilePath)) then
      Result := PWideChar(Result);
  {$ELSE}
  var
    UnicodeResult, FilePath, FileName: WideString;
  begin
    Result := AFilePath;
    if not FileExists(Result) then Exit;
    FilePath := WideString(ExtractFilePath(AFilePath));
    FileName := WideString(ExtractFileName(AFilePath));
    {$IFDEF FPC}
    UnicodeResult := '';
    {$ENDIF}
    SetLength(UnicodeResult, MAX_PATH);
    if PathMakeUniqueName(PWideChar(UnicodeResult), MAX_PATH,
      nil, PWideChar(FileName), PWideChar(FilePath)) then
      Result := AnsiString(PWideChar(UnicodeResult));
  {$ENDIF}
{$ENDIF}
end;

function ForceDirectoriesEx(Dir: string): Boolean;

// Обход ошибки ForceDirectories в случае использования префикса для
// поддержки длинных имен. Проверить ошибку можно вот таким кодом:
//
//  S := '\\?\w:\test\'; // этой папки быть не должно!
//  ForceDirectories(S); // с префиксом она не создастся
//
// Глюк заключается в том что при вызове DirectoryExists на строке "\\?\w:"
// функция GetFileAttributes возвращает INVALID_FILE_ATTRIBUTES,
// требуя чтобы для корня был указан слэш в конце в случае использования префикса,
// причем про эту особенность в MSDN нигде не написано.
//
// Соответсвтенно т.к. это будет второй рекурсивный вызов,
// он вернет первому False и в первом не выполнится CreateDir

  {$IFDEF MSWINDOWS}
  function InternalForce(Dir: string): Boolean;
  var
    PreviosDir: string;
  begin
    Result := Dir <> '';
    if not Result or DirectoryExists(Dir) then
      Exit;

    Dir := ExcludeTrailingPathDelimiter(Dir);
    PreviosDir := ExtractFilePath(Dir);
    if PreviosDir = Dir then
      Result := True
    else
      Result := InternalForce(PreviosDir) and CreateDir(Dir);
  end;
  {$ENDIF}

begin
  {$IFDEF MSWINDOWS}
  if LongPrefixPresent(Dir) then
    Result := InternalForce(Dir)
  else
  {$ENDIF}
    Result := ForceDirectories(Dir);
end;

function FileTimeToLocalFileDate(AFileTime: TFileTime): Cardinal;
{$IFDEF LINUX}
var
  SystemTime: TSystemTime;
{$ENDIF}
begin
  {$IFDEF LINUX}
  DateTimeToSystemTime(FileTimeToLocalDateTime(AFileTime), SystemTime);
  if (SystemTime.Year < 1980) or (SystemTime.Year > 2107) then
    Result := 0
  else
    with SystemTime do
    begin
      LongRec(Result).Lo := (Second shr 1) or (Minute shl 5) or (Hour shl 11);
      LongRec(Result).Hi := Day or (Month shl 5) or ((Year - 1980) shl 9);
    end;
  {$ELSE}
  Result := DateTimeToFileDate(FileTimeToLocalDateTime(AFileTime));
  {$ENDIF}
end;

function FileTimeToLocalDateTime(AFileTime: TFileTime): TDateTime;
var
  {$IFDEF LINUX}
  UnixTime: Int64;
  {$ELSE}
  SystemTime: TSystemTime;
  {$ENDIF}
begin
  {$IFDEF LINUX}
  UnixTime := FileTimeToUnixDate(AFileTime);
  Result := UnixToDateTime(UnixTime, False);
  {$ELSE}
  // Rouse_ 25.10.2013
  // Правка небольшой ошибки замеченой Владиславом Нечепоренко
  //FileTimeToSystemTime(CurrentItem.Attributes.ftLastWriteTime, SystemTyme);
  FileTimeToLocalFileTime(AFileTime, AFileTime);
  {$IFDEF FPC}
  SystemTime := Default(TSystemTime);
  {$ENDIF}
  FileTimeToSystemTime(AFileTime, SystemTime);
  Result := SystemTimeToDateTime(SystemTime);
  {$ENDIF}
end;

function DateTimeToFileTime(ADateTime: TDateTime): TFileTime;
{$IFDEF MSWINDOWS}
var
  SystemTime: TSystemTime;
begin
	{$IFDEF FPC}
	Result := Default(TFileTime);
	{$ENDIF}
	DateTimeToSystemTime(ADateTime, SystemTime);
	SystemTimeToFileTime(SystemTime, Result);
	LocalFileTimeToFileTime(Result, Result);
{$ELSE}
begin
  Result := UnixDateToFileTime(DateTimeToUnix(ADateTime, False));
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

procedure FinallyFileBuffers(AHandle: THandle);
begin
  {$IFDEF MSWINDOWS}
  FlushFileBuffers(AHandle);
  {$ELSE}
  FileFlush(AHandle);
  {$ENDIF}
end;

end.
