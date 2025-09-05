////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : FWZipLinuxDelphiCompability
//  * Purpose   : Совместимость библиотеки FWZip с Linux+Delphi
//  * Author    : Danil Zaripov (github.com/DonilZ)
//  * Copyright : © Fangorn Wizards Lab 1998 - 2024.
//  * Version   : 2.0.9
//  ****************************************************************************
//  * Latest Source : https://github.com/AlexanderBagel/FWZip
//  * Fork Source   : https://github.com/DonilZ/FWZip
//  ****************************************************************************
//

unit FWZipLinuxDelphiCompability;

{$I fwzip.inc}

{$IFNDEF LINUX_DELPHI}
  {$MESSAGE ERROR 'FOR LINUX + DELPHI ONLY!!!'}
{$ENDIF}

interface

uses
  SysUtils, Classes, DateUtils, Types, AnsiStrings,
  Posix.Base, Posix.SysStat, Posix.SysStatvfs, Posix.SysTypes, Posix.Errno, Posix.Fcntl;


const
  LOCK_SH = 1;    // Shared lock
  LOCK_EX = 2;    // Exclusive lock
  LOCK_NB = 4;    // Non-blocking
  LOCK_UN = 8;    // Unlock

  ESysEINTR   = 4;    { Interrupted system call }
  ESysEAGAIN  = 11;   { Try again }
  ESysEDEADLK = 35;   { Resource deadlock would occur }

  SFCreateError   = 'Unable to create file "%s"';
  SFOpenError     = 'Unable to open file "%s"';


type
  TSystemTime = record
    Year: Word;
    Month: Word;
    DayOfWeek: Word;
    Day: Word;
    Hour: Word;
    Minute: Word;
    Second: Word;
    Millisecond: Word;
  end;


  UTimBuf = record
    actime  : time_t;
    modtime : time_t;
  end;
  TUtimBuf  = UtimBuf;
  pUtimBuf  = ^UtimBuf;


  { Данный хелпер был добавлен из-за разных реализаций блокировки файла в функции FileOpen в Lazarus и RAD Studio.
    В Lazarus файл после открытия блокируется при помощи функции flock(), а в RAD Studio при помощи функции fcntl().

    Таким образом, в RAD Studio мы получаем неприятную ситуацию:
      Если открыть файл при помощи функции FileOpen() в режиме "fmOpenWrite or fmShareExclusive",
      а затем сделать это еще раз, но в режиме "fmOpenRead or fmShareDenyWrite" посредством вызова TFileStream.Create,
      то мы не получим выброса исключения EFOpenError. Мы получим новый корректный Handle файла, чего не ожидали.
    Поэтому для идентичного поведения в этом случае из Lazarus была перенесена реализация функции FileOpen() }

  TFileStreamHelper = class helper for TFileStream
    constructor CreateWithFpLock(const FileName: string; Mode: Word); overload;
    constructor CreateWithFpLock(const FileName: string; Mode: Word; Rights: Cardinal); overload;
  end;


  function open(path: PAnsiChar; flags: Integer): Integer; cdecl; external libc name 'open'; overload;
  function open(path: PAnsiChar; flags: Integer; mode: mode_t): Integer; cdecl; external libc name 'open'; overload;
  function close(fd: Integer): Integer; cdecl; external libc name 'close';
  function flock(fd: Integer; operation: Integer): Integer; cdecl; external libc name _PU + 'flock';
  function utime(path: PAnsiChar; times: Putimbuf): Integer; cdecl; external libc name _PU + 'utime';
  function fsync(fd: Integer): Integer; cdecl; external libc name _PU + 'fsync';

  procedure DateTimeToSystemTime(DateTime: TDateTime; var SystemTime: TSystemTime);
  function  SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;

  function FileOpen(const FileName: string; Mode: Word): THandle;
  function FileFlush(Handle: THandle): Boolean;

  function DiskFree(Drive: Byte): Int64;
  function AddDisk(const Path:string): Byte;


implementation


{ TFileStreamHelper }

constructor TFileStreamHelper.CreateWithFpLock(const FileName: string; Mode: Word);
begin
  CreateWithFpLock(FileName, Mode, FileAccessRights);
end;


constructor TFileStreamHelper.CreateWithFpLock(const FileName: string; Mode: Word; Rights: Cardinal);
begin
  with Self do begin
    FFileName := FileName;
  end;

  if (Mode and fmCreate) > 0 then begin
    FHandle := FileCreate(FileName, Mode, Rights);
  end
  else begin
    FHandle := FileOpen(FileName, Mode);
  end;

  if (Handle = INVALID_HANDLE_VALUE) then begin
    if Mode = fmCreate then begin
      raise EFCreateError.CreateFmt(SFCreateError, [FileName])
    end
    else begin
      raise EFOpenError.CreateFmt(SFOpenError, [Filename]);
    end;
  end;
end;


procedure DateTimeToSystemTime(DateTime: TDateTime; var SystemTime: TSystemTime);
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  DecodeDateTime(DateTime, Year, Month, Day, Hour, Min, Sec, MSec);

  SystemTime.Year := Year;
  SystemTime.Month := Month;
  SystemTime.Day := Day;
  SystemTime.Hour := Hour;
  SystemTime.Minute := Min;
  SystemTime.Second := Sec;
  SystemTime.Millisecond := MSec;
  SystemTime.DayOfWeek := DayOfTheWeek(DateTime); // 1-7 (Воскресенье=1)
end;


function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;
begin
  Result := EncodeDateTime(
    SystemTime.Year,
    SystemTime.Month,
    SystemTime.Day,
    SystemTime.Hour,
    SystemTime.Minute,
    SystemTime.Second,
    SystemTime.Millisecond
  );
end;


function IntToHandle(anIntValue: Integer): THandle;
begin
  if anIntValue < 0 then begin
    Exit(INVALID_HANDLE_VALUE);
  end;

  Result := THandle(anIntValue);
end;


function FileOpenNoLocking(const FileName: string; Mode: Integer): THandle;

    function IsHandleDirectory(Handle : Longint) : boolean;
    Var Info : _stat;
    begin
      Result := (fstat(Handle, Info) < 0) or S_ISDIR(info.st_mode);
    end;
Var
  LinuxFlags, fd: Integer;
begin
  LinuxFlags := 0;
  case (Mode and (fmOpenRead or fmOpenWrite or fmOpenReadWrite)) of
    fmOpenRead: LinuxFlags := LinuxFlags or O_RdOnly;
    fmOpenWrite: LinuxFlags := LinuxFlags or O_WrOnly;
    fmOpenReadWrite: LinuxFlags := LinuxFlags or O_RdWr;
  end;
  repeat
    fd := open(PAnsiChar(AnsiString(FileName)), LinuxFlags);
  until (fd <> -1) or (errno <> ESysEINTR);
  { Do not allow to open directories with FileOpen.
    This would cause weird behavior of TFileStream.Size,
    TMemoryStream.LoadFromFile etc. }
  if (fd <> -1) and IsHandleDirectory(fd) then begin
    close(fd);
    fd := -1;
  end;
  Result := IntToHandle(fd);
end;


function DoFileLocking(Handle: THandle; Mode: Integer): THandle;
var
  lockop, lockres, closeres, lockerr: Integer;
begin
  Result := Handle;
  if (Handle = INVALID_HANDLE_VALUE) then begin
    Exit;
  end;

  case (Mode and (fmShareExclusive or fmShareDenyWrite or fmShareDenyNone)) of
    fmShareExclusive:
      lockop := LOCK_EX or LOCK_NB;
    fmShareDenyWrite,
    fmShareDenyNone:
      lockop := LOCK_SH or LOCK_NB;
    else begin
      { fmShareDenyRead does not exit under *nix, only shared access
        (similar to fmShareDenyWrite) and exclusive access (same as
        fmShareExclusive)
      }
      repeat
        closeres := close(Handle);
      until (closeres <> -1) or (errno <> ESysEINTR);

      Result := INVALID_HANDLE_VALUE;
      Exit;
    end;
  end;

  repeat
    lockres := flock(Handle, lockop);
  until (lockres = 0) or (errno <> ESysEIntr);

  lockerr := errno;
  { Only return an error if locks are working and the file was already
    locked. Not if locks are simply unsupported (e.g., on Angstrom Linux
    you always get ESysNOLCK in the default configuration) }
  if (lockres <> 0) and ((lockerr = ESysEAGAIN) or (lockerr = EsysEDEADLK)) then begin
    repeat
      closeres := close(Handle);
    until (closeres <> -1) or (errno <> ESysEINTR);

    Result := INVALID_HANDLE_VALUE;
  end;
end;


function FileOpen(const FileName: string; Mode: Word): THandle;
begin
  Result := FileOpenNoLocking(FileName, Mode);
  Result := DoFileLocking(Result, Mode);
end;


function FileFlush(Handle: THandle): Boolean;
begin
  Result := False;
  if Handle = INVALID_HANDLE_VALUE then begin
    Exit;
  end;

  Result := (fsync(Handle) = 0);
end;


{
  The Diskfree and Disksize functions need a file on the specified drive, since this
  is required for the fpstatfs system call.
  These filenames are set in drivestr[0..26], and have been preset to :
   0 - '.'      (default drive - hence current dir is ok.)
   1 - '/fd0/.'  (floppy drive 1 - should be adapted to local system )
   2 - '/fd1/.'  (floppy drive 2 - should be adapted to local system )
   3 - '/'       (C: equivalent of dos is the root partition)
   4..26          (can be set by you're own applications)
  ! Use AddDisk() to Add new drives !
  They both return -1 when a failure occurs.
}

const
  FixDriveStr : array[0..3] of PAnsiChar = ('.', '/fd0/.', '/fd1/.', '/.');
var
  Drives: byte = 4;
  DriveStr: array[4..26] of PAnsiChar;


function DiskFree(Drive: Byte): int64;
var
  fs : _statvfs;
begin
  if (
      (Drive in [Low(FixDriveStr)..High(FixDriveStr)]) and
      (not (fixdrivestr[Drive] = nil)) and
      (statvfs(PAnsiChar(fixdrivestr[drive]), fs) <> -1)
  )
  or
  (
    (Drive in [Low(DriveStr)..High(DriveStr)]) and
    (not (drivestr[Drive] = nil)) and
    (statvfs(PAnsiChar(drivestr[drive]), fs) <> -1)
  )
  then begin
   Diskfree := int64(fs.f_bavail) * int64(fs.f_bsize);
  end
  else begin
   Diskfree := -1;
  end;
end;


function AddDisk(const Path:string): Byte;
var
  AnsiPath: AnsiString;
begin
  AnsiPath := AnsiString(Path);

  if not (DriveStr[Drives] = nil) then begin
    FreeMem(DriveStr[Drives]);
  end;

  GetMem(DriveStr[Drives], Length(AnsiPath) + 1);

  AnsiStrings.StrPCopy(DriveStr[Drives], AnsiPath);

  Result := Drives;

  Inc(Drives);

  if Drives > 26 then begin
    Drives := 4;
  end;
end;


function DiskSize(aDrive: Byte): Int64;
var
  fs : _statvfs;
begin
  if (
    (aDrive in [Low(FixDriveStr)..High(FixDriveStr)]) and
    (not (fixdrivestr[aDrive] = nil)) and
    (statvfs(PAnsiChar(fixdrivestr[aDrive]), fs) <> -1)
  )
  or
  (
    (aDrive in [Low(DriveStr)..High(DriveStr)]) and (not (drivestr[aDrive] = nil)) and (statvfs(PAnsiChar(drivestr[aDrive]), fs) <> -1)
  )
  then begin
    DiskSize := Int64(fs.f_blocks) * Int64(fs.f_bsize);
  end
  else begin
    DiskSize := -1;
  end;
end;


procedure FreeDriveStr;
var
  i: Integer;
begin
  for i := Low(drivestr) to High(drivestr) do begin
    if Assigned(drivestr[i]) then begin
      FreeMem(drivestr[i]);
      drivestr[i] := nil;
    end;
  end;
end;


initialization
  FillChar(DriveStr, SizeOf(DriveStr), 0);

finalization
  FreeDriveStr;

end.

