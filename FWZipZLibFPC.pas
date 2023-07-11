////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : FWZipZLibFPC
//  * Purpose   : Линковка ZLib для Free Pascal
//  * Author    : Александр (Rouse_) Багель
//  * Copyright : © Fangorn Wizards Lab 1998 - 2023.
//  * Version   : 2.0.0
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Stable Release : http://rouse.drkb.ru/components.php#fwzip
//  * Latest Source  : https://github.com/AlexanderBagel/FWZip
//  ****************************************************************************
//
//  Используемые источники:
//  ftp://ftp.info-zip.org/pub/infozip/doc/appnote-iz-latest.zip
//  https://zlib.net/zlib-1.2.13.tar.gz
//  http://www.base2ti.com/
//

unit FWZipZLibFPC;

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+}
{$ELSE}
  {$MESSAGE ERROR 'FPC ONLY!!!'}
{$ENDIF}

interface

const
  {** version ids *******************************************************************************}

  ZLIB_VERSION: PAnsiChar = '1.2.13';

  ZLIB_VERNUM = $12D0;

  ZLIB_VER_MAJOR = 1;
  ZLIB_VER_MINOR = 2;
  ZLIB_VER_REVISION = 13;
  ZLIB_VER_SUBREVISION = 0;

  {** return codes ******************************************************************************}

  Z_OK            = 0;
  Z_STREAM_END    = 1;
  Z_NEED_DICT     = 2;
  Z_ERRNO         = (-1);
  Z_STREAM_ERROR  = (-2);
  Z_DATA_ERROR    = (-3);
  Z_MEM_ERROR     = (-4);
  Z_BUF_ERROR     = (-5);
  Z_VERSION_ERROR = (-6);

  {** compression levels ************************************************************************}

  Z_NO_COMPRESSION      =   0;
  Z_BEST_SPEED          =   1;
  Z_BEST_COMPRESSION    =   9;
  Z_DEFAULT_COMPRESSION = (-1);

type
  TZAlloc = function (opaque: Pointer; items, size: Integer): Pointer; cdecl;
  TZFree  = procedure (opaque, block: Pointer); cdecl;

  {** TZStreamRec *******************************************************************************}

  {$IFDEF LINUX}
  LongwordType = NativeUInt;
  {$ELSE}
  LongwordType = Longword;
  {$ENDIF}

  TZStreamRec = packed record
    next_in  : PByte;         // next input byte
    avail_in : Cardinal;      // number of bytes available at next_in
    total_in : LongwordType;  // total nb of input bytes read so far

    next_out : PByte;         // next output byte should be put here
    avail_out: Cardinal;      // remaining free space at next_out
    total_out: LongwordType;  // total nb of bytes output so far

    msg      : PAnsiChar;     // last error message, NULL if no error
    state    : Pointer;       // not visible by applications

    zalloc   : TZAlloc;       // used to allocate the internal state
    zfree    : TZFree;        // used to free the internal state
    opaque   : Pointer;       // private data object passed to zalloc and zfree

    data_type: Integer;       // best guess about the data type: ascii or binary
    adler    : LongwordType;  // adler32 value of the uncompressed data
    reserved : LongwordType;  // reserved for future use
  end;

{** external routines ***************************************************************************}

  function deflateInit_(var strm: TZStreamRec; level: Integer;
    version: PAnsiChar; recsize: Integer): Integer; cdecl;

  function deflateInit2_(var strm: TZStreamRec; level, method, windowBits,
    memLevel, strategy: Integer; version: PAnsiChar; recsize: Integer): Integer; cdecl;

  function deflate(var strm: TZStreamRec; flush: Integer): Integer; cdecl;

  function deflateEnd(var strm: TZStreamRec): Integer; cdecl;

  function deflateReset(var strm: TZStreamRec): Integer; cdecl;

  function inflateInit_(var strm: TZStreamRec; version: PAnsiChar;
    recsize: Integer): Integer; cdecl;

  function inflateInit2_(var strm: TZStreamRec; windowBits: Integer;
    version: PAnsiChar; recsize: Integer): Integer; cdecl;

  function inflate(var strm: TZStreamRec; flush: Integer): Integer; cdecl;

  function inflateEnd(var strm: TZStreamRec): Integer; cdecl;

  function inflateReset(var strm: TZStreamRec): Integer; cdecl;

  function adler32(adler: Longint; const buf; len: Integer): Longint; cdecl;

  function crc32(crc: Longint; const buf; len: Integer): Longint; cdecl;

implementation

  {$IFDEF MSWINDOWS}
    {$IFDEF CPU64}
      {$LINKLIB fpc_lib\libzlib_coff_win_amd64.a}
    {$ELSE}
      {$LINKLIB fpc_lib\libzlib_coff_x386.a}
    {$ENDIF}
  {$ELSE}
    {$IFDEF CPU64}
      {$LINKLIB fpc_lib\libzlib_elf64_for_x86-64.a}
    {$ELSE}
      {$LINKLIB fpc_lib\libzlib_elf_intel_386.a}
    {$ENDIF}
  {$ENDIF}


{** external routines ***************************************************************************}

function deflateInit_ external;
function deflateInit2_ external;
function deflate external;
function deflateEnd external;
function deflateReset external;
function inflateInit_ external;
function inflateInit2_ external;
function inflate external;
function inflateEnd external;
function inflateReset external;
function adler32 external;
function crc32 external;

{** c function implementations ******************************************************************}

{$IFDEF MSWINDOWS}

function malloc(size : size_t): Pointer; cdecl;
  public name {$IFDEF CPU32}'_malloc'{$ELSE}'malloc'{$ENDIF};
begin
  GetMem(result, size);
end;

procedure free(block: Pointer); cdecl;
  public name {$IFDEF CPU32}'_free'{$ELSE}'free'{$ENDIF};
begin
  FreeMem(block);
end;

function memset(dest: Pointer; val: Integer; count: size_t): Pointer; cdecl;
  public name {$IFDEF CPU32}'_memset'{$ELSE}'memset'{$ENDIF};
begin
  FillChar(dest^, count, val);
  Result := dest;
end;

procedure memcpy(dest, src: Pointer; count: size_t); cdecl;
  public name {$IFDEF CPU32}'_memcpy'{$ELSE}'memcpy'{$ENDIF};
begin
  Move(src^, dest^, count);
end;

{$ENDIF}

end.
