////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : FWZipZLib
//  * Purpose   : Базовые стримы сжатия и распаковки.
//  *           : Вынесено из ZLibEx в отдельный модуль
//  *           : для совместимости со старыми версиями Delphi
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

{*************************************************************************************************
*  ZLibEx.pas                                                                                    *
*                                                                                                *
*  copyright (c) 2000-2013 base2 technologies                                                    *
*  copyright (c) 1995-2002 Borland Software Corporation                                          *
*                                                                                                *
*************************************************************************************************}

unit FWZipZLib;

interface

{$I fwzip.inc}

uses
  Classes,
  SysUtils,
  {$IFDEF USE_ZLIB_EX}
  ZLibExApi
  {$ELSE}
    {$IFDEF USE_ZLIB_DLL}
    ZLib_external
    {$ELSE}
    ZLib
    {$ENDIF}
  {$ENDIF};

type
  TStreamPos = Int64;

  TCompressionLevel = (
    clNone,
    clFastest,
    clDefault,
    clMax,
    clLevel1,
    clLevel2,
    clLevel3,
    clLevel4,
    clLevel5,
    clLevel6,
    clLevel7,
    clLevel8,
    clLevel9
  );

  TZStrategy = (
    zsDefault,
    zsFiltered,
    zsHuffman,
    zsRLE,
    zsFixed
  );

  TZError = (
    zeError,
    zeStreamError,
    zeDataError,
    zeMemoryError,
    zeBufferError,
    zeVersionError
  );

  TZFlush = (
    zfNoFlush,
    zfPartialFlush,
    zfSyncFlush,
    zfFullFlush,
    zfFinish,
    zfBlock,
    zfTrees
  );

const
  ZLevels: Array [TCompressionLevel] of Integer = (
    Z_NO_COMPRESSION,       // zcNone
    Z_BEST_SPEED,           // zcFastest
    Z_DEFAULT_COMPRESSION,  // zcDefault
    Z_BEST_COMPRESSION,     // zcMax
    1,                      // zcLevel1
    2,                      // zcLevel2
    3,                      // zcLevel3
    4,                      // zcLevel4
    5,                      // zcLevel5
    6,                      // zcLevel6
    7,                      // zcLevel7
    8,                      // zcLevel8
    9                       // zcLevel9
  );

  {** compression methods ***********************************************************************}

  Z_DEFLATED = 8;

  {** compression levels ************************************************************************}

  Z_NO_COMPRESSION      =   0;
  Z_BEST_SPEED          =   1;
  Z_BEST_COMPRESSION    =   9;
  Z_DEFAULT_COMPRESSION = (-1);

  {** flush constants ***************************************************************************}

  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;
  Z_BLOCK         = 5;
  Z_TREES         = 6;

  {** compression strategies ********************************************************************}

  Z_FILTERED         = 1;
  Z_HUFFMAN_ONLY     = 2;
  Z_RLE              = 3;
  Z_FIXED            = 4;
  Z_DEFAULT_STRATEGY = 0;

  ZStrategies: Array [TZStrategy] of Integer = (
    Z_DEFAULT_STRATEGY,     // zsDefault
    Z_FILTERED,             // zsFiltered
    Z_HUFFMAN_ONLY,         // zsHuffman
    Z_RLE,                  // zsRLE
    Z_FIXED                 // zsFixed
  );

  ZErrors: Array [TZError] of Integer = (
    Z_ERRNO,                // zeError
    Z_STREAM_ERROR,         // zeStreamError
    Z_DATA_ERROR,           // zeDataError
    Z_MEM_ERROR,            // zeMemoryError
    Z_BUF_ERROR,            // zeBufferError
    Z_VERSION_ERROR         // zeVersionError
  );

  ZFlushes: Array [TZFlush] of Integer = (
    Z_NO_FLUSH,             // zfNoFlush
    Z_PARTIAL_FLUSH,        // zfPartialFlush
    Z_SYNC_FLUSH,           // zfSyncFlush
    Z_FULL_FLUSH,           // zfFullFlush
    Z_FINISH,               // zfFinish
    Z_BLOCK,                // zfBlock
    Z_TREES                 // zfTrees
  );

  {** return code messages **********************************************************************}

  z_errmsg: Array [0..9] of String = (
    'Need dictionary',      // Z_NEED_DICT      (2)
    'Stream end',           // Z_STREAM_END     (1)
    'OK',                   // Z_OK             (0)
    'File error',           // Z_ERRNO          (-1)
    'Stream error',         // Z_STREAM_ERROR   (-2)
    'Data error',           // Z_DATA_ERROR     (-3)
    'Insufficient memory',  // Z_MEM_ERROR      (-4)
    'Buffer error',         // Z_BUF_ERROR      (-5)
    'Incompatible version', // Z_VERSION_ERROR  (-6)
    ''
  );

type

  {** TCustomZStream ****************************************************************************}

  TCustomZStream = class(TStream)
  private
    FStream    : TStream;
    FStreamPos : TStreamPos;
    FOnProgress: TNotifyEvent;

    FZStream   : TZStreamRec;
    FBuffer    : Array [Word] of Byte;

    function  GetStreamPosition: TStreamPos;
    procedure SetStreamPosition(value: TStreamPos);
  protected
    constructor Create(stream: TStream);

    function  StreamRead(var buffer; count: Longint): Longint;
    function  StreamWrite(const buffer; count: Longint): Longint;
    function  StreamSeek(offset: Longint; origin: Word): Longint;

    procedure StreamReadBuffer(var buffer; count: Longint);
    procedure StreamWriteBuffer(const buffer; count: Longint);

    procedure DoProgress; dynamic;

    property StreamPosition: TStreamPos read GetStreamPosition write SetStreamPosition;

    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

  {** TZCompressionStream ***********************************************************************}

  TZCompressionStream = class(TCustomZStream)
  private
    function GetCompressionRate: Single;
  public
    constructor Create(dest: TStream;
      compressionLevel: TCompressionLevel = clDefault); overload;

    constructor Create(dest: TStream; compressionLevel: TCompressionLevel;
      windowBits, memLevel: Integer; strategy: TZStrategy); overload;

    destructor  Destroy; override;

    function  Read(var buffer; count: Longint): Longint; override;
    function  Write(const buffer; count: Longint): Longint; override;
    function  Seek(offset: Longint; origin: Word): Longint; override;

    property CompressionRate: Single read GetCompressionRate;
    property OnProgress;
  end;

  {** TZDecompressionStream *********************************************************************}

  TZDecompressionStream = class(TCustomZStream)
  public
    constructor Create(source: TStream); overload;
    constructor Create(source: TStream; windowBits: Integer); overload;

    destructor  Destroy; override;

    function  Read(var buffer; count: Longint): Longint; override;
    function  Write(const buffer; count: Longint): Longint; override;
    function  Seek(offset: Longint; origin: Word): Longint; override;

    property OnProgress;
  end;

type
  EZLibErrorClass = class of EZlibError;

  EZLibError = class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor Create(code: Integer; const dummy: String = ''); overload;
    constructor Create(error: TZError; const dummy: String = ''); overload;

    property ErrorCode: Integer read FErrorCode write FErrorCode;
  end;

  EZCompressionError = class(EZLibError);
  EZDecompressionError = class(EZLibError);

implementation

const
  SZInvalid = 'Invalid ZStream operation!';

function ZCompressCheck(code: Integer): Integer;
begin
  result := code;

  if code < 0 then
  begin
    raise EZCompressionError.Create(code);
  end;
end;

function ZDecompressCheck(code: Integer; raiseBufferError: Boolean = True): Integer;
begin
  Result := code;

  if code < 0 then
  begin
    if (code <> Z_BUF_ERROR) or raiseBufferError then
    begin
      raise EZDecompressionError.Create(code);
    end;
  end;
end;

{** zlib deflate routines ***********************************************************************}

function ZDeflateInit(var stream: TZStreamRec;
  level: TCompressionLevel): Integer;
begin
  result := deflateInit_(stream, ZLevels[level], ZLIB_VERSION,
    SizeOf(TZStreamRec));
end;

function ZDeflateInit2(var stream: TZStreamRec;
  level: TCompressionLevel; windowBits, memLevel: Integer;
  strategy: TZStrategy): Integer;
begin
  {$IFDEF OLDEST_ZLIB}
  result := ZDeflateInit(stream, level);
  {$ELSE}
  result := deflateInit2_(stream, ZLevels[level], Z_DEFLATED, windowBits,
    memLevel, ZStrategies[strategy], ZLIB_VERSION, SizeOf(TZStreamRec));
  {$ENDIF}
end;

function ZDeflate(var stream: TZStreamRec; flush: TZFlush): Integer;
begin
  result := deflate(stream, ZFlushes[flush]);
end;

function ZDeflateEnd(var stream: TZStreamRec): Integer;
begin
  result := deflateEnd(stream);
end;

{** zlib inflate routines ***********************************************************************}

function ZInflateInit(var stream: TZStreamRec): Integer;
begin
  result := inflateInit_(stream, ZLIB_VERSION, SizeOf(TZStreamRec));
end;

function ZInflateInit2(var stream: TZStreamRec;
  windowBits: Integer): Integer;
begin
  {$IFDEF OLDEST_ZLIB}
  result := ZInflateInit(stream);
  {$ELSE}
  result := inflateInit2_(stream, windowBits, ZLIB_VERSION,
    SizeOf(TZStreamRec));
  {$ENDIF}
end;

function ZInflate(var stream: TZStreamRec; flush: TZFlush): Integer;
begin
  result := inflate(stream, ZFlushes[flush]);
end;

function ZInflateEnd(var stream: TZStreamRec): Integer;
begin
  result := inflateEnd(stream);
end;

function ZInflateReset(var stream: TZStreamRec): Integer;
begin
  result := inflateReset(stream);
end;

{** EZLibError **********************************************************************************}

constructor EZLibError.Create(code: Integer; const dummy: String);
begin
  inherited Create(z_errmsg[2 - code]);

  FErrorCode := code;
end;

constructor EZLibError.Create(error: TZError; const dummy: String);
begin
  Create(ZErrors[error], dummy);
end;

{** TCustomZStream ******************************************************************************}

constructor TCustomZStream.Create(stream: TStream);
begin
  inherited Create;

  FStream := stream;
  FStreamPos := stream.Position;

  {$IFDEF OLDEST_ZLIB}
    FZStream.zalloc := zlibAllocMem;
    FZStream.zfree := zlibFreeMem;
  {$ENDIF}
end;

function TCustomZStream.StreamRead(var buffer; count: Longint): Longint;
begin
  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  result := FStream.Read(buffer,count);

  FStreamPos := FStreamPos + result;
end;

function TCustomZStream.StreamWrite(const buffer; count: Longint): Longint;
begin
  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  result := FStream.Write(buffer,count);

  FStreamPos := FStreamPos + result;
end;

function TCustomZStream.StreamSeek(offset: Longint; origin: Word): Longint;
begin
  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  result := FStream.Seek(offset,origin);

  FStreamPos := FStream.Position;
end;

procedure TCustomZStream.StreamReadBuffer(var buffer; count: Longint);
begin
  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  FStream.ReadBuffer(buffer,count);

  FStreamPos := FStreamPos + count;
end;

procedure TCustomZStream.StreamWriteBuffer(const buffer; count: Longint);
begin
  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  FStream.WriteBuffer(buffer,count);

  FStreamPos := FStreamPos + count;
end;

procedure TCustomZStream.DoProgress;
begin
  if Assigned(FOnProgress) then FOnProgress(Self);
end;

function TCustomZStream.GetStreamPosition: TStreamPos;
begin
  result := FStream.Position;
end;

procedure TCustomZStream.SetStreamPosition(value: TStreamPos);
begin
  FStream.Position := value;
  FStreamPos := FStream.Position;
end;

{** TZCompressionStream *************************************************************************}

constructor TZCompressionStream.Create(dest: TStream;
  compressionLevel: TCompressionLevel);
begin
  inherited Create(dest);

  FZStream.next_out := @FBuffer;
  FZStream.avail_out := SizeOf(FBuffer);

  ZCompressCheck(ZDeflateInit(FZStream, compressionLevel));
end;

constructor TZCompressionStream.Create(dest: TStream;
  compressionLevel: TCompressionLevel; windowBits, memLevel: Integer;
  strategy: TZStrategy);
begin
  {$IFDEF USE_AUTOGENERATED_ZLIB_HEADER}
  Create(dest, compressionLevel);
  {$ELSE}
  inherited Create(dest);

  FZStream.next_out := @FBuffer;
  FZStream.avail_out := SizeOf(FBuffer);

  ZCompressCheck(ZDeflateInit2(FZStream, compressionLevel, windowBits,
    memLevel, strategy));
  {$ENDIF}
end;

destructor TZCompressionStream.Destroy;
begin
  FZStream.next_in := Nil;
  FZStream.avail_in := 0;

  try
    while ZCompressCheck(ZDeflate(FZStream, zfFinish)) <> Z_STREAM_END do
    begin
      StreamWriteBuffer(FBuffer, SizeOf(FBuffer) - FZStream.avail_out);

      FZStream.next_out := @FBuffer;
      FZStream.avail_out := SizeOf(FBuffer);
    end;

    if FZStream.avail_out < SizeOf(FBuffer) then
    begin
      StreamWriteBuffer(FBuffer, SizeOf(FBuffer) - FZStream.avail_out);
    end;
  finally
    ZDeflateEnd(FZStream);
  end;

  inherited Destroy;
end;

function TZCompressionStream.Read(var buffer; count: Longint): Longint;
begin
  raise EZCompressionError.Create(SZInvalid);
end;

function TZCompressionStream.Write(const buffer; count: Longint): Longint;
var
  writeCount: Longint;
begin
  result := count;

  FZStream.next_in := @buffer;
  FZStream.avail_in := count;

  while FZStream.avail_in > 0 do
  begin
    ZCompressCheck(ZDeflate(FZStream, zfNoFlush));

    if FZStream.avail_out = 0 then
    begin
      writeCount := StreamWrite(FBuffer,SizeOf(FBuffer));

      if writeCount = SizeOf(FBuffer) then
      begin
        FZStream.next_out := @FBuffer;
        FZStream.avail_out := SizeOf(FBuffer);

        DoProgress;
      end
      else
      begin
        StreamPosition := StreamPosition - writeCount;

        result := Cardinal(count) - Cardinal(FZStream.avail_in);

        FZStream.avail_in := 0;
      end;
    end;
  end;
end;

function TZCompressionStream.Seek(offset: Longint; origin: Word): Longint;
begin
  if (offset = 0) and (origin = soFromCurrent) then
  begin
    result := FZStream.total_in;
  end
  else raise EZCompressionError.Create(SZInvalid);
end;

function TZCompressionStream.GetCompressionRate: Single;
begin
  if FZStream.total_in = 0 then result := 0
  else result := (1.0 - (FZStream.total_out / FZStream.total_in)) * 100.0;
end;

{** TZDecompressionStream ***********************************************************************}

constructor TZDecompressionStream.Create(source: TStream);
begin
  inherited Create(source);

  FZStream.next_in := @FBuffer;
  FZStream.avail_in := 0;

  ZDecompressCheck(ZInflateInit(FZStream));
end;

constructor TZDecompressionStream.Create(source: TStream;
  windowBits: Integer);
begin
  {$IFDEF USE_AUTOGENERATED_ZLIB_HEADER}
  Create(source);
  {$ELSE}
  inherited Create(source);

  FZStream.next_in := @FBuffer;
  FZStream.avail_in := 0;

  ZDecompressCheck(ZInflateInit2(FZStream, windowBits));
  {$ENDIF}
end;

destructor TZDecompressionStream.Destroy;
begin
  ZInflateEnd(FZStream);

  inherited Destroy;
end;

function TZDecompressionStream.Read(var buffer; count: Longint): Longint;
var
  zresult: Integer;
begin
  FZStream.next_out := @buffer;
  FZStream.avail_out := count;

  zresult := Z_OK;

  while (FZStream.avail_out > 0) and (zresult <> Z_STREAM_END) do
  begin
    if FZStream.avail_in = 0 then
    begin
      FZStream.avail_in := StreamRead(FBuffer,SizeOf(FBuffer));

      if FZStream.avail_in = 0 then
      begin
        result := Cardinal(count) - Cardinal(FZStream.avail_out);

        Exit;
      end;

      FZStream.next_in := @FBuffer;

      DoProgress;
    end;

    zresult := ZDecompressCheck(ZInflate(FZStream, zfNoFlush));
  end;

  if (zresult = Z_STREAM_END) and (FZStream.avail_in > 0) then
  begin
    StreamPosition := StreamPosition - FZStream.avail_in;

    FZStream.avail_in := 0;
  end;

  result := Cardinal(count) - Cardinal(FZStream.avail_out);
end;

function TZDecompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EZDecompressionError.Create(SZInvalid);
end;

function TZDecompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  buf: Array [0..8191] of Byte;
  i  : Integer;
begin
  if (offset = 0) and (origin = soFromBeginning) then
  begin
    ZDecompressCheck(ZInflateReset(FZStream));

    FZStream.next_in := @FBuffer;
    FZStream.avail_in := 0;

    StreamPosition := 0;
  end
  else if ((offset >= 0) and (origin = soFromCurrent)) or
          (((Cardinal(offset) - Cardinal(FZStream.total_out)) > 0) and
          (origin = soFromBeginning)) then
  begin
    if origin = soFromBeginning then Dec(offset, FZStream.total_out);

    if offset > 0 then
    begin
      for i := 1 to offset div SizeOf(buf) do ReadBuffer(buf, SizeOf(buf));
      ReadBuffer(buf, offset mod SizeOf(buf));
    end;
  end
  else if (offset = 0) and (origin = soFromEnd) then
  begin
    while Read(buf, SizeOf(buf)) > 0 do ;
  end
  else raise EZDecompressionError.Create(SZInvalid);

  result := FZStream.total_out;
end;


end.
