////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : FWZipCrc32
//  * Purpose   : Набор функций для рассчета контрольной суммы блока данных
//  *           : Класс TFWZipCRC32Stream используется в качестве посредника
//  *           : между двумя стримами и предназначен для бастрого
//  *           : рассчета контрольной суммы передаваемых блоков данных
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

unit FWZipCrc32;

interface

uses
  Classes,
  SysUtils,
  FWZipConsts;

type
  TFWZipCRC32Stream = class(TStream)
  private
    FOwner: TStream;
    FCRC32: Cardinal;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(AOwner: TStream);
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function CRC32: Cardinal;
  end;

  function CRC32Calc(CurrCRC: Cardinal;
    Buffer: PByte; const BufferLen: Int64): Cardinal; overload;
  function CRC32Calc(Buffer: PByte; const BufferLen: Int64): Cardinal; overload;
  function FileCRC32(const FileName: string): Cardinal;

implementation


function CRC32Calc(CurrCRC: Cardinal;
  Buffer: PByte; const BufferLen: Int64): Cardinal;
var
  I: Integer;
begin
  Result := CurrCRC;
  for I := 0 to BufferLen - 1 do
  begin
    Result := ((Result shr 8) and $00FFFFFF) xor
      CRC32Table[(Result xor Buffer^) and $FF];
    Inc(Buffer);
  end;
end;

function CRC32Calc(Buffer: PByte; const BufferLen: Int64): Cardinal;
begin
  Result := CRC32Calc($FFFFFFFF, Buffer, BufferLen) xor $FFFFFFFF;
end;

function FileCRC32(const FileName: string): Cardinal;
var
  Buff: Pointer;
  F: TFileStream;
  Size: Integer;
begin
  Result := $FFFFFFFF;
  GetMem(Buff, $FFFF);
  try
    F := TFileStream.Create(FileName, fmOpenRead);
    try
      Size := 1;
      while Size > 0 do
      begin
        Size := F.Read(Buff^, $FFFF);
        Result := CRC32Calc(Result, Buff, Size);
      end;
    finally
      F.Free;
    end;
  finally
    FreeMem(Buff);
  end;
  Result := Result xor $FFFFFFFF;
end;

{ TFWZipCRC32Stream }

function TFWZipCRC32Stream.CRC32: Cardinal;
begin
  Result := FCRC32 xor $FFFFFFFF;
end;

constructor TFWZipCRC32Stream.Create(AOwner: TStream);
begin
  FOwner := AOwner;
  FCRC32 := $FFFFFFFF;
end;

function TFWZipCRC32Stream.GetSize: Int64;
begin
  Result := FOwner.Size;
end;

function TFWZipCRC32Stream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := FOwner.Read(Buffer, Count);
  FCRC32 := CRC32Calc(FCRC32, @Buffer, Result);
end;

function TFWZipCRC32Stream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := FOwner.Seek(Offset, Origin);
end;

function TFWZipCRC32Stream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  Result := FOwner.Seek(Offset, Origin);
end;

function TFWZipCRC32Stream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FOwner.Write(Buffer, Count);
  FCRC32 := CRC32Calc(FCRC32, @Buffer, Result);
end;

end.
