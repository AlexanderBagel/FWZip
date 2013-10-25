////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : FWZipStream
//  * Purpose   : ��������������� ������ ��� ��������� ���������� �� ����,
//  *           : � ���������� ��������� ZLib
//  * Author    : ��������� (Rouse_) ������
//  * Copyright : � Fangorn Wizards Lab 1998 - 2013.
//  * Version   : 1.0.9
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//
//  ������������ ���������:
//  ftp://ftp.info-zip.org/pub/infozip/doc/appnote-iz-latest.zip
//  http://zlib.net/zlib-1.2.5.tar.gz
//
//  �������� ���� ������:
//  ��� ��������� � ����� ������� ����� ������ ������� Deflate � ����
//  ���������� ����������� ��������� � ������� ������� ��������� ������.
//  �.�. � ����� ���������� ���� ������ � ������ ����.
//  ��� ���������� ���������� ������ ��������� ������������.
//  ������ ����� ��������� �������� ������ ��������� "�� ����"
//  ��������� ��������� ��� �������� ����.
//  ��� ��������� ������������ � ������������ � ������������� � ������ Read.
//  ���-�� �����, �������� ����������� ����� ����� ��������,
//  ��������� ����������� ���������� � ���������� ������������ ������.
//  ���������� ������������ � ������ Write, � ���� ������ ����� ��������
//  ����������� ����� TCompressionStream � �������������� �������.
//  ������������ �������������� � ������ Read, � ���� ������ ����� ��������
//  ����������� ����� ������� �� ������� �
//  �������������� ������� � TDecompressionStream.
//

unit FWZipStream;

interface

{$I fwzip.inc}

uses
  Classes,
  FWZipConsts,
  FWZipCrypt,
  FWZipCrc32,
  {$IFDEF USE_ZLIB_EX}
  {$UNDEF USE_ZLIB_DLL}
  ZLibEx,
  ZLibExApi
  {$ELSE}
    {$IFDEF USE_ZLIB_DLL}
    ZLib_external
    {$ELSE}
    ZLib
    {$ENDIF}
  {$ENDIF};

type
  TFWZipItemStream = class(TStream)
  private
    FOwner: TStream;
    FCryptor: TFWZipCryptor;
    FDecryptor: TFWZipDecryptor;
    FSize, FStart, FPosition: Int64;
    FHeader: Word;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(AOwner: TStream; Cryptor: TFWZipCryptor;
      Decryptor: TFWZipDecryptor; CompressLevel: Byte; ASize: Int64);
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

implementation

{ TFWZipItemStream }

constructor TFWZipItemStream.Create(AOwner: TStream; Cryptor: TFWZipCryptor;
  Decryptor: TFWZipDecryptor; CompressLevel: Byte; ASize: Int64);
begin
  inherited Create;
  FOwner := AOwner;
  FCryptor := Cryptor;
  FDecryptor := Decryptor;
  // Rouse_ 17.03.2011
  // ��������� ���-�� ����� ������� ���������� �� ������ ���������
  FSize := ASize + 2;
  FStart := AOwner.Position;
  FPosition := 0;
  // ��������������� ����������� ��������� ZLib ������
  // ��. deflate.c - int ZEXPORT deflate (strm, flush)

  // uInt header = (Z_DEFLATED + ((s->w_bits-8)<<4)) << 8;
  FHeader := (Z_DEFLATED + (7 {32k Window size} shl 4)) shl 8;

  // if (s->strategy >= Z_HUFFMAN_ONLY || s->level < 2)
  //     level_flags = 0;
  // else if (s->level < 6)
  //     level_flags = 1;
  // else if (s->level == 6)
  //     level_flags = 2;
  // else
  //     level_flags = 3;
  //
  // ��� CompressLevel (level_flags)
  // ������� �� ��� ������������ GeneralPurposeBitFlag
  // ����� �� �� ������� ����� ��������������� ������������ ��������

  case CompressLevel of
    PBF_COMPRESS_SUPERFAST:
      CompressLevel := 0;
    PBF_COMPRESS_FAST:
      CompressLevel := 1;
    PBF_COMPRESS_NORMAL:
      CompressLevel := 2;
    PBF_COMPRESS_MAXIMUM:
      CompressLevel := 3;
  end;

  // header |= (level_flags << 6);
  FHeader := FHeader or (CompressLevel shl 6);

  // if (s->strstart != 0) header |= PRESET_DICT;
  // ������� �� ������������ - ��������� ��� ���������

  // header += 31 - (header % 31);
  Inc(FHeader, 31 - (FHeader mod 31));

  // putShortMSB(s, header);
  FHeader := (FHeader shr 8) + (FHeader and $FF) shl 8;
end;

function TFWZipItemStream.GetSize: Int64;
begin
  Result := FSize;
end;

function TFWZipItemStream.Read(var Buffer; Count: Integer): Longint;
var
  P: PByte;
  DecryptBuff: Pointer;
begin
  if FPosition = 0 then
  begin
    // ���� ������������ ������ � ������ ������
    // ���������� ����� ���� ���������� ��������� ZLib
    P := @FHeader;
    Move(P^, Buffer, 2);
    FOwner.Position := FStart;
    P := @Buffer;
    Inc(P, 2);
    if Count > Size then
      Count := Size;
    FOwner.Position := FStart;
    if FDecryptor <> nil then
    begin
      // � ������ ���� ���� ����������, ���������� ����������� �����
      GetMem(DecryptBuff, Count - 2);
      try
        Result := FOwner.Read(DecryptBuff^, Count - 2);
        FDecryptor.DecryptBuffer(DecryptBuff, Result);
        Move(DecryptBuff^, P^, Result);
      finally
        FreeMem(DecryptBuff);
      end;
    end
    else
      Result := FOwner.Read(P^, Count - 2);
    Inc(Result, 2);
    Inc(FPosition, Result);
  end
  else
  begin
    FOwner.Position := FStart + Position - 2;
    if Count > Size - Position then
      Count := Size - Position;
    if FDecryptor <> nil then
    begin
      // � ������ ���� ���� ����������, ���������� ����������� �����
      GetMem(DecryptBuff, Count);
      try
        Result := FOwner.Read(DecryptBuff^, Count);
        FDecryptor.DecryptBuffer(DecryptBuff, Result);
        P := @Buffer;
        Move(DecryptBuff^, P^, Result);
      finally
        FreeMem(DecryptBuff);
      end;
    end
    else
      Result := FOwner.Read(Buffer, Count);
    Inc(FPosition, Result);
  end;
end;

function TFWZipItemStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := Seek(Int64(Offset), TSeekOrigin(Origin));
end;

function TFWZipItemStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := Size + Offset;
  end;
  Result := FPosition;
end;

function TFWZipItemStream.Write(const Buffer; Count: Integer): Longint;
var
  EncryptBuffer: PByte;
begin
  if FCryptor = nil then
    Result := FOwner.Write(Buffer, Count)
  else
  begin
    // �������� �����
    GetMem(EncryptBuffer, Count);
    try
      Move(Buffer, EncryptBuffer^, Count);
      // ��������� ���� ����� ��������� ���������� ��������� ZLib
      if FPosition = 0 then
      begin
        Inc(EncryptBuffer, 2);
        FCryptor.EncryptBuffer(EncryptBuffer, Count - 2);
        Dec(EncryptBuffer, 2);
      end
      else
        FCryptor.EncryptBuffer(EncryptBuffer, Count);
      Result := FOwner.Write(EncryptBuffer^, Count);
    finally
      FreeMem(EncryptBuffer);
    end;
  end;
  Inc(FPosition, Result);
end;

end.
