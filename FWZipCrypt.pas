////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : FWZipCrypt
//  * Purpose   : Реализация криптографии по методу PKWARE
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

unit FWZipCrypt;

interface

  // Переполнения и выход за диапазон неизбежны
  // поэтому отключаем данные проверки
  {$OVERFLOWCHECKS OFF}
  {$RANGECHECKS OFF}

uses
  Windows,
  Classes,
  FWZipConsts;

{
XIII. Decryption
----------------

The encryption used in PKZIP was generously supplied by Roger
Schlafly.  PKWARE is grateful to Mr. Schlafly for his expert
help and advice in the field of data encryption.

PKZIP encrypts the compressed data stream.  Encrypted files must
be decrypted before they can be extracted.

Each encrypted file has an extra 12 bytes stored at the start of
the data area defining the encryption header for that file.  The
encryption header is originally set to random values, and then
itself encrypted, using three, 32-bit keys.  The key values are
initialized using the supplied encryption password.  After each byte
is encrypted, the keys are then updated using pseudo-random number
generation techniques in combination with the same CRC-32 algorithm
used in PKZIP and described elsewhere in this document.

The following is the basic steps required to decrypt a file:

1) Initialize the three 32-bit keys with the password.
2) Read and decrypt the 12-byte encryption header, further
   initializing the encryption keys.
3) Read and decrypt the compressed data stream using the
   encryption keys.


Step 1 - Initializing the encryption keys
-----------------------------------------

Key(0) <- 305419896
Key(1) <- 591751049
Key(2) <- 878082192

loop for i <- 0 to length(password)-1
    update_keys(password(i))
end loop


Where update_keys() is defined as:


update_keys(char):
  Key(0) <- crc32(key(0),char)
  Key(1) <- Key(1) + (Key(0) & 000000ffH)
  Key(1) <- Key(1) * 134775813 + 1
  Key(2) <- crc32(key(2),key(1) >> 24)
end update_keys


Where crc32(old_crc,char) is a routine that given a CRC value and a
character, returns an updated CRC value after applying the CRC-32
algorithm described elsewhere in this document.


Step 2 - Decrypting the encryption header
-----------------------------------------

The purpose of this step is to further initialize the encryption
keys, based on random data, to render a plaintext attack on the
data ineffective.


Read the 12-byte encryption header into Buffer, in locations
Buffer(0) thru Buffer(11).

loop for i <- 0 to 11
    C <- buffer(i) ^ decrypt_byte()
    update_keys(C)
    buffer(i) <- C
end loop


Where decrypt_byte() is defined as:


unsigned char decrypt_byte()
    local unsigned short temp
    temp <- Key(2) | 2
    decrypt_byte <- (temp * (temp ^ 1)) >> 8
end decrypt_byte


After the header is decrypted,  the last 1 or 2 bytes in Buffer
should be the high-order word/byte of the CRC for the file being
decrypted, stored in Intel low-byte/high-byte order, or the high-order
byte of the file time if bit 3 of the general purpose bit flag is set.
Versions of PKZIP prior to 2.0 used a 2 byte CRC check; a 1 byte CRC check is
used on versions after 2.0.  This can be used to test if the password
supplied is correct or not.


Step 3 - Decrypting the compressed data stream
----------------------------------------------

The compressed data stream can be decrypted as follows:


loop until done
    read a character into C
    Temp <- C ^ decrypt_byte()
    update_keys(temp)
    output Temp
end loop
}

const
  EncryptedHeaderSize = 12;
  LastEncryptedHeaderByte = EncryptedHeaderSize - 1;

type
  TZipKeys = array [0..2] of Cardinal;

  TFWZipKeys = class
  private
    FKeys: TZipKeys;
  protected
    procedure UpdateKeys(Value: Byte);
    function DecryptByte: Byte;
  public
    constructor Create(const Password: AnsiString);
  end;

  TFWZipCryptor = class(TFWZipKeys)
  protected
    function EncryptByte(Value: Byte): Byte;
  public
    procedure GenerateEncryptionHeader(Stream: TStream;
      IsDescryptorFlagPresent: Boolean;
      CRC32, FileDate: Cardinal);
    procedure EncryptBuffer(Buffer: PByte; Size: Int64);
  end;

  TFWZipDecryptor = class(TFWZipKeys)
  public
    function LoadEncryptionHeader(Stream: TStream;
      IsDescryptorFlagPresent: Boolean;
      CRC32, FileDate: Cardinal): Boolean;
    procedure DecryptBuffer(Buffer: PByte; Size: Int64);
  end;

implementation

const
  DefaultKeys: TZipKeys = (305419896, 591751049, 878082192);

{ TFWZipKeys }

constructor TFWZipKeys.Create(const Password: AnsiString);
var
  I: Integer;
begin
  inherited Create;
{
Step 1 - Initializing the encryption keys
-----------------------------------------

Key(0) <- 305419896
Key(1) <- 591751049
Key(2) <- 878082192

loop for i <- 0 to length(password)-1
    update_keys(password(i))
end loop

}
  FKeys := DefaultKeys;
  for I := 1 to Length(Password) do
    UpdateKeys(Byte(Password[I]));
end;

function TFWZipKeys.DecryptByte: Byte;
var
  temp: Word;
begin
{
Where decrypt_byte() is defined as:

unsigned char decrypt_byte()
    local unsigned short temp
    temp <- Key(2) | 2
    decrypt_byte <- (temp * (temp ^ 1)) >> 8
end decrypt_byte

}
  temp := FKeys[2] or 2;
  Result := (temp * (temp xor 1)) shr 8;
end;

procedure TFWZipKeys.UpdateKeys(Value: Byte);
begin
{
  Key(0) <- crc32(key(0),char)
  Key(1) <- Key(1) + (Key(0) & 000000ffH)
  Key(1) <- Key(1) * 134775813 + 1
  Key(2) <- crc32(key(2),key(1) >> 24)
}
  FKeys[0] := ((FKeys[0] shr 8) and $FFFFFF) xor
    CRC32Table[(FKeys[0] xor Value) and $FF];
  FKeys[1] := FKeys[1] + (FKeys[0] and $FF);
  FKeys[1] := FKeys[1] * 134775813 + 1;
  FKeys[2] := ((FKeys[2] shr 8) and $FFFFFF) xor
    CRC32Table[(FKeys[2] xor (FKeys[1] shr 24)) and $FF];
end;

{ TFWZipCryptor }

procedure TFWZipCryptor.EncryptBuffer(Buffer: PByte; Size: Int64);
var
  temp: Byte;
begin
  // реверсированный вариант TFWZipDecryptor.DecryptBuffer
  while Size > 0 do
  begin
    Dec(Size);
    temp := DecryptByte;
    UpdateKeys(Buffer^);
    Buffer^ := temp xor Buffer^;
    Inc(Buffer);
  end;
end;

function TFWZipCryptor.EncryptByte(Value: Byte): Byte;
var
  temp: Byte;
begin
  temp := DecryptByte;
  UpdateKeys(Value);
  Result := temp xor Value;
end;

procedure TFWZipCryptor.GenerateEncryptionHeader(Stream: TStream;
  IsDescryptorFlagPresent: Boolean; CRC32, FileDate: Cardinal);
var
  Buffer: array [0..EncryptedHeaderSize - 1] of Byte;
  I: Integer;
begin
  // реверсированный вариант TFWZipDecryptor.LoadEncryptionHeader
  Randomize;
  for I := 0 to LastEncryptedHeaderByte - 2 do
    Buffer[I] := EncryptByte(Byte(Random(MAXBYTE)));
  if IsDescryptorFlagPresent then
  begin
    Buffer[10] := EncryptByte(LoByte(LoWord(FileDate)));
    Buffer[11] := EncryptByte(HiByte(LoWord(FileDate)));
  end
  else
  begin
    Buffer[10] := EncryptByte(LoByte(HiWord(CRC32)));
    Buffer[11] := EncryptByte(HiByte(HiWord(CRC32)));
  end;
  Stream.WriteBuffer(Buffer[0], EncryptedHeaderSize);
end;

{ TFWZipDecryptor }

procedure TFWZipDecryptor.DecryptBuffer(Buffer: PByte; Size: Int64);
var
  temp: Byte;
begin
{
Step 3 - Decrypting the compressed data stream
----------------------------------------------

The compressed data stream can be decrypted as follows:

loop until done
    read a character into C
    Temp <- C ^ decrypt_byte()
    update_keys(temp)
    output Temp
end loop
}
  while Size > 0 do
  begin
    Dec(Size);
    temp := Buffer^ xor DecryptByte;
    UpdateKeys(temp);
    Buffer^ := temp;
    Inc(Buffer);
  end;
end;

function TFWZipDecryptor.LoadEncryptionHeader(Stream: TStream;
  IsDescryptorFlagPresent: Boolean; CRC32, FileDate: Cardinal): Boolean;
var
  Buffer: array [0..EncryptedHeaderSize - 1] of Byte;
  I: Integer;
  C: Byte;
begin
{
Read the 12-byte encryption header into Buffer, in locations
Buffer(0) thru Buffer(11).

loop for i <- 0 to 11
    C <- buffer(i) ^ decrypt_byte()
    update_keys(C)
    buffer(i) <- C
end loop
}
  Stream.ReadBuffer(Buffer[0], EncryptedHeaderSize);
  for I := 0 to LastEncryptedHeaderByte do
  begin
    C := Buffer[I] xor DecryptByte;
    UpdateKeys(C);
    Buffer[I] := C;
  end;

{
After the header is decrypted,  the last 1 or 2 bytes in Buffer
should be the high-order word/byte of the CRC for the file being
decrypted, stored in Intel low-byte/high-byte order, or the high-order
byte of the file time if bit 3 of the general purpose bit flag is set.
}
  if IsDescryptorFlagPresent then
    Result := Buffer[LastEncryptedHeaderByte] = HiByte(LoWord(FileDate))
  else
    Result := Buffer[LastEncryptedHeaderByte] = HiByte(HiWord(CRC32));
end;

end.
