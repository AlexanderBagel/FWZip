library zlib_d2010;

uses
  ZLib;

{$R *.res}

{$IF CompilerVersion < 21}
  {$MESSAGE FATAL ' Сборка данной библотеки возможна только с использование Delphi 2010 и выше '}
{$IFEND}

exports
  adler32,
  deflateInit_,
  DeflateInit2_,
  deflate,
  deflateEnd,
  inflateInit_,
  inflateInit2_,
  inflate,
  inflateEnd,
  inflateReset;

begin
end.
