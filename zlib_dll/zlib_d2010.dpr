library zlib_d2010;

uses
  ZLib;

{$R *.res}

{$IF CompilerVersion < 21}
  {$MESSAGE FATAL ' ������ ������ ��������� �������� ������ � ������������� Delphi 2010 � ���� '}
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
