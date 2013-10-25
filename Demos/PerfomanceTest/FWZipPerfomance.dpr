program FWZipPerfomance;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  FWZipConsts in '..\..\FWZipConsts.pas',
  FWZipCrc32 in '..\..\FWZipCrc32.pas',
  FWZipCrypt in '..\..\FWZipCrypt.pas',
  FWZipReader in '..\..\FWZipReader.pas',
  FWZipStream in '..\..\FWZipStream.pas',
  FWZipWriter in '..\..\FWZipWriter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
