program ZipAnalizer2;

uses
  Forms,
  uZipAnalizer2 in 'uZipAnalizer2.pas' {dlgZipAnalizer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdlgZipAnalizer, dlgZipAnalizer);
  Application.Run;
end.
