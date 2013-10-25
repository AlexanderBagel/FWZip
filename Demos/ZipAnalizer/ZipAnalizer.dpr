program ZipAnalizer;

uses
  Forms,
  uZipAnalizer in 'uZipAnalizer.pas' {dlgZipAnalizer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdlgZipAnalizer, dlgZipAnalizer);
  Application.Run;
end.
