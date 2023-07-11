program ZipAnalizer2;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  uZipAnalizer2 in 'uZipAnalizer2.pas' {dlgZipAnalizer};

{$IFNDEF FPC}
  {$R *.res}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TdlgZipAnalizer, dlgZipAnalizer);
  Application.Run;
end.
