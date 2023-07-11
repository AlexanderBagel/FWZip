program ZipAnalizer;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  uZipAnalizer in 'uZipAnalizer.pas' {dlgZipAnalizer};

{$IFNDEF FPC}
  {$R *.res}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TdlgZipAnalizer, dlgZipAnalizer);
  Application.Run;
end.
