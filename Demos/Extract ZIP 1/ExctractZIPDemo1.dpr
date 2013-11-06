////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : ExctractZIPDemo1
//  * Purpose   : Демонстрация распаковки архива.
//  *           : Используется архив созданный демоприложением CreateZIPDemo1
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

// Данный пример показывает два варианта извлечения информации из архива.

program ExctractZIPDemo1;

{$APPTYPE CONSOLE}

uses
  Windows,
  Classes,
  SysUtils,
  TypInfo,
  FWZipReader;

function ExtractResultStr(Value: TExtractResult): string;
begin
  Result := GetEnumName(TypeInfo(TExtractResult), Integer(Value));
end;

var
  Zip: TFWZipReader;
  Index: Integer;
  S: TStringStream;
  OemString: AnsiString;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    Zip := TFWZipReader.Create;
    try
      // Открываем ранее созданный архив
      Zip.LoadFromFile('..\DemoResults\CreateZIPDemo1.zip');

      // Первый вариант распаковки - ручной доступ к каждому элементу архива

      // В примере CreateZIPDemo1 мы создали в корне архива файл Test.txt
      // Нам необходимо получить индекс этого элемента в архиве
      Index := Zip.GetElementIndex('test.txt');
      if Index >= 0 then
      begin
        // Распаковать можно в память:
        S := TStringStream.Create('');
        try
          Zip[Index].ExtractToStream(S, '');
          // Файл извлечен, выведем его содержимое в окно консоли
          {$IFDEF UNICODE}
          Writeln(OemString);
          {$ELSE}
          OemString := AnsiString(S.DataString);
          AnsiToOem(PAnsiChar(OemString), PAnsiChar(OemString));
          Writeln(OemString);
          {$ENDIF}
        finally
          S.Free;
        end;

        // Распаковать так-же можно на диск:
        Write('Extract "', Zip[Index].FileName, '": ');
        Writeln(ExtractResultStr(
          Zip[Index].Extract('..\DemoResults\CreateZIPDemo1\ManualExtract\', '')));
      end;

      // Таким-же образом можно получить содержимое остальных файлов

      // Второй вариант распаковки - автоматической распаковка архива
      // в указанную папку на диске
      Zip.ExtractAll('..\DemoResults\CreateZIPDemo1\');

      // Третий вариант распаковки - автоматическая распаковка по маске
      // (данный код распакует все файлы находящиеся в папке AddFolderDemo архива)
      Zip.ExtractAll('AddFolderDemo*', '..\DemoResults\CreateZIPDemo1\ExtractMasked\');
    finally
      Zip.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
