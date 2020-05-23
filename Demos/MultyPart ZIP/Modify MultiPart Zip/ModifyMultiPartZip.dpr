////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : ModifyMultiPartZip
//  * Purpose   : Демонстрация модификации многотомного архива
//  * Author    : Александр (Rouse_) Багель
//  * Copyright : © Fangorn Wizards Lab 1998 - 2020.
//  * Version   : 1.1.0
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

// Хоть модификация многотомных архивов идея достаточно странная сама по себе,
// но архитектура фреймворка легко позволяет это осуществить.
// При чем модификатор будет спокойно работать и с обычными архивами и с многотомными
// модифицируя оба типа архивов, а так-же легко может провести конвертацию
// без этапа перепаковки данных из многотомного архива в обычный и наоборот.

program ModifyMultiPartZip;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Classes,
  TypInfo,
  FWZipZLib,
  FWZipWriter,
  FWZipStream,
  FWZipModifier;

var
  Writer: TFWZipWriter;
  S: TStringStream;
  MultiStreamRead, MultiStreamWrite: TFWFileMultiStream;
  Modifier: TFWZipModifier;
  I, Index1, Index2: Integer;
  BuildZipResult: TBuildZipResult;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try

    // Модификация многотомного архиве ничем не отличается от модификации
    // обычного архива, за исключением использования наследников
    // TFWAbstractMultiStream для чтения и записи

    // Например возьмем архив созданный в примере CreateMultyPartZip.dpr
    // и удалим из него все *.pas файлы.
    // А так-же добавим файлы из обычного не многотомного архива

    MultiStreamRead := TFWFileMultiStream.CreateRead(
      '..\..\DemoResults\MultyPartZip\MultyPartZip.zip');
    try

      Modifier := TFWZipModifier.Create;
      try
        // загружаем многотомный архив в модификатор
        Index1 := Modifier.AddZipFile(MultiStreamRead);

        // добавляем все элементы архива
        Modifier.AddFromZip(Index1);

        // удаляем все PAS файлы
        for I := Modifier.Count - 1 downto 0 do
          if AnsiLowerCase(ExtractFileExt(Modifier[I].FileName)) = '.pas' then
            Modifier.DeleteItem(I);

        // теперь создадим простой архив
        Writer := TFWZipWriter.Create;
        try
          S := TStringStream.Create('Просто тестовые данные для демонстрации');
          Writer.AddStream('test_stream.txt', S, soOwned);
          Writer.BuildZip('..\..\DemoResults\MultyPartZip\stream.zip')
        finally
          Writer.Free;
        end;

        // загружаем обычный архив в модификатор
        Index2 := Modifier.AddZipFile('..\..\DemoResults\MultyPartZip\stream.zip');

        // добавляем единственный элемент простого архива
        Modifier.AddFromZip(Index2, 'test_stream.txt');

        // ... и сохранияем все в новый архив
        // Причем!
        // т.к. мы не указываем второй параметр, отвечающий за размер каждого тома
        // то он берется по умолчанию и новый архив будет сформирован с томами
        // другого размера, чем был в изначальном архиве.
        // Все изменения произойдут на лету, вам не нужно их контролировать.

        MultiStreamWrite := TFWFileMultiStream.CreateWrite(
          '..\..\DemoResults\MultyPartZip\MultyPartZipWithoutPas.zip');
        try
          BuildZipResult := Modifier.BuildZip(MultiStreamWrite);
        finally
          MultiStreamWrite.Free;
        end;

      finally
        Modifier.Free;
      end;

    finally
      MultiStreamRead.Free;
    end;

    // ... выводим результат
    Writeln(GetEnumName(TypeInfo(TBuildZipResult), Integer(BuildZipResult)));

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
