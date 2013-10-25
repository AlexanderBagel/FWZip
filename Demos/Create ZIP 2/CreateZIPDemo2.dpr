////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : CreateZIPDemo2
//  * Purpose   : Демонстрация изменения добавленных записей
//  * Author    : Александр (Rouse_) Багель
//  * Copyright : © Fangorn Wizards Lab 1998 - 2012.
//  * Version   : 1.0.4
//  * Home Page : http://rouse.drkb.ru
//  ****************************************************************************
//
//  Используемые источники:
//  ftp://ftp.info-zip.org/pub/infozip/doc/appnote-iz-latest.zip
//  http://zlib.net/zlib-1.2.5.tar.gz
//

// Данный пример показывает различные варианты изменения записей
// в еще не сформированном архиве.

program CreateZIPDemo2;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  TypInfo,
  {$IFDEF USE_ZLIB_DLL}
  ZLib_external
  {$ELSE}
  ZLib
  {$ENDIF},
  FWZipWriter;

var
  Zip: TFWZipWriter;
  Item: TFWZipWriterItem;
  I: Integer;
  BuildZipResult: TBuildZipResult;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    Zip := TFWZipWriter.Create;
    try
      // Для начала добавим в корень архива файлы из корневой директории
      Zip.AddFolder('..\..\', False);

      // Теперь изменим им свойства:
      for I := 0 to Zip.Count - 1 do
      begin
        Item := Zip[I];
        // Изменим коментарий
        Item.Comment := 'Тестовый коментарий к файлу ' + Item.FileName;
        // Установим пароль
        Item.Password := 'password';
        // Изменим тип сжатия
        Item.CompressionLevel := TCompressionLevel(Byte(I mod 3));
      end;

      // Теперь каждый элемент архива имеет коментарий, зашифрован паролем и
      // имеет собственную степень сжатия в зависимости от своей
      // порядковой позиции в архиве.
      // Ну и сам архив так-же имеет коментарий.
      Zip.Comment := 'Тестовый коментарий ко всему архиву';

      // создаем архив и выводим результат
      ForceDirectories('..\DemoResults\');
      BuildZipResult := Zip.BuildZip('..\DemoResults\CreateZIPDemo2.zip');
      // ... и вывести результат
      Writeln(GetEnumName(TypeInfo(TBuildZipResult), Integer(BuildZipResult)));

    finally
      Zip.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
