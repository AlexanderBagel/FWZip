////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : CreateMultiPartZip
//  * Purpose   : Демонстрация создания архива с разбитием на части по 1 килобайту
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

// Данный пример показывает как можно разделить большой архив на несколько частей,
// для их последующей записи на внешние носители информации
// или передачи частями по сети при неустойчивом соединении, затрудняющем
// передачу архива большого размера

program CreateMultiPartZip;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  TypInfo,
  FWZipZLib,
  FWZipWriter,
  FWZipStream;

var
  Zip: TFWZipWriter;
  Item: TFWZipWriterItem;
  I: Integer;
  BuildZipResult: TBuildZipResult;
  MultiStream: TFWFileMultiStream;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    Zip := TFWZipWriter.Create;
    try
      // Для начала добавим в корень архива файлы из корневой директории
      Zip.AddFolder('..\..\..\', False);

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
      Zip.Comment := 'Тестовый коментарий ко всему архиву';

      ForceDirectories('..\..\DemoResults\MultyPartZip\');

      // Вся логика создания многотомных архивов заключается в классе
      // TFWAbstractMultiStream от которого реализуются наследники
      // предоставляющие конкретную реализацию.
      // В частности TFWFileMultiStream позволяет работать с архивами
      // расположенными на локальном диске.
      // Если потребуется работа с архивами расположенными удаленно
      // на FTP или облаке, допустим SharePoint ресурсе,
      // потребуется реализовывать собственный наследник.

      // Первый параметром идет имя архива, вторым максимальный размер его частей.
      // Созданные тома многотомного архива не обязательно будут соответствовать
      // указанному значению, т.к. согласно спецификации, структуры TCentralDirectoryFileHeader
      // должны целиком располагаться внутри тома, поэтому размер финальных
      // томов архива может немного уменьшаться в диапазоне от 1 байта
      // до SizeOf(TCentralDirectoryFileHeader).
      // Так-же размер последнего тома не может быть меньше чем размер структуры
      // TEndOfCentralDir, а если используется Zip64, то к этому размеру добавляется размеры
      // TZip64EOFCentralDirectoryRecord и TZip64EOFCentralDirectoryLocator.
      MultiStream := TFWFileMultiStream.CreateWrite(
        '..\..\DemoResults\MultyPartZip\MultyPartZip.zip', $20000);
      try
        // Для создания многотомного архива указываем не имя файла, а сам MultiStream
        BuildZipResult := Zip.BuildZip(MultiStream);
      finally
        MultiStream.Free;
      end;

      // ... и выведим результат
      Writeln(GetEnumName(TypeInfo(TBuildZipResult), Integer(BuildZipResult)));

    finally
      Zip.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
