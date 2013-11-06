////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : ExctractZIPDemo2
//  * Purpose   : Демонстрация распаковки зашифрованного архива.
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

// Данный пример показывает создание и распаковку зашифрованного архива.
// Для демонстрации работы со списком паролей мы создадим архив,
// в котором каждому элементу назначим произвольный пароль из списка.
// При чем код излечения данных не будет знать какому файлу
// какой из паролей соответствует и есть ли пароль вообще.

program ExctractZIPDemo2;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  FWZipWriter,
  FWZipReader,
  FWZipConsts;

const
  PasswordList: array [0..3] of string = (
    '', 'password1', 'password2', 'password3');

procedure OnPassword(Self, Sender: TObject; const FileName: string;
  var Password: string; var CancelExtract: Boolean);
begin
  Password := PasswordList[3];
end;

var
  Writer: TFWZipWriter;
  Reader: TFWZipReader;
  Item: TFWZipWriterItem;
  I: Integer;
  ExtractResult: TExtractResult;
  Method: TMethod;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    Writer := TFWZipWriter.Create;
    try

      // Для начала добавим в корень архива файлы из корневой директории
      Writer.AddFolder('..\..\', False);

      // Теперь назначим им пароли случайным образом
      Randomize;

      // У первого элемента пароль всегда будет присутствовать (для демонстрации)
      Item := Writer[0];
      Item.Password := PasswordList[Random(3) + 1];
      Item.NeedDescriptor := True;

      for I := 1 to Writer.Count - 1 do
      begin
        Item := Writer[I];
        // Если используется шифрование желательно включать дескриптор файла
        // см. Readme.txt
        Item.NeedDescriptor := True;
        Item.Password := PasswordList[Random(4)];
      end;

      // Сохраняем результат
      ForceDirectories('..\DemoResults\');
      Writer.BuildZip('..\DemoResults\ExctractZIPDemo2.zip');
    finally
      Writer.Free;
    end;

    Reader := TFWZipReader.Create;
    try
      Reader.LoadFromFile('..\DemoResults\ExctractZIPDemo2.zip');

      // Теперь наша задача извлечь данные из архива
      // В ручном режиме распаковки придется перебирать пароли самостоятельно
      // Например вот так:
      I := 0;
      repeat
        ExtractResult := Reader[0].Extract(
          '..\DemoResults\ExctractZIPDemo2\ManualExtract\', PasswordList[I]);
        Inc(I);
      until ExtractResult <> erNeedPassword;

      // Если предполагается использовать режим автоматической распаковки,
      // то указать пароли можно двумя способами

      // 1. через список паролей
      Reader.PasswordList.Add(PasswordList[1]);
      Reader.PasswordList.Add(PasswordList[2]);

      // 2. через обработчик
      Method.Code := @OnPassword;
      Method.Data := Reader;
      Reader.OnPassword := TZipNeedPasswordEvent(Method);

      // для демонстрации в список паролей добавлены только два пароля
      // третий будет передан через обработчик события OnPassword

      Reader.ExtractAll('..\DemoResults\ExctractZIPDemo2\');
    finally
      Reader.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
