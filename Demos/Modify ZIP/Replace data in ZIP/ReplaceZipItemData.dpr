////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : ReplaceZipItemData
//  * Purpose   : Демонстрация изменения данных в уже созданном архиве
//  * Author    : Александр (Rouse_) Багель
//  * Copyright : © Fangorn Wizards Lab 1998 - 2015.
//  * Version   : 1.0.11
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

// Данный пример показывает как можно изменить данные в уже созданном архиве
// без необходимости распаковки неизмененных элементов и их повторного сжатия.

program ReplaceZipItemData;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Classes,
  FWZipModifier;

var
  Modifier: TFWZipModifier;
  Index: TReaderIndex;
  S: TStringStream;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    // создаем экземпляр модификатора архивов
    Modifier := TFWZipModifier.Create;
    try
      // подключаем ранее созданный архив
      Index := Modifier.AddZipFile('..\..\DemoResults\split_main_archive.zip');
      // добавляем из него первые два элемента
      Modifier.AddFromZip(Index, 'test1.txt');
      Modifier.AddFromZip(Index, 'test2.txt');

      // вместо третьего пишем новые данные
      S := TStringStream.Create('новые данные для третьего элемента архива');
      try
        S.Position := 0;
        Modifier.AddStream('test3.txt', S);
      finally
        S.Free;
      end;

      // ну и добавляем последний элемент
      Modifier.AddFromZip(Index, 'test4.txt');
      // теперь делаем новый архив,
      // при этом данные от первого второго и четвертого элемента
      // скопируются как есть без распаковки,
      // а вместо третьего элемента будет добавлен новый блок данных
      Modifier.BuildZip('..\..\DemoResults\replaced_data_archive1.zip');
    finally
      Modifier.Free;
    end;

    // предыдущий вариант был с сохранением порядка элемента в архиве
    // если же порядок не важен, то можно сделать еще проще:
    // создаем экземпляр модификатора архивов
    Modifier := TFWZipModifier.Create;
    try
      // подключаем ранее созданный архив
      Index := Modifier.AddZipFile('..\..\DemoResults\split_main_archive.zip');
      // добавляем все элементы
      Modifier.AddFromZip(Index);
      // теперь удалим запись о третьем элементе
      Modifier.DeleteItem(2);

      // и пишем новые данные
      S := TStringStream.Create('новые данные для третьего элемента архива');
      try
        S.Position := 0;
        Modifier.AddStream('test3.txt', S);
      finally
        S.Free;
      end;

      // теперь делаем новый архив, принцип тот же самый
      Modifier.BuildZip('..\..\DemoResults\replaced_data_archive2.zip');
    finally
      Modifier.Free;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
