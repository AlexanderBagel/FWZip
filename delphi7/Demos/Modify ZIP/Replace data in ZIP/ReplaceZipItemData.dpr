////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : ReplaceZipItemData
//  * Purpose   : Демонстрация изменения данных в уже созданном архиве
//  * Author    : Александр (Rouse_) Багель
//  * Copyright : © Fangorn Wizards Lab 1998 - 2023.
//  * Version   : 2.0.2
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Stable Release : http://rouse.drkb.ru/components.php#fwzip
//  * Latest Source  : https://github.com/AlexanderBagel/FWZip
//  ****************************************************************************
//
//  Используемые источники:
//  ftp://ftp.info-zip.org/pub/infozip/doc/appnote-iz-latest.zip
//  https://zlib.net/zlib-1.2.13.tar.gz
//  http://www.base2ti.com/
// http://www.base2ti.com/
//

// Данный пример показывает как можно изменить данные в уже созданном архиве
// без необходимости распаковки неизмененных элементов и их повторного сжатия.

program ReplaceZipItemData;

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  Classes,
  FWZipReader,
  FWZipModifier;

var
  Modifier: TFWZipModifier;
  Reader: TFWZipReader;
  Index: TReaderIndex;
  S: TStringStream;
  I: Integer;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    // создаем экземпляр модификатора архивов
    Modifier := TFWZipModifier.Create;
    try
      // подключаем ранее созданный архив
      Index := Modifier.AddZipFile('..\..\DemoResults\split_main_archive.zip');

      // добавляем из него первый элемент указывая его имя
      Modifier.AddFromZip(Index, 'test1.txt');

      // добавляем второй элемент, взяв имя из внутреннего ридера
      Modifier.AddFromZip(Index, Modifier.Reader[Index].Item[1].FileName);

      // вместо третьего пишем новые данные
      S := TStringStream.Create('новые данные для третьего элемента архива');
      try
        S.Position := 0;
        Modifier.AddStream('test3.txt', S);
      finally
        S.Free;
      end;

      // ну и добавляем последний элемент с одновременным изменением имени
      Modifier.AddFromZip(Index, 'test4.txt', 'New test4.txt');
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

    // третий вариант с передачей ридера снаружи, причем время жизни ридера
    // контролирем мы сами
    // Содаем ридер который будем использовать не только для модификации
    // но и для каких-то своих задач
    Reader := TFWZipReader.Create;
    try
      // читаем данные из ранее созданного тестового архива
      Reader.LoadFromFile('..\..\DemoResults\split_main_archive.zip');

      // создаем экземпляр модификатора архивов
      Modifier := TFWZipModifier.Create;
      try
        // подключаем архив через доступный нам ридер
        Index := Modifier.AddZipFile(Reader);

        // добавляем из него все элементы кроме последнего
        for I := 0 to Modifier.Reader[Index].Count - 2 do
          Modifier.AddFromZip(Index, Modifier.Reader[Index].Item[I].FileName);

        // вместо последнего пишем новые данные
        S := TStringStream.Create('новые данные для последнего элемента архива');
        try
          S.Position := 0;
          Modifier.AddStream('test4.txt', S);
        finally
          S.Free;
        end;

        // и ребилдим архив
        Modifier.BuildZip('..\..\DemoResults\replaced_data_archive3.zip');
      finally
        Modifier.Free;
      end;

    finally
      Reader.Free;
    end;

    // четвертый вариант, это небольшая модификация епрвого варианта,
    // только подключение архива происходит так-же как и в третьем через ридер
    // но в этот раз время жизни ридера будет контролировать модификатор

    // создаем экземпляр модификатора архивов
    Modifier := TFWZipModifier.Create;
    try
      // создаем экземпляр ридера
      Reader := TFWZipReader.Create;

      // открываем ранее созданный архив
      Reader.LoadFromFile('..\..\DemoResults\split_main_archive.zip');

      // и подкючаем его к модификатору указывая вторым параметром,
      // что разрушать ридер должен модификатор
      Index := Modifier.AddZipFile(Reader, roOwned);

      // добавляем из него первый элемент указывая его имя
      Modifier.AddFromZip(Index, 'test1.txt');

      // добавляем второй элемент, взяв имя из внутреннего ридера
      Modifier.AddFromZip(Index, Modifier.Reader[Index].Item[1].FileName);

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
      Modifier.BuildZip('..\..\DemoResults\replaced_data_archive4.zip');
    finally
      Modifier.Free;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
