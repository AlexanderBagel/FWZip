////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : CreateZIPDemo1
//  * Purpose   : Демонстрация создания архива используя различные
//  *           : варианты добавления данных
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

// Данный пример показывает различные варианты добавления информации в архив
// Для каждого из способов добавления в архиве будет создана отдельная папка

program CreateZIPDemo1;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  TypInfo,
  FWZipWriter;

procedure CheckResult(Value: Integer);
  begin
    if Value < 0 then
      raise Exception.Create('Ошибка добавления данных');
  end;

var
  Zip: TFWZipWriter;
  S: TStringStream;
  PresentFiles: TStringList;
  SR: TSearchRec;
  I, ItemIndex: Integer;
  BuildZipResult: TBuildZipResult;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    Zip := TFWZipWriter.Create;
    try
      // У всего архива включим UTF8 кодировку (если необходимо)
      Zip.UseUTF8String := True;

      // добавим комментарий по необходимости
      Zip.Comment := 'Общий комментарий к архиву';

      // Сначала добавим в архив файлы и папки
      // не существующие физически на диске

      // Создаем и добавляем текстовый файл в корень архива (AddStream)
      S := TStringStream.Create('Тестовый текстовый файл №1');
      try
        S.Position := 0;
        ItemIndex := Zip.AddStream('test.txt', S);
        CheckResult(ItemIndex);
        // Можно добавить коментарий к самому элементу
        Zip.Item[ItemIndex].Comment := 'Мой тестовый комментарий';
      finally
        S.Free;
      end;

      // Создаем и добавляем текстовый файл в корень архива (AddStream)
      // владельцем стрима данных бдет сам архив
      S := TStringStream.Create('Тестовый текстовый файл №2');
      S.Position := 0;
      ItemIndex := Zip.AddStream('test2.txt', S, soOwned);
      CheckResult(ItemIndex);

      // Для сохранении файла в определенной папке
      // достаточно указать ее наличие в пути к файлу, например вот так
      S := TStringStream.Create('Тестовый текстовый файл №2');
      try
        S.Position := 0;
        CheckResult(Zip.AddStream(
          'AddStreamData\SubFolder1\Subfolder2\Test.txt', S));
      finally
        S.Free;
      end;

      // Теперь будут показаны пять вариантов добавления файлов
      // физически присутствующих на диске

      // Вариант первый:
      // добавляем в архив содержимое папки "Create ZIP 2" вызовом
      // базоовго метода AddFolder
      if Zip.AddFolder('..\Create ZIP 2\') = 0 then
        raise Exception.Create('Ошибка добавления данных');

      // Вариант второй:
      // добавляем содержимое нашей корневой директории в папку AddFolderDemo
      // при помощи вызова расширенной функции AddFolder,
      // в которой можем указать наименование папки внутри архива и указать
      // необходимость добавления подпапок (третий параметр)
      if Zip.AddFolder('AddFolderDemo', '..\..\', '*.pas', False) = 0 then
        raise Exception.Create('Ошибка добавления данных');

      // Вариант третий. Используем те-же файлы из корневой директории,
      // Только добавлять будем руками при помощи метода AddFile
      PresentFiles := TStringList.Create;
      try
        // Для начала их все найдем
        if FindFirst('..\..\*.pas', faAnyFile, SR) = 0 then
        try
          repeat
            if (SR.Name = '.') or (SR.Name = '..') then Continue;
            if SR.Attr and faDirectory <> 0 then
              Continue
            else
              PresentFiles.Add(SR.Name);
          until FindNext(SR) <> 0;
        finally
          FindClose(SR);
        end;

        // Теперь добавим по одному,
        // указывая в какой папке и под каким именем их размещать.
        for I := 0 to PresentFiles.Count - 1 do
          CheckResult(Zip.AddFile('..\..\' + PresentFiles[I],
            'AddFile\' + PresentFiles[I]));

        // Четвертый вариант - добавление списком при помощи метода AddFiles.
        // Каждый элемент списка должен быть сформирован следующим образом:
        // "Относительный путь и имя в архиве"="Путь к файлу"
        // Т.е. ValueFromIndex указывает на путь к файлу,
        // а Names - относительный путь в архиве
        // Если не указать относительный путь, то будет браться только имя файла.
        for I := 0 to PresentFiles.Count - 1 do
          PresentFiles[I] :=
            'AddFiles\' + PresentFiles[I] + '=' + '..\..\' + PresentFiles[I];
        if Zip.AddFiles(PresentFiles) <> PresentFiles.Count then
          raise Exception.Create('Ошибка добавления данных');

      finally
        PresentFiles.Free;
      end;

      // И последний вариант, то-же добавление списком,
      // только в данный список можно помещать папки. Метод AddFilesAndFolders.
      // Файлы помещаются в список по тому-же принципу что и в методе AddFiles.
      // Записи для папок формируются по принципу: "Относительный путь в архиве"="Путь к папке"
      // Т.е. ValueFromIndex указывает на путь к папке,
      // а Names - относительный путь в архиве от корня

      // Здесь добавим все файлы и папки из корня проекта
      PresentFiles := TStringList.Create;
      try
        if FindFirst('..\..\*.*', faAnyFile, SR) = 0 then
        try
          repeat
            if (SR.Name = '.') or (SR.Name = '..') then Continue;
            // пропускаем папку demos, т.к. там могут быть залоченные сейчас файлы
            if AnsiLowerCase(SR.Name) = 'demos' then
              Continue;
            PresentFiles.Add('AddFilesAndFolders\' + SR.Name + '=..\..\' + SR.Name);
          until FindNext(SR) <> 0;
        finally
          FindClose(SR);
        end;
        Zip.AddFilesAndFolders(PresentFiles, True);
      finally
        PresentFiles.Free;
      end;

      // Вот собственно и все - осталось создать сам архив...
      ForceDirectories('..\DemoResults\');
      BuildZipResult := Zip.BuildZip('..\DemoResults\CreateZIPDemo1.zip');
      // ... и вывести результат
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
