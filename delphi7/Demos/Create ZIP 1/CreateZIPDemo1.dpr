////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : CreateZIPDemo1
//  * Purpose   : Демонстрация создания архива используя различные
//  *           : варианты добавления данных
//  * Author    : Александр (Rouse_) Багель
//  * Copyright : © Fangorn Wizards Lab 1998 - 2025.
//  * Version   : 2.0.8
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
//

// Данный пример показывает различные варианты добавления информации в архив
// Для каждого из способов добавления в архиве будет создана отдельная папка

program CreateZIPDemo1;

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+}
{$ELSE}
	{$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  Classes,
  TypInfo,
  FWZipConsts,
  FWZipWriter,
  FWZipUtils;

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
  Attributes: TFileAttributeData;
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
      {$IFDEF FPC}
      S := TStringStream.Create(AnsiString('Тестовый текстовый файл №1'));
      {$ELSE}
      S := TStringStream.Create('Тестовый текстовый файл №1');
      {$ENDIF}
      try
        S.Position := 0;
        ItemIndex := Zip.AddStream('test.txt', S);
        CheckResult(ItemIndex);
        // Можно добавить коментарий к самому элементу
        Zip.Item[ItemIndex].Comment := 'Мой тестовый комментарий';

        // Для добавленного через стрим элемента, в качестве аттрибутов
        // используется текущее время.
        // Впрочем можно назначить произвольные аттрибуты
        // Для примера возьмем иx у самого себя
        if GetFileAttributes(ParamStr(0), Attributes) then
        begin

          // Важно! Поля с размером в атрибутах игнорируются.
          // Используются только поля хранящие время.

          // Вот этот способ необходимо использовать для уже добавленого элемента
          Zip.Item[ItemIndex].ChangeAttributes(Attributes);

          // А так же можно можно указать аттрибуты сразу при добавлении
          CheckResult(Zip.AddStream('file_with_custom_attributes.txt', S));
        end;
      finally
        S.Free;
      end;

      // Создаем и добавляем текстовый файл в корень архива (AddStream)
      // владельцем стрима данных будет сам архив
      {$IFDEF FPC}
      S := TStringStream.Create(AnsiString('Тестовый текстовый файл №2'));
      {$ELSE}
      S := TStringStream.Create('Тестовый текстовый файл №2');
      {$ENDIF}
      S.Position := 0;
      ItemIndex := Zip.AddStream('Тестовый файл номер №2.txt', S, soOwned);
      CheckResult(ItemIndex);

      // Для сохранении файла в определенной папке
      // достаточно указать ее наличие в пути к файлу, например вот так
      {$IFDEF FPC}
      S := TStringStream.Create(AnsiString('Тестовый текстовый файл №3'));
      {$ELSE}
      S := TStringStream.Create('Тестовый текстовый файл №3');
      {$ENDIF}
      try
        S.Position := 0;
        CheckResult(Zip.AddStream(
          'AddStreamData\SubFolder1\Subfolder2\Тестовый файл номер №3.txt', S));
      finally
        S.Free;
      end;

      // Создадим пустую папку в архиве
      CheckResult(Zip.AddEmptyFolder('FirstEmptyFolder'));

      // Создадим пустую папку с заранее указаным путём
      CheckResult(Zip.AddEmptyFolder('SecondEmptyFolder\Subfolder1\Subfolder2'));

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
        if FindFirst(PathCanonicalize('..\..\*.pas'), faAnyFile, SR) = 0 then
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
            'AddFiles\' + PresentFiles[I] + '=..\..\' + PresentFiles[I];
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
        if FindFirst(PathCanonicalize('..\..\*'), faAnyFile, SR) = 0 then
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
