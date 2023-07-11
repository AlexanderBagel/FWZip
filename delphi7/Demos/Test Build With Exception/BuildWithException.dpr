////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : BuildWithException
//  * Purpose   : Демонстрация работы с исключениями
//  *           : при создании и распаковке архива
//  * Author    : Александр (Rouse_) Багель
//  * Copyright : © Fangorn Wizards Lab 1998 - 2023.
//  * Version   : 2.0.0
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

// Данный пример показывает работу с различными ошибками могущими возникнуть
// в процессе создания и распаковки архива, а так-же способы их обработки.

program BuildWithException;

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFNDEF FPC}
  Windows,
  {$ENDIF}
  Classes,
  SysUtils,
  TypInfo,
  FWZipConsts,
  FWZipWriter,
  FWZipReader,
  FWZipUtils;

const
  INVALID_HANDLE_VALUE = THandle(-1);
  ReadLock = fmOpenRead or fmShareDenyNone;
  {$IFDEF LINUX}
  WriteLock = fmOpenWrite or fmShareExclusive;
  {$ELSE}
  WriteLock = fmOpenWrite or fmShareDenyNone;
  {$ENDIF}

var
  Writer: TFWZipWriter;
  Reader: TFWZipReader;
  Method: TMethod;
  hLockedFile: THandle;

//
// Процедура выводит результат работы функции BuildZip
// =============================================================================
procedure ShowBuildResult(Value: TBuildZipResult);
begin
  Writeln(GetEnumName(TypeInfo(TBuildZipResult), Integer(Value)));
end;

//
// Процедура выводит результат работы функции Extract
// =============================================================================
procedure ShowManualExtractResult(const ElementName: string;
  Value: TExtractResult);
begin
  Writeln(Format('%s -> %s', [ElementName,
    GetEnumName(TypeInfo(TExtractResult), Integer(Value))]));
end;

procedure DropLock;
begin
  FileClose(hLockedFile);
  hLockedFile := INVALID_HANDLE_VALUE;
end;

//
// смотри описание обработчика ниже
// =============================================================================
procedure OnException1({%H-}Self, Sender: TObject; {%H-}E: Exception;
  const ItemIndex: Integer; var Action: TExceptionAction;
  var NewFilePath: string; {%H-}NewFileData: TMemoryStream);
var
  CurrentFilePath: string;
  Src: THandleStream;
  Dst: TFileStream;
  hFile: THandle;
begin
  {$IFDEF LINUX}
  // с блокировкой под юниксом проблемы, поэтому пусть будет так
  DropLock;
  {$ENDIF}
  CurrentFilePath := string(TFWZipWriter(Sender)[ItemIndex].FilePath);
  NewFilePath := ChangeFileExt(CurrentFilePath, '.tmp');
  hFile := FileOpen(CurrentFilePath, ReadLock);
  try
    Src := THandleStream.Create(hFile);
    try
      Dst := TFileStream.Create(NewFilePath, fmCreate);
      try
        Dst.CopyFrom(Src, 0);
      finally
        Dst.Free;
      end;
    finally
      Src.Free;
    end;
  finally
    FileClose(hFile);
  end;
  Action := eaUseNewFilePathAndDel;
end;

//
// смотри описание обработчика ниже
// =============================================================================
procedure OnException2({%H-}Self, Sender: TObject; {%H-}E: Exception;
  const {%H-}ItemIndex: Integer; var Action: TExceptionAction;
  var {%H-}NewFilePath: string; {%H-}NewFileData: TMemoryStream);
begin
  DropLock;
  Action := eaRetry;
end;

//
// смотри описание обработчика ниже
// =============================================================================
procedure OnException3({%H-}Self, Sender: TObject; {%H-}E: Exception;
  const ItemIndex: Integer; var Action: TExceptionAction;
  var {%H-}NewFilePath: string; NewFileData: TMemoryStream);
var
  Src: THandleStream;
  hFile: THandle;
begin
  {$IFDEF LINUX}
  // с блокировкой под юниксом проблемы, поэтому пусть будет так
  DropLock;
  {$ENDIF}
  hFile := FileOpen(TFWZipWriter(Sender)[ItemIndex].FilePath, ReadLock);
  try
    Src := THandleStream.Create(hFile);
    try
      NewFileData.CopyFrom(Src, 0);
    finally
      Src.Free;
    end;
  finally
    FileClose(hFile);
  end;
  Action := eaUseNewFileData;
end;

//
// смотри описание обработчика ниже
// =============================================================================
procedure OnDuplicate({%H-}Self, Sender: TObject;
  var Path: string; var Action: TDuplicateAction);
begin
  Path := MakeUniqueName(Path);
  Action := daUseNewFilePath;
end;

var
  I: Integer;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try

    // Самая банальная ошибка при создании архива - это отсутствие доступа
    // к добавляемому в архив файлу.
    // Например вот такой код пытается заархивировать содержимое корневой
    // папки в которой искусственно залочен один из элементов
    // В этом случае возникнет ошибка доступа к залоченому файлу.
    // Если не назначены обработчики исключений, то такой файл будет пропущен
    // (действие по умолчанию eaSkip) и функция BuildZip
    // вернет следующие коды ошибок:
    // brFailed - в случае если в папке небыло других файлов
    // кроме залоченного (т.е. в архив добавлять нечего)
    // brPartialBuild - в случае если в архив все-же были добавлены какие-либо файлы,
    // но некоторые из них были пропущены

    Writer := TFWZipWriter.Create;
    try
      Writer.AddFolder('', '..\..\', '*.pas', False);
      // лочим один из файлов для демонстрации
      hLockedFile := FileOpen(PathCanonicalize('..\..\' + Writer[0].FileName), WriteLock);
      try
        Write('BuildWithException1.zip -> ');
        ShowBuildResult(Writer.BuildZip('..\DemoResults\BuildWithException1.zip'));
      finally
        FileClose(hLockedFile);
      end;
    finally
      Writer.Free;
    end;

    // узнать какие файлы были пропущены и попытаться исправить данную ситуацию
    // можно перекрытием события OnException
    // В следующем примере будет показано как все-же обработать такую ошибку
    // и добавить проблемный файл в архив

    Writer := TFWZipWriter.Create;
    try
      Writer.AddFolder('', '..\..\', '*.pas', False);

      // лочим один из файлов для демонстрации
      hLockedFile := FileOpen(PathCanonicalize('..\..\' + Writer[0].FileName), WriteLock);
      try

        // Назначаем обработчик через который мы будем обрабатывать ошибку
        // В обработчике OnException1 будет создаваться копия файла
        // после чего мы укажем в параметре NewFilePath новый путь к файлу,
        // а свойство Action выставим eaUseNewFilePathAndDel
        // Таким образом мы уведомляем FWZip что нужно повторить попытку
        // архивации файла, при этом необходимо использовать новый путь к файлу,
        // после чего данный файл следует удалить.

        Method.Code := @OnException1;
        Method.Data := Writer;
        Writer.OnException := TZipBuildExceptionEvent(Method);

        Write('BuildWithException2.zip -> ');
        ShowBuildResult(Writer.BuildZip('..\DemoResults\BuildWithException2.zip'));

        {$IFDEF LINUX}
        if hLockedFile = INVALID_HANDLE_VALUE then
          hLockedFile := FileOpen(PathCanonicalize('..\..\' + Writer[0].FileName), WriteLock);
        {$ENDIF}

        // второй вариант, обработки показан в обработчике OnException2
        // в нем мы снимаем исскуственную блокировку файла и выставляем
        // свойство Action в eaRetry.
        // Таким образом мы уведомляем FWZip что нужно повторить попытку
        // архивации файла

        Method.Code := @OnException2;
        Method.Data := Writer;
        Writer.OnException := TZipBuildExceptionEvent(Method);

        Write('BuildWithException3.zip -> ');
        ShowBuildResult(Writer.BuildZip('..\DemoResults\BuildWithException3.zip'));

        // для демонстрации возвращаем блокировку на место
        if hLockedFile = INVALID_HANDLE_VALUE then
          hLockedFile := FileOpen(PathCanonicalize('..\..\' + Writer[0].FileName), WriteLock);

        // третий вариант, обработки показан в обработчике OnException3
        // в нем мы загружаем любым способом данные файла в стрим и
        // выставляем свойство Action в eaUseNewFileData
        // Таким образом данные будут браться непосредственно из стрима
        // NewFileData

        Method.Code := @OnException3;
        Method.Data := Writer;
        Writer.OnException := TZipBuildExceptionEvent(Method);

        Write('BuildWithException4.zip -> ');
        ShowBuildResult(Writer.BuildZip('..\DemoResults\BuildWithException4.zip'));

        // ориентируясь на тип исключения можно реализовывать разнулю логику обработчика.
        // если вы не знаете как обработать то или иное исключение,
        // следует выставить свойство Action в eaSkip (выставлено по умолчанию)
        // для того чтобы пропустить проблемный файл, или eaAbort,
        // прервав таким образом создание архива.

      finally
        if hLockedFile <> INVALID_HANDLE_VALUE then
          FileClose(hLockedFile);
      end;
    finally
      Writer.Free;
    end;

    // При распаковке частой ошибочной ситуацией является попытка
    // перезаписи уже существующего на диске файла, либо ошибка
    // распаковки архива, созданного сторонним архиватором.
    // Обработка ошибки распаковки решается использованием более
    // новой версии ZLib (см. Readme.txt пункт 9)
    // Возникновение ошибки по другим причинам приведет к возникновению
    // события OnException. Если данное событие не перекрыто,
    // то в случае вызова метода TFWZipReader.ExtractAll
    // распаковка архива будет остановлена.
    // В случае, если данное событие перекрыто, то решение
    // о остановке распаковки должен принимать программист,
    // выставлением флага Handled:
    // (Handled = True, исключение обработано, можно продолжить распаковку)

    // для демонстрации проэмулируем ошибку перезаписи,
    // для этого нужно дважды распаковать один и тот-же архив
    // в одну и ту-же папку

    Reader := TFWZipReader.Create;
    try
      Reader.LoadFromFile('..\DemoResults\BuildWithException1.zip');

      // распаковываем первый раз

      Reader.ExtractAll('..\DemoResults\BuildWithExceptionUnpack\');

      // теперь пробуем распаковать повторно.
      // исключения в данном случае не произойдет, но все элементы будут пропущены

      Reader.ExtractAll('..\DemoResults\BuildWithExceptionUnpack\');

      // пропуск элементов можно увидеть и при ручной распаковке.
      // т.к. вызов Reader[I].Extract в отличие от Reader.ExtractAll
      // возвращает результат

      Writeln('Manual extract:');
      for I := 0 to Reader.Count - 1 do
        ShowManualExtractResult(
          string(Reader[I].FileName),
          Reader[I].Extract('..\DemoResults\BuildWithExceptionUnpack\', ''));

      // как можно заметить, все элементы действительно были пропущены
      // (Reader[I].Extract вернул erSkiped для каждого элемента)

      // Для обработки данной ситуации в режиме автоматической распаковки (ExtractAll)
      // необходимо перекрыть событие OnDuplicate у класса TFWZipReader.
      // В случае ручной распаковки, перекрывать событие OnDuplicate требуется
      // у каждого элемента (Reader.Items[индекс элемента].OnDuplicate)

      Method.Code := @OnDuplicate;
      Method.Data := Reader;
      Reader.OnDuplicate := TZipDuplicateEvent(Method);

      // теперь попробуем повторно распаковать.
      // при возникновении события OnDuplicate в обработчике элементу будет
      // назначено новое имя и параметр Action будет выставлен в daUseNewFilePath.
      // Таким образом мы укажем TFWZipReader-у что необходимо распаковать
      // файл с новым именем...
      // (т.е. получим аналог создания файлов в проводнике Windows, например:
      // New folder -> New folder (2) -> New folder (3) и т.д.)

      Reader.ExtractAll('..\DemoResults\BuildWithExceptionUnpack\');

    finally
      Reader.Free;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
