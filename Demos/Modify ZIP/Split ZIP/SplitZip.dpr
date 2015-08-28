////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : SplitZip
//  * Purpose   : Демонстрация работы c разбитием архива
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

// Данный пример показывает как можно разделить архив на несколько частей,
// без необходимости распаковки данных и повторного сжатия.

program SplitZip;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Classes,
  FWZipWriter,
  FWZipModifier;

procedure AddItem(AWriter: TFWZipWriter; const AName, AData: string);
var
  S: TStringStream;
begin
  S := TStringStream.Create(AData);
  try
    S.Position := 0;
    AWriter.AddStream(AName, S);
  finally
    S.Free;
  end;
end;

var
  Writer: TFWZipWriter;
  Modifier: TFWZipModifier;
  Index: TReaderIndex;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    // создаем архив который будем разделять
    Writer := TFWZipWriter.Create;
    try
      // добавляем 4 элемента в архив
      AddItem(Writer, 'test1.txt', 'первый элемент');
      AddItem(Writer, 'test2.txt', 'второй элемент');
      AddItem(Writer, 'test3.txt', 'третий элемент');
      AddItem(Writer, 'test4.txt', 'четвертый элемент');
      // сохраняем
      ForceDirectories('..\..\DemoResults\');
      Writer.BuildZip('..\..\DemoResults\split_main_archive.zip');
    finally
      Writer.Free;
    end;
    // создаем экземпляр модификатора архивов
    Modifier := TFWZipModifier.Create;
    try
      // подключаем ранее созданный архив
      Index := Modifier.AddZipFile('..\..\DemoResults\split_main_archive.zip');
      // добавляем из него первые два элемента
      Modifier.AddFromZip(Index, 'test1.txt');
      Modifier.AddFromZip(Index, 'test2.txt');
      // и сохраняем в новый архив
      // при этом реальной перепаковки данных не произойдет,
      // данные возьмутся как есть в виде массива байт прямо в сжатом виде
      // из оригинального архива
      Modifier.BuildZip('..\..\DemoResults\splited_archive1.zip');

      // теперь удаляем добавленные элементы и добавляем вторые два
      Modifier.Clear;
      Modifier.AddFromZip(Index, 'test3.txt');
      Modifier.AddFromZip(Index, 'test4.txt');
      // сохраняем во торой архив
      Modifier.BuildZip('..\..\DemoResults\splited_archive2.zip');
    finally
      Modifier.Free;
    end;

    // ворт и все, мы разделили изначальный архив на две части
    // не занимаясь перепаковкой данных
    // как обьединять смотрите в примере MergeZip
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
