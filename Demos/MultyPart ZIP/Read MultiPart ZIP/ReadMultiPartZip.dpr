////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : ReadMultiPartZip
//  * Purpose   : Демонстрация чтения многотомного архива
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

program ReadMultiPartZip;

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  TypInfo,
  FWZipStream,
  FWZipReader;

var
  Reader: TFWZipReader;
  MultiStream: TFWFileMultiStream;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try

    // Для чтения многотомных архивов так-же потребуется класс
    // наследник от TFWAbstractMultiStream.
    // В случае чтения используется конструктор CreateRead.
    // Первый параметр - путь к заголовку архива.
    // Второй параметр, режим открытия.
    // rsmQuick - ищутся все тома с конца не совпадающие размером с
    // первым томом, вплоть до тех пор, пока не найдется равный.
    // Остальные тома считаются равными первому.
    // Это быстрый способ открытия многотомного архива, но не совсем надежный,
    // по причине того что в тех случаях, когда один из томов
    // содержащий записи TCentralDirectoryFileHeader совпадет по размерам
    // с первым томом, все предыдущие будут считаться полными томами
    // содержащими данные, хотя это может быть не так.
    // Для гарантированного открытия нужно использовать режим rsmFull.
    // В этом случае будет зачитан фактический размер каждого тома архива.
    // Но, это более медленный вариант, особенно в том случае, когда
    // реальных томов достаточно много.
    // На количестве томов 60 тысяч и более время открытия может
    // исчислятся минутами!
    MultiStream := TFWFileMultiStream.CreateRead(
      '..\..\DemoResults\MultyPartZip\MultyPartZip.zip', rsmQuick);
    try

      Reader := TFWZipReader.Create;
      try

        // Чтение многотомных архивов осуществляется ТОЛЬКО через вызов метода
        // LoadFromStream с передачей параметров наследника класса TFWAbstractMultiStream
        Reader.LoadFromStream(MultiStream);

        // Вся остальная работа с архивом выглядит так-же как и с обычным.
        Reader.PasswordList.Add('password');

        // Например проверка целостности архива
        Reader.Check;

        // ... или его распаковка
        Reader.ExtractAll('*.pas', '..\..\DemoResults\MultyPartZip\');

        Writeln('done.');

      finally
        Reader.Free;
      end;
    finally
      MultiStream.Free;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
