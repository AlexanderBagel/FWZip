////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : UseExDataBlob
//  * Purpose   : Демонстрация работы с блоками ExData
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

// Данный пример показывает работу с блоками ExData.
// Их добавление при создании архива и извлечение.
// Данные блоки могут содержать любые дополнительные параметры связанные
// с элементом архива. Список зарезервированных блоков см. в коментарии
// обработчика OnSaveExData.

program UseExDataBlob;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  FWZipWriter,
  FWZipReader,
  FWZipConsts;

const
  TestExDataBlob: Cardinal = $DEADBEEF;

var
  Writer: TFWZipWriter;
  Reader: TFWZipReader;
  Method: TMethod;

//
//  Обработчик при помощи которого мы добавляем блок расширенных данных ExData
//  к каждому элементу массива
// =============================================================================
procedure OnSaveExData(Self, Sender: TObject; ItemIndex: Integer;
  UserExDataBlockCount: Integer; var Tag: Word; Data: TStream);
var
  RandomValue: Cardinal;
begin

  // При добавлении блока расширенных данных следует указать его Тэг и
  // записать сами данные в стрим Data, при этом размер Data не может
  // превысить значение MAXWORD а значение Тэг должно быть отлично от нуля.

  // Данный обработчик будет вызываться до тех пор, пока вы не укажете
  // что более блоков данных нет. Для этого нужно не заполнять стрим Data.

  // Количество вызовов обработчика для текущего элемента можно узнать по
  // переменной UserExDataBlockCount

  // Обратите внимание, следующие значения тэгов зарезервированы:

  {
          The current Header ID mappings defined by PKWARE are:

          0x0001        ZIP64 extended information extra field
          0x0007        AV Info
          0x0008        Reserved for future Unicode file name data (PFS)
          0x0009        OS/2 extended attributes      (also Info-ZIP)
          0x000a        NTFS (Win9x/WinNT FileTimes)
          0x000c        OpenVMS                       (also Info-ZIP)
          0x000d        Unix
          0x000e        Reserved for file stream and fork descriptors
          0x000f        Patch Descriptor
          0x0014        PKCS#7 Store for X.509 Certificates
          0x0015        X.509 Certificate ID and Signature for
                        individual file
          0x0016        X.509 Certificate ID for Central Directory
          0x0017        Strong Encryption Header
          0x0018        Record Management Controls
          0x0019        PKCS#7 Encryption Recipient Certificate List
          0x0065        IBM S/390 (Z390), AS/400 (I400) attributes
                        - uncompressed
          0x0066        Reserved for IBM S/390 (Z390), AS/400 (I400)
                        attributes - compressed

          The Header ID mappings defined by Info-ZIP and third parties are:

          0x07c8        Info-ZIP Macintosh (old, J. Lee)
          0x2605        ZipIt Macintosh (first version)
          0x2705        ZipIt Macintosh v 1.3.5 and newer (w/o full filename)
          0x2805        ZipIt Macintosh 1.3.5+
          0x334d        Info-ZIP Macintosh (new, D. Haase's 'Mac3' field)
          0x4154        Tandem NSK
          0x4341        Acorn/SparkFS (David Pilling)
          0x4453        Windows NT security descriptor (binary ACL)
          0x4704        VM/CMS
          0x470f        MVS
          0x4854        Theos, old inofficial port
          0x4b46        FWKCS MD5 (see below)
          0x4c41        OS/2 access control list (text ACL)
          0x4d49        Info-ZIP OpenVMS (obsolete)
          0x4d63        Macintosh SmartZIP, by Macro Bambini
          0x4f4c        Xceed original location extra field
          0x5356        AOS/VS (binary ACL)
          0x5455        extended timestamp
          0x554e        Xceed unicode extra field
          0x5855        Info-ZIP Unix (original; also OS/2, NT, etc.)
          0x6542        BeOS (BeBox, PowerMac, etc.)
          0x6854        Theos
          0x7441        AtheOS (AtheOS/Syllable attributes)
          0x756e        ASi Unix
          0x7855        Info-ZIP Unix (new)
          0xfb4a        SMS/QDOS
  }

  // Для примера мы заполним три блока данных:
  case UserExDataBlockCount of
    0:
    begin
      // Выбираем незарезервированное значение тэга (например $FFFA)
      Tag := $FFFA;
      // и пишем сами данные
      Data.WriteBuffer(TestExDataBlob, 4);
    end;
    1..2:
    begin
      // Выбираем другое незарезервированное значение тэга
      Tag := $FFFB + UserExDataBlockCount;
      // данные рандомные - просто для демонстрации записи двух и более полей
      Randomize;
      RandomValue := Random(MaxInt);
      Data.WriteBuffer(RandomValue, 4);
    end;
  end;
end;

//
//  Обработчик при помощи которого мы получаем блок расширенных данных ExData
//  который не смог обработать TFWZipReader.
//  Обработчик вызывается для каждого нераспознанного элемента ExData,
//  количество которых не ограничено.
//  Данный обработчик вызывается при вызове метода TFWZipReader.LoadFromFile
//  Sender в данном обработчике является TFWZipReaderItem,
//  т.е. элементом при чтении которого не распознался тэг ExData.
// =============================================================================
procedure OnLoadExData(Self, Sender: TObject; ItemIndex: Integer;
  Tag: Word; Data: TStream);
var
  Value: Cardinal;
begin
  // так как мы знаем как нужно обрабатывать только блок данных с тэгом $FFFA
  // то остальные необходимо пропустить
  if Tag = $FFFA then
  begin
    // Для демонстрации просто проверяем совпадает ли значение в блоке с нашим
    if Data.Size <> 4 then
      raise Exception.Create('Неверный размер блока ExData');
    Data.ReadBuffer(Value, Data.Size);
    if Value <> TestExDataBlob then
      raise Exception.Create('Неверное значение блока ExData');

    // связать данные с элементом массива можно например используя поле Tag
    TFWZipReaderItem(Sender).Tag := Integer(Value);

    // вот такой вызов делать нельзя, т.к. в данный момент Sender
    // находится в конструкторе и не добавлен в список элементов
    // главного класса
    // Reader[ItemIndex].Tag := Integer(Value); - ошибочный код
  end;
end;

begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    Writer := TFWZipWriter.Create;
    try
      // Для начала добавим в корень архива файлы из корневой директории
      Writer.AddFolder('', '..\..\', '*.*', False);

      // Назначаем обработчик через который мы будем добавлять блок расширенных данных
      Method.Code := @OnSaveExData;
      Method.Data := Writer;
      Writer.OnSaveExData := TZipSaveExDataEvent(Method);

      // Сохраняем результат
      ForceDirectories('..\DemoResults\');
      Writer.BuildZip('..\DemoResults\UseExDataBlob.zip');
    finally
      Writer.Free;
    end;

    Reader := TFWZipReader.Create;
    try

      // Теперь наша задача получить расширенные блоки данных.
      // Для этого необходимо назначить обработчик OnLoadExData
      // и открыть сам архив.
      Method.Code := @OnLoadExData;
      Method.Data := Reader;
      Reader.OnLoadExData := TZipLoadExDataEvent(Method);
      Reader.LoadFromFile('..\DemoResults\UseExDataBlob.zip');
    finally
      Reader.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
