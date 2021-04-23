unit Experiments.Eduardo.Experimento1;

{$mode objfpc}{$H+}

interface

procedure WriteToConfigurationFile(ADesign : string);

procedure WriteChoices;

const
  FolderMessages =
    'mensagens'+DirectorySeparator;
  FolderChoices =
    'tentativas'+DirectorySeparator;

implementation

uses Classes, SysUtils
   , Constants
   , LazFileUtils
   , FileMethods
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   ;

var
  MessageA1 : string;
  MessageA2 : string;
  MessageB : string;
  Delays : array [0..4] of string = ('1 mês', '3 meses', '6 meses', '1 ano', '2 anos');
  DelaysIndex : integer = 0;

const
  ITI = 500;

procedure SetupStimuli;
begin
  LoadMessageFromFile(MessageA1, GlobalContainer.RootMedia+FolderMessages+'MensagemA1.html');
  LoadMessageFromFile(MessageA2, GlobalContainer.RootMedia+FolderMessages+'MensagemA2.html');
  LoadMessageFromFile(MessageB,  GlobalContainer.RootMedia+FolderMessages+'MensagemB.html');
  //LoadMessageFromFile(MessageC,  GlobalContainer.RootMedia+FolderMessages+'MensagemC.html');
end;


procedure WriteMSG(ABlc : integer; AName: string; AMessage : string);
var
  i : integer;
begin
  i := ConfigurationFile.TrialCount[ABlc]+1;
  with ConfigurationFile do
  begin
    WriteToTrial(i, ABlc, _Name, AName);
    WriteToTrial(i, ABlc, _Cursor, '0');
    WriteToTrial(i, ABlc, _Kind, T_HTM);
    WriteToTrial(i, ABlc, _ITI, ITI.ToString);
    WriteToTrial(i, ABlc, _Msg, AMessage)
  end;
end;

procedure WriteChoice(ABlc : integer; AName: string;
  ALeft: string; ARight: string; ALNextTrial: string; ARNextTrial: string);
var
  i : integer;
begin
  i := ConfigurationFile.TrialCount[ABlc]+1;
  with ConfigurationFile do
  begin
    WriteToTrial(i, ABlc, _Name, AName);
    WriteToTrial(i, ABlc, _Cursor, '0');
    WriteToTrial(i, ABlc, _Kind, T_CHO);
    WriteToTrial(i, ABlc, _ITI, ITI.ToString);
    WriteToTrial(i, ABlc, 'L', ALeft);
    WriteToTrial(i, ABlc, 'L'+_NextTrial, ALNextTrial);
    WriteToTrial(i, ABlc, 'R', ARight);
    WriteToTrial(i, ABlc, 'R'+_NextTrial, ARNextTrial);
  end;
end;

procedure WriteTXTInput(ABlc : integer; AName: string; AMessage : string);
var
  i : integer;
begin
  i := ConfigurationFile.TrialCount[ABlc]+1;
  with ConfigurationFile do
  begin
    WriteToTrial(i, ABlc, _Name, AName);
    WriteToTrial(i, ABlc, _Cursor, '0');
    WriteToTrial(i, ABlc, _Kind, T_INP);
    WriteToTrial(i, ABlc, _ITI, ITI.ToString);
    WriteToTrial(i, ABlc, _Msg, AMessage);
  end;
end;

procedure WriteOperantTask(ABlc : integer; AName: string; AHasDelay : Boolean);
var
  i : integer;
begin
  i := ConfigurationFile.TrialCount[ABlc]+1;
  with ConfigurationFile do
  begin
    WriteToTrial(i, ABlc, _Name, AName);
    WriteToTrial(i, ABlc, _Cursor, '0');
    WriteToTrial(i, ABlc, _Kind, T_EO1);
    WriteToTrial(i, ABlc, _ITI, ITI.ToString);
    if AHasDelay then
      WriteToTrial(i, ABlc, 'Type', 'C')
    else
      WriteToTrial(i, ABlc, 'Type', 'B');
  end;
end;

procedure WriteDiscountBloc(ABlc : integer);
var
  i : integer;
  LNow : real;
  LLater : real = 100.0;
  LDelay : string;
  LNextTrial : string;
begin
  if DelaysIndex > High(Delays) then DelaysIndex := 0;
  LDelay := Delays[DelaysIndex];
  LNow := 50.0;
  for i := 0 to 9 do
    begin
      case i of
      0 : LNextTrial := '10';
      else
        LNextTrial := '-1';
      end;

      WriteChoice(ABlc,FloatToStrF(LNow,ffFixed,0,2)+#32+LDelay,
        'Ganhar '+#13+'R$' + FloatToStrF(LNow,ffFixed,0,2)   + ' reais' + #13 + 'agora',
        'Ganhar '+#13+'R$' + FloatToStrF(LLater,ffFixed,0,2) + ' reais' + #13 + 'daqui ' + LDelay,
        '',LNextTrial);
      LNow := LNow * 0.75;
    end;

  LNow := 50.0;
  for i := 0 to 8 do
    begin
      case i of
      0 : LNextTrial := '-10';
      8 : LNextTrial := '';
      else
        LNextTrial := '-1';
      end;
      LNow := LNow * 1.25;
      WriteChoice(ABlc,FloatToStrF(LNow,ffFixed,0,2)+#32+LDelay,
        'Ganhar '+#13+'R$' + FloatToStrF(LNow,ffFixed,0,2)   + ' reais' + #13 + 'agora',
        'Ganhar '+#13+'R$' + FloatToStrF(LLater,ffFixed,0,2) + ' reais' + #13 + 'daqui ' + LDelay,
        LNextTrial,'');
    end;
  Inc(DelaysIndex);
end;

procedure WriteDemandBloc(ABlc : integer);
var
  i : integer;
const
  LValues : array [0..20] of string =
    ('0,50','1,00','1,50','2,00',
    '3,00','4,00','5,00','6,00','7,00',
    '8,00','9,00','10,00','12,00',
    '15,00','20,00','25,00','30,00',
    '50,00','100,00','200,00','400,00');
begin
  WriteTXTInput(ABlc, 'Demanda 1', 'Quanto você consumiria se o cigarro fosse gratuito?');
  for i := Low(LValues) to High(LValues) do
    WriteTXTInput(ABlc, 'Demanda '+(i+2).ToString,
      'Quanto você consumiria se cada cigarro custasse R$'+LValues[i]+'?');
end;

procedure WriteB(ABlc : integer);
var
  i : integer;
begin
  for i := 0 to 59 do
    WriteOperantTask(ABlc, 'Operant '+(i+1).ToString, False);
end;

procedure WriteC(ABlc : integer);
var
  i : integer;
begin
  for i := 0 to 59 do
    WriteOperantTask(ABlc, 'Operant '+(i+1).ToString, True);
end;

procedure WriteToConfigurationFile(ADesign : string);
var
  LCondition : char;
  LConditionI : integer = 0;
  procedure WriteACondition;
  var
    i : integer;
  begin
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem A1');
    WriteMSG(LConditionI, 'M1', MessageA1);

    for i := Low(Delays) to High(Delays)do
    begin
      Inc(LConditionI);
      ConfigurationFile.WriteToBloc(LConditionI, _Name, Delays[i]);
      ConfigurationFile.WriteToBloc(LConditionI, _CrtMaxTrials, '10');
      WriteDiscountBloc(LConditionI);
    end;

    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem A2');
    WriteMSG(LConditionI, 'M1', MessageA2);

    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Demanda');
    WriteDemandBloc(LConditionI);
  end;


  procedure WriteBCondition;
  begin
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem B/C');
    WriteMSG(LConditionI, 'M1', MessageB);


    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'B');
    WriteB(LConditionI);
  end;


  procedure WriteCCondition;
  begin
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem C/B');
    WriteMSG(LConditionI, 'M1', MessageB);

    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'C');
    WriteC(LConditionI);
  end;

begin
  SetupStimuli;
  for LCondition in ADesign do
  case LCondition of
    'A': WriteACondition;
    'B': WriteBCondition;
    'C': WriteCCondition;
  end;
end;

procedure WriteChoices;
var
  i : integer;
  LStringList : TStringList;
  LNow : real;
  LLater : real = 100.0;
  LDelay : string;
  LNextTrial : string;
begin
  LStringList := TStringList.Create;
  try
    for LDelay in Delays do
      begin
        LNow := 50.0;
        for i := 0 to 9 do
          begin
            case i of
            0 : LNextTrial := '10';
            else
              LNextTrial := '-1';
            end;

            LStringList.Append(
              'Ganhar R$' + FloatToStrF(LNow,ffFixed,0,2)   + ' reais agora' + #9 +     //left
              'Ganhar R$' + FloatToStrF(LLater,ffFixed,0,2) + ' reais daqui ' + LDelay + #9 + //right
              LNextTrial);
            LNow := LNow * 0.75;
          end;

        LNow := 50.0;
        for i := 0 to 8 do
          begin
            case i of
            0 : LNextTrial := '-10';
            8 : LNextTrial := '';
            else
              LNextTrial := '-1';
            end;
            LNow := LNow * 1.25;
            LStringList.Append(
              'Ganhar R$' + FloatToStrF(LNow,ffFixed,0,2)   + ' reais agora' + #9 +
              'Ganhar R$' + FloatToStrF(LLater,ffFixed,0,2) + ' reais daqui ' + LDelay + #9 +
              LNextTrial);

          end;
      end;
    LStringList.SaveToFile(GlobalContainer.RootMedia+FolderChoices+'desconto.txt');
  finally
    LStringList.Free;
  end;
end;

end.
