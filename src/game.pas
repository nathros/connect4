
{***********************************************}
{*   Connect 4 - Classes and Data Structures   *}
{* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *}
{*                                             *}
{*    Author: Nathan Bartram                   *}
{*    Contact: nathan_bartram@hotmail.co.uk    *}
{*    Licence: GPL                             *}
{*                                             *}
{***********************************************}

unit Game;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, IniFiles;

type
  TGrid = array[0..8,0..7] of byte;

type
  TTinyPoint = record
    x: byte;
    y: byte;
  end;

type
  TWinDirection = record
    Start: TTinyPoint;
    Parts: byte;
  end;

type
  TMovesHistory = record
    StartPlayer: boolean;
    Move: array[1..42] of TTinyPoint;
  end;

type
  TWinPosition = class
  private
    _HorizontalWin: boolean;
    _VerticalWin: boolean;
    _LeftDiagonalWin: boolean;
    _RightDiagonalWin: boolean;
    function TinyPoint(x,y: byte): TTinyPoint;
  public
    Horizontal: TWinDirection;
    Vertical: TWinDirection;
    LeftDiagonal: TWinDirection;
    RightDiagonal: TWinDirection;
    constructor Create;
    procedure ClearData;
    property IsHorizontalWin: boolean read _HorizontalWin write _HorizontalWin;
    property IsVerticalWin: boolean read _VerticalWin write _VerticalWin;
    property IsLeftDiagonalWin: boolean read _LeftDiagonalWin write _LeftDiagonalWin;
    property IsRightDiagonalWin: boolean read _RightDiagonalWin write _RightDiagonalWin;
  end;

type
  TSettings = class
  private
    Config: TMemIniFile;
    _PlayerStart: byte; // 0 = random, 1 = player1, 2 = player2.
    _ShowToolBar: boolean;
    _NewCoinPosition: byte;//  0 = random, 1 = left, 2 = centre, 3 = right.
    _ShowCoinPreview: boolean;
    _ShowStatusBar: boolean;
    _FormLeft: integer;
    _FormTop: integer;
    _BotLevel: integer;
    _IsPlayer1Bot: boolean;
    _IsPlayer2Bot: boolean;
    _Player1Name: string;
    _Player2Name: string;
    _Skin: string;
  public
    constructor Create;
    destructor Free;
    procedure Read;
    procedure Write;
    property PlayerStart: byte read _PlayerStart write _PlayerStart;
    property ShowToolBar: boolean read _ShowToolBar write _ShowToolBar;
    property NewCoinPosition: byte read _NewCoinPosition write _NewCoinPosition;
    property ShowCoinPreview: boolean read _ShowCoinPreview write _ShowCoinPreview;
    property ShowStatusBar: boolean read _ShowStatusBar write _ShowStatusBar;
    property FormLeft: integer read _FormLeft write _FormLeft;
    property FormTop: integer read _FormTop write _FormTop;
    property BotLevel: integer read _BotLevel write _BotLevel;
    property IsPlayer1Bot: boolean read _IsPlayer1Bot write _IsPlayer1Bot;
    property IsPlayer2Bot: boolean read _IsPlayer2Bot write _IsPlayer2Bot;
    property Player1Name: string read _Player1Name write _Player1Name;
    property Player2Name: string read _Player2Name write _Player2Name;
    property Skin: string read _Skin write _Skin;
  end;

type

  { TGame }

  TGame = class
  private
    MovesGrid: TGrid; // 0 = no move, 1 = player1, 2 = player2.
    PlayerGo: boolean; // false = player1, true = player2.
    NumberOfMoves: byte; // Counts moves made, needed for NewCoin in main unit.
    MovesHistory: TMovesHistory;
    WinLoseCount: integer;
    function GetPlayerNumber: byte; overload;
    // Functions used by bot
    function MinMax(Depth: integer;Player: boolean;CurrentGrid:TGrid): integer;
    function GetScore(x,y: byte;player:boolean;CurrentGrid:TGrid):integer;
    function GetDropLocation(x: byte;Grid:TGrid): byte; overload;
    function IsWin(x,y:byte;player:boolean;CurrentGrid: TGrid): boolean; overload;
    function GetPlayerNumber(p:boolean): byte; overload;
    function ChooseBest(ResultList, WinLoseResults: array of integer): integer;
  public
    Settings: TSettings;
    WinPosition: TWinPosition;
    constructor Create;
    destructor Free;
    function GetNumberOfMoves: byte;
    procedure IncNumberOfMoves;
    procedure DecNumberOfMoves;
    procedure ResetNumberOfMoves;
    function GetDropLocation(x: byte): byte; overload;// Returns y value of new drop location.
    procedure SetMove(x, y: byte); // Sets move in MovesGrid.
    function GetPlayerGo: boolean;
    procedure SwitchPlayer;
    function IsWin: boolean; overload;
    procedure NewGame;
    function IsGridFull: boolean;
    procedure UndoMove;
    procedure RedoMove;
    procedure GetMoveFromList(const MoveNumber: byte; out x, y :byte);
    function GetLastMoveX: byte;
    function GetLastMoveY: byte;
    procedure SaveGame(const FileName: string);
    procedure LoadGame(const FileName: string);
    function IsCurrentPlayerBot: boolean;
    function IsOtherPlayerBot: boolean;
    function IsStartPlayerBot: boolean;
    function GetFirstPlayer: boolean;
    function GetBotMove: byte;
  end;

implementation

//---TGame--------------------------------------------------------------------//
constructor TGame.Create;
begin
  Settings:=TSettings.Create;
  WinPosition:=TWinPosition.Create;
end;
//----------------------------------------------------------------------------//
destructor TGame.Free;
begin
  Settings.Free;
  WinPosition.Free;
end;
//----------------------------------------------------------------------------//
function TGame.GetNumberOfMoves: byte;
begin
  Result:=NumberOfMoves;
end;
//----------------------------------------------------------------------------//
procedure TGame.IncNumberOfMoves;
begin
  Inc(NumberOfMoves);
end;
//----------------------------------------------------------------------------//
procedure TGame.DecNumberOfMoves;
begin
  Dec(NumberOfMoves);
end;
//----------------------------------------------------------------------------//
procedure TGame.ResetNumberOfMoves;
begin
  NumberOfMoves:=0;
end;
//----------------------------------------------------------------------------//
function TGame.GetDropLocation(x: byte): byte; overload;
var i: byte;
begin
  for i:=6 downto 0 do
  if MovesGrid[x,i]=0 then Break;
  Result:=i;
end;
//----------------------------------------------------------------------------//
procedure TGame.SetMove(x, y: byte);
begin
  MovesGrid[x,y]:=GetPlayerNumber;
  MovesHistory.Move[NumberOfMoves]:=WinPosition.TinyPoint(x,y);
  if NumberOfMoves<>42 then
  MovesHistory.Move[NumberOfMoves+1]:=WinPosition.TinyPoint(0,0);
end;
//----------------------------------------------------------------------------//
function TGame.GetPlayerGo: boolean;
begin
  Result:=PlayerGo;
end;
//----------------------------------------------------------------------------//
procedure TGame.SwitchPlayer;
begin
  PlayerGo:=not PlayerGo;
end;
//----------------------------------------------------------------------------//
function TGame.IsWin: boolean; overload;
var
  Count, i, j, PlayerNumber, XPos, YPos: byte;
  Found4InRow: boolean = false;
begin
  WinPosition.ClearData;
  XPos:=GetLastMoveX;
  YPos:=GetLastMoveY;
  PlayerNumber:=GetPlayerNumber;
  Found4InRow:=false;
  // Check horizontal.
  for i := 1 to 4 do
    begin
      if MovesGrid[i  ,YPos]=PlayerNumber then
      if MovesGrid[i+1,YPos]=PlayerNumber then
      if MovesGrid[i+2,YPos]=PlayerNumber then
      if MovesGrid[i+3,YPos]=PlayerNumber then
        begin
          j:=3;
          while MovesGrid[i+j,YPos]=PlayerNumber do Inc(j);
          WinPosition.Horizontal.Start:=WinPosition.TinyPoint(i,YPos);
          WinPosition.Horizontal.Parts:=j;
          WinPosition.IsHorizontalWin:=true;
          Found4InRow:=true;
          Break;
        end;
    end;
  // Check vertical.
  for i := 1 to 4 do
    begin
      if MovesGrid[XPos,i  ]=PlayerNumber then
      if MovesGrid[XPos,i+1]=PlayerNumber then
      if MovesGrid[XPos,i+2]=PlayerNumber then
      if MovesGrid[XPos,i+3]=PlayerNumber then
        begin
          WinPosition.Vertical.Start:=WinPosition.TinyPoint(XPos,i);
          WinPosition.Vertical.Parts:=4;
          WinPosition.IsVerticalWin:=true;
          Found4InRow:=true;
          Break;
        end;
    end;
  // Check left diagonal.
  if (XPos<>0) and (YPos<>0) then
  Count:=0;
  repeat
    Count:=Count+1;
    XPos:=XPos-1;
    YPos:=YPos-1;
  until (XPos=0) or (YPos=0);

  for i := 1 to 4 do
    begin
      if MovesGrid[XPos+i  ,YPos+i  ]=PlayerNumber then
      if MovesGrid[XPos+i+1,YPos+i+1]=PlayerNumber then
      if MovesGrid[XPos+i+2,YPos+i+2]=PlayerNumber then
      if MovesGrid[XPos+i+3,YPos+i+3]=PlayerNumber then
        begin
          j:=3;
          while MovesGrid[XPos+i+j,YPos+i+j]=PlayerNumber do Inc(j);
          WinPosition.LeftDiagonal.Start:=WinPosition.TinyPoint(XPos+i,YPos+i);
          WinPosition.LeftDiagonal.Parts:=j;
          WinPosition.IsLeftDiagonalWin:=true;
          Found4InRow:=true;
          Break;
        end;
    end;
  // Check right diagonal.
  repeat
    Count:=Count-1;
    XPos:=XPos+1;
    YPos:=YPos+1;
  until count=0;

  repeat
    XPos:=XPos+1;
    YPos:=YPos-1;
  until (XPos=8) or (YPos=0);

  for i := 1 to 4 do
    begin
      if MovesGrid[XPos-i  ,YPos+i  ]=PlayerNumber then
      if MovesGrid[XPos-i-1,YPos+i+1]=PlayerNumber then
      if MovesGrid[XPos-i-2,YPos+i+2]=PlayerNumber then
      if MovesGrid[XPos-i-3,YPos+i+3]=PlayerNumber then
        begin
          j:=3;
          while MovesGrid[XPos-i-j,YPos+i+j]=PlayerNumber do Inc(j);
          WinPosition.RightDiagonal.Start:=WinPosition.TinyPoint(XPos-i,YPos+i);
          WinPosition.RightDiagonal.Parts:=j;
          WinPosition.IsRightDiagonalWin:=true;
          Found4InRow:=true;
          Break;
        end;
    end;
  Result:=Found4InRow;
end;
//----------------------------------------------------------------------------//
procedure TGame.NewGame;
var i, j: byte;
begin
  for i:=0 to 8 do
    for j:=0 to 7 do
      MovesGrid[i,j]:=0;

  for i:=1 to 42 do
    MovesHistory.Move[i]:=WinPosition.TinyPoint(0,0);

  case Settings.PlayerStart of
    0: if Random(2)=0 then PlayerGo:=true
       else PlayerGo:=false;
    1: PlayerGo:=true;
    2: PlayerGo:=false;
  end;
  MovesHistory.StartPlayer:=PlayerGo;
  NumberOfMoves:=0;
  WinPosition.ClearData;
end;
//----------------------------------------------------------------------------//
function TGame.IsGridFull: boolean;
var i: byte;
begin
  for i:=1 to 7 do
    if MovesGrid[i,1]=0 then
      begin
        Result:=false;
        Exit;
      end;
  Result:=true;
end;
//----------------------------------------------------------------------------//
procedure TGame.UndoMove;
begin
  DecNumberOfMoves;
  MovesGrid[MovesHistory.Move[NumberOfMoves].x,MovesHistory.Move[NumberOfMoves].y]:=0;
  SwitchPlayer;
end;
//----------------------------------------------------------------------------//
procedure TGame.RedoMove;
var x, y: byte;
begin
  GetMoveFromList(NumberOfMoves,x,y);
  MovesGrid[x,y]:=GetPlayerNumber;
  //SwitchPlayer;
end;
//----------------------------------------------------------------------------//
procedure TGame.GetMoveFromList(const MoveNumber: byte; out x, y :byte);
begin
  if MoveNumber>42 then
    begin
      x:=0;
      y:=x;
    end
  else
    begin
      x:=MovesHistory.Move[MoveNumber].x;
      y:=MovesHistory.Move[MoveNumber].y;
    end;
end;
//----------------------------------------------------------------------------//
function TGame.GetLastMoveX: byte;
begin
  Result:=MovesHistory.Move[NumberOfMoves].x;
end;
//----------------------------------------------------------------------------//
function TGame.GetLastMoveY: byte;
begin
  Result:=MovesHistory.Move[NumberOfMoves].y;
end;
//----------------------------------------------------------------------------//
procedure TGame.SaveGame(const FileName: string);
var
  DataFile: file of byte;
  i: byte;
begin
  AssignFile(DataFile, ChangeFileExt(FileName,'.save'));
  Rewrite(DataFile);
  if MovesHistory.StartPlayer then Write(DataFile,1)
  else Write(DataFile,0);
  if IsWin or IsGridFull then Write(DataFile,NumberOfMoves+1)
  else Write(DataFile,NumberOfMoves);
  for i:=1 to 42 do
    begin
      Write(DataFile,MovesHistory.Move[i].x);
      Write(DataFile,MovesHistory.Move[i].y);
    end;
  CloseFile(DataFile);
end;
//----------------------------------------------------------------------------//
procedure TGame.LoadGame(const FileName: string);
var
  DataFile: file of byte;
  i, temp: byte;
begin
  NewGame;
  AssignFile(DataFile, FileName);
  Reset(DataFile);
  Read(DataFile,temp);
  if temp=1 then PlayerGo:=true
  else PlayerGo:=false;
  MovesHistory.StartPlayer:=PlayerGo;
  Read(DataFile,NumberOfMoves);
  for i:=1 to 42 do
    begin
      Read(DataFile,MovesHistory.Move[i].x);
      Read(DataFile,MovesHistory.Move[i].y);
      if i<NumberOfMoves then
      MovesGrid[MovesHistory.Move[i].x,MovesHistory.Move[i].y]:=GetPlayerNumber;
      SwitchPlayer;
    end;
  CloseFile(DataFile);
  PlayerGo:=MovesHistory.StartPlayer;
end;
//----------------------------------------------------------------------------//
function TGame.GetPlayerNumber: byte; overload;
begin
  if PlayerGo then Result:=1
  else Result:=2;
end;
//----------------------------------------------------------------------------//
function TGame.IsCurrentPlayerBot: boolean;
begin
  if PlayerGo and Settings.IsPlayer1Bot then Result:=true
  else if (not PlayerGo) and Settings.IsPlayer2Bot then Result:=true
  else Result:=false;
end;
//----------------------------------------------------------------------------//
function TGame.IsOtherPlayerBot: boolean;
begin
  if PlayerGo then Result:=Settings.IsPlayer1Bot
  else Result:=Settings.IsPlayer2Bot;
end;
//----------------------------------------------------------------------------//
function TGame.IsStartPlayerBot: boolean;
begin
  if MovesHistory.StartPlayer then Result:=Settings.IsPlayer2Bot
  else Result:=Settings.IsPlayer1Bot;
end;
//----------------------------------------------------------------------------//
function TGame.GetFirstPlayer: boolean;
begin
  Result:=MovesHistory.StartPlayer;
end;
//----------------------------------------------------------------------------//
function TGame.GetBotMove: byte;
var
  TempGrid: TGrid;
  MoveResults, WinLoseResults: array[1..7] of integer;
  i, y: integer;
begin
  TempGrid:=MovesGrid;
  for i:=1 to 7 do // Check for win
    begin
      y:=GetDropLocation(i,TempGrid);
      if y=0 then Continue
      else
        begin
          TempGrid[i,y]:=GetPlayerNumber(PlayerGo);
          If IsWin(i,y,PlayerGo,TempGrid) then
            begin
              Result:=i;
              Exit;
            end;
          TempGrid[i,y]:=0;
        end;
    end;
  for i:=1 to 7 do // check for lose
    begin
      y:=GetDropLocation(i,TempGrid);
      if y=0 then Continue
      else
        begin
          TempGrid[i,y]:=GetPlayerNumber(not PlayerGo);
          If IsWin(i,y,not PlayerGo,TempGrid) then
            begin
              Result:=i;
              Exit;
            end;
          TempGrid[i,y]:=0;
        end;
    end;
  TempGrid:=MovesGrid;
  for i:=1 to 7 do // Start Minmax
    begin
      y:=GetDropLocation(i,TempGrid);
      if y=0 then Continue
      else
        begin
          WinLoseCount:=0;
          TempGrid[i,y]:=GetPlayerNumber(PlayerGo);
          MoveResults[i]:=MinMax(Settings.BotLevel,not PlayerGo,TempGrid);
          WinLoseResults[i]:=WinLoseCount;
          TempGrid[i,y]:=0;
        end;
    end;
  Result:=ChooseBest(MoveResults,WinLoseResults);
end;
//---Bot Functions------------------------------------------------------------//
function TGame.MinMax(Depth: integer; Player: boolean; CurrentGrid:TGrid): integer;
var
  MoveStats: array[1..7] of integer;
  x, y: byte;
  temp: integer;
begin
  for x:=1 to 7 do
    begin
      y:=GetDropLocation(x,CurrentGrid);
      if y=0 then
        begin
          MoveStats[x]:=0;
          Continue;
        end
      else
        begin
          CurrentGrid[x,y]:=GetPlayerNumber(Player);
          if IsWin(x,y,Player,CurrentGrid) then
            begin
              if Player=PlayerGo then
                begin
                  Result:=42;
                  Inc(WinLoseCount);
                end
              else
                begin
                  Result:=-42;
                  Dec(WinLoseCount);
                end;
              Exit;
            end
          else if Depth=0 then
            begin
              MoveStats[x]:=GetScore(x,y,player,CurrentGrid);
              if Player<>PlayerGo then MoveStats[x]:=MoveStats[x]*-1;
            end
          else if Depth<>0 then MoveStats[x]:=MinMax(Depth-1,not Player,CurrentGrid);
        end;
      CurrentGrid[x,y]:=0;
    end;
  temp:=MoveStats[1];
  if Player=PlayerGo then
    begin
      for x:=2 to 7 do
        if temp<MoveStats[x] then temp:=MoveStats[x];
    end
  else
    begin
      for x:=2 to 7 do
        if temp>MoveStats[x] then temp:=MoveStats[x];
    end;
  Result:=temp;
end;
//----------------------------------------------------------------------------//
function TGame.ChooseBest(ResultList, WinLoseResults: array of integer): integer;
var
  x, y, x1, i, count, countWinLose: integer;
  //Duplicate: array[0..6] of integer = (-1,-1,-1,-1,-1,-1,-1);
begin
  count:=0;
  x:=-100;
  for i:=0 to 6 do
    begin
      y:=GetDropLocation(i+1);
      if y=0 then ResultList[i]:=-100;
      if x<ResultList[i] then x:=ResultList[i];
    end;
  for i:=0 to 6 do
    if x=ResultList[i] then
      begin
        Inc(count);
        Result:=i+1;
      end;

  if count>1 then
    begin
      x1:=-1000000;
      for i:=0 to 6 do
        if (ResultList[i]=x) and (x1<WinLoseResults[i]) then x1:=WinLoseResults[i];
      countWinLose:=0;
      for i:=0 to 6 do
        if (x1=WinLoseResults[i]) and (x=ResultList[i]) then Inc(countWinLose);
      if countWinLose>=1 then
        begin
          y:=Random(countWinLose)+1;
          for i:=1 to 6 do
            begin
              if (WinLoseResults[i]=x1) and (x=ResultList[i]) then Dec(y);
              if y=0 then
                begin
                  Result:=i+1;
                  Exit;
                end;
            end;
        end
      else
        begin
          y:=Random(count)+1;
          for i:=0 to 6 do
            begin
              if ResultList[i]=x then Dec(y);
              if y=0 then
                begin
                  Result:=i+1;
                  Exit;
                end;
            end;
        end;
    end;

end;
//----------------------------------------------------------------------------//
function TGame.IsWin(x,y:byte;player:boolean;CurrentGrid:TGrid): boolean; overload;
var Count, i, PlayerNumber, XPos, YPos: byte;
begin
  XPos:=x;
  YPos:=y;
  PlayerNumber:=GetPlayerNumber(player);
  Result:=false;
  // Check horizontal.
  for i := 1 to 4 do
    begin
      if CurrentGrid[i  ,YPos]=PlayerNumber then
      if CurrentGrid[i+1,YPos]=PlayerNumber then
      if CurrentGrid[i+2,YPos]=PlayerNumber then
      if CurrentGrid[i+3,YPos]=PlayerNumber then
        begin
          Result:=true;
          Exit;
        end;
    end;
  // Check vertical.
  for i := 1 to 4 do
    begin
      if CurrentGrid[XPos,i  ]=PlayerNumber then
      if CurrentGrid[XPos,i+1]=PlayerNumber then
      if CurrentGrid[XPos,i+2]=PlayerNumber then
      if CurrentGrid[XPos,i+3]=PlayerNumber then
        begin
          Result:=true;
          Exit;
        end;
    end;
  // Check left diagonal.
  if (XPos<>0) and (YPos<>0) then
  Count:=0;
  repeat
    Count:=Count+1;
    XPos:=XPos-1;
    YPos:=YPos-1;
  until (XPos=0) or (YPos=0);

  for i := 1 to 4 do
    begin
      if CurrentGrid[XPos+i  ,YPos+i  ]=PlayerNumber then
      if CurrentGrid[XPos+i+1,YPos+i+1]=PlayerNumber then
      if CurrentGrid[XPos+i+2,YPos+i+2]=PlayerNumber then
      if CurrentGrid[XPos+i+3,YPos+i+3]=PlayerNumber then
        begin
          Result:=true;
          Exit;
        end;
    end;
  // Check right diagonal.
  repeat
    Count:=Count-1;
    XPos:=XPos+1;
    YPos:=YPos+1;
  until count=0;

  repeat
    XPos:=XPos+1;
    YPos:=YPos-1;
  until (XPos=8) or (YPos=0);

  for i := 1 to 4 do
    begin
      if CurrentGrid[XPos-i  ,YPos+i  ]=PlayerNumber then
      if CurrentGrid[XPos-i-1,YPos+i+1]=PlayerNumber then
      if CurrentGrid[XPos-i-2,YPos+i+2]=PlayerNumber then
      if CurrentGrid[XPos-i-3,YPos+i+3]=PlayerNumber then
        begin
          Result:=true;
          Exit;
        end;
    end;
end;
//----------------------------------------------------------------------------//
function TGame.GetScore(x,y: byte;player:boolean;CurrentGrid:TGrid):integer;
var PlayerNumber, i, count, temp: Integer;
begin // when end of tree is reached and no win or lose, then gen score based on adjacent moves
  PlayerNumber:=GetPlayerNumber(player);
  count:=0;
  // Check horizontal
  for i:=x-1 downto 1 do
    if CurrentGrid[i,y]=PlayerNumber then Inc(count)
    else Break;
  for i:=x+1 to 7 do
    if CurrentGrid[i,y]=PlayerNumber then Inc(count)
    else Break;
  // Check vertical
  for i:=y-1 downto 1 do
    if CurrentGrid[x,i]=PlayerNumber then Inc(count)
    else Break;
  for i:=y+1 to 6 do
    if CurrentGrid[x,i]=PlayerNumber then Inc(count)
    else Break;
  // Check left diagonal
  for i:=x-1 downto 1 do
    begin
      temp:=y-(x-i);
      if temp<>0 then
        if CurrentGrid[i,temp]=PlayerNumber then Inc(count)
        else Break;
    end;
  for i:=x+1 to 7 do
    begin
      temp:=y+(x-i);
      if temp<>7 then
        if CurrentGrid[i,temp]=PlayerNumber then Inc(count)
        else Break;
    end;
   // Check right diagonal
    for i:=x-1 downto 1 do
    begin
      temp:=y+(x-i);
      if temp<>7 then
        if CurrentGrid[i,temp]=PlayerNumber then Inc(count)
        else Break;
    end;
  for i:=x+1 to 7 do
    begin
      temp:=y-(x-i);
      if temp<>0 then
        if CurrentGrid[i,temp]=PlayerNumber then Inc(count)
        else Break;
    end;
  Result:=count;
end;
//----------------------------------------------------------------------------//
function TGame.GetDropLocation(x: byte;Grid:TGrid): byte; overload;
var i: byte;
begin
  for i:=6 downto 0 do
  if Grid[x,i]=0 then Break;
  Result:=i;
end;
//----------------------------------------------------------------------------//
function TGame.GetPlayerNumber(p:boolean): byte; overload;
begin
  if p then Result:=1
  else Result:=2;
end;
//---Bot Functions------------------------------------------------------------//

//---TSettings----------------------------------------------------------------//
constructor TSettings.Create;
begin
  Config:=TMemIniFile.Create('Config.ini',false);
  Read;
end;
//----------------------------------------------------------------------------//
destructor TSettings.Free;
begin
  Config.Free;
end;
//----------------------------------------------------------------------------//
procedure TSettings.Write;
begin
  try
    Config.WriteInteger('Settings','PlayerStart',PlayerStart);
    Config.WriteInteger('Settings','NewCoinPosition',NewCoinPosition);
    Config.WriteBool('Settings','ShowCoinPreview',ShowCoinPreview);
    Config.WriteBool('View','ShowToolBar',ShowToolBar);
    Config.WriteBool('View','ShowStatusBar',ShowStatusBar);
    Config.WriteInteger('View','FormLeft',FormLeft);
    Config.WriteInteger('View','FormTop',FormTop);
    Config.WriteInteger('Settings','BotLevel',BotLevel);
    Config.WriteBool('Player','IsPlayer1Bot',IsPlayer1Bot);
    Config.WriteBool('Player','IsPlayer2Bot',IsPlayer2Bot);
    Config.WriteString('Player','Player1Name',Player1Name);
    Config.WriteString('Player','Player2Name',Player2Name);
    Config.WriteString('View','Skin',Skin);
  except
    // Do nothing.
  end;
end;
//----------------------------------------------------------------------------//
procedure TSettings.Read;
begin
  try
    PlayerStart:=Config.ReadInteger('Settings','PlayerStart',1);
    NewCoinPosition:=Config.ReadInteger('Settings','NewCoinPosition',1);
    ShowCoinPreview:=Config.ReadBool('Settings','ShowCoinPreview',true);
    ShowToolBar:=Config.ReadBool('View','ShowToolBar',true);
    ShowStatusBar:=Config.ReadBool('View','ShowStatusBar',true);
    FormLeft:=Config.ReadInteger('View','FormLeft',-1000);
    FormTop:=Config.ReadInteger('View','FormTop',-1000);
    BotLevel:=Config.ReadInteger('Settings','BotLevel',4);
    IsPlayer1Bot:=Config.ReadBool('Player','IsPlayer1Bot',false);
    IsPlayer2Bot:=Config.ReadBool('Player','IsPlayer2Bot',true);
    Player1Name:=Config.ReadString('Player','Player1Name','Player 1');
    Player2Name:=Config.ReadString('Player','Player2Name','Player 2');
    Skin:=Config.ReadString('View','Skin','Crystal');
  except
    // Do Nothing.
  end;
end;
//----------------------------------------------------------------------------//

//---TWinPosition-------------------------------------------------------------//
function TWinPosition.TinyPoint(x, y: byte): TTinyPoint;
begin
  Result.x:=x;
  Result.y:=y;
end;
//----------------------------------------------------------------------------//
procedure TWinPosition.ClearData;
begin
  Horizontal.Start:=TinyPoint(0,0);
  Horizontal.Parts:=0;
  Vertical.Start:=TinyPoint(0,0);
  Vertical.Parts:=0;
  LeftDiagonal.Start:=TinyPoint(0,0);
  LeftDiagonal.Parts:=0;
  RightDiagonal.Start:=TinyPoint(0,0);
  RightDiagonal.Parts:=0;
  _HorizontalWin:=false;
  _VerticalWin:=false;
  _LeftDiagonalWin:=false;
  _RightDiagonalWin:=false;
end;
//----------------------------------------------------------------------------//
constructor TWinPosition.Create;
begin
  ClearData;
end;
//----------------------------------------------------------------------------//
end.
