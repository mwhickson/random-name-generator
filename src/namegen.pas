{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}
{$R+}

program namegen;

const
	NameFile = './data/names.txt';

type
	NameArray = array[0..500] of string;
	NameSegmentArray = array[0..10000] of string;
	LetterProbability = record
		Letter: char;
		Visits: integer;
	end;
	LetterBase = record
		Letter: char;
		NextLetter: array[0..25] of LetterProbability;
		Visits: integer;
	end;
	LetterMap = array[0..25] of LetterBase;

function GetNameData(filename: string): NameSegmentArray;
var
	f: TextFile;
	i: integer;
	s: string;
begin
	i := 0;
	Result[i] := '';
	AssignFile(f, filename);

	Reset(f);

	while not EOF(f) do
	begin
		ReadLn(f, s);
		Result[i] := s;
		i := i + 1;
	end;

	CloseFile(f);
end;

function GetName(const data: NameSegmentArray; const startsWith: char; const minimumLength: integer; const maximumLength: integer): string;
var
	map: LetterMap;
	i: integer;
begin
	
	{*
	WriteLn('GetName');
	for i := 0 to 10000 do
	begin
		if data[i] <> '' then
			//WriteLn(data[i])
		else
		begin
			WriteLn(i, ' segments found');
			break;
		end;
	end;
	Result := 'boo!';
	*}
end;

function GetNames(const data: NameSegmentArray; const startsWith: char; const minimumLength: integer; const maximumLength: integer; const count: integer): NameArray;
var
	i: integer;
begin
	Result[0] := '';

	for i := 0 to count - 1 do
	begin
		Result[i] := GetName(data, startsWith, minimumLength, maximumLength);
	end;
end;

var
	NameData: NameSegmentArray;
	Names: NameArray;
	i: integer;
begin
	Randomize;

	WriteLn('*** Random Name Generator ***');

	NameData := GetNameData(NameFile);

	Names := GetNames(NameData, '?', 3, 10, 1);

	for i := 0 to 500 do
	begin
		if Names[i] <> '' then
			WriteLn(Names[i])
		else
			break;
	end;
end.

