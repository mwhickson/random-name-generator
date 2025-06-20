{$ifdef FPC} {$mode objfpc}{$H+}{$J-} {$endif}
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}
{$R+}

program namegen;

const
	NameFile = './data/names.txt';

type
	NameArray = array[0..500] of string;
	NameSegmentArray = array[0..10000] of string;

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

function GetName(const data: string; const startsWith: char; const minimumLength: integer; const maximumLength: integer): string;
begin
	// TODO:
	Result := ''
end;

function GetNames(const data: string; const startsWith: char; const minimumLength: integer; const maximumLength: integer; const count: integer): NameArray;
begin
	// TODO:
	Result[0] := '';
end;

var
	NameData: NameSegmentArray;
	i: integer;
begin
	WriteLn('*** Random Name Generator ***');
	NameData := GetNameData(NameFile);
	{*
	for i := 0 to 10000 do
	begin
		if NameData[i] <> '' then
			WriteLn(NameData[i])
		else
		begin
			WriteLn(i, ' segments found');
			break;
		end;
	end;
	*}
end.

