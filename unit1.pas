unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MouseAndKeyInput, LCLType;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo2.Clear;
  Memo2.SetFocus;
  KeyInput.Press(memo1.Lines.Text);
  if Memo2.Lines.Text = Memo1.Lines.Text then Memo2.Lines.Add('Test Passed')
  else
    Memo2.Lines.Add('Test Failed!');

end;

procedure TForm1.Button2Click(Sender: TObject);
const
  LoopCount = 4;

var
  i: Integer;
begin
  for i := 1 to LoopCount do
  begin
    // Move cursor to (3792, 4018)
    //ExecuteProcess('/usr/bin/xdotool', ['mousemove', '3792', '4070']);
    //Sleep(95);
    //ExecuteProcess('/usr/bin/xdotool', ['mousemove', '3792', '4030']);
    //Sleep(95);
    // Left button click event at (3792, 4034)
    //ExecuteProcess('/usr/bin/xdotool', ['click', '1']);
    MouseInput.Move([],3792,4070,275);
    MouseInput.Move([],3800,4030,150);
    Sleep(700);
    MouseInput.Click(mbLeft,[],3792,4030);
    Sleep(700);

    // Move cursor to (3542, 3973)
    //ExecuteProcess('/usr/bin/xdotool', ['mousemove', '3542', '3973']);
    //Sleep(95);
    // Left button click event at (3542, 3973)
    //ExecuteProcess('/usr/bin/xdotool', ['click', '1']);
    MouseInput.Move([],3542,3973,150);
    Sleep(700);
    MouseInput.Click(mbLeft,[],3542,3973);
    Sleep(700);

    // Move cursor to (2306, 2565)
    //ExecuteProcess('/usr/bin/xdotool', ['mousemove', '2306', '2565']);

    // Left button click event at (2306, 2565)
    //ExecuteProcess('/usr/bin/xdotool', ['click', '1']);
    MouseInput.Move([],2306,2565,150);
    Sleep(700);
    MouseInput.Click(mbLeft,[],2306,2565);
    Sleep(700);
  end;

end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  button3.Caption:='Typing in 3 Seconds.....';
  Application.ProcessMessages;
  sleep(3000);
  KeyInput.Press(Memo1.Lines.Text);
  Button3.Caption:='Retype in 3 Seconds';

end;

procedure TForm1.Memo1Change(Sender: TObject);
begin

end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  mouseX, mouseY: Integer;
begin
  mouseX := Mouse.CursorPos.X;
  mouseY := Mouse.CursorPos.Y;
  //display the result
  label1.caption := 'Mouse position: (' + IntToStr(mouseX) + ', ' + IntToStr(mouseY) + ')';

end;

end.
