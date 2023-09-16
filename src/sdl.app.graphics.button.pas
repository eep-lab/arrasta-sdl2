{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.graphics.button;

interface

uses
  Classes, SysUtils
  //, SDL2
  , sdl.app.graphics.picture
  , sdl.app.events.abstract;

type
  { TButton }

  TButton = class(TPicture)
  private
    FOnClick: TNotifyEvent;
  protected
    FIsPressed: Boolean;
    procedure MouseDown(Sender: TObject; Shift: TCustomShiftState; X, Y: Integer); override;
    procedure MouseUp(Sender: TObject; Shift: TCustomShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; virtual;
    procedure LoadFromFile(AFilename: string); override;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

implementation

//uses sdl.app.video.methods, sdl.colors;

{ TButton }

constructor TButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsPressed := False;
end;

procedure TButton.MouseDown(Sender: TObject; Shift: TCustomShiftState; X, Y: Integer);
begin
  if Visible then begin
    inherited MouseDown(Sender, Shift, X, Y);
    FIsPressed := True;
  end;
end;

procedure TButton.MouseUp(Sender: TObject; Shift: TCustomShiftState; X, Y: Integer);
begin
  if Visible then begin
    inherited MouseUp(Sender, Shift, X, Y);
    FIsPressed := False;
    // test if InRect(BoundsRect, Point(X, Y)) is needed
    if Assigned(OnClick) then
      OnClick(Self);
  end;
end;

procedure TButton.Paint;
begin
  inherited Paint;

end;

procedure TButton.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TButton.LoadFromFile(AFilename: string);
begin
  inherited LoadFromFile(AFilename);
  Height:=Height div 3;
end;

end.
