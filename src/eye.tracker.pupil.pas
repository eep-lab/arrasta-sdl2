{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit eye.tracker.pupil;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , SDL2
  , eye.tracker.types
  , eye.tracker.client
  , pupil.client;

type

  { TPupilEyeTracker }

  TPupilEyeTracker = class sealed (TEyeTrackerClient)
    private
      FMonitor : TSDL_Rect;
      FPupilClient : TPupilClient;
      FOnGazeOnScreenEvent : TGazeOnScreenEvent;
      procedure SurfaceEvent(Sender: TObject; AMultiPartMessage : TPupilMessage);
    protected
      function GetGazeOnScreenEvent: TGazeOnScreenEvent; override;
      procedure SetGazeOnScreenEvent(
        AGazeOnScreenEvent: TGazeOnScreenEvent); override;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Invalidate;
  end;

implementation

uses SimpleMsgPack, sdl.app.video.methods;

{ TPupilEyeTracker }

procedure TPupilEyeTracker.SetGazeOnScreenEvent(
  AGazeOnScreenEvent: TGazeOnScreenEvent);
begin
  if FOnGazeOnScreenEvent = AGazeOnScreenEvent then Exit;
  FOnGazeOnScreenEvent := AGazeOnScreenEvent;
end;

constructor TPupilEyeTracker.Create;
begin
  FPupilClient := TPupilClient.Create;
  FPupilClient.OnSurfacesEvent := @SurfaceEvent;
  FPupilClient.Start;
  FPupilClient.StartSubscriber;
  FPupilClient.Subscribe(SUB_SURFACES_EVENT);
  Invalidate;
end;

destructor TPupilEyeTracker.Destroy;
begin
  FPupilClient.Free;
  inherited Destroy;
end;

procedure TPupilEyeTracker.Invalidate;
begin
  FMonitor := MonitorFromWindow;
end;

procedure TPupilEyeTracker.SurfaceEvent(Sender: TObject;
  AMultiPartMessage: TPupilMessage);

var
  LGazeOnSurface : TSimpleMsgPack;
  LLastGazes : TGazes = nil;
  Li : integer;
  function NormToScreen(AGaze : TSimpleMsgPack) : TGaze;
  begin
    Result.X := Round(AGaze[0].AsFloat*FMonitor.w);
    Result.Y := Round((1.0 - AGaze[1].AsFloat)*FMonitor.h);
  end;

begin
  with AMultiPartMessage.Payload do begin
    if LowerCase(S['name']) = 'screen' then begin
      LGazeOnSurface := O['gaze_on_surfaces'];
      if LGazeOnSurface.Count > 0 then begin
        SetLength(LLastGazes, LGazeOnSurface.Count);
        for Li := Low(LLastGazes) to High(LLastGazes) do begin
          LLastGazes[Li] := NormToScreen(LGazeOnSurface[Li].O['norm_pos']);
        end;
        if Assigned(FOnGazeOnScreenEvent) then begin
          FOnGazeOnScreenEvent(Self, LLastGazes);
        end;
      end;
    end;
  end;
end;

function TPupilEyeTracker.GetGazeOnScreenEvent: TGazeOnScreenEvent;
begin
  Result := FOnGazeOnScreenEvent;
end;

end.

