unit MathUtils;

interface

uses Math, Windows;

function PointDistance(p1, p2: TPoint): Real;


implementation


function PointDistance(p1, p2: TPoint): Real;
begin
  Result := sqrt(Power(p2.X - p1.X, 2) + Power(p2.Y - p1.Y, 2));
end;


end.
