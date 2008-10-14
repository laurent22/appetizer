#include "MathUtil.h"


double MathUtil::GetPointDistance(wxPoint a, wxPoint b) {
  return (double)(sqrt(pow((double)(b.x - a.x), 2.0) + pow((double)(b.y - a.y), 2)));
}