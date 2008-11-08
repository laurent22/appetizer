/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "MathUtil.h"
#include <math.h>

double MathUtil::GetPointDistance(const wxPoint& a, const wxPoint& b) {
  double p1 = pow((double)(b.x - a.x), 2);
  double p2 = pow((double)(b.y - a.y), 2);
  return (double)(sqrt(p1 + p2));
}