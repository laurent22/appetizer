/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef STABLE_H
#define STABLE_H

#ifndef CALLBACK
#define CALLBACK __stdcall
#endif // CALLBACK

#define _T(x)      L ## x


#ifdef __WINDOWS__
#include <windows.h>
#include <CommCtrl.h>
#include <commoncontrols.h>
#include <Guiddef.h>
#endif // __WINDOWS__

#include <cmath>

#include <QApplication>
#include <QGraphicsItem>
#include <QGraphicsView>
#include <QIcon>
#include <QImage>
#include <QPainter>
#include <QPixmap>
#include <QRect>

#endif // STABLE_H
