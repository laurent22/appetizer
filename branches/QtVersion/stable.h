/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef STABLE_H
#define STABLE_H

// Disable warning for using unsafe functions
#define _CRT_SECURE_NO_DEPRECATE 1
#define _T(x)      L ## x
#define _(x)       x
#define SAFE_DELETE(x)   if (x) { delete x; x = NULL; }

#ifdef __WINDOWS__
#include <windows.h>
#include <CommCtrl.h>
#include <commoncontrols.h>
#include <Guiddef.h>
#endif // __WINDOWS__

#include <cmath>

#include <QApplication>
#include <QBitmap>
#include <QBrush>
#include <QDate>
#include <QDebug>
#include <QDir>
#include <QFileInfo>
#include <QFont>
#include <QGraphicsItem>
#include <QGraphicsRectItem>
#include <QGraphicsSimpleTextItem>
#include <QGraphicsView>
#include <QIcon>
#include <QImage>
#include <QMutex>
#include <QMutexLocker>
#include <QPainter>
#include <QPainterPath>
#include <QPen>
#include <QPixmap>
#include <QPropertyAnimation>
#include <QRect>
#include <QRegExp>
#include <QResizeEvent>
#include <QSettings>
#include <QThread>
#include <QWidget>

#include <tinyxml/tinyxml.h>

#endif // STABLE_H
