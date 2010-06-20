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

#include <algorithm>
#include <cmath>

#include <QApplication>
#include <QBitmap>
#include <QBoxLayout>
#include <QBrush>
#include <QDate>
#include <QDebug>
#include <QDialog>
#include <QDir>
#include <QFileInfo>
#include <QFont>
#include <QFormLayout>
#include <QGraphicsItem>
#include <QGraphicsRectItem>
#include <QGraphicsSimpleTextItem>
#include <QGraphicsView>
#include <QIcon>
#include <QLabel>
#include <QLineEdit>
#include <QImage>
#include <QMenu>
#include <QMessageBox>
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
#include <QTabWidget>
#include <QThread>
#include <QTime>
#include <QTimer>
#include <QVariant>
#include <QWidget>

#include <tinyxml/tinyxml.h>

#include <gettext_lib/QtGettext.h>
#include <VectorUtil.h>

#if defined(__WINDOWS__) && defined(__DEBUG__)
// Libraries and macros to detect memory leaks
#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>
#define DEBUG_NEW new(_NORMAL_BLOCK, __FILE__, __LINE__)
#define new DEBUG_NEW
#endif // __WINDOWS__ && __DEBUG__

#endif // STABLE_H
